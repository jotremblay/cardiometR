# Value vocabularies: phase labels, sex, and test modality.
#
# This is the module that protects a non-English user, and it is the cheapest
# one to get wrong quietly.
#
# The analysis layer compares the `phase` column against English words in six
# places across four files. A French export writes REPOS and EXERCICE, which
# match none of them, and nothing errors. Every breath is then treated as
# exercise, so the stage table, the resting values, and the threshold search
# are all wrong while looking entirely normal.
#
# Normalising the VALUES here means those six sites never have to change.


#' Canonical phase vocabulary
#'
#' Four lowercase words: `rest`, `warmup`, `exercise`, `recovery`.
#'
#' This set is not arbitrary. It is the largest set that every existing
#' downstream matcher already accepts, so normalising to it needs no change
#' anywhere else in the package. In particular `cooldown` is deliberately NOT
#' produced: the exclusion list in `filter_exercise_data()` holds the literal
#' `"cool"` and tests with `%in%`, not `grepl`, so a `cooldown` value would
#' slip through and contaminate the exercise window of every plot. Cooldown
#' folds into `recovery`, which every site accepts.
#'
#' @return A character vector of the canonical phase names.
#'
#' @keywords internal
canonical_phases <- function() {
  c("rest", "warmup", "exercise", "recovery")
}


# Default vocabularies. The dialect layer can extend these from YAML, but they
# are complete enough on their own that a French file works without one.
.phase_vocab <- c(
  # rest
  rest = "rest", resting = "rest", baseline = "rest", pre = "rest",
  pretest = "rest", repos = "rest", aurepos = "rest", base = "rest",
  reposinitial = "rest",
  # warm up
  warmup = "warmup", warm = "warmup", warmingup = "warmup",
  echauffement = "warmup", misedntrain = "warmup", misenrtain = "warmup",
  miseentrain = "warmup", chauffe = "warmup",
  # exercise
  exercise = "exercise", exercice = "exercise", exer = "exercise",
  ex = "exercise", work = "exercise", load = "exercise", test = "exercise",
  effort = "exercise", travail = "exercise", charge = "exercise",
  epreuve = "exercise", incrementall = "exercise", ramp = "exercise",
  rampe = "exercise",
  # recovery
  recovery = "recovery", recover = "recovery", cooldown = "recovery",
  cool = "recovery", cooling = "recovery", post = "recovery",
  postexercise = "recovery", recuperation = "recovery", recup = "recovery",
  retouraucalme = "recovery", recuperationactive = "recovery",
  postefort = "recovery"
)

.sex_vocab <- c(
  m = "M", male = "M", man = "M", masculin = "M", homme = "M", h = "M",
  hombre = "M", masc = "M",
  f = "F", female = "F", woman = "F", feminin = "F", femme = "F",
  fem = "F", mujer = "F"
)

# Matched against the protocol and ergometer text, which is free prose.
.modality_patterns <- c(
  treadmill = "tapis|treadmill|bruce|balke|naughton|course|marche|running|walk|run|ergotapis",
  cycling = "velo|cycle|cycling|bike|bicyclette|ergocycle|cycloergometre|ergometre|ergometer|watt|lode|excalibur|monark"
)


#' Map free-text phase labels to the canonical vocabulary
#'
#' Unrecognised labels become `NA`, never a guess. Every downstream site either
#' guards with `!is.na(phase)` or tolerates `NA`, so the pipeline falls back to
#' power-based stage detection, which is the right conservative answer.
#'
#' @param x Character vector of phase labels as written in the file.
#' @param vocab Optional named character vector, normalised key to canonical
#'   phase, merged over the built-in vocabulary.
#'
#' @return A list with `values` (the canonical vector), `table` (a data frame
#'   of raw label, canonical name and row count) and `unmapped` (the distinct
#'   labels that matched nothing).
#'
#' @keywords internal
normalize_phase <- function(x, vocab = NULL) {
  raw <- as.character(x)
  lookup <- .phase_vocab
  if (length(vocab) > 0) {
    lookup[names(vocab)] <- unname(vocab)
  }

  keys <- norm_key(raw)
  out <- unname(lookup[keys])

  # "PALIER 3", "STAGE 4", "ETAPE 2" are stage counters written into the phase
  # column. They mean exercise.
  stage_like <- is.na(out) &
    grepl("^(palier|stage|etape|step|niveau|level|phase)[0-9]*$", keys)
  out[stage_like] <- "exercise"

  # A recognised "no phase here" marker becomes NA like anything else, but
  # there is nothing for the user to fix, so it is not reported.
  known_absent <- !is.na(out) & out == "none"
  out[known_absent] <- NA_character_

  present <- nzchar(keys)
  unmapped <- unique(raw[is.na(out) & present & !known_absent])

  counts <- as.data.frame(
    table(raw = raw, canonical = ifelse(is.na(out), NA_character_, out),
          useNA = "ifany"),
    stringsAsFactors = FALSE
  )
  counts <- counts[counts$Freq > 0, , drop = FALSE]
  names(counts)[names(counts) == "Freq"] <- "n"
  counts <- counts[order(-counts$n), , drop = FALSE]
  rownames(counts) <- NULL

  list(values = out, table = counts, unmapped = unmapped)
}


#' Map a sex label to M, F, or O
#'
#' Note the `H`/`F` trap: in French `H` is homme and `F` is femme, while in
#' English `F` is female. Both resolve correctly here only because `H` appears
#' in no English vocabulary. A future Dutch block, where `V` is vrouw, would
#' need the same care.
#'
#' @param x A sex label.
#' @param vocab Optional named character vector merged over the built-in one.
#'
#' @return `"M"`, `"F"`, or `"O"` when nothing matches.
#'
#' @keywords internal
normalize_sex <- function(x, vocab = NULL) {
  lookup <- .sex_vocab
  if (length(vocab) > 0) {
    lookup[names(vocab)] <- unname(vocab)
  }
  if (length(x) == 0) {
    return("O")
  }
  hit <- unname(lookup[norm_key(x)[[1L]]])
  if (is.na(hit)) "O" else hit
}


#' Guess the test modality from protocol or ergometer text
#'
#' `detect_modality()` greps the protocol string for English words, so a French
#' protocol name matches nothing and the modality falls back to whatever the
#' data happens to suggest. Reading the text here, in both languages, gives the
#' metadata a modality it can state outright.
#'
#' @param text Character vector; all elements are considered together.
#' @param patterns Optional named character vector of regular expressions,
#'   modality name to pattern, merged over the built-in set.
#'
#' @return `"treadmill"`, `"cycling"`, or `NA` when the text says nothing.
#'
#' @keywords internal
detect_modality_from_text <- function(text, patterns = NULL) {
  rules <- .modality_patterns
  if (length(patterns) > 0) {
    rules[names(patterns)] <- unname(patterns)
  }
  key <- norm_key(paste(stats::na.omit(as.character(text)), collapse = " "))
  if (!nzchar(key)) {
    return(NA_character_)
  }
  for (modality in names(rules)) {
    if (grepl(rules[[modality]], key)) {
      return(modality)
    }
  }
  NA_character_
}

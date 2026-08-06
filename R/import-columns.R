# Column-name normalisation and alias resolution.
#
# These helpers are pure: they take character vectors and lookup tables and
# return data. They know nothing about files, dialects, or S7 classes. The
# dialect layer builds the lookup tables and calls into here.

# Accent folding table. Written as \u escapes so this source file stays ASCII
# and behaves identically whatever locale R is started in.
#
# iconv(x, to = "ASCII//TRANSLIT") is NOT usable for this. On macOS it turns
# an accented uppercase E into a stray apostrophe plus E, and it
# returns NA outright for strings containing a middle dot or superscripts.
.accent_from <- paste0(
  "\u00e1\u00e0\u00e2\u00e4\u00e3\u00e5",   # a with acute, grave, circumflex...
  "\u00e9\u00e8\u00ea\u00eb",               # e
  "\u00ed\u00ec\u00ee\u00ef",               # i
  "\u00f3\u00f2\u00f4\u00f6\u00f5",         # o
  "\u00fa\u00f9\u00fb\u00fc",               # u
  "\u00e7\u00f1\u00fd\u00ff",               # c cedilla, n tilde, y
  "\u00c1\u00c0\u00c2\u00c4\u00c3\u00c5",   # A
  "\u00c9\u00c8\u00ca\u00cb",               # E
  "\u00cd\u00cc\u00ce\u00cf",               # I
  "\u00d3\u00d2\u00d4\u00d6\u00d5",         # O
  "\u00da\u00d9\u00db\u00dc",               # U
  "\u00c7\u00d1\u00dd"                      # C, N, Y
)
.accent_to <- paste0(
  "aaaaaa", "eeee", "iiii", "ooooo", "uuuu", "cnyy",
  "AAAAAA", "EEEE", "IIII", "OOOOO", "UUUU", "CNY"
)

# Subscript and superscript digits, so that a typographic V-dot-O-subscript-2
# keeps its digit instead of losing it to the punctuation strip.
.subscript_digits <- paste0(
  "\u2080\u2081\u2082\u2083\u2084",
  "\u2085\u2086\u2087\u2088\u2089"
)
.superscript_digits <- paste0(
  "\u2070\u00b9\u00b2\u00b3\u2074",
  "\u2075\u2076\u2077\u2078\u2079"
)


#' Fold a label to a comparison key
#'
#' Reduces a column header or a label cell to a form that can be compared
#' across languages, cases, and typographic conventions: accents removed,
#' combining marks dropped, sub- and superscript digits made ordinary, ligatures
#' expanded, everything lower case, and every non-alphanumeric character
#' removed.
#'
#' Dropping the combining marks and mapping the subscripts is not cosmetic.
#' Without them a typographic "VE/VO2" written with overdots and a
#' subscript folds to `vevo`, losing the trailing digit, and the column stops
#' matching its own alias.
#'
#' @param x A character vector. `NA` becomes `""`.
#'
#' @return A character vector of keys, the same length as `x`. Never `NA`.
#'
#' @keywords internal
norm_key <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""

  # Combining diacritics, including the overdot in V-dot-O2.
  x <- gsub("[\u0300-\u036f]", "", x, perl = TRUE)

  x <- chartr(.subscript_digits, "0123456789", x)
  x <- chartr(.superscript_digits, "0123456789", x)

  x <- gsub("\u00df", "ss", x, fixed = TRUE)
  x <- gsub("\u0153", "oe", x, fixed = TRUE)
  x <- gsub("\u0152", "OE", x, fixed = TRUE)
  x <- gsub("\u00e6", "ae", x, fixed = TRUE)
  x <- gsub("\u00c6", "AE", x, fixed = TRUE)

  x <- chartr(.accent_from, .accent_to, x)
  tolower(gsub("[^A-Za-z0-9]+", "", x))
}


# Anchored so it only fires on a trailing parenthetical. perl = TRUE matters:
# in the default engine the bracket expression terminates early and this
# silently matches nothing.
.header_paren_re <- "^(.*?)\\s*[\\(\\[]([^\\)\\]]*)[\\)\\]]\\s*$"

#' Split a trailing unit off a header
#'
#' `"Height (cm)"` becomes base `"Height"` and unit `"cm"`. This does two jobs.
#' It stops `"Height (cm)"` folding to the useless key `heightcm`, and it
#' recovers a unit for files that have no separate units row.
#'
#' @param x A character vector of headers.
#'
#' @return A list with `base` and `unit`, both the same length as `x`. `unit` is
#'   `NA` where the header carried no parenthetical.
#'
#' @keywords internal
split_header <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)

  matches <- regmatches(x, regexec(.header_paren_re, x, perl = TRUE))
  matched <- lengths(matches) == 3L

  base <- x
  unit <- rep(NA_character_, length(x))
  if (any(matched)) {
    base[matched] <- trimws(vapply(matches[matched], `[`, character(1), 2L))
    unit[matched] <- trimws(vapply(matches[matched], `[`, character(1), 3L))
  }

  # A header that is nothing BUT a parenthetical keeps its original text;
  # otherwise "(n/a)" would collapse to an empty key and match everything.
  empty_base <- !nzchar(base)
  base[empty_base] <- x[empty_base]
  unit[empty_base] <- NA_character_

  list(base = base, unit = unit)
}


#' Resolve source column names to canonical names
#'
#' Matching is by exact alias only, after folding with [norm_key()]. Fuzzy
#' matching is deliberately not used: the canonical CPET names sit one edit
#' apart from each other (`vo2`/`vco2`, `ve`/`vt`, `rq`/`rf`, `peo2`/`feo2`), so
#' any edit-distance matcher can map VCO2 onto VO2 and produce an RER of 1.0
#' everywhere with no error at all. Near misses become suggestions for the user
#' to act on, never silent substitutions.
#'
#' @param raw_names Character vector of source column headers.
#' @param lookup Named character vector mapping a normalised key to a canonical
#'   column name.
#' @param ignore Character vector of source headers that are known to this
#'   format but unused, so they can be dropped without a warning.
#' @param ignore_patterns Character vector of regular expressions, matched
#'   against the folded header, for whole families of unused columns such as
#'   the 24 ECG channels.
#' @param user_mapping Optional named character vector, `c("Raw Header" =
#'   "canonical")`, which overrides `lookup`.
#'
#' @return A list with:
#'   \describe{
#'     \item{mapping}{Named character vector, source name to canonical name,
#'       `NA` where unmatched.}
#'     \item{unknown}{Source names that matched nothing and are not ignored.}
#'     \item{ignored}{Source names dropped by the ignore list.}
#'     \item{conflicts}{Data frame of canonical names claimed more than once.}
#'     \item{suggestions}{Named list of near-miss hints for `unknown`.}
#'     \item{inline_units}{Named character vector of units taken from trailing
#'       parentheticals.}
#'   }
#'
#' @keywords internal
resolve_columns <- function(raw_names, lookup, ignore = character(),
                            ignore_patterns = character(),
                            user_mapping = NULL) {
  raw_names <- as.character(raw_names)
  parts <- split_header(raw_names)
  keys <- norm_key(parts$base)

  hit <- unname(lookup[keys])

  if (length(user_mapping) > 0) {
    idx <- match(norm_key(names(user_mapping)), keys)
    keep <- !is.na(idx)
    hit[idx[keep]] <- unname(user_mapping[keep])
  }

  # Two source columns claiming the same canonical name. Keep the one with a
  # closer match to the alias, then the leftmost, and report the rest.
  conflicts <- data.frame(
    canonical = character(),
    kept = character(),
    dropped = character(),
    stringsAsFactors = FALSE
  )
  for (canon in unique(hit[!is.na(hit)])) {
    claimants <- which(!is.na(hit) & hit == canon)
    if (length(claimants) < 2L) next
    keep <- claimants[[1L]]
    conflicts <- rbind(conflicts, data.frame(
      canonical = canon,
      kept = raw_names[[keep]],
      dropped = paste(raw_names[setdiff(claimants, keep)], collapse = ", "),
      stringsAsFactors = FALSE
    ))
    hit[setdiff(claimants, keep)] <- NA_character_
  }

  unmatched <- raw_names[is.na(hit)]
  unmatched_keys <- norm_key(split_header(unmatched)$base)
  is_ignored <- unmatched_keys %in% norm_key(ignore)
  for (pattern in ignore_patterns) {
    is_ignored <- is_ignored | grepl(pattern, unmatched_keys)
  }
  ignored <- unmatched[is_ignored]
  unknown <- unmatched[!is_ignored]

  list(
    mapping = stats::setNames(hit, raw_names),
    unknown = unknown,
    ignored = ignored,
    conflicts = conflicts,
    suggestions = suggest_aliases(unknown, lookup),
    inline_units = stats::setNames(parts$unit, raw_names)
  )
}


#' Suggest close alias matches for unrecognised headers
#'
#' Used only to write a better message. The result is never applied.
#'
#' @param unknown Character vector of unrecognised source headers.
#' @param lookup Named character vector, normalised key to canonical name.
#' @param max_dist Largest edit distance to report.
#'
#' @return A named list, one entry per header that has any near match.
#'
#' @keywords internal
suggest_aliases <- function(unknown, lookup, max_dist = 2L) {
  if (length(unknown) == 0 || length(lookup) == 0) {
    return(list())
  }
  known <- names(lookup)
  out <- lapply(unknown, function(header) {
    key <- norm_key(split_header(header)$base)
    # Short names are all within two edits of each other, so a suggestion for
    # one would be noise rather than help. "S V3" is not a misspelling of
    # anything; the twelve ECG channels would each sprout half a dozen
    # meaningless hints.
    if (nchar(key) < 4L) return(character())
    distances <- utils::adist(key, known)[1, ]
    # Never suggest an alias so short that the distance is most of its length.
    near <- known[distances <= max_dist & distances > 0 &
                    nchar(known) >= nchar(key) - max_dist]
    if (length(near) == 0) return(character())
    utils::head(unique(sprintf("%s (%s)", near, lookup[near])), 3L)
  })
  names(out) <- unknown
  out[lengths(out) > 0]
}

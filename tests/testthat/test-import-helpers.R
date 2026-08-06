# Unit tests for the pure import helpers.
#
# These functions have no callers yet. Testing them before wiring them up is
# deliberate: it means the engine that uses them is assembled from verified
# parts rather than debugged as a whole against one file.


# ---- format_age ------------------------------------------------------------

test_that("age is shown in completed years", {
  # COSMED stores age as a fraction of a year, computed from date of birth and
  # test date. A real export reads 31.2902181427408.
  expect_identical(format_age(31.2902181427408), "31")
  expect_identical(format_age(64), "64")

  # Completed years, not rounded years: at 31.9 a person is still 31.
  expect_identical(format_age(31.9), "31")

  expect_identical(format_age(NA_real_), "--")
  expect_identical(format_age(numeric()), "--")
})


# ---- norm_key --------------------------------------------------------------

test_that("norm_key folds case, accents, and punctuation", {
  expect_identical(norm_key("ÉCHAUFFEMENT"), "echauffement")
  expect_identical(norm_key("Récupération"), "recuperation")
  expect_identical(norm_key("Âge"), "age")
  expect_identical(norm_key("D.O.B."), "dob")
  expect_identical(norm_key("VE/VO2"), "vevo2")
  expect_identical(norm_key("  Ambient Relative Humidity  "),
                   "ambientrelativehumidity")
})

test_that("norm_key keeps digits written as subscripts or after an overdot", {
  # A typographic VE/VO2: V + combining dot above, O + subscript two. Without
  # the combining-mark strip and the subscript map this folds to "vevo" and the
  # column silently stops matching its own alias.
  typographic <- "V̇E/V̇O₂"
  expect_identical(norm_key(typographic), "vevo2")
  expect_identical(norm_key("V̇O₂"), "vo2")
})

test_that("norm_key never returns NA, where iconv would", {
  # iconv(x, to = "ASCII//TRANSLIT") returns NA for these on macOS, and
  # inserts a stray apostrophe before an accented capital.
  hard <- c(NA, "", "Échauffement", "km·h⁻¹",
            "V̇O₂")
  keys <- norm_key(hard)
  expect_false(any(is.na(keys)))
  expect_length(keys, length(hard))
  expect_false(any(grepl("'", keys, fixed = TRUE)))
})

test_that("norm_key expands ligatures", {
  expect_identical(norm_key("œsophage"), "oesophage")
  expect_identical(norm_key("Straße"), "strasse")
})


# ---- split_header ----------------------------------------------------------

test_that("split_header separates a trailing parenthetical unit", {
  parts <- split_header(c("Height (cm)", "PeO2 [mmHg]", "VE/VO2",
                          "Vitesse (km/h)", "Ambient Humidity  "))
  expect_identical(parts$base,
                   c("Height", "PeO2", "VE/VO2", "Vitesse", "Ambient Humidity"))
  expect_identical(parts$unit, c("cm", "mmHg", NA, "km/h", NA))
})

test_that("split_header leaves a header that is only a parenthetical alone", {
  parts <- split_header("(n/a)")
  expect_identical(parts$base, "(n/a)")
  expect_true(is.na(parts$unit))
})


# ---- resolve_columns -------------------------------------------------------

test_lookup <- function() {
  c(t = "time_s", temps = "time_s",
    vo2 = "vo2_ml", vco2 = "vco2_ml",
    ve = "ve_l", ventilation = "ve_l", vt = "vt_l",
    rq = "rer", rer = "rer", rf = "bf",
    peo2 = "peto2_mmhg", feo2 = "feo2_pct",
    power = "power_w", puissance = "power_w",
    fc = "hr_bpm", hr = "hr_bpm")
}

test_that("the near-miss canonical names stay distinct", {
  # Every one of these pairs is a single edit apart, which is why no fuzzy
  # matcher can be allowed near this problem. Confusing VCO2 with VO2 gives an
  # RER of exactly 1.0 everywhere and a V-slope of exactly 1, with no error.
  resolved <- resolve_columns(
    c("VO2", "VCO2", "VE", "VT", "RQ", "Rf", "PeO2", "FeO2"),
    test_lookup()
  )
  expect_identical(
    unname(resolved$mapping),
    c("vo2_ml", "vco2_ml", "ve_l", "vt_l", "rer", "bf",
      "peto2_mmhg", "feo2_pct")
  )
})

test_that("a near miss is suggested but never applied", {
  resolved <- resolve_columns(c("t", "VCO2", "Puissanc"), test_lookup())

  expect_true(is.na(resolved$mapping[["Puissanc"]]))
  expect_true("Puissanc" %in% resolved$unknown)
  expect_match(paste(unlist(resolved$suggestions), collapse = " "), "power_w")
})

test_that("two columns claiming one canonical name is reported, not merged", {
  resolved <- resolve_columns(c("VE", "Ventilation"), test_lookup())

  expect_equal(nrow(resolved$conflicts), 1L)
  expect_identical(resolved$conflicts$canonical, "ve_l")
  expect_identical(resolved$conflicts$kept, "VE")
  expect_equal(sum(resolved$mapping == "ve_l", na.rm = TRUE), 1L)
})

test_that("a user mapping overrides the alias table", {
  resolved <- resolve_columns(
    c("Debit ventilatoire", "VO2"),
    test_lookup(),
    user_mapping = c("Debit ventilatoire" = "ve_l")
  )
  expect_identical(resolved$mapping[["Debit ventilatoire"]], "ve_l")
})

test_that("known-but-unused columns are ignored quietly", {
  resolved <- resolve_columns(
    c("VO2", "GPS Dist.", "Nonsense"),
    test_lookup(),
    ignore = c("GPS Dist.")
  )
  expect_identical(resolved$ignored, "GPS Dist.")
  expect_identical(resolved$unknown, "Nonsense")
})

test_that("French headers resolve through the same table", {
  resolved <- resolve_columns(c("Temps", "Puissance", "FC"), test_lookup())
  expect_identical(unname(resolved$mapping),
                   c("time_s", "power_w", "hr_bpm"))
})


# ---- units -----------------------------------------------------------------

test_that("parse_unit canonicalises spelling, case, and notation", {
  expect_identical(parse_unit(c("mL/min", "ml/min", "ML/MIN", "cc/min")),
                   rep("mL/min", 4))
  expect_identical(parse_unit(c("L/min", "l/min", "L(btps)/min")),
                   c("L/min", "L/min", "L/min"))
  expect_identical(parse_unit(c("m/s", "km/h", "km·h⁻¹", "mph")),
                   c("m/s", "km/h", "km/h", "mph"))
  expect_identical(parse_unit(c("°C", "C", "degC")), rep("degC", 3))
  expect_identical(parse_unit(c("mmHg", "Torr", "kPa")),
                   c("mmHg", "mmHg", "kPa"))
  expect_identical(parse_unit("L(btps)"), "L")
  expect_identical(parse_unit("Watt"), "W")
})

test_that("dimensionless placeholders are not units but do mark a units row", {
  expect_true(all(is.na(parse_unit(c("---", "-", "")))))
  # A COSMED units row is mostly "---", so refusing those would make the row
  # impossible to find.
  expect_true(all(is_unit_token(c("s", "---", "mL/min", "%", "bpm"))))
  expect_false(any(is_unit_token(c("713.5", "REST"))))
})

test_that("convert_unit reports the factor it applied", {
  converted <- convert_unit(c(1.5, 3.0), "vo2_ml", from = "L/min")
  expect_equal(converted$values, c(1500, 3000))
  expect_equal(converted$factor, 1000)
  expect_identical(converted$to, "mL/min")

  speed <- convert_unit(c(0, 4), "speed_kmh", from = "m/s")
  expect_equal(speed$values, c(0, 14.4))
  expect_equal(speed$factor, 3.6)
})

test_that("convert_unit leaves values alone when nothing is known", {
  same <- convert_unit(c(1, 2), "vo2_ml", from = NA_character_)
  expect_equal(same$values, c(1, 2))
  expect_equal(same$factor, 1)

  dimensionless <- convert_unit(c(0.85, 1.1), "rer", from = "---")
  expect_equal(dimensionless$values, c(0.85, 1.1))
})

test_that("convert_unit refuses an unknown conversion loudly", {
  expect_warning(
    result <- convert_unit(c(1, 2), "vo2_ml", from = "mmHg"),
    "No conversion"
  )
  expect_equal(result$values, c(1, 2))
  expect_identical(result$source, "unconverted")
})

test_that("magnitude inference catches VO2 reported in L/min", {
  litres <- infer_unit_from_magnitude(c(0.4, 1.2, 3.5), "vo2_ml")
  expect_identical(litres$unit, "L/min")
  expect_identical(litres$confidence, "high")

  millilitres <- infer_unit_from_magnitude(c(400, 1200, 3500), "vo2_ml")
  expect_identical(millilitres$unit, "mL/min")
})

test_that("magnitude inference catches an Excel day fraction", {
  # The real COSMED file declares this column as seconds while storing day
  # fractions, so the heuristic has to be allowed to overrule the units row.
  fractions <- infer_unit_from_magnitude(c(3.47e-4, 1.48e-2), "time_s")
  expect_identical(fractions$unit, "day")
  expect_identical(fractions$confidence, "high")

  seconds <- infer_unit_from_magnitude(c(30, 600, 1281), "time_s")
  expect_identical(seconds$unit, "s")
})

test_that("as_seconds parses clock notation that as.numeric drops", {
  expect_equal(as_seconds(c("00:10:30", "1:00:00", "05:30")),
               c(630, 3600, 330))
  expect_equal(as_seconds(c("30", "60,5")), c(30, 60.5))
  expect_true(is.na(as_seconds("junk")))

  # The behaviour this replaces.
  expect_true(is.na(suppressWarnings(as.numeric("00:10:30"))))
})

test_that("as_numeric_loose handles a French decimal comma", {
  expect_equal(as_numeric_loose(c("1,5", "2.5", "3")), c(1.5, 2.5, 3))
  # A comma alongside a dot is a thousands separator, not a decimal mark.
  expect_equal(as_numeric_loose("1,234.5"), 1234.5)
  expect_true(is.na(as_numeric_loose("---")))
})


# ---- value vocabularies ----------------------------------------------------

test_that("French phase labels map to the canonical vocabulary", {
  mapped <- normalize_phase(c("REPOS", "ÉCHAUFFEMENT", "EXERCICE",
                              "RÉCUPÉRATION", "Recup", "PALIER 3"))
  expect_identical(
    mapped$values,
    c("rest", "warmup", "exercise", "recovery", "recovery", "exercise")
  )
  expect_length(mapped$unmapped, 0)
})

test_that("English phase labels still map, whatever their case", {
  mapped <- normalize_phase(c("REST", "WARMUP", "EXERCISE", "Cooldown"))
  # Cooldown folds into recovery on purpose; see canonical_phases().
  expect_identical(mapped$values,
                   c("rest", "warmup", "exercise", "recovery"))
})

test_that("an unrecognised phase becomes NA rather than a guess", {
  mapped <- normalize_phase(c("REST", "ZZZ"))
  expect_identical(mapped$values, c("rest", NA_character_))
  expect_identical(mapped$unmapped, "ZZZ")
})

test_that("the canonical phase vocabulary satisfies every downstream matcher", {
  # This test is the guard against anyone later "simplifying" the vocabulary
  # and quietly reintroducing the silent-failure bug. Each expectation mirrors
  # a real matcher in the analysis layer.
  canonical <- canonical_phases()
  expect_identical(canonical, c("rest", "warmup", "exercise", "recovery"))

  # methods-stages.R: exercise phases
  expect_true(grepl("exercise|work|exer", "exercise"))

  # methods-stages.R: the non-exercise regex
  expect_true(all(grepl("rest|warmup|warm.?up|recovery|cool",
                        c("rest", "warmup", "recovery"))))

  # methods-stages.R: the %in% list
  expect_true(all(c("rest", "warmup", "recovery") %in%
    c("rest", "warmup", "warm-up", "warm up", "recovery",
      "cooldown", "cool-down", "cool down", "cool")))

  # methods-quality.R: exercise and rest lists
  expect_true("exercise" %in% c("exercise", "ex", "work", "load"))
  expect_true(all(c("rest", "warmup") %in%
                    c("rest", "warmup", "warm-up", "baseline")))

  # methods-validate.R: exact equality, the strictest constraint of all
  expect_identical(tolower("rest"), "rest")

  # plots.R filter_exercise_data(): exact %in% against a list holding "cool",
  # NOT "cooldown". This is precisely why cooldown is folded into recovery.
  exclusion_list <- c("rest", "warmup", "recovery", "cool")
  expect_true(all(setdiff(canonical, "exercise") %in% exclusion_list))
  expect_false("cooldown" %in% exclusion_list)

  # And the exercise phase must NOT be excluded from the exercise window.
  expect_false("exercise" %in% exclusion_list)
})

test_that("French sex labels map to M and F", {
  labels <- c("Homme", "Femme", "Masculin", "Féminin", "H", "F", "M",
              "Male", "Female")
  expect_identical(
    vapply(labels, normalize_sex, character(1), USE.NAMES = FALSE),
    c("M", "F", "M", "F", "M", "F", "M", "M", "F")
  )
})

test_that("an unknown sex label becomes O", {
  expect_identical(normalize_sex("unspecified"), "O")
  expect_identical(normalize_sex(NA), "O")
  expect_identical(normalize_sex(character()), "O")
})

test_that("modality is detected from French and English protocol text", {
  expect_identical(detect_modality_from_text("Rampe vélo 25W/min"), "cycling")
  expect_identical(detect_modality_from_text("Tapis roulant incrémental"),
                   "treadmill")
  expect_identical(detect_modality_from_text("Bruce protocol"), "treadmill")
  expect_identical(detect_modality_from_text("Lode Excalibur"), "cycling")
  expect_true(is.na(detect_modality_from_text("")))
  expect_true(is.na(detect_modality_from_text("Protocol 3")))
})

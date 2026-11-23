suppressMessages(library(cardiometR))

# Example: a plain-language, two-page French report for cycling step tests.
#
# Usage: Rscript make_report.R <file.xlsx> [template.typ] [output.pdf]
#   template.typ  defaults to cpet_report_udem.typ next to this script
#   output.pdf    defaults to <file>_report.pdf next to the input file
#
# Assumes a cycling step test with a warm-up phase, then an "exercise" phase
# whose first power is the starting power of the test.
args <- commandArgs(trailingOnly = TRUE)
f <- args[1]
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
tpl <- if (length(args) >= 2 && nzchar(args[2])) args[2] else file.path(script_dir, "cpet_report_udem.typ")

# Protocol and pre-test conditions applied to every participant: edit as needed.
STAGE_S <- 180            # step duration (s)
INCR_W <- 25              # step increment (W)
NUTRITIONAL_STATE <- "fed"  # "fed" or "fasted"
CAFFEINE <- FALSE         # caffeine before the test

d <- read_cpet(f)
d@participant@age <- as.integer(round(d@participant@age))  # decimal age breaks %d in the package

# starting power = first power of the exercise phase (right after warm-up)
b <- d@breaths
e <- b[which(b$phase == "exercise" & !is.na(b$power_w)), ]
start_w <- e$power_w[1]

settings <- list(protocol = "step", starting_intensity = start_w,
                 stage_duration = STAGE_S, increment_size = INCR_W,
                 threshold_methods = "ve_vco2")
a <- analyze_cpet(d, settings = settings)

# stages: renumber from 1, full stages = 180 s, last stage = real duration
ss <- a@stage_summary
ss <- ss[order(ss$power_w), ]
n <- nrow(ss)
last_p <- ss$power_w[n]
tt <- e$time_s[e$power_w == last_p]
step <- stats::median(diff(e$time_s))
last_dur <- max(tt) + step - min(tt)
ss$stage <- seq_len(n)
ss$stage_name <- paste("Stage", seq_len(n))
ss$duration_s <- c(rep(STAGE_S, n - 1), min(last_dur, STAGE_S))
a@stage_summary <- ss

# recompute MAP/PPO (Kuipers) from the corrected durations
m <- compute_map_kuipers(ss)
a@map_watts <- m$map_watts
a@ppo_watts <- m$ppo_watts
a@kuipers_fraction <- m$kuipers_fraction
a@map_per_kg <- m$ppo_watts / d@participant@weight_kg

# the report label reads this slot (file sampling = 10 s); the analysis itself averages over 30 s
a@data@averaging_window <- 30

a@pre_test_conditions@nutritional_state <- NUTRITIONAL_STATE
a@pre_test_conditions@caffeine_intake <- CAFFEINE

# population comparison: one range bar per metric (replaces table + Z-score strip)
comparison_block <- function(a, d) {
  p <- d@participant
  st <- get_normative_data(sport = default_sport_for_modality("cycling"), level = "recreational",
                           sex = as.character(p@sex), age = p@age)
  wt <- p@weight_kg
  rows <- list(
    list("Aptitude aérobie (VO#sub[2]max / VO#sub[2]pic)", a@peaks@vo2_kg_peak, st$vo2max_low, st$vo2max_high, st$vo2max_typical, "mL/kg/min", 1),
    list("Puissance maximale relative", a@map_per_kg, st$map_per_kg_low, st$map_per_kg_high, st$map_per_kg_typical, "W/kg", 2),
    list("Puissance maximale", a@ppo_watts, st$map_per_kg_low * wt, st$map_per_kg_high * wt, st$map_per_kg_typical * wt, "W", 0)
  )
  ge <- tryCatch(a@economy_metrics@gross_efficiency, error = function(e) numeric(0))
  if (length(ge) == 1 && is.finite(ge) && is.finite(st$efficiency_low))
    rows[[4]] <- list("Efficacité brute", ge, st$efficiency_low, st$efficiency_high, st$efficiency_typical, "%", 1)
  one <- function(r) {
    v <- r[[2]]; lo <- r[[3]]; hi <- r[[4]]; mu <- r[[5]]; dec <- r[[7]]
    ax_lo <- max(0, min(lo, v) * 0.8); ax_hi <- max(hi, v) * 1.08
    pc <- function(x) round(100 * (x - ax_lo) / (ax_hi - ax_lo), 1)
    fmt <- function(x) formatC(x, format = "f", digits = dec, decimal.mark = ",")
    status <- if (v > hi) "Au-dessus de la plage du groupe" else if (v < lo) "En dessous de la plage du groupe" else "Dans la plage habituelle"
    sprintf('#block(breakable: false, width: 100%%, above: 1.4mm)[
  #grid(columns: (40mm, 1fr, 38mm), column-gutter: 3mm, align: horizon,
    text(size: 8.5pt, weight: 600)[%s],
    box(width: 100%%, height: 7mm)[
      #place(top + left, dy: 2.6mm, rect(width: 100%%, height: 1.8mm, fill: rule_hair, radius: 1mm))
      #place(top + left, dx: %s%%, dy: 1.8mm, rect(width: %s%%, height: 3.4mm, fill: rgb("#c3c6cb"), radius: 0.6mm))
      #place(top + left, dx: %s%%, dy: 1.2mm, rect(width: 0.5mm, height: 4.6mm, fill: rgb("#6b7078")))
      #place(top + left, dx: %s%%, dy: 0.3mm, move(dx: -0.5mm, rect(width: 1mm, height: 6.4mm, fill: rgb("#D62839"), radius: 0.4mm)))
    ],
    [#text(size: 10pt, weight: 700, fill: rgb("#D62839"))[%s] #text(size: 7pt, fill: faint)[%s]#linebreak()#text(size: 7pt, fill: muted)[%s]]
  )
  #v(-0.8mm)
  #pad(left: 43mm, text(size: 7pt, fill: muted)[Groupe de comparaison : %s à %s (moyenne %s)])
]', r[[1]], pc(lo), pc(hi) - pc(lo), pc(mu), pc(v), fmt(v), r[[6]], status, fmt(lo), fmt(hi), fmt(mu))
  }
  legend <- '#block(above: 1.5mm, below: 0mm, fill: tint, radius: 1mm, inset: (x: 3mm, y: 2mm))[#text(size: 8pt, fill: mid_ink)[
  #box(rect(width: 1mm, height: 4mm, fill: rgb("#D62839"), radius: 0.4mm)) #h(1.2mm) *Participant* #h(7mm)
  #box(rect(width: 9mm, height: 3mm, fill: rgb("#c3c6cb"), radius: 0.6mm)) #h(1.2mm) Plage habituelle de la population #h(7mm)
  #box(rect(width: 0.5mm, height: 3.6mm, fill: rgb("#6b7078"))) #h(1.2mm) Moyenne de la population
]]'
  paste(c(legend, vapply(rows, one, character(1))), collapse = "\n  ")
}

# Ventilatory thresholds. The test starts at ~70 % of VO2peak, so no stage lies
# below the first threshold (SV1): it cannot be located and is left out. The
# second threshold (SV2) is the package's own VE/VCO2 detection (rise of 5 %
# above the lowest value, sustained), run without the SV1 and therefore
# without the "SV2 must be above SV1" check that discarded it.
th <- a@thresholds
sv2 <- if (length(th@vt2_vo2) && is.finite(th@vt2_vo2)) {
  list(vo2 = th@vt2_vo2, pct = 100 * th@vt2_vo2 / a@peaks@vo2_peak, hr = th@vt2_hr, power = th@vt2_power)
} else NULL
if (is.null(sv2)) cat("SV2 non determine\n") else
  cat("SV2:", round(sv2$power), "W |", round(sv2$vo2), "mL/min |", round(sv2$pct), "% VO2pk\n")

vt2_row <- if (is.null(sv2)) {
  "[#text(weight: 600, fill: vt2_col)[Deuxième seuil ventilatoire (SV2)]], table.cell(colspan: 4)[#text(fill: muted)[Non déterminé]],"
} else {
  paste0("[#text(weight: 600, fill: vt2_col)[Deuxième seuil ventilatoire (SV2)]], [", round(sv2$vo2), "], [", round(sv2$pct), " %], [", round(sv2$hr), "], [", round(sv2$power), "],")
}

out <- if (length(args) >= 3 && nzchar(args[3])) args[3] else file.path(dirname(f), sub("\\.xlsx$", "_report.pdf", basename(f)))
cfg <- {
  tmp <- tempfile(fileext = ".typ")
  txt <- paste(readLines(tpl, warn = FALSE), collapse = "\n")
  txt <- sub("%%COMPARAISON%%", gsub("\\", "\\\\", comparison_block(a, d), fixed = TRUE), txt, fixed = TRUE)
  txt <- sub("%%VT1ROW%%", "", txt, fixed = TRUE)
  txt <- sub("%%VT2ROW%%", vt2_row, txt, fixed = TRUE)
  writeLines(txt, tmp)
  ReportConfig(language = "fr", template = tmp)
}
generate_report(a, output_file = out, config = cfg)
cat("start_w:", start_w, " last_stage_s:", last_dur, " MAP:", m$map_watts,
    " PPO:", m$ppo_watts, " frac:", round(m$kuipers_fraction, 2), "\n")
print(as.data.frame(ss[, c("stage", "duration_s", "power_w")]))

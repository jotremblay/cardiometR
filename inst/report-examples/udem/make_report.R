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
                 stage_duration = STAGE_S, increment_size = INCR_W)
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

# VT1 can't be located when the first stage is already past it: VE/VO2 at stage 1
# is clearly above the end of the warm-up (heuristic, +5 %). Needs a warm-up baseline.
vt1_undetermined <- FALSE
base_rows <- b[which(b$time_s < min(e$time_s) & b$time_s >= min(e$time_s) - 110 & b$power_w > 0 & b$power_w < start_w), ]
s1 <- e[which(e$power_w == start_w), ]
s1 <- s1[s1$time_s >= max(s1$time_s) - 30, ]
if (nrow(base_rows) >= 5 && nrow(s1) >= 2) {
  ratio <- mean(s1$ve_vo2, na.rm = TRUE) / mean(base_rows$ve_vo2, na.rm = TRUE)
  vt1_undetermined <- is.finite(ratio) && ratio >= 1.05
  cat("VE/VO2 stage1 / warm-up baseline:", round(ratio, 3), " -> VT1", if (vt1_undetermined) "non determine" else "retained", "\n")
} else cat("VT1 check skipped (no warm-up baseline below the first stage)\n")

# Plausibility: the first threshold lies below the functional threshold power
# (FTP, 0.72-0.77 x MAP). A detected SV1 at or above the lower FTP bound, or an
# SV2 that is missing or not above the SV1, is not reliable for a test that
# starts at ~70 % of VO2peak.
th <- a@thresholds
vt1_w <- if (length(th@vt1_power)) th@vt1_power else NA_real_
vt2_w <- if (length(th@vt2_power)) th@vt2_power else NA_real_
if (is.finite(vt1_w) && is.finite(a@map_watts) && vt1_w >= 0.72 * a@map_watts) {
  vt1_undetermined <- TRUE
  cat("SV1", vt1_w, "W >= FTP low bound", round(0.72 * a@map_watts), "W -> non determine\n")
}
vt2_undetermined <- !is.finite(vt2_w) || (is.finite(vt1_w) && vt2_w <= vt1_w)
if (vt2_undetermined) cat("SV2 missing or not above SV1 -> non determine\n")

fmt_int <- function(x) if (length(x) && is.finite(x)) format(round(x), big.mark = "") else "—"
ND <- "Non déterminé : le premier palier du test est déjà au-dessus du seuil"
vt1_row_std <- "[#text(weight: 600, fill: vt1_col)[Premier seuil ventilatoire (SV1)]], [{{vt1_vo2}}], [{{vt1_percent}} %], [{{vt1_hr}}], [{{vt1_power}}],"
vt2_row_std <- "[#text(weight: 600, fill: vt2_col)[Deuxième seuil ventilatoire (SV2)]], [{{vt2_vo2}}], [{{vt2_percent}} %], [{{vt2_hr}}], [{{vt2_power}}],"
vt2_row_nd <- "[#text(weight: 600, fill: vt2_col)[Deuxième seuil ventilatoire (SV2)]], table.cell(colspan: 4)[#text(fill: muted)[Non déterminé]],"
vt1_row_nd <- paste0("[#text(weight: 600, fill: vt1_col)[Premier seuil ventilatoire (SV1)]], table.cell(colspan: 4)[#text(fill: muted)[", ND, "]],")
vt_rows_nd <- paste0("[VT1], table.cell(colspan: 3)[#text(fill: muted)[Non déterminé]],\n        [VT2], [", fmt_int(a@vt2_range[1]), "], [", fmt_int(a@vt2_range[2]), "], [", fmt_int(a@thresholds@vt2_vo2), "]")

out <- if (length(args) >= 3 && nzchar(args[3])) args[3] else file.path(dirname(f), sub("\\.xlsx$", "_report.pdf", basename(f)))
cfg <- {
  tmp <- tempfile(fileext = ".typ")
  txt <- paste(readLines(tpl, warn = FALSE), collapse = "\n")
  txt <- sub("%%COMPARAISON%%", gsub("\\", "\\\\", comparison_block(a, d), fixed = TRUE), txt, fixed = TRUE)
  txt <- sub("%%VT1ROW%%", if (vt1_undetermined) vt1_row_nd else vt1_row_std, txt, fixed = TRUE)
  txt <- sub("%%VT2ROW%%", if (vt2_undetermined) vt2_row_nd else vt2_row_std, txt, fixed = TRUE)
  txt <- sub("%%VTROWS%%", if (vt1_undetermined) vt_rows_nd else "{{vt_rows_content}}", txt, fixed = TRUE)
  writeLines(txt, tmp)
  ReportConfig(language = "fr", template = tmp)
}
generate_report(a, output_file = out, config = cfg)
cat("start_w:", start_w, " last_stage_s:", last_dur, " MAP:", m$map_watts,
    " PPO:", m$ppo_watts, " frac:", round(m$kuipers_fraction, 2), "\n")
print(as.data.frame(ss[, c("stage", "duration_s", "power_w")]))

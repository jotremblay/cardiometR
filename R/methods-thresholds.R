# S7 Methods for Ventilatory Threshold Detection

#' @name detect_thresholds
#' @rdname detect_thresholds
#' @export
method(detect_thresholds, CpetData) <- function(x,
                                                methods = c("v_slope", "ve_vo2", "ve_vco2"),
                                                window_s = 30,
                                                ...) {
  breaths <- x@breaths

  if (nrow(breaths) < 20) {
    cli::cli_warn("Insufficient data for threshold detection")
    return(empty_thresholds("unable"))
  }

  methods_norm <- normalize_threshold_methods(methods)
  if (length(methods_norm) == 0) {
    cli::cli_warn("No valid threshold methods specified")
    return(empty_thresholds("unable"))
  }

  # Smoothing window based on window_s
  avg_interval <- mean(diff(breaths$time_s), na.rm = TRUE)
  if (is.na(avg_interval) || avg_interval <= 0) {
    avg_interval <- 1
  }
  k <- max(5, round(window_s / avg_interval))
  k <- min(k, nrow(breaths) %/% 2)

  vo2_s <- smooth_series(breaths$vo2_ml, k)
  vco2_s <- smooth_series(breaths$vco2_ml, k)
  ve_s <- smooth_series(breaths$ve_l, k)
  peto2_s <- if ("peto2_mmhg" %in% names(breaths)) smooth_series(breaths$peto2_mmhg, k) else NULL
  petco2_s <- if ("petco2_mmhg" %in% names(breaths)) smooth_series(breaths$petco2_mmhg, k) else NULL

  vt1_candidates <- list()
  vt2_candidates <- list()

  if ("v_slope" %in% methods_norm) {
    vslope <- detect_vslope_threshold(vo2_s, vco2_s)
    if (!is.null(vslope$vo2)) {
      vt1_candidates$v_slope <- vslope$vo2
    }
  }

  if ("ve_vo2" %in% methods_norm && !all(is.na(ve_s)) && !all(is.na(vo2_s))) {
    ve_vo2 <- ve_s * 1000 / vo2_s
    idx <- detect_threshold_rise(ve_vo2)
    if (!is.na(idx)) {
      vt1_candidates$ve_vo2 <- vo2_s[idx]
    }
  }

  if ("peto2" %in% methods_norm && !is.null(peto2_s)) {
    idx <- detect_threshold_rise(peto2_s)
    if (!is.na(idx)) {
      vt1_candidates$peto2 <- vo2_s[idx]
    }
  }

  if ("ve_vco2" %in% methods_norm && !all(is.na(ve_s)) && !all(is.na(vco2_s))) {
    ve_vco2 <- ve_s * 1000 / vco2_s
    idx <- detect_threshold_rise(ve_vco2)
    if (!is.na(idx)) {
      vt2_candidates$ve_vco2 <- vo2_s[idx]
    }
  }

  if ("petco2" %in% methods_norm && !is.null(petco2_s)) {
    idx <- detect_threshold_drop(petco2_s)
    if (!is.na(idx)) {
      vt2_candidates$petco2 <- vo2_s[idx]
    }
  }

  vt1_values <- unlist(vt1_candidates, use.names = FALSE)
  vt2_values <- unlist(vt2_candidates, use.names = FALSE)

  vt1_vo2 <- if (length(vt1_values) > 0) stats::median(vt1_values, na.rm = TRUE) else NA_real_
  vt2_vo2 <- if (length(vt2_values) > 0) stats::median(vt2_values, na.rm = TRUE) else NA_real_

  # Enforce VT2 > VT1 if both available
  if (!is.na(vt1_vo2) && !is.na(vt2_vo2) && vt2_vo2 <= vt1_vo2) {
    vt2_vo2 <- NA_real_
    vt2_candidates <- list()
  }

  vt1_method <- if (length(vt1_candidates) > 0) paste(names(vt1_candidates), collapse = ", ") else NULL
  vt2_method <- if (length(vt2_candidates) > 0) paste(names(vt2_candidates), collapse = ", ") else NULL

  vt1_vals <- lookup_threshold_values(vt1_vo2, breaths)
  vt2_vals <- lookup_threshold_values(vt2_vo2, breaths)

  confidence <- threshold_confidence(length(vt1_candidates) + length(vt2_candidates))

  Thresholds(
    vt1_vo2 = vt1_vo2,
    vt1_hr = vt1_vals$hr,
    vt1_power = vt1_vals$power,
    vt1_method = vt1_method,
    vt2_vo2 = vt2_vo2,
    vt2_hr = vt2_vals$hr,
    vt2_power = vt2_vals$power,
    vt2_method = vt2_method,
    confidence = confidence
  )
}

normalize_threshold_methods <- function(methods) {
  if (is.null(methods) || length(methods) == 0) return(character())

  normalize_one <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z0-9]", "", x)
    switch(x,
      vslope = "v_slope",
      vslopeanalysis = "v_slope",
      v_slope = "v_slope",
      vevo2 = "ve_vo2",
      vev02 = "ve_vo2",
      ve_vo2 = "ve_vo2",
      vevco2 = "ve_vco2",
      vevco2slope = "ve_vco2",
      ve_vco2 = "ve_vco2",
      peto2 = "peto2",
      petco2 = "petco2",
      NULL
    )
  }

  normalized <- vapply(methods, normalize_one, character(1), USE.NAMES = FALSE)
  normalized <- normalized[!is.na(normalized) & nzchar(normalized)]
  unique(normalized)
}

smooth_series <- function(x, k) {
  if (length(x) < k || k < 5) return(x)
  zoo::rollmean(x, k = k, fill = NA, align = "center")
}

detect_vslope_threshold <- function(vo2, vco2) {
  df <- data.frame(vo2 = vo2, vco2 = vco2)
  df <- df[complete.cases(df), ]
  if (nrow(df) < 20) return(list(vo2 = NA_real_))

  df <- df[order(df$vo2), ]
  n <- nrow(df)

  candidates <- unique(round(seq(floor(n * 0.2), floor(n * 0.8), length.out = 30)))
  best <- list(sse = Inf, idx = NA_integer_)

  for (idx in candidates) {
    if (idx < 5 || idx > n - 5) next
    left <- df[1:idx, ]
    right <- df[(idx + 1):n, ]

    fit_left <- tryCatch(stats::lm(vco2 ~ vo2, data = left), error = function(e) NULL)
    fit_right <- tryCatch(stats::lm(vco2 ~ vo2, data = right), error = function(e) NULL)
    if (is.null(fit_left) || is.null(fit_right)) next

    sse <- sum(stats::resid(fit_left)^2) + sum(stats::resid(fit_right)^2)
    if (!is.na(sse) && sse < best$sse) {
      best$sse <- sse
      best$idx <- idx
    }
  }

  if (is.na(best$idx)) return(list(vo2 = NA_real_))
  list(vo2 = df$vo2[best$idx])
}

detect_threshold_rise <- function(x, rise_pct = 0.05, sustain = 5) {
  if (all(is.na(x)) || length(x) < (sustain + 5)) return(NA_integer_)

  x_clean <- x
  x_clean[is.na(x_clean)] <- Inf
  start_idx <- which.min(x_clean)
  if (length(start_idx) == 0 || is.infinite(x_clean[start_idx]) || start_idx >= length(x)) {
    return(NA_integer_)
  }

  baseline <- x_clean[start_idx]

  threshold <- baseline * (1 + rise_pct)
  for (i in seq(start_idx + 1, length(x) - sustain + 1)) {
    window <- x[i:(i + sustain - 1)]
    if (sum(!is.na(window)) < sustain) next
    if (all(window >= threshold, na.rm = TRUE)) return(i)
  }

  NA_integer_
}

detect_threshold_drop <- function(x, drop_pct = 0.05, sustain = 5) {
  if (all(is.na(x)) || length(x) < (sustain + 5)) return(NA_integer_)

  x_clean <- x
  x_clean[is.na(x_clean)] <- -Inf
  start_idx <- which.max(x_clean)
  if (length(start_idx) == 0 || is.infinite(x_clean[start_idx]) || start_idx >= length(x)) {
    return(NA_integer_)
  }

  baseline <- x_clean[start_idx]

  threshold <- baseline * (1 - drop_pct)
  for (i in seq(start_idx + 1, length(x) - sustain + 1)) {
    window <- x[i:(i + sustain - 1)]
    if (sum(!is.na(window)) < sustain) next
    if (all(window <= threshold, na.rm = TRUE)) return(i)
  }

  NA_integer_
}

lookup_threshold_values <- function(vo2_target, breaths) {
  if (is.null(vo2_target) || is.na(vo2_target)) {
    return(list(hr = NULL, power = NULL))
  }

  idx <- which.min(abs(breaths$vo2_ml - vo2_target))

  list(
    hr = if ("hr_bpm" %in% names(breaths)) breaths$hr_bpm[idx] else NULL,
    power = if ("power_w" %in% names(breaths)) breaths$power_w[idx] else NULL
  )
}

threshold_confidence <- function(n_methods) {
  if (n_methods >= 3) "high"
  else if (n_methods == 2) "moderate"
  else if (n_methods == 1) "low"
  else "unable"
}

empty_thresholds <- function(confidence = "unable") {
  Thresholds(
    vt1_vo2 = NULL,
    vt1_hr = NULL,
    vt1_power = NULL,
    vt1_method = NULL,
    vt2_vo2 = NULL,
    vt2_hr = NULL,
    vt2_power = NULL,
    vt2_method = NULL,
    confidence = confidence
  )
}

# Global variable declarations for R CMD check
# These are column names used in dplyr/tidyr pipelines

utils::globalVariables(c(
  # Breath-by-breath data columns
  "time_s",
  "time_bin",
  "time_min",
  "time_plot",
  "vo2_ml",
  "vco2_ml",
  "ve_l",
  "rer",
  "hr_bpm",
  "power_w",
  "peto2_mmhg",
  "petco2_mmhg",

  # Derived variables
  "ve_vo2",
  "ve_vco2",
  "o2_pulse",

  # Stage-related
  "stage",
  "stage_name",
  "breath_group",
  "phase",
  "power_diff",
  "power_rounded",

  # Plotting variables
  ".data",
  "variable",
  "value",
  "type",

  # Comparison variables
  "measured",
  "pct_predicted",
  "parameter",
  "norm_typical",
  "Predicted",
  "Value",
  "% Predicted"
))

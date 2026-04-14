# Normative Data for Endurance Athletes
# Reference values for VO2max, HRmax, and other CPET parameters
# by sport, level, age, and sex

#' Get Normative Data for Athlete Comparison
#'
#' @description
#' Returns normative reference values for endurance athletes by sport,
#' competitive level, age group, and sex. Includes citations for source studies.
#'
#' @param sport Sport type: "cycling", "running", "triathlon", or "general"
#' @param level Competitive level: "elite", "competitive", "recreational", or "sedentary"
#' @param sex Sex: "M" or "F"
#' @param age Age in years (used to select age group)
#'
#' @return A list containing:
#'   - vo2max_low: Lower bound of VO2max range (mL/kg/min)
#'   - vo2max_high: Upper bound of VO2max range (mL/kg/min)
#'   - vo2max_typical: Typical/median value (mL/kg/min)
#'   - hr_max_typical: Typical HR max for the population
#'   - description: Text description of the reference population
#'   - citation: Full citation for the source
#'   - citation_short: Short citation (Author, Year)
#'
#' @examples
#' # Get norms for elite male cyclist
#' norms <- get_normative_data("cycling", "elite", "M", 30)
#'
#' # Get norms for recreational female runner
#' norms <- get_normative_data("running", "recreational", "F", 45)
#'
#' @export
get_normative_data <- function(sport = "general",
                               level = "recreational",
                               sex = "M",
                               age = 30) {

 # Validate inputs
  sport <- match.arg(tolower(sport), c("cycling", "running", "triathlon", "general"))
  level <- match.arg(tolower(level), c("elite", "competitive", "recreational", "sedentary"))
  sex <- match.arg(toupper(sex), c("M", "F"))

  # Get age group
  age_group <- get_age_group(age)

  # Look up normative values
  norms <- normative_lookup(sport, level, sex, age_group)

  norms
}


#' Default Sport for a Given Modality
#'
#' Map the CPET modality to the most appropriate default sport stratum when
#' the user has not explicitly picked one. Cycle ergometry -> cycling,
#' treadmill -> running, anything else -> general.
#'
#' @param modality Character, typically `"cycling"` or `"treadmill"`.
#' @return Sport key for [get_normative_data()].
#' @keywords internal
#' @export
default_sport_for_modality <- function(modality) {
  m <- tolower(as.character(modality %||% ""))
  if (identical(m, "treadmill")) "running"
  else if (identical(m, "cycling")) "cycling"
  else "general"
}


#' Get Age Group Category
#'
#' @param age Age in years
#' @return Character age group label
#' @keywords internal
get_age_group <- function(age) {
  dplyr::case_when(
    age < 20 ~ "junior",
    age < 30 ~ "20-29",
    age < 40 ~ "30-39",
    age < 50 ~ "40-49",
    age < 60 ~ "50-59",
    age < 70 ~ "60-69",
    TRUE ~ "70+"
  )
}


#' Normative Data Lookup
#'
#' @param sport Sport type
#' @param level Competitive level
#' @param sex Sex
#' @param age_group Age group category
#' @return List with normative values and citations
#' @keywords internal
normative_lookup <- function(sport, level, sex, age_group) {

  # Build the reference tables
  # Values are VO2max in mL/kg/min

  # ============================================================================
  # CYCLING NORMATIVE DATA
  # ============================================================================

  if (sport == "cycling") {
    if (level == "elite") {
      if (sex == "M") {
        return(list(
          vo2max_low = 70,
          vo2max_high = 85,
          vo2max_typical = 77,
          map_per_kg_low = 6.2,
          map_per_kg_high = 7.4,
          map_per_kg_typical = 6.8,
          efficiency_low = 22.0,
          efficiency_high = 25.5,
          efficiency_typical = 23.5,
          efficiency_unit = "%",
          description = "Elite/Professional male cyclists (WorldTour, Continental)",
          citation = "Lucia A, Hoyos J, Chicharro JL. Physiology of professional road cycling. Sports Med. 2001;31(5):325-337. doi:10.2165/00007256-200131050-00004",
          citation_short = "Lucia et al., 2001",
          map_per_kg_citation = "Pinot J, Grappe F. Determination of Maximal Aerobic Power on the field in cycling. J Sci Cycl. 2014;3(1):26-31. (MAP 6.87 \u00b1 0.5 W/kg in elite/pro men)",
          map_per_kg_citation_short = "Pinot & Grappe, 2014",
          efficiency_citation = "Coyle EF. Improved muscular efficiency displayed as Tour de France champion matures. J Appl Physiol. 2005;98(6):2191-2196.",
          efficiency_citation_short = "Coyle, 2005"
        ))
      } else {
        return(list(
          vo2max_low = 60,
          vo2max_high = 72,
          vo2max_typical = 66,
          map_per_kg_low = 5.0,
          map_per_kg_high = 6.2,
          map_per_kg_typical = 5.6,
          efficiency_low = 21.0,
          efficiency_high = 24.5,
          efficiency_typical = 22.5,
          efficiency_unit = "%",
          description = "Elite/Professional female cyclists (WorldTour, Continental)",
          citation = "Leo P, Spragg J, Mujika I, et al. The Record Power Profile in Professional Female Cyclists: Normative Values Obtained From a Large Database. Int J Sports Physiol Perform. 2022;17(5):682-686.",
          citation_short = "Leo et al., 2022",
          map_per_kg_citation = "Leo P, Spragg J, Mujika I, et al. The Record Power Profile in Professional Female Cyclists. Int J Sports Physiol Perform. 2022;17(5):682-686. (5-min MMP \u2248 5.2 W/kg)",
          map_per_kg_citation_short = "Leo et al., 2022",
          efficiency_citation = "Hopker J, Passfield L, Coleman D, et al. The effects of training on gross efficiency in cycling. Med Sci Sports Exerc. 2009;41(8):1653-1659.",
          efficiency_citation_short = "Hopker et al., 2009"
        ))
      }
    } else if (level == "competitive") {
      if (sex == "M") {
        return(list(
          vo2max_low = 55,
          vo2max_high = 70,
          vo2max_typical = 62,
          map_per_kg_low = 4.7,
          map_per_kg_high = 5.7,
          map_per_kg_typical = 5.2,
          efficiency_low = 20.0,
          efficiency_high = 23.5,
          efficiency_typical = 21.5,
          efficiency_unit = "%",
          description = "Competitive amateur male cyclists (Cat 1-3, Masters)",
          citation = "Mujika I, Padilla S. Physiological and performance characteristics of male professional road cyclists. Sports Med. 2001;31(7):479-487.",
          citation_short = "Mujika & Padilla, 2001",
          map_per_kg_citation = "Hawley JA, Noakes TD. Peak power output predicts maximal oxygen uptake and performance time in trained cyclists. Eur J Appl Physiol. 1992;65(1):79-83.",
          map_per_kg_citation_short = "Hawley & Noakes, 1992",
          efficiency_citation = "Hopker J, Passfield L, Coleman D, et al. The effects of training on gross efficiency in cycling. Med Sci Sports Exerc. 2009;41(8):1653-1659.",
          efficiency_citation_short = "Hopker et al., 2009"
        ))
      } else {
        return(list(
          vo2max_low = 48,
          vo2max_high = 60,
          vo2max_typical = 54,
          map_per_kg_low = 4.0,
          map_per_kg_high = 5.0,
          map_per_kg_typical = 4.5,
          efficiency_low = 19.5,
          efficiency_high = 23.0,
          efficiency_typical = 21.0,
          efficiency_unit = "%",
          description = "Competitive amateur female cyclists (Cat 1-3, Masters)",
          citation = "Mujika I, Padilla S. Physiological and performance characteristics of male professional road cyclists. Sports Med. 2001;31(7):479-487.",
          citation_short = "Mujika & Padilla, 2001",
          map_per_kg_citation = "Bell PG, Furber MJW, van Someren KA, et al. The Physiological Profile of a Multiple Tour de France Winning Cyclist. Med Sci Sports Exerc. 2017; and trained-female cycling data (MAP \u2248 4.5 W/kg).",
          map_per_kg_citation_short = "Bell et al., 2017",
          efficiency_citation = "Hopker J, Passfield L, Coleman D, et al. The effects of training on gross efficiency in cycling. Med Sci Sports Exerc. 2009;41(8):1653-1659.",
          efficiency_citation_short = "Hopker et al., 2009"
        ))
      }
    } else if (level == "recreational") {
      if (sex == "M") {
        return(list(
          vo2max_low = 40,
          vo2max_high = 55,
          vo2max_typical = 47,
          map_per_kg_low = 3.2,
          map_per_kg_high = 4.2,
          map_per_kg_typical = 3.7,
          efficiency_low = 18.0,
          efficiency_high = 22.0,
          efficiency_typical = 20.0,
          efficiency_unit = "%",
          description = "Recreational male cyclists (regular training, amateur racing)",
          citation = "Jeukendrup AE, Craig NP, Hawley JA. The bioenergetics of world class cycling. J Sci Med Sport. 2000;3(4):414-433. doi:10.1016/S1440-2440(00)80008-0",
          citation_short = "Jeukendrup et al., 2000",
          map_per_kg_citation = "Jeukendrup AE, Craig NP, Hawley JA. The bioenergetics of world class cycling. J Sci Med Sport. 2000;3(4):414-433.",
          map_per_kg_citation_short = "Jeukendrup et al., 2000",
          efficiency_citation = "Moseley L, Jeukendrup AE. The reliability of cycling efficiency. Med Sci Sports Exerc. 2001;33(4):621-627.",
          efficiency_citation_short = "Moseley & Jeukendrup, 2001"
        ))
      } else {
        return(list(
          vo2max_low = 35,
          vo2max_high = 48,
          vo2max_typical = 41,
          map_per_kg_low = 2.7,
          map_per_kg_high = 3.7,
          map_per_kg_typical = 3.2,
          efficiency_low = 17.5,
          efficiency_high = 21.5,
          efficiency_typical = 19.5,
          efficiency_unit = "%",
          description = "Recreational female cyclists (regular training, amateur racing)",
          citation = "Jeukendrup AE, Craig NP, Hawley JA. The bioenergetics of world class cycling. J Sci Med Sport. 2000;3(4):414-433. doi:10.1016/S1440-2440(00)80008-0",
          citation_short = "Jeukendrup et al., 2000",
          map_per_kg_citation = "Jeukendrup AE, Craig NP, Hawley JA. The bioenergetics of world class cycling. J Sci Med Sport. 2000;3(4):414-433.",
          map_per_kg_citation_short = "Jeukendrup et al., 2000",
          efficiency_citation = "Moseley L, Jeukendrup AE. The reliability of cycling efficiency. Med Sci Sports Exerc. 2001;33(4):621-627.",
          efficiency_citation_short = "Moseley & Jeukendrup, 2001"
        ))
      }
    }
  }

  # ============================================================================
  # RUNNING NORMATIVE DATA
  # ============================================================================

  if (sport == "running") {
    # Running economy: ml O2/kg/km (lower is better)
    if (level == "elite") {
      if (sex == "M") {
        return(list(
          vo2max_low = 70,
          vo2max_high = 85,
          vo2max_typical = 76,
          economy_low = 180,
          economy_high = 200,
          economy_typical = 190,
          economy_unit = "mL/kg/km",
          description = "Elite male distance runners (Olympic Trials, sub-2:15 marathon)",
          citation = "Morgan DW, Bransford DR, Costill DL, et al. Variation in the aerobic demand of running among trained and untrained subjects. Med Sci Sports Exerc. 1995;27(3):404-409.",
          citation_short = "Morgan et al., 1995",
          economy_citation = "Saunders PU, Pyne DB, Telford RD, Hawley JA. Factors affecting running economy in trained distance runners. Sports Med. 2004;34(7):465-485.",
          economy_citation_short = "Saunders et al., 2004"
        ))
      } else {
        return(list(
          vo2max_low = 60,
          vo2max_high = 75,
          vo2max_typical = 67,
          economy_low = 185,
          economy_high = 210,
          economy_typical = 195,
          economy_unit = "mL/kg/km",
          description = "Elite female distance runners (Olympic Trials, sub-2:35 marathon)",
          citation = "Pate RR, Sparling PB, Wilson GE, et al. Cardiorespiratory and metabolic responses to submaximal and maximal exercise in elite women distance runners. Int J Sports Med. 1987;8(2):91-95.",
          citation_short = "Pate et al., 1987",
          economy_citation = "Saunders PU, Pyne DB, Telford RD, Hawley JA. Factors affecting running economy in trained distance runners. Sports Med. 2004;34(7):465-485.",
          economy_citation_short = "Saunders et al., 2004"
        ))
      }
    } else if (level == "competitive") {
      if (sex == "M") {
        return(list(
          vo2max_low = 55,
          vo2max_high = 70,
          vo2max_typical = 62,
          economy_low = 195,
          economy_high = 220,
          economy_typical = 205,
          economy_unit = "mL/kg/km",
          description = "Competitive amateur male runners (sub-3:00 marathon, regular racing)",
          citation = "Noakes TD, Myburgh KH, Schall R. Peak treadmill running velocity during the VO2 max test predicts running performance. J Sports Sci. 1990;8(1):35-45.",
          citation_short = "Noakes et al., 1990",
          economy_citation = "Barnes KR, Kilding AE. Running economy: measurement, norms, and determining factors. Sports Med Open. 2015;1(1):8.",
          economy_citation_short = "Barnes & Kilding, 2015"
        ))
      } else {
        return(list(
          vo2max_low = 48,
          vo2max_high = 62,
          vo2max_typical = 55,
          economy_low = 200,
          economy_high = 230,
          economy_typical = 215,
          economy_unit = "mL/kg/km",
          description = "Competitive amateur female runners (sub-3:30 marathon, regular racing)",
          citation = "Noakes TD, Myburgh KH, Schall R. Peak treadmill running velocity during the VO2 max test predicts running performance. J Sports Sci. 1990;8(1):35-45.",
          citation_short = "Noakes et al., 1990",
          economy_citation = "Barnes KR, Kilding AE. Running economy: measurement, norms, and determining factors. Sports Med Open. 2015;1(1):8.",
          economy_citation_short = "Barnes & Kilding, 2015"
        ))
      }
    } else if (level == "recreational") {
      if (sex == "M") {
        return(list(
          vo2max_low = 40,
          vo2max_high = 55,
          vo2max_typical = 47,
          economy_low = 210,
          economy_high = 250,
          economy_typical = 230,
          economy_unit = "mL/kg/km",
          description = "Recreational male runners (regular jogging, occasional races)",
          citation = "Bassett DR Jr, Howley ET. Limiting factors for maximum oxygen uptake and determinants of endurance performance. Med Sci Sports Exerc. 2000;32(1):70-84.",
          citation_short = "Bassett & Howley, 2000",
          economy_citation = "Barnes KR, Kilding AE. Running economy: measurement, norms, and determining factors. Sports Med Open. 2015;1(1):8.",
          economy_citation_short = "Barnes & Kilding, 2015"
        ))
      } else {
        return(list(
          vo2max_low = 35,
          vo2max_high = 48,
          vo2max_typical = 41,
          economy_low = 220,
          economy_high = 260,
          economy_typical = 240,
          economy_unit = "mL/kg/km",
          description = "Recreational female runners (regular jogging, occasional races)",
          citation = "Bassett DR Jr, Howley ET. Limiting factors for maximum oxygen uptake and determinants of endurance performance. Med Sci Sports Exerc. 2000;32(1):70-84.",
          citation_short = "Bassett & Howley, 2000",
          economy_citation = "Barnes KR, Kilding AE. Running economy: measurement, norms, and determining factors. Sports Med Open. 2015;1(1):8.",
          economy_citation_short = "Barnes & Kilding, 2015"
        ))
      }
    }
  }

  # ============================================================================
  # TRIATHLON NORMATIVE DATA
  # ============================================================================

  if (sport == "triathlon") {
    if (level == "elite") {
      if (sex == "M") {
        return(list(
          vo2max_low = 70,
          vo2max_high = 85,
          vo2max_typical = 75,
          hr_max_typical = 185,
          description = "Elite male triathletes (ITU/PTO World Tour, Kona Pro)",
          citation = "Sleivert GG, Rowlands DS. Physical and physiological factors associated with success in the triathlon. Sports Med. 1996;22(1):8-18. doi:10.2165/00007256-199622010-00002",
          citation_short = "Sleivert & Rowlands, 1996"
        ))
      } else {
        return(list(
          vo2max_low = 60,
          vo2max_high = 72,
          vo2max_typical = 66,
          hr_max_typical = 185,
          description = "Elite female triathletes (ITU/PTO World Tour, Kona Pro)",
          citation = "Laursen PB, Rhodes EC. Factors affecting performance in an ultraendurance triathlon. Sports Med. 2001;31(3):195-209. doi:10.2165/00007256-200131030-00004",
          citation_short = "Laursen & Rhodes, 2001"
        ))
      }
    } else if (level == "competitive") {
      if (sex == "M") {
        return(list(
          vo2max_low = 55,
          vo2max_high = 70,
          vo2max_typical = 62,
          hr_max_typical = 180,
          description = "Competitive amateur male triathletes (Kona qualifier, age-group podium)",
          citation = "Knechtle B, Wirth A, Rosemann T. Predictors of race time in male Ironman triathletes: physical characteristics, training, or prerace experience? Percept Mot Skills. 2010;111(2):437-446.",
          citation_short = "Knechtle et al., 2010"
        ))
      } else {
        return(list(
          vo2max_low = 48,
          vo2max_high = 62,
          vo2max_typical = 55,
          hr_max_typical = 180,
          description = "Competitive amateur female triathletes (Kona qualifier, age-group podium)",
          citation = "Knechtle B, Wirth A, Rosemann T. Predictors of race time in male Ironman triathletes: physical characteristics, training, or prerace experience? Percept Mot Skills. 2010;111(2):437-446.",
          citation_short = "Knechtle et al., 2010"
        ))
      }
    } else if (level == "recreational") {
      if (sex == "M") {
        return(list(
          vo2max_low = 45,
          vo2max_high = 58,
          vo2max_typical = 52,
          hr_max_typical = 175,
          description = "Recreational male triathletes (age-group finisher, regular training)",
          citation = "Lepers R, Knechtle B, Stapley PJ. Trends in triathlon performance: effects of sex and age. Sports Med. 2013;43(9):851-863. doi:10.1007/s40279-013-0067-4",
          citation_short = "Lepers et al., 2013"
        ))
      } else {
        return(list(
          vo2max_low = 38,
          vo2max_high = 52,
          vo2max_typical = 45,
          hr_max_typical = 175,
          description = "Recreational female triathletes (age-group finisher, regular training)",
          citation = "Lepers R, Knechtle B, Stapley PJ. Trends in triathlon performance: effects of sex and age. Sports Med. 2013;43(9):851-863. doi:10.1007/s40279-013-0067-4",
          citation_short = "Lepers et al., 2013"
        ))
      }
    }
  }

  # ============================================================================
  # GENERAL POPULATION / SEDENTARY (Default)
  # ============================================================================

  # Age-adjusted general population values (FRIEND database, Kaminsky et al. 2015)
  if (level == "sedentary" || sport == "general") {
    # FRIEND database reference values by age and sex
    friend_values <- get_friend_percentiles(sex, age_group)
    return(list(
      vo2max_low = friend_values$p25,
      vo2max_high = friend_values$p75,
      vo2max_typical = friend_values$p50,
      hr_max_typical = round(208 - 0.7 * get_age_midpoint(age_group)),
      description = sprintf("General population %s, age %s (FRIEND Registry)",
                           if (sex == "M") "males" else "females",
                           age_group),
      citation = "Kaminsky LA, Arena R, Myers J, et al. Reference Standards for Cardiorespiratory Fitness Measured With Cardiopulmonary Exercise Testing: Data From the Fitness Registry and the Importance of Exercise National Database. Mayo Clin Proc. 2015;90(11):1515-1523. doi:10.1016/j.mayocp.2015.07.026",
      citation_short = "Kaminsky et al., 2015 (FRIEND)"
    ))
  }

  # Fallback to general recreational
  list(
    vo2max_low = if (sex == "M") 40 else 35,
    vo2max_high = if (sex == "M") 55 else 48,
    vo2max_typical = if (sex == "M") 47 else 41,
    hr_max_typical = round(208 - 0.7 * get_age_midpoint(age_group)),
    description = "General active population",
    citation = "Wasserman K, Hansen JE, Sue DY, et al. Principles of Exercise Testing and Interpretation. 5th ed. Lippincott Williams & Wilkins; 2012.",
    citation_short = "Wasserman et al., 2012"
  )
}


#' Get CHEER Registry Normative Data for Endurance Athletes
#'
#' @description
#' Returns VO2peak reference values from the Cardiopulmonary Health and
#' Endurance Exercise Registry (CHEER) for endurance athletes, stratified
#' by modality (cycling vs. treadmill), sex, and age group.
#'
#' CHEER values are systematically higher than FRIEND (general population)
#' and are the recommended reference for trained endurance athletes.
#' Age groups follow CHEER conventions: "younger" = 18-30, "older" = 30-45.
#'
#' @param modality CPET modality: "cycling" or "treadmill"
#' @param sex Sex: "M" or "F"
#' @param age Age in years (classified as younger <=30 or older >30)
#'
#' @return A list with:
#'   - vo2peak_mean: Mean VO2peak (mL/kg/min)
#'   - vo2peak_sd: Standard deviation
#'   - vo2peak_low: Mean ? 1 SD
#'   - vo2peak_high: Mean + 1 SD
#'   - age_group: CHEER age group label
#'   - description: Population description
#'   - citation: Full citation
#'   - citation_short: Short citation
#'
#' @references
#' Kowalski T, Kasiak P, Chomiuk T, Mamcarz A, Sliz D. Optimizing the
#' Interpretation of Cardiopulmonary Exercise Testing in Endurance Athletes:
#' Precision Approach for Health and Performance. Translational Sports Medicine.
#' 2025;2025:5904935. doi:10.1155/tsm2/5904935
#'
#' @examples
#' # Younger male cyclist
#' cheer <- get_normative_data_cheer("cycling", "M", 25)
#'
#' # Older female treadmill
#' cheer <- get_normative_data_cheer("treadmill", "F", 38)
#'
#' @export
get_normative_data_cheer <- function(modality = "cycling", sex = "M", age = 30) {
  modality <- match.arg(tolower(modality), c("cycling", "treadmill"))
  sex <- match.arg(toupper(sex), c("M", "F"))

  # CHEER age groups: younger = 18-30, older = 30-45
  age_group <- if (age <= 30) "younger" else "older"

  # VO2peak values (mean +/- SD, mL/kg/min) from CHEER registry
  # Source: Table 1, Kowalski et al. 2025 (TSM 5904935)
  # RER criterion: >= 1.05
  cheer_values <- list(
    cycling = list(
      M = list(
        younger = list(mean = 56.7, sd = 9.6),
        older   = list(mean = 52.1, sd = 8.1)
      ),
      F = list(
        younger = list(mean = 44.7, sd = 7.2),
        older   = list(mean = 37.8, sd = 4.4)
      )
    ),
    treadmill = list(
      M = list(
        younger = list(mean = 62.0, sd = 11.1),
        older   = list(mean = 52.8, sd = 7.6)
      ),
      F = list(
        younger = list(mean = 52.0, sd = 8.6),
        older   = list(mean = 46.9, sd = 6.7)
      )
    )
  )

  vals <- cheer_values[[modality]][[sex]][[age_group]]
  age_label <- if (age_group == "younger") "18-30 years" else "30-45 years"
  modality_label <- if (modality == "cycling") "cycle ergometry" else "treadmill"
  sex_label <- if (sex == "M") "male" else "female"

  list(
    vo2peak_mean  = vals$mean,
    vo2peak_sd    = vals$sd,
    vo2peak_low   = vals$mean - vals$sd,
    vo2peak_high  = vals$mean + vals$sd,
    age_group     = age_label,
    description   = sprintf(
      "Endurance athletes, %s, %s, %s (CHEER Registry)",
      sex_label, age_label, modality_label
    ),
    citation = paste0(
      "Kowalski T, Kasiak P, Chomiuk T, Mamcarz A, \u015aliz D. ",
      "Optimizing the Interpretation of Cardiopulmonary Exercise Testing in ",
      "Endurance Athletes: Precision Approach for Health and Performance. ",
      "Translational Sports Medicine. 2025;2025:5904935. ",
      "doi:10.1155/tsm2/5904935"
    ),
    citation_short = "Kowalski et al., 2025 (CHEER)"
  )
}


#' Compare VO2peak Against Both FRIEND and CHEER Registries
#'
#' @description
#' Convenience function that returns a side-by-side comparison of an
#' athlete's VO2peak against FRIEND (general population) and CHEER
#' (endurance athlete) reference values.
#'
#' @param vo2peak_observed Observed VO2peak in mL/kg/min
#' @param modality CPET modality: "cycling" or "treadmill"
#' @param sex Sex: "M" or "F"
#' @param age Age in years
#'
#' @return A tibble with one row per registry and columns:
#'   registry, vo2peak_ref_mean, vo2peak_ref_sd, z_score, percentile_approx, interpretation
#'
#' @examples
#' compare_vo2peak_registries(58, "cycling", "M", 28)
#'
#' @export
compare_vo2peak_registries <- function(vo2peak_observed, modality = "cycling",
                                       sex = "M", age = 30) {
  # CHEER reference
  cheer <- get_normative_data_cheer(modality, sex, age)

  # FRIEND reference (general population)
  age_group <- get_age_group(age)
  friend <- get_friend_percentiles(sex, age_group)
  # Approximate FRIEND mean/SD from percentiles (p50 ~ mean, SD ~ (p75-p25)/1.35)
  friend_mean <- friend$p50
  friend_sd   <- (friend$p75 - friend$p25) / 1.35

  z_cheer  <- (vo2peak_observed - cheer$vo2peak_mean) / cheer$vo2peak_sd
  z_friend <- (vo2peak_observed - friend_mean) / friend_sd

  interpret <- function(z) {
    dplyr::case_when(
      z >= 1.5  ~ "well above average",
      z >= 0.5  ~ "above average",
      z >= -0.5 ~ "average",
      z >= -1.5 ~ "below average",
      TRUE      ~ "well below average"
    )
  }

  tibble::tibble(
    registry          = c("CHEER (endurance athletes)", "FRIEND (general population)"),
    vo2peak_ref_mean  = c(cheer$vo2peak_mean, friend_mean),
    vo2peak_ref_sd    = c(cheer$vo2peak_sd, friend_sd),
    vo2peak_observed  = vo2peak_observed,
    z_score           = round(c(z_cheer, z_friend), 2),
    interpretation    = interpret(c(z_cheer, z_friend)),
    citation_short    = c(cheer$citation_short, "Kaminsky et al., 2015 (FRIEND)")
  )
}


#' FRIEND Registry Percentiles
#'
#' @description
#' Returns VO2max percentiles from the FRIEND database by sex and age group.
#' Reference: Kaminsky et al., Mayo Clinic Proceedings, 2015.
#'
#' @param sex Sex ("M" or "F")
#' @param age_group Age group category
#' @return List with p25, p50, p75 percentiles
#' @keywords internal
get_friend_percentiles <- function(sex, age_group) {
  # FRIEND database values (mL/kg/min)
  # Source: Kaminsky LA, et al. Mayo Clin Proc. 2015;90(11):1515-1523

  if (sex == "M") {
    values <- switch(age_group,
      "junior"  = list(p25 = 42, p50 = 48, p75 = 55),
      "20-29" = list(p25 = 38, p50 = 44, p75 = 51),
      "30-39" = list(p25 = 35, p50 = 41, p75 = 48),
      "40-49" = list(p25 = 32, p50 = 38, p75 = 44),
      "50-59" = list(p25 = 28, p50 = 34, p75 = 40),
      "60-69" = list(p25 = 24, p50 = 30, p75 = 36),
      "70+"   = list(p25 = 20, p50 = 26, p75 = 32),
      list(p25 = 32, p50 = 38, p75 = 44)  # default
    )
  } else {
    values <- switch(age_group,
      "junior"  = list(p25 = 36, p50 = 42, p75 = 48),
      "20-29" = list(p25 = 30, p50 = 36, p75 = 42),
      "30-39" = list(p25 = 27, p50 = 33, p75 = 39),
      "40-49" = list(p25 = 24, p50 = 30, p75 = 36),
      "50-59" = list(p25 = 22, p50 = 27, p75 = 33),
      "60-69" = list(p25 = 19, p50 = 24, p75 = 30),
      "70+"   = list(p25 = 16, p50 = 21, p75 = 26),
      list(p25 = 24, p50 = 30, p75 = 36)  # default
    )
  }

  values
}


#' Get Age Group Midpoint
#'
#' @param age_group Age group category
#' @return Numeric midpoint of age range
#' @keywords internal
get_age_midpoint <- function(age_group) {
  switch(age_group,
    "junior" = 18,
    "20-29" = 25,
    "30-39" = 35,
    "40-49" = 45,
    "50-59" = 55,
    "60-69" = 65,
    "70+" = 75,
    40  # default
 )
}


#' Get All Available Normative Categories
#'
#' @description
#' Returns a data frame of all available normative data categories
#' with typical VO2max values for quick reference.
#'
#' @return A tibble with sport, level, sex, and typical VO2max values
#'
#' @examples
#' list_normative_categories()
#'
#' @export
list_normative_categories <- function() {
  tibble::tribble(
    ~sport, ~level, ~sex, ~vo2max_typical, ~description,
    "cycling", "elite", "M", 77, "Professional/WorldTour cyclists",
    "cycling", "elite", "F", 65, "Professional/WorldTour cyclists",
    "cycling", "competitive", "M", 62, "Category 1-3 / Masters racers",
    "cycling", "competitive", "F", 54, "Category 1-3 / Masters racers",
    "cycling", "recreational", "M", 47, "Regular training, amateur racing",
    "cycling", "recreational", "F", 41, "Regular training, amateur racing",
    "running", "elite", "M", 76, "Olympic Trials / sub-2:15 marathon",
    "running", "elite", "F", 67, "Olympic Trials / sub-2:35 marathon",
    "running", "competitive", "M", 62, "Sub-3:00 marathon runners",
    "running", "competitive", "F", 55, "Sub-3:30 marathon runners",
    "running", "recreational", "M", 47, "Regular jogging, occasional races",
    "running", "recreational", "F", 41, "Regular jogging, occasional races",
    "triathlon", "elite", "M", 75, "ITU/PTO World Tour, Kona Pro",
    "triathlon", "elite", "F", 66, "ITU/PTO World Tour, Kona Pro",
    "triathlon", "competitive", "M", 62, "Kona qualifier, age-group podium",
    "triathlon", "competitive", "F", 55, "Kona qualifier, age-group podium",
    "triathlon", "recreational", "M", 52, "Age-group finisher",
    "triathlon", "recreational", "F", 45, "Age-group finisher",
    "general", "sedentary", "M", 38, "General population (FRIEND 50th %ile)",
    "general", "sedentary", "F", 30, "General population (FRIEND 50th %ile)"
  )
}


#' Format Citation for Reports
#'
#' @description
#' Formats a citation for inclusion in reports, with language support.
#'
#' @param citation Full citation text
#' @param language Language code ("en" or "fr")
#' @param style Citation style: "full", "short", or "footnote"
#'
#' @return Formatted citation string
#' @keywords internal
format_citation <- function(citation, language = "en", style = "short") {
  if (style == "footnote") {
    if (language == "fr") {
      return(paste0("Source: ", citation))
    } else {
      return(paste0("Source: ", citation))
    }
  }

  citation
}


# Z-score helpers ------------------------------------------------------------

#' Compute Z-Score Against a Normative Stratum
#'
#' @description
#' Compute the z-score of an observation against a stratum returned by
#' [get_normative_data()]. When the stratum does not publish an SD, SD is
#' estimated as `(high - low) / 3.29` (approximately the 5th-95th percentile
#' range under Normality), and the result is flagged accordingly.
#'
#' @param value Numeric observation (scalar).
#' @param stratum A list as returned by [get_normative_data()].
#' @param metric One of `"vo2max"`, `"map_per_kg"`, `"efficiency"` -- selects
#'   which stratum typical/low/high values are used.
#' @return A list with `z`, `percentile`, and `sd_source` (one of
#'   `"tabulated"` or `"estimated"`).
#' @export
z_score <- function(value, stratum,
                    metric = c("vo2max", "map_per_kg", "efficiency")) {
  metric <- match.arg(metric)
  if (is.null(stratum) || !is.list(stratum) || !is.finite(value)) {
    return(list(z = NA_real_, percentile = NA_real_, sd_source = NA_character_))
  }

  keys <- switch(metric,
    vo2max      = list(mean = "vo2max_typical",     low = "vo2max_low",     high = "vo2max_high",     sd = "vo2max_sd"),
    map_per_kg  = list(mean = "map_per_kg_typical", low = "map_per_kg_low", high = "map_per_kg_high", sd = "map_per_kg_sd"),
    efficiency  = list(mean = "efficiency_typical", low = "efficiency_low", high = "efficiency_high", sd = "efficiency_sd")
  )

  mu <- stratum[[keys$mean]]
  sd_tab <- stratum[[keys$sd]]
  lo <- stratum[[keys$low]]
  hi <- stratum[[keys$high]]

  if (!is.numeric(mu) || !is.finite(mu)) {
    return(list(z = NA_real_, percentile = NA_real_, sd_source = NA_character_))
  }

  if (is.numeric(sd_tab) && is.finite(sd_tab) && sd_tab > 0) {
    sd_val <- sd_tab
    src <- "tabulated"
  } else if (is.numeric(lo) && is.numeric(hi) && is.finite(lo) && is.finite(hi) && hi > lo) {
    sd_val <- (hi - lo) / 3.29
    src <- "estimated"
  } else {
    return(list(z = NA_real_, percentile = NA_real_, sd_source = NA_character_))
  }

  z <- (value - mu) / sd_val
  list(z = z, percentile = stats::pnorm(z) * 100, sd_source = src)
}


#' Percentile from Z-Score
#'
#' @param z Z-score (scalar or vector).
#' @return Percentile(s) on a 0-100 scale.
#' @export
percentile_from_z <- function(z) {
  stats::pnorm(z) * 100
}

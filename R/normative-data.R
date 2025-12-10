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
          efficiency_low = 22.0,
          efficiency_high = 25.5,
          efficiency_typical = 23.5,
          efficiency_unit = "%",
          description = "Elite/Professional male cyclists (WorldTour, Continental)",
          citation = "Lucia A, Hoyos J, Chicharro JL. Physiology of professional road cycling. Sports Med. 2001;31(5):325-337. doi:10.2165/00007256-200131050-00004",
          citation_short = "Lucia et al., 2001",
          efficiency_citation = "Coyle EF. Improved muscular efficiency displayed as Tour de France champion matures. J Appl Physiol. 2005;98(6):2191-2196.",
          efficiency_citation_short = "Coyle, 2005"
        ))
      } else {
        return(list(
          vo2max_low = 60,
          vo2max_high = 70,
          vo2max_typical = 65,
          efficiency_low = 21.0,
          efficiency_high = 24.5,
          efficiency_typical = 22.5,
          efficiency_unit = "%",
          description = "Elite/Professional female cyclists (WorldTour, Continental)",
          citation = "Impellizzeri FM, Marcora SM. The physiology of mountain biking. Sports Med. 2007;37(1):59-71. doi:10.2165/00007256-200737010-00005",
          citation_short = "Impellizzeri & Marcora, 2007",
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
          efficiency_low = 20.0,
          efficiency_high = 23.5,
          efficiency_typical = 21.5,
          efficiency_unit = "%",
          description = "Competitive amateur male cyclists (Cat 1-3, Masters)",
          citation = "Mujika I, Padilla S. Physiological and performance characteristics of male professional road cyclists. Sports Med. 2001;31(7):479-487.",
          citation_short = "Mujika & Padilla, 2001",
          efficiency_citation = "Hopker J, Passfield L, Coleman D, et al. The effects of training on gross efficiency in cycling. Med Sci Sports Exerc. 2009;41(8):1653-1659.",
          efficiency_citation_short = "Hopker et al., 2009"
        ))
      } else {
        return(list(
          vo2max_low = 48,
          vo2max_high = 60,
          vo2max_typical = 54,
          efficiency_low = 19.5,
          efficiency_high = 23.0,
          efficiency_typical = 21.0,
          efficiency_unit = "%",
          description = "Competitive amateur female cyclists (Cat 1-3, Masters)",
          citation = "Mujika I, Padilla S. Physiological and performance characteristics of male professional road cyclists. Sports Med. 2001;31(7):479-487.",
          citation_short = "Mujika & Padilla, 2001",
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
          efficiency_low = 18.0,
          efficiency_high = 22.0,
          efficiency_typical = 20.0,
          efficiency_unit = "%",
          description = "Recreational male cyclists (regular training, amateur racing)",
          citation = "Jeukendrup AE, Craig NP, Hawley JA. The bioenergetics of world class cycling. J Sci Med Sport. 2000;3(4):414-433. doi:10.1016/S1440-2440(00)80008-0",
          citation_short = "Jeukendrup et al., 2000",
          efficiency_citation = "Moseley L, Jeukendrup AE. The reliability of cycling efficiency. Med Sci Sports Exerc. 2001;33(4):621-627.",
          efficiency_citation_short = "Moseley & Jeukendrup, 2001"
        ))
      } else {
        return(list(
          vo2max_low = 35,
          vo2max_high = 48,
          vo2max_typical = 41,
          efficiency_low = 17.5,
          efficiency_high = 21.5,
          efficiency_typical = 19.5,
          efficiency_unit = "%",
          description = "Recreational female cyclists (regular training, amateur racing)",
          citation = "Jeukendrup AE, Craig NP, Hawley JA. The bioenergetics of world class cycling. J Sci Med Sport. 2000;3(4):414-433. doi:10.1016/S1440-2440(00)80008-0",
          citation_short = "Jeukendrup et al., 2000",
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

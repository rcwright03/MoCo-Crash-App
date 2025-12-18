# import dataset and use functions here
library(dplyr)
library(lubridate)
library(tidyr)

crash_df = read.csv("Data/crash_data_truncated.csv")

# create group of missing tokens
missing_tokens <- c(
  "n/a", '', 'unknown', 'other'
)
# set all missing/unknown/n/a tokens to NA to either remove or impute
crash_df <- crash_df %>%
  mutate(
    across(
      where(is.character),
      ~ {
        x <- tolower(trimws(.x))
        ifelse(x %in% missing_tokens, NA, x)
      }
    )
  )
print(nrow(crash_df))
handle_missing_vals(FALSE)
print(nrow(crash_df))

# function to get mode for imputation
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}
# mode imputation function
mode_impute <- function(df) {
  df %>%
    mutate(
      across(
        where(~ is.character(.x) || is.factor(.x)),
        ~ {
          mode_val <- get_mode(.x)
          ifelse(is.na(.x), mode_val, .x)
        }
      )
    )
}

# imputate missing values or remove them (depending on user response)
handle_missing_vals <- function(df, impute=FALSE) {
  if (impute == TRUE) {
    # perform imputation
    df <- mode_impute()
  } else {
    df <- df %>%
      drop_na()
  }
  return(df)
}

crash_df <- handle_missing_vals(crash_df, impute=FALSE)
print(nrow(crash_df))

# group features that need to be grouped
grouped_df <- crash_df %>%
  mutate(
    # parse date time col
    Crash_DateTime = mdy_hms(Crash.Date.Time, quiet=TRUE),
    # group by quarter
    Crash_Quarter = case_when(
      quarter(Crash_DateTime) == 1 ~ "Q1",
      quarter(Crash_DateTime) == 2 ~ "Q2",
      quarter(Crash_DateTime) == 3 ~ "Q3",
      quarter(Crash_DateTime) == 4 ~ "Q4"
    ),
    # group by time
    Time_of_day = case_when(
      hour(Crash_DateTime) >= 0 & hour(Crash_DateTime) < 6 ~ "12:00AM - 5:59AM",
      hour(Crash_DateTime) >= 6 & hour(Crash_DateTime) < 12 ~ "6:00AM - 11:59PM",
      hour(Crash_DateTime) >= 12 & hour(Crash_DateTime) < 18 ~ "12:00PM - 5:59PM",
      hour(Crash_DateTime) >= 18 & hour(Crash_DateTime) < 24 ~ "6:00AM - 11:59PM"
    )
  )

testTable <- table(crash_df$ACRS.Report.Type)
print(testTable)

# group route type
grouped_crash_df <- crash_df %>%
  mutate(
    Route_Type_Grouped = case_when(
      Route.Type %in% c("interstate (state)") ~ "Interstate",
      Route.Type %in% c("maryland (state) route", "maryland (state)", "us (state)") ~ "US/State Route",
      Route.Type %in% c("county route", "county", "local route") ~ "County/Local",
      Route.Type %in% c("municipality route", "municipality", "government route", "government") ~ "Municipal/Gov",
      Route.Type %in% c("ramp", "spur", "service road", "crossover") ~ "Ramp/Spur/Service",
      Route.Type %in% c("other public roadway", "private route", "bicycle route") ~ "Other/Private",
      Route.Type %in% c("unknown") ~ "Unknown",
      TRUE ~ "Other"  # catch any unexpected values
    )
  )

# group weather
grouped_crash_df <- grouped_crash_df %>%
  mutate(
    Weather_Grouped = case_when(
      Weather %in% c("rain", "raining", "freezing rain or freezing drizzle") ~ "Rain",
      Weather %in% c("snow", "sleet or hail", "sleet", "wintry mix", "blowing snow") ~ "Snow/Sleet",
      Weather %in% c("clear", "cloudy") ~ "Clear/Cloudy",
      Weather %in% c("fog, smog, smoke", "foggy") ~ "Fog/Smoke",
      Weather %in% c("severe crosswinds", "severe winds", "blowing sand, soil, dirt") ~ "Severe Winds",
      Weather %in% c("unknown", "n/a", "other") ~ "Unknown/Other",
      TRUE ~ "Other"
    )
  )

# group surface conditions
grouped_crash_df <- grouped_crash_df %>%
  mutate(
    Surface_Condition_Grouped = case_when(
      Surface.Condition %in% c("dry", "0") ~ "Dry",
      Surface.Condition %in% c("wet", "water (standing, moving)", "water(standing/moving)") ~ "Wet",
      Surface.Condition %in% c("snow", "ice/frost", "ice", "slush") ~ "Snow/Ice/Slush",
      Surface.Condition %in% c("mud, dirt, gravel", "sand", "oil") ~ "Mud/Dirt/Gravel",
      Surface.Condition %in% c("other", "n/a", "unknown") ~ "Unknown/Other",
      TRUE ~ "Other"
    )
  )

# group light
grouped_crash_df <- grouped_crash_df %>%
  mutate(
    Light_Grouped = case_when(
      Light %in% c("daylight") ~ "Daylight",
      Light %in% c("dark - lighted", "dark lights on", "dark no lights") ~ "Dark - Lighted",
      Light %in% c("dark - not lighted") ~ "Dark - Not Lighted",
      Light %in% c("dawn", "dusk") ~ "Dawn/Dusk",
      Light %in% c("dark - unknown lighting", "dark -- unknown lighting", "other", "unknown", "n/a") ~ "Unknown/Other",
      TRUE ~ "Other"
    )
  )

# group traffic control
grouped_crash_df <- grouped_crash_df %>%
  mutate(
    Traffic_Control_Grouped = case_when(
      Traffic.Control %in% c("no controls", "0") ~ "No Control",
      Traffic.Control %in% c("stop sign", "yield sign") ~ "Stop/Yield",
      Traffic.Control %in% c("traffic control signal", "traffic signal", "flashing traffic control signal",
                             "other signal", "lane use control signal", "flashing traffic signal", "ramp meter signal") ~ "Traffic Signal",
      Traffic.Control %in% c("railroad crossing", "flashing railroad crossing signal (may include gates)",
                             "railway crossing device") ~ "Railroad",
      Traffic.Control %in% c("other pavement marking (excluding edgelines, centerlines, or lane lines)",
                             "reduce speed ahead warning sign", "curve ahead warning sign", "other warning sign",
                             "intersection ahead warning sign", "school zone", "school zone sign",
                             "school zone sign device", "bicycle crossing sign", "pedestrian crossing sign",
                             "pedestrian crossing") ~ "Warning/Pavement",
      Traffic.Control %in% c("person (including flagger, law enforcement, crossing guard, etc.)", "person") ~ "Person/Flagger",
      Traffic.Control %in% c("other", "n/a", "unknown", "warning sign") ~ "Unknown/Other",
      TRUE ~ "Other"
    )
  )

# group driver substance abuse column
grouped_crash_df <- grouped_crash_df %>%
  mutate(
    Driver_Substance_Grouped = case_when(
      Driver.Substance.Abuse %in% c("not suspect of alcohol use, not suspect of drug use", "none detected") ~ "No Substance Detected",
      Driver.Substance.Abuse %in% c("suspect of alcohol use, not suspect of drug use", "suspect of alcohol use, unknown",
                                    "alcohol present", "alcohol contributed") ~ "Suspected Alcohol",
      Driver.Substance.Abuse %in% c("not suspect of alcohol use, suspect of drug use", "illegal drug present",
                                    "illegal drug contributed") ~ "Suspected Drugs",
      Driver.Substance.Abuse %in% c("medication contributed", "medication present", "combination contributed",
                                    "combined substance present") ~ "Medication/Combination",
      Driver.Substance.Abuse %in% c("unknown, unknown", "unknown, not suspect of drug use",
                                    "unknown, suspect of drug use", "not suspect of alcohol use, unknown",
                                    "other", "unknown", "n/a") ~ "Unknown/Other",
      TRUE ~ "Other"
    )
  )

# group driver distracted by col
grouped_crash_df <- grouped_crash_df %>%
  mutate(
    Driver_Distracted_Grouped = case_when(
      Driver.Distracted.By %in% c("not distracted", "0", "no driver present") ~ "Not Distracted",
      Driver.Distracted.By %in% c("talking/listening", "talking or listening to cellular phone",
                                  "other cellular phone related", "dialing cellular phone",
                                  "other electronic device (navigational palm pilot)",
                                  "using other device controls integral to vehicle") ~ "Cellphone/Electronic",
      Driver.Distracted.By %in% c("manually operating (dialing, playing game, etc.)",
                                  "adjusting audio and or climate controls",
                                  "using device object brought into vehicle",
                                  "by moving object in vehicle", "eating or drinking", "smoking related") ~ "Manual/In-Vehicle Actions",
      Driver.Distracted.By %in% c("looked but did not see", "inattentive or lost in thought",
                                  "distracted by outside person object or event", "by other occupants") ~ "Looked/Inattentive/External",
      Driver.Distracted.By %in% c("unknown", "other", "other action (looking away from task, etc.)") ~ "Unknown/Other",
      TRUE ~ "Other"
    )
  )

# group vehicle first impact location
grouped_crash_df <- grouped_crash_df %>%
  mutate(
    First_Impact_Grouped = case_when(
      Vehicle.First.Impact.Location %in% c("12 o clock", "12 oclock") ~ "Front",
      Vehicle.First.Impact.Location %in% c("1 o clock", "2 o clock", "1 oclock", "2 oclock") ~ "Front-Right",
      Vehicle.First.Impact.Location %in% c("3 o clock", "3 oclock") ~ "Right",
      Vehicle.First.Impact.Location %in% c("4 o clock", "5 o clock", "4 oclock", "5 oclock") ~ "Back-Right",
      Vehicle.First.Impact.Location %in% c("6 o clock", "6 oclock") ~ "Rear",
      Vehicle.First.Impact.Location %in% c("7 o clock", "8 o clock", "7 oclock", "8 oclock") ~ "Back-Left",
      Vehicle.First.Impact.Location %in% c("9 o clock", "9 oclock") ~ "Left",
      Vehicle.First.Impact.Location %in% c("10 o clock", "11 o clock", "10 oclock", "11 oclock") ~ "Front-Left",
      Vehicle.First.Impact.Location %in% c("top", "roof top") ~ "Top",
      Vehicle.First.Impact.Location %in% c("underside") ~ "Underside",
      Vehicle.First.Impact.Location %in% c("non-collision", "cargo loss") ~ "Non-Collision",
      Vehicle.First.Impact.Location %in% c("vehicle not at scene") ~ "Vehicle Not at Scene",
      Vehicle.First.Impact.Location %in% c("unknown", "0") ~ "Unknown/Other",
      TRUE ~ "Other"
    )
  )

# group vehicle movement column
grouped_crash_df <- grouped_crash_df %>%
  mutate(
    Vehicle_Movement_Grouped = case_when(
      Vehicle.Movement %in% c("moving constant speed", "entering traffic lane", "accelerating",
                              "starting from lane", "driverless moving veh.") ~ "Moving Straight/Constant",
      Vehicle.Movement %in% c("turning left", "turning right", "making left turn", "making right turn",
                              "making u-turn", "making u turn", "changing lanes", "leaving traffic lane",
                              "right turn on red", "overtaking/passing", "passing") ~ "Turning/Lane Changes",
      Vehicle.Movement %in% c("slowing or stopping", "stopped in traffic", "stopped in traffic lane",
                              "parking", "parked", "starting from parked") ~ "Stopping/Slowing/Parked",
      Vehicle.Movement %in% c("negotiating a curve", "skidding") ~ "Negotiating Curve/Skidding",
      Vehicle.Movement %in% c("0", "unknown", "n/a", "other") ~ "Unknown/Other",
      TRUE ~ "Other"
    )
  )

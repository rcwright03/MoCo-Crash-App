# import dataset and use functions here
library(dplyr)
library(lubridate)
library(tidyr)
library(DescTools)
library(reshape2)
library(ggplot2)
library(plotly)
library(mice)
library(missForest)
library(nnet)
library(caret)
library(e1071)
library(naivebayes)
library(class)

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
        x[x %in% missing_tokens] <- NA
        x
      }
    )
  )
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}
# mode imputation (using mode for imputation b/c other methods take a very long time)
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
# impute missing values or remove them (depending on user response)
handle_missing_vals <- function(df, impute=FALSE) {
  if (impute) {
    df <- mode_impute(df)
  } else {
    df <- na.omit(df)
  }
  return(df)
}

#print(nrow(crash_df)) testing print statement
imputed_crash_df <- handle_missing_vals(crash_df, TRUE)
removed_crash_df <- handle_missing_vals(crash_df, FALSE)
#print(nrow(imputed_crash_df)) testing print statement
#print(nrow(removed_crash_df)) testing print statement

group_cols <- function(grouped_df, df){
  grouped_df <- df %>%
    mutate(
      Crash_DateTime = as.POSIXct(
        Crash.Date.Time,
        format = "%m/%d/%Y %H:%M",
        tz = "UTC"
      ),
      
      Crash_Quarter = quarter(Crash_DateTime),
      
      Time_of_day = case_when(
        hour(Crash_DateTime) >= 0  & hour(Crash_DateTime) < 6  ~ "12:00AM - 5:59AM",
        hour(Crash_DateTime) >= 6  & hour(Crash_DateTime) < 12 ~ "6:00AM - 11:59AM",
        hour(Crash_DateTime) >= 12 & hour(Crash_DateTime) < 18 ~ "12:00PM - 5:59PM",
        hour(Crash_DateTime) >= 18 & hour(Crash_DateTime) < 24 ~ "6:00PM - 11:59PM",
        TRUE ~ NA_character_
      ),
      
      Route_Type_Grouped = case_when(
        Route.Type %in% c("interstate (state)") ~ "Interstate",
        Route.Type %in% c("maryland (state) route", "maryland (state)", "us (state)") ~ "US/State Route",
        Route.Type %in% c("county route", "county", "local route") ~ "County/Local",
        Route.Type %in% c("municipality route", "municipality", "government route", "government") ~ "Municipal/Gov",
        Route.Type %in% c("ramp", "spur", "service road", "crossover") ~ "Ramp/Spur/Service",
        Route.Type %in% c("other public roadway", "private route", "bicycle route") ~ "Other/Private",
        Route.Type %in% c("unknown") ~ "Unknown",
        TRUE ~ "Other"  # catch any unexpected values
      ),
      
      Weather_Grouped = case_when(
        Weather %in% c("rain", "raining", "freezing rain or freezing drizzle") ~ "Rain",
        Weather %in% c("snow", "sleet or hail", "sleet", "wintry mix", "blowing snow") ~ "Snow/Sleet",
        Weather %in% c("clear", "cloudy") ~ "Clear/Cloudy",
        Weather %in% c("fog, smog, smoke", "foggy") ~ "Fog/Smoke",
        Weather %in% c("severe crosswinds", "severe winds", "blowing sand, soil, dirt") ~ "Severe Winds",
        Weather %in% c("unknown", "n/a", "other") ~ "Unknown/Other",
        TRUE ~ "Other"
      ),
      
      Surface_Condition_Grouped = case_when(
        Surface.Condition %in% c("dry", "0") ~ "Dry",
        Surface.Condition %in% c("wet", "water (standing, moving)", "water(standing/moving)") ~ "Wet",
        Surface.Condition %in% c("snow", "ice/frost", "ice", "slush") ~ "Snow/Ice/Slush",
        Surface.Condition %in% c("mud, dirt, gravel", "sand", "oil") ~ "Mud/Dirt/Gravel",
        Surface.Condition %in% c("other", "n/a", "unknown") ~ "Unknown/Other",
        TRUE ~ "Other"
      ),
      
      Light_Grouped = case_when(
        Light %in% c("daylight") ~ "Daylight",
        Light %in% c("dark - lighted", "dark lights on", "dark no lights") ~ "Dark - Lighted",
        Light %in% c("dark - not lighted") ~ "Dark - Not Lighted",
        Light %in% c("dawn", "dusk") ~ "Dawn/Dusk",
        Light %in% c("dark - unknown lighting", "dark -- unknown lighting", "other", "unknown", "n/a") ~ "Unknown/Other",
        TRUE ~ "Other"
      ),
      
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
      ),
      
      Driver_Substance_Grouped = case_when(
        Driver.Substance.Abuse %in% c("not suspect of alcohol use, not suspect of drug use", "none detected") ~ "No Substance Detected",
        Driver.Substance.Abuse %in% c("suspect of alcohol use, not suspect of drug use", "suspect of alcohol use, unknown",
                                      "alcohol present", "alcohol contributed") ~ "Suspected Alcohol",
        Driver.Substance.Abuse %in% c("not suspect of alcohol use, suspect of drug use", "illegal drug present",
                                      "illegal drug contributed", "unknown, suspect of drug use") ~ "Suspected Drugs",
        Driver.Substance.Abuse %in% c("medication contributed", "medication present", "combination contributed",
                                      "combined substance present", "suspect of alcohol use, suspect of drug use") ~ "Medication/Combination",
        Driver.Substance.Abuse %in% c("unknown, unknown", "unknown, not suspect of drug use", "not suspect of alcohol use, unknown",
                                      "other", "unknown", "n/a", "") ~ "Unknown/Other",
        TRUE ~ "Other"
      ),
      
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
      ),
      
      First_Impact_Grouped = case_when(
        Vehicle.First.Impact.Location %in% c("twelve o clock", "twelve oclock") ~ "Front",
        Vehicle.First.Impact.Location %in% c("one o clock", "two o clock", "one oclock", "two oclock") ~ "Front-Right",
        Vehicle.First.Impact.Location %in% c("three o clock", "three oclock") ~ "Right",
        Vehicle.First.Impact.Location %in% c("four o clock", "five o clock", "four oclock", "five oclock") ~ "Back-Right",
        Vehicle.First.Impact.Location %in% c("six o clock", "six oclock") ~ "Rear",
        Vehicle.First.Impact.Location %in% c("seven o clock", "eight o clock", "seven oclock", "eight oclock") ~ "Back-Left",
        Vehicle.First.Impact.Location %in% c("nine o clock", "nine oclock") ~ "Left",
        Vehicle.First.Impact.Location %in% c("ten o clock", "eleven o clock", "ten oclock", "eleven oclock") ~ "Front-Left",
        Vehicle.First.Impact.Location %in% c("top", "roof top") ~ "Top",
        Vehicle.First.Impact.Location %in% c("underside") ~ "Underside",
        Vehicle.First.Impact.Location %in% c("non-collision", "cargo loss") ~ "Non-Collision",
        Vehicle.First.Impact.Location %in% c("vehicle not at scene") ~ "Vehicle Not at Scene",
        Vehicle.First.Impact.Location %in% c("unknown", "0") ~ "Unknown/Other",
        TRUE ~ "Other"
      ),
      
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
      ),
      
      Vehicle_Body_Type_Grouped = case_when(
        Vehicle.Body.Type %in% c("passenger car", "station wagon", "limousine") ~ "Passenger Car",
        Vehicle.Body.Type %in% c("(sport) utility vehicle", "sport utility vehicle", "pickup", "pickup truck",
                                 "cargo van/light truck 2 axles (over 10,000lbs (4,536 kg))", "van",
                                 "van - cargo", "van - passenger (<9 seats)", "van - passenger (9 or 12 seats)",
                                 "other light trucks (10,000lbs (4,536 kg) or less)", "recreational off-highway vehicles (rov)"
        ) ~ "SUV/Van/Light Pickup",
        Vehicle.Body.Type %in% c("medium/heavy trucks 3 axles (over 10,000lbs (4,536 kg))", "single-unit truck",
                                 "other trucks", "truck tractor") ~ "Heavy Truck",
        Vehicle.Body.Type %in% c("motorcycle", "motorcycle - 2 wheeled", "motorcycle - 3 wheeled", "moped",
                                 "moped or motorized bicycle", "all terrain vehicle (atv)", "autocycle",
                                 "all-terrain vehicle/all-terrain cycle (atv/atc)") ~ "Motorcycle/ATV",
        Vehicle.Body.Type %in% c("bus - mini", "bus - other type", "bus - school", "bus - transit",
                                 "cross country bus", "other bus", "school bus", "transit bus") ~ "Bus",
        Vehicle.Body.Type %in% c("ambulance/emergency", "ambulance/non emergency", "fire vehicle/emergency",
                                 "fire vehicle/non emergency", "police vehicle/emergency", "police vehicle/non emergency") ~ "Emergency",
        Vehicle.Body.Type %in% c("recreational vehicle", "farm vehicle", "low speed vehicle", "snowmobile"
        ) ~ "Other/Off-Road",
        TRUE ~ "Other"
      ),
      
      Collision_Type_Grouped = case_when(
        Collision.Type %in% c("front to rear", "same dir rear end", "same dir rend left turn",
                              "same dir rend right turn") ~ "Rear-End",
        Collision.Type %in% c("head on", "front to front") ~ "Head-On",
        Collision.Type %in% c("angle", "angle meets left head on", "angle meets left turn",
                              "angle meets right turn", "straight movement angle") ~ "Angle",
        Collision.Type %in% c("opposite direction sideswipe", "same direction sideswipe",
                              "sideswipe, opposite direction", "sideswipe, same direction") ~ "Sideswipe",
        Collision.Type %in% c("same dir both left turn", "same direction left turn", "same direction right turn",
                              "head on left turn", "opposite dir both left turn") ~ "Turn Conflict",
        Collision.Type %in% c("single vehicle") ~ "Single Vehicle",
        TRUE ~ "Other"
      ),
      
      Speed_Limit_Grouped = case_when(
        Speed.Limit < 15 & Speed.Limit >= 0 ~ '0-15',
        Speed.Limit < 30 & Speed.Limit >= 15 ~ '15-30',
        Speed.Limit < 45 & Speed.Limit >= 30 ~ '30-45',
        Speed.Limit < 60 & Speed.Limit >= 45 ~ '45-60',
        Speed.Limit >= 60 ~ '60+'
      ),
      
      ACRS_Report_Type = ACRS.Report.Type,
      Injury_Severity = Injury.Severity,
      Vehicle_Damage_Extent = Vehicle.Damage.Extent,
      Parked_Vehicle = Parked.Vehicle
    ) %>%
    select(Crash_Quarter, Time_of_day, Route_Type_Grouped, Weather_Grouped, Surface_Condition_Grouped,
           Light_Grouped, Traffic_Control_Grouped, Driver_Substance_Grouped, Driver_Distracted_Grouped,
           First_Impact_Grouped, Vehicle_Movement_Grouped, Vehicle_Body_Type_Grouped, Collision_Type_Grouped,
           Speed_Limit_Grouped, ACRS_Report_Type, Injury_Severity, Vehicle_Damage_Extent, Parked_Vehicle)
  grouped_df <- grouped_df %>%
    mutate(across(everything(), as.factor))
  return(grouped_df)
}

grouped_crash_df_imp <- group_cols(grouped_crash_df_imp, imputed_crash_df)
grouped_crash_df_rem <- group_cols(grouped_crash_df_rem, removed_crash_df)

# tables for santiy checks
# imptable <- table(grouped_crash_df_imp$Driver_Substance_Grouped)
# print(imptable)
# remtable <- table(grouped_crash_df_rem$Driver_Substance_Grouped)
# print(remtable)

# sanity checks
# crashQuarterTable <- table(grouped_crash_df$Crash_Quarter)
# print(crashQuarterTable)
# crashTimeTable <- table(grouped_crash_df$Time_of_day)
# print(crashTimeTable)
# check that there are 0 NA values
# sum(is.na(as.POSIXct(
#   crash_df$Crash.Date.Time,
#   format = "%m/%d/%Y %H:%M"
# )))

# sanity check for number of rows and cols
# crash_df_ncol <- ncol(crash_df)
# crash_df_nrow <- nrow(crash_df)
# grouped_crash_df_ncol <- ncol(grouped_crash_df)
# grouped_crash_df_nrow <- nrow(grouped_crash_df)
# print(crash_df_ncol)
# print(crash_df_nrow)
# print(grouped_crash_df_ncol)
# print(grouped_crash_df_nrow)

# create correlation matrix using cramers V
cramers_v_matrix <- function(df) {
  vars <- colnames(df)
  n <- length(vars)
  
  mat <- matrix(NA, n, n)
  colnames(mat) <- rownames(mat) <- vars
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      mat[i, j] <- CramerV(
        table(df[[i]], df[[j]]),
        bias.correct = TRUE
      )
    }
  }
  
  mat
}

corr_mat <- cramers_v_matrix(grouped_crash_df_rem)
corr_long <- melt(corr_mat)
grouped_heatmap <- ggplot(corr_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(
    aes(label = sprintf("%.2f", value)),
    size = 3,
    color = "black"
  ) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank()
  ) +
  labs(fill = "Cramér's V")

create_distribution_plot <- function(df, column) {
  plot_df <- as.data.frame(table(df[[column]]))
  colnames(plot_df) <- c("category", "count")
  
  p <- ggplot(
    plot_df,
    aes(
      x = category,
      y = count,
      fill = category,
      text = paste(
        "Category:", category,
        "<br>Count:", count
      )
    )
  ) +
    geom_col() +
    labs(
      x = column,
      y = "Frequency",
      title = paste("Distribution of", column)
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggplotly(p, tooltip = "text")
}

feature_column_map <- c(
  crashQuarter = "Crash_Quarter",
  timeOfDay = "Time_of_day",
  routeType = "Route_Type_Grouped",
  weather = "Weather_Grouped",
  surfaceCondition = "Surface_Condition_Grouped",
  light = "Light_Grouped",
  trafficControl = "Traffic_Control_Grouped",
  driverSubstanceAbuse = "Driver_Substance_Grouped",
  driverDistractedBy = "Driver_Distracted_Grouped",
  vehicleFirstImpactLocation = "First_Impact_Grouped",
  vehicleMovement = "Vehicle_Movement_Grouped",
  vehicleBodyType = "Vehicle_Body_Type_Grouped",
  collisionType = "Collision_Type_Grouped",
  speedLimit = "Speed_Limit_Grouped",
  acrsReportType = "ACRS_Report_Type",
  injurySeverity = "Injury_Severity",
  vehicleDamageExtent = "Vehicle_Damage_Extent",
  parkedVehicle = "Parked_Vehicle"
)

processData <- function(trainingSize, impute=FALSE) {
  if(impute){
    df <- grouped_crash_df_imp
  } else {
    df <- grouped_crash_df_rem
  }
  set.seed(42)
  # print("start") testing print statement
  split_idx <- sample(nrow(df), (trainingSize/100)*nrow(df))
  
  training_data <- df[split_idx, ]
  testing_data  <- df[-split_idx, ]
  
  # print("success") testing print statement
  return(list(training=training_data, testing=testing_data))
}

# create random forest model
createRF <- function(targetVar, numTrees, varPerSplit, train_data, test_data){
  library(randomForest)
  if (targetVar == "Injury_Severity"){
    # create model to classify injury severity
    rfModel <- randomForest(Injury_Severity ~ . - ACRS_Report_Type - Vehicle_Damage_Extent,
                            data=train_data, ntree=numTrees, mtry=sqrt(varPerSplit), importance=TRUE)
    # make prediction with test data
    # produce confusion matrix and graph
  } else if (targetVar == "ACRS_Report_Type") {
    # create model to classify ACRS report type
    rfModel <- randomForest(ACRS_Report_Type ~ . - Injury_Severity - Vehicle_Damage_Extent,
                            data=train_data, ntree=numTrees, mtry=sqrt(varPerSplit), importance=TRUE)
  } else if (targetVar == "Vehicle_Damage_Extent") {
    # create model to classify vehicle damage extent
    rfModel <- randomForest(Vehicle_Damage_Extent ~ . - ACRS_Report_Type - Injury_Severity,
                            data=train_data, ntree=numTrees, mtry=sqrt(varPerSplit), importance=TRUE)
  }
  # make predictions, display confusion matrix
  predictions <- predict(rfModel, newdata = test_data, type="class")
  
  cm <- confusionMatrix(
    data = predictions,
    reference=test_data[[targetVar]]
  )
  by_class <- as.data.frame(cm$byClass)
  # print(cm) print for testing
  
  # return confusion matrix, metrics, and model
  list(
    cm_df = as.data.frame(cm$table),
    accuracy = as.numeric(cm$overall["Accuracy"]),
    kappa = as.numeric(cm$overall["Kappa"]),
    model = rfModel, # return model for eventual feature importance plot
    macro_metrics = c(
      precision = mean(by_class$Precision, na.rm = TRUE),
      recall    = mean(by_class$Sensitivity, na.rm = TRUE),
      f1        = mean(by_class$F1, na.rm = TRUE)
    ),
    weighted_metrics = c(
      precision = sum(by_class$Precision * colSums(cm$table) / sum(cm$table), na.rm = TRUE),
      recall    = sum(by_class$Sensitivity * colSums(cm$table) / sum(cm$table), na.rm = TRUE),
      f1        = sum(by_class$F1 * colSums(cm$table) / sum(cm$table), na.rm = TRUE)
    )
  )
}
# create logistic regression model
createLogReg <- function(targetVar, train_data, test_data) {
  if (targetVar == "Injury_Severity"){
    # create model to classify injury severity
    lrModel <- multinom(Injury_Severity ~ . - ACRS_Report_Type - Vehicle_Damage_Extent,
                        data=train_data)
  } else if (targetVar == "ACRS_Report_Type") {
    # create model to classify ACRS report type
    lrModel <- multinom(ACRS_Report_Type ~ . - Injury_Severity - Vehicle_Damage_Extent,
                        data=train_data)
  } else if (targetVar == "Vehicle_Damage_Extent") {
    # create model to classify vehicle damage extent
    lrModel <- multinom(Vehicle_Damage_Extent ~ . - ACRS_Report_Type - Injury_Severity,
                        data=train_data)
  }
  predictions <- predict(lrModel, newdata=test_data, type='class')
  cm <- confusionMatrix(
    data = predictions,
    reference=test_data[[targetVar]]
  )
  by_class <- as.data.frame(cm$byClass)
  # print(cm) print for testing
  
  list(
    cm_df = as.data.frame(cm$table),
    accuracy = as.numeric(cm$overall["Accuracy"]),
    kappa = as.numeric(cm$overall["Kappa"]),
    macro_metrics = c(
      precision = mean(by_class$Precision, na.rm = TRUE),
      recall    = mean(by_class$Sensitivity, na.rm = TRUE),
      f1        = mean(by_class$F1, na.rm = TRUE)
    ),
    weighted_metrics = c(
      precision = sum(by_class$Precision * colSums(cm$table) / sum(cm$table), na.rm = TRUE),
      recall    = sum(by_class$Sensitivity * colSums(cm$table) / sum(cm$table), na.rm = TRUE),
      f1        = sum(by_class$F1 * colSums(cm$table) / sum(cm$table), na.rm = TRUE)
    )
  )
}
# naive bayes model
createNaiveBayes <- function(targetVar, useKFold, train_data, test_data) {
  if (useKFold) {
    train_control <- trainControl(
      method='cv',
      number = 5,
      savePredictions='final',
      classProbs=FALSE
    )
  }
  if (targetVar == "Injury_Severity"){
    # create model to classify injury severity
    if (useKFold) {
      nbModel <- caret::train(
        Injury_Severity ~ . - ACRS_Report_Type - Vehicle_Damage_Extent,
        data=train_data,
        method="naive_bayes",
        trControl = train_control
      )
    } else {
      nbModel <- naive_bayes(Injury_Severity ~ . - ACRS_Report_Type - Vehicle_Damage_Extent,
                            data=train_data)
    }
  } else if (targetVar == "ACRS_Report_Type") {
    # create model to classify ACRS report type
    if (useKFold) {
      nbModel <- caret::train(
        ACRS_Report_Type ~ . - Injury_Severity - Vehicle_Damage_Extent,
        data=train_data,
        method="naive_bayes",
        trControl = train_control
      )
    } else {
      nbModel <- naive_bayes(ACRS_Report_Type ~ . - Injury_Severity - Vehicle_Damage_Extent,
                            data=train_data)
    }
  } else if (targetVar == "Vehicle_Damage_Extent") {
    # create model to classify vehicle damage extent
    if (useKFold) {
      nbModel <- caret::train(
        Vehicle_Damage_Extent ~ . - Injury_Severity - ACRS_Report_Type,
        data=train_data,
        method="naive_bayes",
        trControl = train_control
      )
    } else {
      nbModel <- naive_bayes(Vehicle_Damage_Extent ~ . - Injury_Severity - ACRS_Report_Type,
                            data=train_data)
    }
  }
  predictions <- predict(nbModel, newdata=test_data)
  cm <- confusionMatrix(
    data = predictions,
    reference=test_data[[targetVar]]
  )
  by_class <- as.data.frame(cm$byClass)
  # print(cm) print for testing
  
  list(
    cm_df = as.data.frame(cm$table),
    accuracy = as.numeric(cm$overall["Accuracy"]),
    kappa = as.numeric(cm$overall["Kappa"]),
    macro_metrics = c(
      precision = mean(by_class$Precision, na.rm = TRUE),
      recall    = mean(by_class$Sensitivity, na.rm = TRUE),
      f1        = mean(by_class$F1, na.rm = TRUE)
    ),
    weighted_metrics = c(
      precision = sum(by_class$Precision * colSums(cm$table) / sum(cm$table), na.rm = TRUE),
      recall    = sum(by_class$Sensitivity * colSums(cm$table) / sum(cm$table), na.rm = TRUE),
      f1        = sum(by_class$F1 * colSums(cm$table) / sum(cm$table), na.rm = TRUE)
    )
  )
}
# knn model
createKNN <- function(targetVar, kVal, train_data, test_data) {
  if (targetVar == "Injury_Severity"){
    # create model to classify injury severity
    # remove unwanted features
    train_x <- train_data %>%
      select(-ACRS_Report_Type, -Vehicle_Damage_Extent)
    test_x <- test_data %>%
      select(-ACRS_Report_Type, -Vehicle_Damage_Extent)
    # convert factors to numeric via one-hot encoding
    train_x <- model.matrix(~ . -1, data = train_x)
    test_x  <- model.matrix(~ . -1, data = test_x)
    # scale data
    train_x <- scale(train_x)
    
    test_x <- scale(
      test_x,
      center = attr(train_x, "scaled:center"),
      scale  = attr(train_x, "scaled:scale")
    )
    
    knn_pred <- knn(train=train_x,
                    test=test_y,
                    cl=train_data[[targetVar]],
                    k=kVal)
  } else if (targetVar == "ACRS_Report_Type") {
    # create model to classify ACRS report type
    # remove unwanted features
    train_x <- train_data %>%
      select(-Injury_Severity, -Vehicle_Damage_Extent)
    test_x <- test_data %>%
      select(-Injury_Severity, -Vehicle_Damage_Extent)
    # convert factors to numeric via one-hot encoding
    train_x <- model.matrix(~ . -1, data = train_x)
    test_x  <- model.matrix(~ . -1, data = test_x)
    # scale data
    train_x <- scale(train_x)
    
    test_x <- scale(
      test_x,
      center = attr(train_x, "scaled:center"),
      scale  = attr(train_x, "scaled:scale")
    )
    
    knn_pred <- knn(train=train_x,
                    test=test_x,
                    cl=train_data[[targetVar]],
                    k=kVal)
  } else if (targetVar == "Vehicle_Damage_Extent") {
    # create model to classify vehicle damage extent
    # remove unwanted features
    train_x <- train_data %>%
      select(-ACRS_Report_Type, -Injury_Severity)
    test_x <- test_data %>%
      select(-ACRS_Report_Type, -Injury_Severity)
    # convert factors to numeric via one-hot encoding
    train_x <- model.matrix(~ . -1, data = train_x)
    test_x  <- model.matrix(~ . -1, data = test_x)
    # scale data
    train_x <- scale(train_x)
    
    test_x <- scale(
      test_x,
      center = attr(train_x, "scaled:center"),
      scale  = attr(train_x, "scaled:scale")
    )
    knn_pred <- knn(train=train_x,
                    test=test_x,
                    cl=train_data[[targetVar]],
                    k=kVal)
  }
  cm <- confusionMatrix(
    data=knn_pred,
    reference=test_data[[targetVar]]
  )
  by_class <- as.data.frame(cm$byClass)
  # print(cm) print for testing
  
  list(
    cm_df = as.data.frame(cm$table),
    accuracy = as.numeric(cm$overall["Accuracy"]),
    kappa = as.numeric(cm$overall["Kappa"]),
    macro_metrics = c(
      precision = mean(by_class$Precision, na.rm = TRUE),
      recall    = mean(by_class$Sensitivity, na.rm = TRUE),
      f1        = mean(by_class$F1, na.rm = TRUE)
    ),
    weighted_metrics = c(
      precision = sum(by_class$Precision * colSums(cm$table) / sum(cm$table), na.rm = TRUE),
      recall    = sum(by_class$Sensitivity * colSums(cm$table) / sum(cm$table), na.rm = TRUE),
      f1        = sum(by_class$F1 * colSums(cm$table) / sum(cm$table), na.rm = TRUE)
    )
  )
}
# svm model
createSVM <- function(targetVar, kernelType, costParam, train_data, test_data) {
  if (targetVar == "Injury_Severity"){
    # create model to classify injury severity
    svmModel <- svm(Injury_Severity ~ . - ACRS_Report_Type - Vehicle_Damage_Extent,
                      data=train_data,
                      type='C-classification',
                      kernel=kernelType,
                      gamma=0.1,
                      cost=costParam
                      )
  } else if (targetVar == "ACRS_Report_Type") {
    # create model to classify ACRS report type
    print(Sys.time())
    svmModel <- svm(ACRS_Report_Type ~ . - Injury_Severity - Vehicle_Damage_Extent,
                    data=train_data,
                    type='C-classification',
                    kernel=kernelType,
                    gamma=0.1,
                    cost=costParam
    )
  } else if (targetVar == "Vehicle_Damage_Extent") {
    # create model to classify vehicle damage extent
    svmModel <- svm(Vehicle_Damage_Extent ~ . - ACRS_Report_Type - Injury_Severity,
                    data=train_data,
                    type='C-classification',
                    kernel=kernelType,
                    gamma=0.1,
                    cost=costParam
    )
  }
  print(Sys.time())
  predictions <- predict(svmModel, newdata=test_data)
  cm <- confusionMatrix(
    data = predictions,
    reference=test_data[[targetVar]]
  )
  by_class <- as.data.frame(cm$byClass)
  # print(cm) print for testing
  
  list(
    cm_df = as.data.frame(cm$table),
    accuracy = as.numeric(cm$overall["Accuracy"]),
    kappa = as.numeric(cm$overall["Kappa"]),
    macro_metrics = c(
      precision = mean(by_class$Precision, na.rm = TRUE),
      recall    = mean(by_class$Sensitivity, na.rm = TRUE),
      f1        = mean(by_class$F1, na.rm = TRUE)
    ),
    weighted_metrics = c(
      precision = sum(by_class$Precision * colSums(cm$table) / sum(cm$table), na.rm = TRUE),
      recall    = sum(by_class$Sensitivity * colSums(cm$table) / sum(cm$table), na.rm = TRUE),
      f1        = sum(by_class$F1 * colSums(cm$table) / sum(cm$table), na.rm = TRUE)
    )
  )
}

# tables to display for each model
# random forest: feature importance table
# logistic regression: feature coefficient table
# naive bayes: mean and variance per feature per class - top features per class
# knn: graph of distance to nearest neighbors for correct vs incorrect predictions
# svm: number of support vectors
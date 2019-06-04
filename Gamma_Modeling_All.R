library(data.table)
library(tidyverse)
library(plyr)
library(lubridate)
library(zoo)
library(ranger)
library(caret)
library(e1071)
library(gbm)    
library(xgboost)
library(dplyr)

# Clear environment
rm(list=ls())

Sys.setenv(TZ='Europe/Budapest')

# FOLDER DEFINITIONS
dirs <- read.csv("C:/Users/B19883/Documents/R_AUTORUNS/SOURCE.csv", sep = ";", stringsAsFactors = F)
dir <- dirs[1,"Source"]
dir_upl <- dirs[2, "Source"]
dir_dashboard <- dirs[3, "Source"]
dir_gamma_home <- paste0(dirs[5,"Source"], "2019_Q1_Menetrend_Gamma/") 

##subdirectories
raw <- paste0(dir_gamma_home, "raw/")
clean <- paste0(dir_gamma_home,"clean/")
output <- paste0(dir_gamma_home,"output/")


source(paste0(dir_gamma_home, "Gamma_Functions.R"))

############################ I. DATABASE REFRESH #############################


DS_refresh()

DBMerge()



############################ II. DATA PREPARATION #############################

data_raw <- fread(paste0(clean, "Database.csv"), sep = ";", dec = ",") %>% 
  mutate(datetime = as.POSIXct((datetime), format = "%Y-%m-%d %T")) %>% 
  mutate(month = lubridate::month(datetime))


data <- data_raw %>%
  filter(TOA != 0) %>%
  # filter(ForecastOrNot == 1) %>%
  mutate(P_perc = termeles_kW / TOA) %>% 
  select(datetime, temperature, month, cloudCover, icon, precipProbability, precipIntensity, P_perc,
         TOA, month, humidity, dewPoint, ForecastOrNot)
  

data_test_dt <- data %>% filter(as.Date(datetime, tz = "CET") > "2019-05-23")

data_train <- data %>% 
  rbind(., data %>% filter(as.Date(datetime, tz = "CET") > today(tzone = "CET") - days(30))) %>% 
  rbind(., data %>% filter(as.Date(datetime, tz = "CET") > today(tzone = "CET") - days(30))) %>% 
  rbind(., data %>% filter(as.Date(datetime, tz = "CET") > today(tzone = "CET") - days(30))) %>% 
  rbind(., data %>% filter(as.Date(datetime, tz = "CET") > today(tzone = "CET") - days(30))) %>% 
  rbind(., data %>% filter(as.Date(datetime, tz = "CET") > today(tzone = "CET") - days(30))) %>% 
  rbind(., data %>% filter(as.Date(datetime, tz = "CET") > today(tzone = "CET") - days(30))) %>% 
  rbind(., data %>% filter(as.Date(datetime, tz = "CET") > today(tzone = "CET") - days(30))) %>% 
  filter(!datetime %in% data_test_dt$datetime) %>% 
  select(-datetime) %>%
  na.omit()


summary(data_train)
############################ III. MODELING #############################

#################### 1. LINEAR MODEL ##################
train_control <- trainControl(method = "cv", number = 2, verboseIter = TRUE)


set.seed(857)
linear_model <- train(P_perc ~ .,
                      method = "lm",
                      data = data_train,
                      trControl = train_control)
linear_model
summary(linear_model)


#################### 2. SIMPLE TREE MODEL ##################

set.seed(857)
simple_tree_model <- train(P_perc ~ .,
                           method = "rpart",
                           data = data_train,
                           tuneGrid = data.frame(cp = c(0.001,0.01, 0.02, 0.05)),
                           trControl = train_control,
                           control = list(maxdepth = 3))
simple_tree_model

rpart.plot::rpart.plot(simple_tree_model[["finalModel"]], cex=1)


#################### 3. RANDOM FOREST ######################

set.seed(857)
tune_grid <- expand.grid(
  .mtry = c(6, 7, 8, 9),
  .splitrule = "variance",
  .min.node.size = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
)

rf_model <- train(
  formula(P_perc ~ .),
  data = data_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "permutation",
  preProcess = c("center", "scale")
)


p <- 0.4
data_test_dt <- data_test_dt %>% 
  filter(ForecastOrNot == 1) %>%
  mutate(predicted_P_rf = 
           TOA * ifelse(TOA != 0, predict(rf_model, newdata = 
                                            (data_test_dt %>% mutate(icon = ifelse(icon == "cloudy", "partly-cloudy-day", 
                                                                                   ifelse(icon == "wind", "clear-day", icon))) %>%
                                               mutate(cloudCover = cloudCover * p) %>% 
                                               mutate(precipProbability = ifelse(precipProbability > 0.5, precipProbability * p, 0)) %>% 
                                               mutate(ifelse(precipIntensity > 0.5, precipIntensity * p, 0)))), 0)) %>% 
  mutate(ifelse(predicted_P_rf <= 0, 0, ifelse(predicted_P > 500, 500, predicted_P_rf))) %>% 
  mutate(predicted_P_rf = base::round(predicted_P_rf, 0)) %>% 
  mutate(P = TOA * P_perc)

data_test_dt %>% ggplot(aes(datetime, P)) + geom_line() + geom_line(aes(datetime, predicted_P_rf), color = "Red") + 
  geom_line(aes(datetime, TOA), color = "green")



data_test_dt <- data_test_dt %>% 
  mutate(Termeles_kWh = P / 4,
         Sch_dayahead_kWh = predicted_P_rf / 4) %>% 
  mutate(Bevetel_alap_HUF = Termeles_kWh * 32.59,
         Bonusz_HUF = ifelse((P * 0.5 <= predicted_P_rf & P * 1.5 >= predicted_P_rf),
                             ifelse(Termeles_kWh > Sch_dayahead_kWh, (Sch_dayahead_kWh - Termeles_kWh * 0.5) * 3,
                                    (Termeles_kWh * 1.5 - Sch_dayahead_kWh) * 3), 0),
         Bevetel_ossz_HUF = Bevetel_alap_HUF + Bonusz_HUF)

sum(data_test_dt$Bonusz_HUF)


#################### 4. GRADIENT BOOSTING ######################


gbm_grid <- expand.grid(n.trees = c(500, 1000), 
                        interaction.depth = c(7, 10, 15, 20), 
                        shrinkage = c(0.1, 0.2),
                        n.minobsinnode = c(10, 20))
set.seed(857)
gbm_model <- train(P_perc ~ .,
                   method = "gbm",
                   data = data_train,
                   trControl = train_control,
                   preProcess = c("center", "scale"),
                   tuneGrid = gbm_grid,
                   verbose = FALSE # gbm by default prints too much output
)

p <- 0.4
data_test_dt <- data_test_dt %>% 
  filter(ForecastOrNot == 1) %>%
  mutate(predicted_P_gbm = 
           TOA * ifelse(TOA != 0, predict(xgboost_model, newdata = 
                                            (data_test_dt %>% mutate(icon = ifelse(icon == "cloudy", "partly-cloudy-day", 
                                                                                   ifelse(icon == "wind", "clear-day", icon))) %>%
                                               mutate(cloudCover = cloudCover * p) %>% 
                                               mutate(precipProbability = ifelse(precipProbability > 0.5, precipProbability * p, 0)) %>% 
                                               mutate(ifelse(precipIntensity > 0.5, precipIntensity * p, 0)))), 0)) %>% 
  mutate(ifelse(predicted_P_gbm <= 0, 0, ifelse(predicted_P > 500, 500, predicted_P_gbm))) %>% 
  mutate(predicted_P_gbm = base::round(predicted_P_gbm, 0)) %>% 
  mutate(P = TOA * P_perc)

data_test_dt %>% ggplot(aes(datetime, P)) + geom_line() + geom_line(aes(datetime, predicted_P_gbm), color = "Red") + 
  geom_line(aes(datetime, TOA), color = "green")



data_test_dt <- data_test_dt %>% 
  mutate(Termeles_kWh = P / 4,
         Sch_dayahead_kWh_gbm = predicted_P_gbm / 4) %>% 
  mutate(Bevetel_alap_HUF = Termeles_kWh * 32.59,
         Bonusz_HUF = ifelse((P * 0.5 <= predicted_P_gbm & P * 1.5 >= predicted_P_gbm),
                             ifelse(Termeles_kWh > Sch_dayahead_kWh_gbm, (Sch_dayahead_kWh_gbm - Termeles_kWh * 0.5) * 3,
                                    (Termeles_kWh * 1.5 - Sch_dayahead_kWh_gbm) * 3), 0),
         Bevetel_ossz_HUF = Bevetel_alap_HUF + Bonusz_HUF)

sum(data_test_dt$Bonusz_HUF)






#################### 5. XG BOOSTING ######################
xgb_grid <- expand.grid(nrounds = c(500, 1000),
                        max_depth = c(7, 10, 15, 20),
                        eta = c(0.1, 0.2, 0.3),
                        gamma = c(0, 0.1, 0.2),
                        colsample_bytree = c(0.65, 0.8),
                        min_child_weight = c(0.2, 0.3), # similar to n.minobsinnode
                        subsample = c(0.7, 0.8, 0.9))
set.seed(857)
xgboost_model <- train(P_perc ~ .,
                       method = "xgbTree",
                       data = data_train,
                       trControl = train_control,
                       preProcess = c("center", "scale"),
                       tuneGrid = xgb_grid)
xgboost_model

p <- 0.4
data_test_dt <- data_test_dt %>% 
  filter(ForecastOrNot == 1) %>%
  mutate(predicted_P_xgb = 
           TOA * ifelse(TOA != 0, predict(xgboost_model, newdata = 
                                            (data_test_dt %>% mutate(icon = ifelse(icon == "cloudy", "partly-cloudy-day", 
                                                                                   ifelse(icon == "wind", "clear-day", icon))) %>%
                                               mutate(cloudCover = cloudCover * p) %>% 
                                               mutate(precipProbability = ifelse(precipProbability > 0.5, precipProbability * p, 0)) %>% 
                                               mutate(ifelse(precipIntensity > 0.5, precipIntensity * p, 0)))), 0)) %>% 
  mutate(ifelse(predicted_P_xgb <= 0, 0, ifelse(predicted_P > 500, 500, predicted_P_xgb))) %>% 
  mutate(predicted_P_xgb = base::round(predicted_P_xgb, 0)) %>% 
  mutate(P = TOA * P_perc)

data_test_dt %>% ggplot(aes(datetime, P)) + geom_line() + geom_line(aes(datetime, predicted_P_xgb), color = "Red") + 
  geom_line(aes(datetime, TOA), color = "green")



data_test_dt <- data_test_dt %>% 
  mutate(Termeles_kWh = P / 4,
         Sch_dayahead_kWh_xgb = predicted_P_xgb / 4) %>% 
  mutate(Bevetel_alap_HUF = Termeles_kWh * 32.59,
         Bonusz_HUF = ifelse((P * 0.5 <= predicted_P_xgb & P * 1.5 >= predicted_P_xgb),
                             ifelse(Termeles_kWh > Sch_dayahead_kWh_xgb, (Sch_dayahead_kWh_xgb - Termeles_kWh * 0.5) * 3,
                                    (Termeles_kWh * 1.5 - Sch_dayahead_kWh_xgb) * 3), 0),
         Bevetel_ossz_HUF = Bevetel_alap_HUF + Bonusz_HUF)

sum(data_test_dt$Bonusz_HUF)


##



save(xgboost_model, file = paste0(dir_gamma_home, "models/", Sys.Date(), ".rda"))




## MODEL SELECTION
resamples_object <- resamples(list("linear_model" = linear_model,
                                   "simple_tree_model" = simple_tree_model,
                                    "rf_model" = rf_model,
                                   "gbm_model" = gbm_model,
                                   "xgboost_model" = xgboost_model))
models <- summary(resamples_object)

models_cvrmse <- as.data.frame(models$statistics$RMSE)
models_cvrmse_means <- data.frame(model = row.names(models_cvrmse), rmse = models_cvrmse$Mean)

best_model <- as.character(models_cvrmse_means$model[which.min(models_cvrmse_means$rmse)])
best_model




data_test <- data_raw %>% filter(as.Date(datetime, tz = "CET") %in% c(as.Date("2019-03-08", tz = "CET"),
                                             as.Date("2019-04-03", tz = "CET"),
                                             as.Date("2019-04-04", tz = "CET")))


data_test <- data_test %>% filter(ForecastOrNot == 1)


## Checking for test

data_test <- data_test %>% 
  mutate(predicted_P_xgBoost = round(ifelse(TOA != 0, predict(xgboost_model, newdata = data_test), 0)), 0)


data_test <- data_test %>% 
  mutate(predicted_P_rf = round(ifelse(TOA != 0, predict(rf_model, newdata = data_test), 0)), 0)


data_test <- data_test %>% 
  mutate(predicted_P_gbm = round(ifelse(TOA != 0, predict(gbm_model, newdata = data_test), 0)), 0)
data_test$Error_xgBoost <- data_test$predicted_P_xgBoost-data_test$P
data_test$Error_rf <- data_test$predicted_P_rf-data_test$P
data_test$Error_gbm <- data_test$predicted_P_gbm-data_test$P

sqrt(mean(data_test$Error_xgBoost^2, na.rm = T))
sqrt(mean(data_test$Error_rf^2))
sqrt(mean(data_test$Error_gbm^2))

    str(data_test)
    data_test_l <- melt(data_test, measure.vars = c("P", "predicted_P_rf", "predicted_P_xgBoost", "predicted_P_gbm")) %>% 
      filter(as.Date(datetime, tz = "CET") %in% c(as.Date("2019-03-08", tz = "CET")))  %>% filter(pp == "szigetvar_gamma")   
    ggplot(data_test_l, aes(datetime, value, color = variable)) + geom_path()

    str(data_test)
    data_test$Error_rf <- data_test$predicted_P_rf-data_test$P
    
    data_test_l <- melt(data_test, measure.vars = c("P", "predicted_P_rf")) %>% 
      filter(as.Date(datetime, tz = "CET") %in% c(as.Date(c("2019-04-30", "2019-05-01", "2019-05-02"), tz = "CET")))  %>% filter(pp == "szigetvar_gamma")   
    ggplot(data_test_l, aes(datetime, value, color = variable)) + geom_path()
    

data_holdout <- data_train

data_holdout <- data_holdout %>% 
  mutate(predicted_P_xgBoost = round(ifelse(GHI != 0, predict(xgboost_model, newdata = data_holdout), 0)), 0)
data_holdout <- data_holdout %>% 
  mutate(predicted_P_rf = round(ifelse(GHI != 0, predict(rf_model, newdata = data_holdout), 0)), 0)
data_holdout <- data_holdout %>% 
  mutate(predicted_P_gbm = round(ifelse(GHI != 0, predict(gbm_model, newdata = data_holdout), 0)), 0)
data_holdout <- data_holdout %>% 
  mutate(predicted_P_lm = round(ifelse(GHI != 0, predict(linear_model, newdata = data_holdout), 0)), 0)
data_holdout <- data_holdout %>% 
  mutate(predicted_P_cart = round(ifelse(GHI != 0, predict(simple_tree_model, newdata = data_holdout), 0)), 0)


data_holdout$Error_xgBoost <- data_holdout$predicted_P_xgBoost-data_holdout$P
data_holdout$Error_rf <- data_holdout$predicted_P_rf-data_holdout$P
data_holdout$Error_gbm <- data_holdout$predicted_P_gbm-data_holdout$P
data_holdout$Error_lm <- data_holdout$predicted_P_lm-data_holdout$P
data_holdout$Error_cart <- data_holdout$predicted_P_cart-data_holdout$P



sqrt(mean(data_holdout$Error_xgBoost^2))
sqrt(mean(data_holdout$Error_rf^2))
sqrt(mean(data_holdout$Error_gbm^2))
sqrt(mean(data_holdout$Error_lm^2))
sqrt(mean(data_holdout$Error_cart^2))

data_holdout_l <- melt(data_holdout, measure.vars = c("Error_xgBoost", "Error_gbm", "Error_lm"))
ggplot(data_holdout_l, aes(P, value, color = variable)) + geom_point(alpha = 0.07)

############################ IV. MODEL SAVE #############################

save.image(file = paste0(dir_gamma_home, "models/", Sys.Date(), ".RData"))

############################ I. MODELING #############################

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



# DATA IMPORT
data_raw <- fread(paste0(clean, "Database.csv"), sep = ";", dec = ",") %>% 
  mutate(datetime = as.POSIXct((datetime), format = "%Y-%m-%d %T"), tz = "CET") %>% 
  mutate(month = lubridate::month(datetime)) %>%
  filter(as.Date(datetime, tz = "CET") >= "2019-03-04")




data_test <- data.frame()

days <- seq(as.Date(min(data_raw$datetime) + days(3)), as.Date(max(data_raw$datetime)), by = "days")

for (i in days){
      
      datetomodel <- as.Date(i, tz = "CET")
      data_tomodel <- data_raw %>% filter(as.Date(datetime) == datetomodel)
      
      
      data <- data_raw %>%
        filter(ForecastOrNot == 0 & TOA != 0) %>% 
        mutate(P_perc = termeles_kW / TOA) %>% 
        select(datetime, temperature, month, cloudCover, icon, precipProbability, precipIntensity, P_perc,
               TOA, month)
      
      
      
      data_model <- data %>% filter(P_perc != 0 & datetime < datetomodel & !is.na(P_perc)) %>% na.omit()
      
      
      data_train <- data_model %>% select(-datetime) %>% mutate(icon = as.character(icon))
      
      if (max(data_train$month) == 3)
      {data_train$month = NULL
      }
      
      
      train_control <- trainControl(method = "cv", number = 3, verboseIter = TRUE)
      
      # Simple model
      set.seed(20190115)
      tune_grid <- expand.grid(
        .mtry = c(6, 7, 8),
        .splitrule = "variance",
        .min.node.size = c(10, 15)
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
      
      
      data_tomodel <- data_tomodel %>% mutate(icon = ifelse(icon %in% data_train$icon, icon, "partly-cloudy-day"))
      data_tomodel <- data_tomodel %>% 
        mutate(predicted_P_rf = TOA * ifelse(TOA != 0, predict(rf_model, newdata = data_tomodel), 0))
      
      data_tomodel %>% ggplot(aes(datetime, termeles_kW)) + geom_line() + geom_line(aes(datetime, predicted_P_rf), color = "Red")
      
      data_test <- rbind(data_test, data_tomodel)

}

## Getting forecasted results
Forecast_Benedek <- DailyBenedekForecastMerge() 
Forecast_Benedek <- Forecast_Benedek %>% dplyr::rename(Sch_dayahead_kW_Forecast = Forecast_Benedek) %>% as.data.table()
Forecast_Benedek <- Forecast_Benedek %>%  
  mutate(datetime = datetime - lubridate::hours(1))
Forecast_Benedek <- Forecast_Benedek %>%  
  mutate(datetime = as.POSIXct(ifelse(as.Date(datetime, tz  = "CET") >= "2019-04-01", datetime - lubridate::hours(1), datetime), origin = "1970-01-01", tz = "CET"))



# write.table(data_test, "monitoring/MLOnRealData2.csv", row.names = F, sep = ";", dec = ",")
rm(data_test2)
data_test <- fread("monitoring/MLOnRealData2.csv", sep = ";", dec = ",") %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %T", tz = "CET")) %>% 
  filter(Modszer == "Sch_dayahead_kW") %>% 
  select(datetime, Termeles_kW, Sch_dayahead_kW)


data_test2 <- Forecast_Benedek %>% 
  left_join(data_test %>% filter(datetime %in% Forecast_Benedek$datetime))
data_test <- data_test2

data_test <- data_test %>% dplyr::rename(WeatherForecast = Sch_dayahead_kW_Forecast, RealWeather = Sch_dayahead_kW)



data_test <- data_test %>% melt(., measure.vars = c("WeatherForecast", "RealWeather")) %>% 
  dplyr::rename(Modszer = variable, Sch_dayahead_kW = value)


datawa <- data_test %>% 
  mutate(Termeles_kWh = Termeles_kW / 4,
         Sch_dayahead_kWh = Sch_dayahead_kW / 4) %>% 
  mutate(Bevetel_alap_HUF = Termeles_kWh * 32.59,
         Bonusz_HUF = ifelse((Termeles_kW * 0.5 <= Sch_dayahead_kW & Termeles_kW * 1.5 >= Sch_dayahead_kW),
                             ifelse(Termeles_kWh > Sch_dayahead_kWh, (Sch_dayahead_kWh - Termeles_kWh * 0.5) * 3,
                                    (Termeles_kWh * 1.5 - Sch_dayahead_kWh) * 3), 0),
         Bevetel_ossz_HUF = Bevetel_alap_HUF + Bonusz_HUF)



datawa <- datawa %>% select(datetime, Termeles_kW, Modszer, Sch_dayahead_kW, Termeles_kWh, Sch_dayahead_kWh, Bevetel_alap_HUF, Bonusz_HUF, Bevetel_ossz_HUF)

datawa %>% 
  filter(as.Date(datetime, tz = "CET") == "2019-05-1") %>% 
  ggplot(aes(datetime, Sch_dayahead_kW, fill = Modszer)) +
  geom_line() +
  geom_line(aes(datetime, Termeles_kW), color = "red")+
  facet_wrap (Modszer ~ .)



datawa %>% 
  filter(as.Date(datetime, tz = "CET") > "2018-01-01") %>% 
  group_by(Modszer, Datum = as.factor(lubridate::month(datetime))) %>% 
  dplyr::summarize(FajlagosBonusz = sum(Bonusz_HUF, na.rm = T) / sum(Termeles_kWh, na.rm = T)) %>% 
  ggplot(aes(Datum, FajlagosBonusz, fill = Modszer)) +
  geom_col() +
  facet_wrap (Modszer ~ .)


datawa %>% 
  filter(as.Date(datetime, tz = "CET") > "2018-01-01") %>% 
  group_by(Modszer, Datum = as.factor(lubridate::month(datetime))) %>% 
  dplyr::summarize(FajlagosBonusz = sum(Bonusz_HUF, na.rm = T) / sum(Termeles_kWh, na.rm = T)) %>% 
  ggplot(aes(Datum, FajlagosBonusz, fill = Modszer)) +
  geom_bar(stat = "identity", position = "dodge")
  

datawa %>% 
  filter(as.Date(datetime, tz = "CET") > "2019-03-04") %>% 
  group_by(Modszer, Ora = lubridate::hour(datetime), Honap = lubridate::month(datetime)) %>% 
  dplyr::summarize(FajlagosBonusz = sum(Bonusz_HUF, na.rm = T) / sum(Termeles_kWh, na.rm = T)) %>% 
  ggplot(aes(Ora, FajlagosBonusz, fill = Modszer)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~Honap)





datawa1  <- datawa %>% filter(Modszer == "RealWeather") %>% select(datetime, Bonusz_HUF, Termeles_kWh) %>% dplyr::rename(Bonusz_RealWeather_HUF = Bonusz_HUF)
datawa2  <- datawa %>% filter(Modszer == "WeatherForecast") %>% select(datetime, Bonusz_HUF) %>% dplyr::rename(Bonusz_WeatherForecast_HUF = Bonusz_HUF)
datawaj <- datawa1 %>% left_join(datawa2) %>% 
  mutate(Error_Weather_HUF = Bonusz_RealWeather_HUF - Bonusz_WeatherForecast_HUF)



datawaj %>% 
  filter(Termeles_kWh != 0) %>% 
  ggplot(aes(datetime, abs(Error_Weather_HUF))) +
  geom_line()


datawaj %>% 
  filter(as.Date(datetime, tz = "CET") > "2018-01-01") %>% 
  group_by(Honap = as.factor(lubridate::month(datetime))) %>% 
  dplyr::summarize(Error_Weather_HUFkWh = sum(Error_Weather_HUF, na.rm = T) / sum(Termeles_kWh, na.rm = T)) %>% 
  ggplot(aes(Honap, Error_Weather_HUFkWh)) +
  geom_bar(stat = "identity", position = "dodge")



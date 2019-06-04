library(h2o)

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


h2o.init()



# Clear environment
rm(list=ls())

Sys.setenv(TZ='Europe/Budapest')

# FOLDER DEFINITIONS
dirs <- read.csv("C:/Users/B19883/Documents/R_AUTORUNS/SOURCE.csv", sep = ";", stringsAsFactors = F)
dir <- dirs[1,"Source"]
dir_upl <- dirs[2, "Source"]
dir_dashboard <- dirs[3, "Source"]
home_dirgamma <- paste0(dirs[5,"Source"], "2019_Q1_Menetrend_Gamma/") 

##subdirectories
raw <- paste0(home_dirgamma, "raw/")
clean <- paste0(home_dirgamma,"clean/")
output <- paste0(home_dirgamma,"output/")


source(paste0(home_dirgamma, "Gamma_Functions.R"))



# DATA IMPORT
data_raw <- fread(paste0(clean, "Database.csv"), sep = ";", dec = ",")
data_raw$datetime <- as.POSIXct((data_raw$datetime), format = "%Y-%m-%d %T")

data_raw$month <- lubridate::month(data_raw$datetime)

data <- data_raw %>%
  filter(Rad != 0 & ForecastOrNot == 1) %>% 
  mutate(P_perc = P / Rad) %>% 
  select(datetime, month, cloudCover, icon, precipProbability, precipIntensity, P_perc, Rad,
         TOA, month)


data_test_dt <- data %>% filter(month == 5)
data_test <- data %>% filter(month == 5) %>% select(-datetime)

rm(data_train)
data_train <- data %>% filter(month != 5) %>% select(-datetime) %>% rbind(., data %>% select(-datetime) %>% filter(month != 5 & Rad > 300 & P_perc > 0.7)) %>% 
  rbind(., data %>% select(-datetime) %>% filter(month != 5 & Rad > 300 & P_perc > 0.7)) %>% 
  rbind(., data %>% select(-datetime) %>% filter(month != 5 & Rad > 300 & P_perc > 0.7)) %>% 
  rbind(., data %>% select(-datetime) %>% filter(month != 5 & Rad > 300 & P_perc > 0.7)) %>% 
  rbind(., data %>% select(-datetime) %>% filter(month != 5 & Rad > 300 & P_perc > 0.7)) %>% 
  rbind(., data %>% select(-datetime) %>% filter(month != 5 & Rad > 300 & P_perc > 0.7)) %>% 
  rbind(., data %>% select(-datetime) %>% filter(month != 5 & Rad > 300 & P_perc > 0.7)) %>% 
  rbind(., data %>% select(-datetime) %>% filter(month != 5 & Rad > 300 & P_perc > 0.7)) %>% 
  rbind(., data %>% select(-datetime) %>% filter(month != 5 & Rad > 300 & P_perc > 0.7)) %>% 
  rbind(., data %>% select(-datetime) %>% filter(month != 5 & Rad > 300 & P_perc > 0.7))



###### h2o 

h2o_data <- as.h2o(data_train)
h2o_data_test <- as.h2o(data_test)

# splitted_data <- h2o.splitFrame(h2o_data, 
                                # ratios = c(0.8), 
                                # seed = 123)
h2o_data_train <- h2o_data
h2o_data_valid <- h2o_data_test
# h2o_data_test <- splitted_data[[3]]


y <- "P_perc"
X <- setdiff(names(data_train), y)

automl_model <- h2o.automl(X, y,
                           training_frame = h2o_data_train,
                           validation_frame = h2o_data_valid,
                           max_runtime_secs = 30,
                           seed = 123)

lb <- automl_model@leaderboard
print(lb, n = nrow(lb))

automl_model@leader

print(h2o.performance(automl_model@leader, h2o_data_test))




h2o_data_testpred <- cbind(data_test_dt %>% select(datetime, Rad, P_perc), as.data.frame(h2o.predict(automl_model, h2o_data_test))) %>% 
  mutate(predict =  Rad * ifelse(predict < 0, 0, predict),
         P = Rad * P_perc)

data_test_l <- melt(h2o_data_testpred, measure.vars = c("P", "predict"))   

ggplot(data_test_l, aes(datetime, value, color = variable)) + geom_path()


h2o_data_testpred <- h2o_data_testpred %>% 
  mutate(Termeles_kW = P,
         Sch_dayahead_kW = predict, 
         Termeles_kWh = P / 4,
         Sch_dayahead_kWh = Sch_dayahead_kW / 4) %>% 
  mutate(Bevetel_alap_HUF = Termeles_kWh * 32.59,
         Bonusz_HUF = ifelse((Termeles_kW * 0.5 <= Sch_dayahead_kW & Termeles_kW * 1.5 >= Sch_dayahead_kW),
                             ifelse(Termeles_kWh > Sch_dayahead_kWh, (Sch_dayahead_kWh - Termeles_kWh * 0.5) * 3,
                                    (Termeles_kWh * 1.5 - Sch_dayahead_kWh) * 3), 0),
         Bevetel_ossz_HUF = Bevetel_alap_HUF + Bonusz_HUF)


h2o_data_testpred %>% 
  filter(as.Date(datetime, tz = "CET") >= "2019-05-01") %>% 
  group_by(Datum = as.Date(datetime)) %>% 
  dplyr::summarize(Bonusz = sum(Bonusz_HUF, na.rm = T)) %>%  
  ggplot(aes(Datum, Bonusz)) +
  geom_bar(stat = "identity", position = "dodge") 

h2o_data_testpred %>% 
  filter(as.Date(datetime, tz = "CET") >= "2019-05-01") %>% 
  dplyr::summarize(Bonusz = sum(Bonusz_HUF, na.rm = T))



h2o.shutdown(prompt = T)


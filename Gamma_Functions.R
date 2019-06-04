####################### I.  D A T A  P R E P A R A T I O N ################
### I. 1-1 From XML To DF --------------------------------------------
DailyActualProductionXMLToCSV <- function(date) {
  
  # LIBRARIES
  library(tidyverse)
  library(lubridate)
  library(XML)
  library(data.table)
  
  # LOCATION VARIABLES, IMPORTS
  dir <-  paste0(getwd(), "/")
  
  ##subdirectories
  raw <- paste0(dir, "raw/")
  clean <- paste0(dir,"clean/")
  output <- paste0(dir,"output/")
  
  # # DATA IMPORT
  # date <- "2019-01-28"
  source <- paste0(raw, "TEMP/Prod/gamma_napi_ewo/gamma_napi_ewo_", date, ".xml")
  lines <- readLines(paste0(raw, "TEMP/Prod/gamma_napi_ewo/gamma_napi_ewo_", date, ".xml"), encoding = "ISO-8859-1")
  
  lines <- c(head(lines, 1), lines[grep("EDW_XML>", lines)[1]:grep("EDW_XML>", lines)[2]])
  
  doc <- xmlParse(lines)
  
  # Convert XML to data frame
  df <- xmlToDataFrame(nodes = getNodeSet(doc, "//BLOCK")) 
  df <- (t(df)) %>% tail(-1)
  
  df <- data.frame(V1 = df)
  # Number conversion
  df <- df %>% mutate(P = as.numeric(str_extract(V1,"\\(?[0-9,.]+\\)?"))*4) %>%  #*4 to convert to kW
    select(-V1)
  
  # Datetime add
  ## From 00:15:00 to 00:00:00 of the next day - it is how we have the date in XML from MAVIR
  
  df <- df %>% mutate(datetime = seq(ymd_hm(paste0(date, " ", "00:15")),
                                     (ymd_hm(paste0(date, " ", "00:00")) + days(1)), by = '15 mins'),
                      pp = "szigetvar_gamma") %>%
    select(-pp) %>% 
    
    row.names(df) <- NULL
  
  ## Saving in csv
  write.csv(df, paste0(raw, "TEMP/Prod/gamma_napi_ewo/gamma_napi_ewo_", date, ".csv"), row.names = F)
  
}


DailyActualProductionXMLToDF <- function(source) {
  
  # LIBRARIES
  library(tidyverse)
  library(lubridate)
  library(XML)
  library(data.table)
  
  # LOCATION VARIABLES, IMPORTS
  dir <-  paste0(getwd(), "/")
  
  ##subdirectories
  raw <- paste0(dir, "raw/")
  clean <- paste0(dir,"clean/")
  output <- paste0(dir,"output/")
  
  # # DATA IMPORT
  lines <- readLines(source, encoding = "ISO-8859-1")
  
  doc <- xmlParse(lines)
  
  # Convert XML to list
  doc_list <- xmlToList(doc)
  
  df <- data.frame(datetime = seq(as.POSIXct(doc_list$DATA$BLOCK$`START-DATETIME`, format = "%Y-%m-%dT%H:%M:%S", tz = "CET"),
                                  as.POSIXct(doc_list$DATA$BLOCK$`START-DATETIME`, format = "%Y-%m-%dT%H:%M:%S", tz = "CET") + days(1) - minutes(15),
                                  by = "15 min"))
  vect <- list()
  for (i in 2:length(doc_list$DATA$BLOCK)){
    vect[i] <- doc_list$DATA$BLOCK[[i]]$V  
  }
  vect
  
  df$P <- as.numeric(unlist(vect))*4
  
  return(df)    
}



DailyActualProductionsMerged <- function() {
  files <- paste0(dir, "raw/TEMP/Prod/gamma_napi_ewo/", list.files(paste0(dir, "raw/TEMP/Prod/gamma_napi_ewo"), pattern = ".xml"))
  
  df <- data.frame()
  for (i in files){
    df <- rbind(df, DailyActualProductionXMLToDF(i))
  }
  
  df <- unique(df)
  
  return(df)  
  
  
}

### I. 1-2 From forecasting XML To DF --------------------------------------------
DailyForecastToDF <- function(source) {
    
    library(lubridate)
    library(XML)
    library(tidyverse)
    # Forecasted own
    lines <- readLines(source, encoding = "ISO-8859-1")
    doc <- xmlParse(lines)
    
    # Convert XML to data frame
    doc_list <- xmlToList(doc)
    
    
    df <- data.frame(datetime = seq(with_tz(as.POSIXct(strsplit(doc_list$ScheduleTimeInterval, "/")$v[1], format = "%Y-%m-%dT%H:%MZ", tz = "UTC"), "CET"),
                                    with_tz(as.POSIXct(strsplit(doc_list$ScheduleTimeInterval, "/")$v[1], format = "%Y-%m-%dT%H:%MZ", tz = "UTC"), "CET") + days(1) - minutes(15),
                                    by = "15 min"))
    
    
    vect <- list()
    for (i in 1:length(doc_list$ScheduleTimeSeries$Period)){
      vect[i] <- doc_list$ScheduleTimeSeries$Period[i]$Interval$Qty
    }
    
    df$Forecast <- as.numeric(unlist(vect))
    
    return(df)
  }




DailyBenedekForecastMerge <- function() {
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
  
  
  
  files <- paste0(dir_upl, "Feltoltesre_Benedek/", list.files(paste0(dir_upl, "Feltoltesre_Benedek"), pattern = ".xml"))
  
  df <- data.frame()
  for (i in files){
    df <- rbind(df, DailyForecastToDF(i))
  }
  
  df <- unique(df) %>% dplyr::rename("Forecast_Benedek" = "Forecast")
  
  return(df)  
  
}

### I. 2-2 From Hourly Meteorology Data To 15 minutes  --------------------------------------------

DS_FromHourlyToQuarters <- function(datau) {
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
  
  
  # datau <- fread(paste0(raw, "DarkSky_RawDatabase.csv"))
  
  # formatting to POSIxct
  datau$datetime <- as.POSIXct((datau$datetime), format = "%Y-%m-%d %T", tz = "CET")
  
  # creating datetime for each 15 mins
  times <- data.frame(datetime = seq(min(datau$datetime, na.rm = T), max(datau$datetime, na.rm = T), by = '15 mins'))
  
  # joining and transformating to data table
  datau <- (left_join(times, datau, by = "datetime"))
  
  
  # getting variables of datau and their class
  vars <- data.frame(unlist(lapply(sapply(datau, class),  `[[`, 1)), row.names = NULL)
  vars <- cbind(names(datau), vars)
  names(vars) <- c("varnames", "class")
  
  #which no of columns are characters
  vars_char_ind <- which(vars$class %in% "character")
  vars_num_ind <- which(vars$class %in% c("numeric", "integer"))
  
  
  # filtering for datetime > 2018-12-10
  datau <- datau %>% filter(datetime >= "2018-12-16 07:00:00")
  
  ## Linear interpolation for numbers
  for (i in vars_num_ind){
    interp<- approx(datau$datetime,datau[[i]],xout=datau$datetime)
    datau[[i]] <- interp$y
  }
  
  
  # setting the character variables
  for(j in vars_char_ind){
    if(class(datau[[j]]) %in% "character"){
      datau %>% set(, j, na.locf(datau[[j]], fromLast = T))
    }
  }
  
  datau <- data.table(datau)
  
  datau$date <- as.Date(datau$datetime, tz = "CET")
  
  return(datau)
}


### I. 4 -   Function to download past data

DS_getpastdata <- function(period_start, period_end){
    library(jsonlite)
    library(data.table)
    library(tidyverse)
    library(plyr)
    library(lubridate)
    library(zoo)
    library(dplyr)
  
  
  
    # FOLDER DEFINITIONS
    dirs <- read.csv("C:/Users/B19883/Documents/R_AUTORUNS/SOURCE.csv", sep = ";", stringsAsFactors = F)
    dir <- dirs[1,"Source"]
    dir_upl <- dirs[2, "Source"]
    dir_dashboard <- dirs[3, "Source"]
    dir_projects <- dirs[4, "Source"]
    dir_gamma_home <- paste0(dirs[5,"Source"], "2019_Q1_Menetrend_Gamma/") 
    
    
    ##subdirectories
    raw <- paste0(dir_gamma_home, "raw/")
    clean <- paste0(dir_gamma_home,"clean/")
    output <- paste0(dir_gamma_home,"output/")
    
    
    
    period_start <- as.Date(period_start)
    period_end <- as.Date(period_end)
    
    A <- data.frame(date = seq(as.Date(period_start),as.Date(period_end), "days"),
                    time = "00:00:00", stringsAsFactors=F)
    A$daytime <- paste0(A$date, "T", A$time)
    
    
    ds <- data.frame()
    for (i in 1:nrow(A)){
      url <- paste0("https://api.darksky.net/forecast/bd060e57255f4338569d9bb5cb35fa1d/46.0488,17.7983,", A[i, 3], "?units=si&extend=hourly")
      dsraw <- readLines(url)
      dslist <- fromJSON(dsraw)
      ds_i <- dslist$hourly$data
      ds <- plyr::rbind.fill(ds, ds_i)  
    }
    
    ds$datetime <- as.POSIXct(as.numeric(ds$time), origin = "1970-01-01", tz = "CET")
    
    
    ds <- data.table(ds)
    ds <- ds[as.Date(datetime, tz = "CET") >= "2019-03-18" & as.Date(datetime, tz ="CET") !=
               today(), .(datetime, cloudCover, temperature, humidity, icon, precipProbability, precipIntensity, dewPoint, apparentTemperature, ForecastOrNot = 0)] 
    
    
    ds <- DS_FromHourlyToQuarters(ds)
    
    DS <- fread(paste0(raw, "DarkSky_RawDatabase.csv"), sep = ";", dec = ",")
    DS$datetime <- as.POSIXct((DS$datetime), format = "%Y-%m-%d %T", tz = "CET")
    DS$date <- as.Date(DS$datetime, tz = "CET")
    DS <- data.table(DS)
    
    DS_new <- rbind(DS, ds)
    
    DS_new0 <- DS_new[ForecastOrNot == 0]
    DS_new1 <- DS_new[ForecastOrNot == 1]
    
    
    DS_q0 <- DS_FromHourlyToQuarters(DS_new0)
    DS_q1 <- DS_FromHourlyToQuarters(DS_new1)
    
    DS_q0 <- DS_q0[!duplicated(DS_q0)]
    DS_q1 <- DS_q1[!duplicated(DS_q1)]
    
    
    DSNEWQ <- rbind(DS_q0, DS_q1)
  
    return(DSNEWQ)
}


DS_refresh <- function() {
  library(jsonlite)
  library(data.table)
  library(tidyverse)
  library(plyr)
  library(lubridate)
  library(zoo)
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
  
  files <- paste0(paste0(raw, "ORIGINALS/DS/"), list.files(paste0(raw, "ORIGINALS/DS"), pattern = "DS_Forecast_"))
  
  DSf <- data.frame()
  for (i in files){
      DSf_i <- read.csv(i, sep = ";")
      DSf <- rbind.fill(DSf, DSf_i)
    }
    
  DSf <- DSf %>%
    select(datetime, cloudCover, temperature, humidity, icon, precipProbability, precipIntensity, dewPoint, apparentTemperature) %>% 
    mutate(ForecastOrNot = 1,
           datetime = as.POSIXct(datetime, format = "%Y-%m-%d %T", tz = "CET"),
           date = as.Date(datetime, tz = "CET"))
  
  
  DS <- fread(paste0(raw, "DarkSky_RawDatabase.csv"), sep = ";", dec = ",")
  DS$datetime <- as.POSIXct((DS$datetime), format = "%Y-%m-%d %T", tz = "CET")
  DS$date <- as.Date(DS$datetime, tz = "CET")
  DS <- data.table(DS)
  DS <- DS %>% filter(ForecastOrNot == 0)
  
  
  DS_new <- rbind.fill(DS, DSf) %>% as.data.table()
  
  DS_new <- unique(DS_new)
  
  write.table(DS_new, paste0(raw, "DarkSky_RawDatabase.csv"), row.names = F, sep = ";", dec = ",")
  
  DS_new %>% filter(ForecastOrNot == 0) %>% dplyr::summarize(max(datetime))
  as.Date((DS_new %>% filter(ForecastOrNot == 0) %>% dplyr::summarize(max(datetime)))[1, 1], tz = "CET")
  
  DS_new_with_pastdata <- DS_getpastdata(as.Date((DS_new %>% filter(ForecastOrNot == 0) %>% dplyr::summarize(max(datetime)))[1, 1], tz = "CET"),
                 DescTools::Today() - days(1))
  
  write.table(DS_new_with_pastdata, paste0(raw, "DarkSky_RawDatabase.csv"), row.names = F, sep = ";", dec = ",")
  
}

### I. 4-0 Merging databases from raw/c(DarkSky_RawDatabase.csv, Extranet_RawDatabase.csv, Radiation_RawDatabase.csv) to clean/Database.csv  ------

DBMerge <- function(){
    
    library(data.table)
    library(tidyverse)
    library(plyr)
    library(lubridate)
    library(zoo)  
    
    Sys.setenv(TZ='Europe/Budapest')
    
    
    # FOLDER DEFINITIONS
    dirs <- read.csv("C:/Users/B19883/Documents/R_AUTORUNS/SOURCE.csv", sep = ";", stringsAsFactors = F)
    dir <- dirs[1,"Source"]
    dir_upl <- dirs[2, "Source"]
    dir_dashboard <- dirs[3, "Source"]
    dir_projects <- dirs[4, "Source"]
    dir_gamma_home <- paste0(dirs[5,"Source"], "2019_Q1_Menetrend_Gamma/") 
    
    
    ##subdirectories
    raw <- paste0(dir_gamma_home, "raw/")
    clean <- paste0(dir_gamma_home,"clean/")
    output <- paste0(dir_gamma_home,"output/")  
  
    # Data import
    Rad <- read.csv(paste0(raw, "Radiation_RawDatabase.csv"), sep = ";", dec = ",") %>% 
      mutate(datetime = as.POSIXct((datetime), format = "%Y-%m-%d %T", tz = "CET"))
    DS <- fread(paste0(raw, "DarkSky_RawDatabase.csv"), sep = ";", dec = ",") %>% 
      mutate(datetime = as.POSIXct((datetime), format = "%Y-%m-%d %T", tz = "CET"))
    CAMS <- read.csv(paste0(raw, "CAMS_RawDatabase.csv"), sep = ";") %>% 
      mutate(datetime = as.POSIXct((datetime), format = "%Y-%m-%d %T", tz = "CET"))
    
    prod_raw <- read.csv(paste0(dir_gamma_home, "monitoring/Monitoring.csv"), sep = ";", dec = ",", skip = 12)
    
    Prod <- data.frame(datetime = prod_raw[, 1]) %>% 
      mutate(datetime = as.POSIXct(datetime, format = "%Y.%m.%d. %H:%M", tz = "CET") - minutes(15),
      termeles_kW = prod_raw[, grep("Adat", colnames(prod_raw))[1]],
      eromu = "szigetvar_gamma")
      
    rm(prod_raw)
    
    database_joined <- DS %>% left_join(Rad) %>% left_join(Prod) %>% left_join(CAMS) %>% 
      mutate(date = as.Date(datetime, tz = "CET")) %>% 
      na.omit() %>% 
      select(datetime, termeles_kW, cloudCover, temperature, humidity, eromu, icon, precipProbability, precipIntensity,
             dewPoint, apparentTemperature, Rad, ForecastOrNot, GHI, TOA)
    
    
    file.copy(paste0(clean, "Database.csv"), paste0(clean, "old/Database_", Sys.Date(), ".csv"), overwrite = T)
    
    # Save work file
    write.table(database_joined, paste0(clean, "Database.csv"), row.names = FALSE, sep = ";", dec = ",")
}


####################### II.  F O R E C A S T I N G #######################
forecast_gamma <- function(forecast_date){
  
  library(darksky)
  library(data.table)
  library(tidyverse)
  library(plyr)
  library(lubridate)
  library(zoo)  
  library(jsonlite) 
  library(XML)
  library(dplyr)
  library(stringr)
  
  forecast_date <- as.Date(forecast_date, tz = "CET")
  
  
  # loading model
  # load(paste0(dir, "Model_2019-03-07.RData"))
  
  ## API call for weather
  url <- "https://api.darksky.net/forecast/b1b4a5b40f957cbaa84d80efcdaa3426/46.04865,17.80554?units=si&extend=hourly"
  darksky_raw <- readLines(url)
  darksky_list <- fromJSON(darksky_raw)
  
  darksky_hourly <- darksky_list$hourly$data
  
  darksky_hourly$datetime <- as.POSIXct(as.numeric(as.character(darksky_hourly$time)),origin="1970-01-01",tz="CET") 
  # darksky_hourly <- darksky_hourly %>% select(datetime, cloudCover, temperature, humidity, icon, precipProbability)
  
  datau <- fread(paste0(raw, "DarkSky_RawDatabase.csv"))
  datau$datetime <- as.POSIXct((datau$datetime), format = "%Y-%m-%d %T")
  
  datau$month <- month(datau$datetime)
  
  # Correcting any missing value   ## correcting possible missing icon
  # darksky_hourly_cor <- darksky_hourly
  # darksky_hourly_cor$iconmode <- mode(datau$icon)   
  # darksky_hourly_cor <- darksky_hourly %>% mutate(precipIntensity = ifelse(is.na(precipIntensity), 0, precipIntensity),
  #                                                 precipProbability = ifelse(is.na(precipProbability), 0, precipProbability),
  #                                                 ozone = ifelse(is.na(ozone), mean(ozone, na.rm = T), ozone),
  #                                                 cloudCover = ifelse(is.na(cloudCover), 0, cloudCover),
  #                                                 temperature = ifelse(is.na(cloudCover), 20, temperature),
  #                                                 apparentTemperature = ifelse(is.na(apparentTemperature), 20, apparentTemperature),
  #                                                 dewPoint = ifelse(is.na(dewPoint), 0, dewPoint),
  #                                                 humidity = ifelse(is.na(humidity), 20, humidity),
  #                                                 icon = ifelse(is.na(icon), iconmode, icon)
  # )
  # str(darksky_hourly_cor)
  # 
  # darksky_hourly <- darksky_hourly_cor
  # 
  
  ## Creating data frame for each quarter of hour
  # creating datetime for each 15 mins
  times <- data.frame(datetime = seq(min(darksky_hourly$datetime), max(darksky_hourly$datetime), by = '15 mins'))
  
  # joining and transformating to data table
  darksky_hourly <- (left_join(times, darksky_hourly, by = "datetime"))
  
  # taking out precipType
  darksky_hourly <- darksky_hourly %>% select(-precipType)
  
  # getting variables of datau and their class
  vars <- data.frame(unlist(lapply(sapply(darksky_hourly, class),  `[[`, 1)), row.names = NULL)
  vars <- cbind(names(darksky_hourly), vars)
  names(vars) <- c("varnames", "class")
  
  #which no of columns are characters
  vars_char_ind <- which(vars$class %in% "character")
  vars_num_ind <- which(vars$class %in% c("numeric", "integer"))
  
  
  
  ## Linear interpolation for numbers
  for (i in vars_num_ind){
    interp <- approx(darksky_hourly$datetime,darksky_hourly[[i]],xout=darksky_hourly$datetime)
    darksky_hourly[[i]] <- interp$y
  }
  
  
  # setting the character variables
  for(j in vars_char_ind){
    if(class(darksky_hourly[[j]]) %in% "character"){
      darksky_hourly %>% set(, j, na.locf(darksky_hourly[[j]], fromLast = F))
    }
  }
  
  str(darksky_hourly)
  tail(darksky_hourly)
  ## Splitting darksky data to quarter hours ends
  
  
  #Day selection - day+1
  darksky_hourly <- darksky_hourly %>% filter(as.Date(datetime, tz = "CET") == forecast_date)
  
  
  DS <- darksky_hourly
  
  Rad <- read.csv(paste0(raw, "Radiation_RawDatabase.csv"))
  Rad$datetime <- as.POSIXct((Rad$datetime), format = "%Y-%m-%d %T", tz = "CET")
  
  CAMS <- read.csv(paste0(raw, "CAMS_RawDatabase.csv"), sep = ";")
  CAMS$datetime <- as.POSIXct((CAMS$datetime), format = "%Y-%m-%d %T", tz = "CET")
  
  
  
  datau_seg <- data.table(merge(DS, Rad, by = "datetime"))
  datau <- data.table(merge(datau_seg, CAMS, by = "datetime"))
  datau$ForecastOrNot <- 1
  
  
  datau$month <- month(datau$datetime)
  # PREDICTION
  
  
  
  datau_w_prediction <- datau %>% 
    mutate(predicted_P = TOA * ifelse(TOA != 0, predict(rf_model, newdata = (datau %>% mutate(icon = ifelse(icon == "wind", "clear-day", icon)) %>%  
                                                                               mutate(cloudCover = cloudCover * 0.2))), 0)) %>% 
    mutate(ifelse(predicted_P <= 0, 0, ifelse(predicted_P > 500, 500, predicted_P))) %>% 
    mutate(predicted_P = base::round(predicted_P, 0))
  
  
  write.table(datau_w_prediction, paste0(raw, "ORIGINALS/DS/DS_Forecast_", forecast_date, "2.csv"), row.names = F, sep = ";")
  
  
  datau_w_prediction <- datau_w_prediction %>% select(datetime, predicted_P)
  
  ggplot(datau_w_prediction, aes(x = datetime, y = predicted_P)) +
    geom_line(alpha = 0.3) +
    theme_bw()
  
  
  
  # C R E A T I N G  X M L
  
  ## Parameter definition
  forecast_start_utc <- as.character(with_tz(as.POSIXct(paste0(forecast_date - days(1), "24:00")), "UTC")) # Possible parameter
  forecast_end_utc <- as.character(with_tz(as.POSIXct(paste0(forecast_date, "24:00")), "UTC")) # Possible parameter
  
  
  forecast_date_char <- as.character(forecast_date)
  ### Version automatic definition
  #listing files
  filesdf <- data.frame(filenames = list.files(paste0(dir_upl, "Feltoltesre_Benedek")))
  #from list to data.frame
  filesdf <- data.frame(t(data.frame(str_split(filesdf$filenames, "_"))))
  row.names(filesdf) <- NULL
  
  #renaming variables
  colnames(filesdf) <- c("pp", "date", "version")
  
  #version variable cleaning
  filesdf$version <- gsub("\\..*","",filesdf$version)
  filesdf <- filesdf %>% filter(date == gsub("-", "", forecast_date_char))
  
  if (nrow(filesdf) == 0) {
    version = 1
  }
  if (nrow(filesdf) != 0) {
    version = max(as.integer(filesdf$version), na.rm = T) + 1
  }
  
  
  ## Version check
  # read.xlsx(LotsofmydatainExcel.xlsm,sheetName="Deaths",as.data.frame=TRUE)
  
  
  forecast_interval <- gsub(" ", "T", paste0(substr(forecast_start_utc, 1, nchar(forecast_start_utc)-3), "Z/", 
                                             substr(forecast_end_utc, 1, nchar(forecast_end_utc)-3),"Z"))
  current_time <- gsub(" ", "T", paste0(with_tz(Sys.time(), tzone = "CET"), "Z"))
  
  
  datau_w_prediction_forXML <- data.frame(Pos = 1:nrow(datau_w_prediction), Qty = datau_w_prediction$predicted_P)
  
  
  ## XML Construction
  doc <- newXMLDoc()
  root <- newXMLNode("ScheduleMessage",
                     attrs = c(DtdRelease = 3 , DtdVersion = 3), 
                     doc=doc)
  
  newXMLNode("MessageIdentification", attrs = c(v = "15X-K-KILOGA---O_PROD"), parent = root)
  newXMLNode("MessageVersion", attrs = c(v = version), parent = root) ## VERSION IS A PARAMETER !!
  newXMLNode("MessageType", attrs = c(v = "Z71"), parent = root)
  newXMLNode("ProcessType", attrs = c(v = "A17"), parent = root)
  newXMLNode("ScheduleClassificationType", attrs = c(v = "A01"), parent = root)
  newXMLNode("SenderIdentification", attrs = c(v = "15X-K-KILOGA---O", codingScheme = "A01"), parent = root)
  newXMLNode("SenderRole", attrs = c(v = "A20"), parent = root)
  newXMLNode("ReceiverIdentification", attrs = c(v = "15X-KATENERGIA-V", codingScheme = "A01"), parent = root)
  newXMLNode("ReceiverRole", attrs = c(v = "A06"), parent = root)
  newXMLNode("MessageDateTime", attrs = c(v = current_time), parent = root)  
  newXMLNode("ScheduleTimeInterval", attrs = c(v = forecast_interval), parent = root) 
  ScheduleTimeSeries <- newXMLNode("ScheduleTimeSeries", parent = root)  
  newXMLNode("SendersTimeSeriesIdentification", attrs = c(v = "KILOGAMM-NAP-31"), parent = ScheduleTimeSeries)
  newXMLNode("SendersTimeSeriesVersion", attrs = c(v = version), parent = ScheduleTimeSeries) # VERSION IS A PARAMETER !!
  newXMLNode("BusinessType", attrs = c(v = "A01"), parent = ScheduleTimeSeries) 
  newXMLNode("Product", attrs = c(v = "8716867000016"), parent = ScheduleTimeSeries)
  newXMLNode("ObjectAggregation", attrs = c(v = "A02"), parent = ScheduleTimeSeries)
  newXMLNode("InArea", attrs = c(v = "10YHU-MAVIR----U", codingScheme = "A01"), parent = ScheduleTimeSeries)
  newXMLNode("MeteringPointIdentification", attrs = c(v = "HU000120B11-U-NAPE-SZIGVA-0400-32", codingScheme = "A01"), parent = ScheduleTimeSeries)
  newXMLNode("InParty", attrs = c(v = "15X-K-KILOGA---O", codingScheme = "A01"), parent = ScheduleTimeSeries)
  newXMLNode("MeasurementUnit", attrs = c(v = "KWT"), parent = ScheduleTimeSeries)
  Period <- newXMLNode("Period", parent = ScheduleTimeSeries)  
  newXMLNode("TimeInterval", attrs = c(v = forecast_interval), parent = Period)
  newXMLNode("Resolution", attrs = c(v = "PT15M"), parent = Period)
  
  for (j in 1:nrow(datau_w_prediction)){
    IntervalNode = newXMLNode("Interval", parent=Period)  
    newXMLNode("Pos", attrs = c(v = datau_w_prediction_forXML$Pos[j]), parent=IntervalNode)  
    newXMLNode("Qty", attrs = c(v = datau_w_prediction_forXML$Qty[j]), parent=IntervalNode)  
  }  
  
  saveXML(doc, file = paste0(dir_upl, "Feltoltesre_Benedek/15X-K-KILOGA---O_", 
                             gsub("-", "", forecast_date), "_", version,
                             ".xml"), encoding = "UTF-8", prefix = '<?xml version="1.0" encoding="UTF-8"?>\n')
  
  
  
  
  ### XML Verzio 2
  
  ## Parameter definition
  forecast_start_utc <- as.character(with_tz(as.POSIXct(paste0(forecast_date - days(1), "24:00")), "UTC")) # Possible parameter
  forecast_end_utc <- as.character(with_tz(as.POSIXct(paste0(forecast_date, "24:00")), "UTC")) # Possible parameter
  
  
  forecast_date_char <- as.character(forecast_date)
  ### Version automatic definition
  #listing files
  filesdf <- data.frame(filenames = list.files(paste0(dir_upl, "Feltoltesre_Benedek")))
  #from list to data.frame
  filesdf <- data.frame(t(data.frame(str_split(filesdf$filenames, "_"))))
  row.names(filesdf) <- NULL
  
  #renaming variables
  colnames(filesdf) <- c("pp", "date", "version")
  
  
  # dir_upl2 <- 
  #version variable cleaning
  filesdf$version <- gsub("\\..*","",filesdf$version)
  filesdf <- filesdf %>% filter(date == gsub("-", "", forecast_date_char))
  
  version <- 2
  
  
  ## Version check
  # read.xlsx(LotsofmydatainExcel.xlsm,sheetName="Deaths",as.data.frame=TRUE)
  
  
  forecast_interval <- gsub(" ", "T", paste0(substr(forecast_start_utc, 1, nchar(forecast_start_utc)-3), "Z/", 
                                             substr(forecast_end_utc, 1, nchar(forecast_end_utc)-3),"Z"))
  current_time <- gsub(" ", "T", paste0(with_tz(Sys.time(), tzone = "CET"), "Z"))
  
  
  datau_w_prediction_forXML <- data.frame(Pos = 1:nrow(datau_w_prediction), Qty = datau_w_prediction$predicted_P)
  
  
  ## XML Construction
  doc <- newXMLDoc()
  root <- newXMLNode("ScheduleMessage",
                     attrs = c(DtdRelease = 3 , DtdVersion = 3), 
                     doc=doc)
  
  newXMLNode("MessageIdentification", attrs = c(v = "15X-K-KILOGA---O_PROD"), parent = root)
  newXMLNode("MessageVersion", attrs = c(v = version), parent = root) ## VERSION IS A PARAMETER !!
  newXMLNode("MessageType", attrs = c(v = "Z71"), parent = root)
  newXMLNode("ProcessType", attrs = c(v = "A17"), parent = root)
  newXMLNode("ScheduleClassificationType", attrs = c(v = "A01"), parent = root)
  newXMLNode("SenderIdentification", attrs = c(v = "15X-K-KILOGA---O", codingScheme = "A01"), parent = root)
  newXMLNode("SenderRole", attrs = c(v = "A20"), parent = root)
  newXMLNode("ReceiverIdentification", attrs = c(v = "15X-KATENERGIA-V", codingScheme = "A01"), parent = root)
  newXMLNode("ReceiverRole", attrs = c(v = "A06"), parent = root)
  newXMLNode("MessageDateTime", attrs = c(v = current_time), parent = root)  
  newXMLNode("ScheduleTimeInterval", attrs = c(v = forecast_interval), parent = root) 
  ScheduleTimeSeries <- newXMLNode("ScheduleTimeSeries", parent = root)  
  newXMLNode("SendersTimeSeriesIdentification", attrs = c(v = "KILOGAMM-NAP-31"), parent = ScheduleTimeSeries)
  newXMLNode("SendersTimeSeriesVersion", attrs = c(v = version), parent = ScheduleTimeSeries) # VERSION IS A PARAMETER !!
  newXMLNode("BusinessType", attrs = c(v = "A01"), parent = ScheduleTimeSeries) 
  newXMLNode("Product", attrs = c(v = "8716867000016"), parent = ScheduleTimeSeries)
  newXMLNode("ObjectAggregation", attrs = c(v = "A02"), parent = ScheduleTimeSeries)
  newXMLNode("InArea", attrs = c(v = "10YHU-MAVIR----U", codingScheme = "A01"), parent = ScheduleTimeSeries)
  newXMLNode("MeteringPointIdentification", attrs = c(v = "HU000120B11-U-NAPE-SZIGVA-0400-32", codingScheme = "A01"), parent = ScheduleTimeSeries)
  newXMLNode("InParty", attrs = c(v = "15X-K-KILOGA---O", codingScheme = "A01"), parent = ScheduleTimeSeries)
  newXMLNode("MeasurementUnit", attrs = c(v = "KWT"), parent = ScheduleTimeSeries)
  Period <- newXMLNode("Period", parent = ScheduleTimeSeries)  
  newXMLNode("TimeInterval", attrs = c(v = forecast_interval), parent = Period)
  newXMLNode("Resolution", attrs = c(v = "PT15M"), parent = Period)
  
  for (j in 1:nrow(datau_w_prediction)){
    IntervalNode = newXMLNode("Interval", parent=Period)  
    newXMLNode("Pos", attrs = c(v = datau_w_prediction_forXML$Pos[j]), parent=IntervalNode)  
    newXMLNode("Qty", attrs = c(v = datau_w_prediction_forXML$Qty[j]), parent=IntervalNode)  
  }  
  
  saveXML(doc, file = paste0(dir_upl, "Feltoltesre_Benedek/masverziok/15X-K-KILOGA---O_", 
                             gsub("-", "", forecast_date), "_", version,
                             ".xml"), encoding = "UTF-8", prefix = '<?xml version="1.0" encoding="UTF-8"?>\n')
  
  
  
  ## Verzio 3
  
  ## Parameter definition
  forecast_start_utc <- as.character(with_tz(as.POSIXct(paste0(forecast_date - days(1), "24:00")), "UTC")) # Possible parameter
  forecast_end_utc <- as.character(with_tz(as.POSIXct(paste0(forecast_date, "24:00")), "UTC")) # Possible parameter
  
  
  
  forecast_date_char <- as.character(forecast_date)
  ### Version automatic definition
  
  #listing files
  filesdf <- data.frame(filenames = list.files(paste0(dir_upl, "Feltoltesre_Benedek")))
  #from list to data.frame
  filesdf <- data.frame(t(data.frame(str_split(filesdf$filenames, "_"))))
  row.names(filesdf) <- NULL
  
  #renaming variables
  colnames(filesdf) <- c("pp", "date", "version")
  
  #version variable cleaning
  filesdf$version <- gsub("\\..*","",filesdf$version)
  filesdf <- filesdf %>% filter(date == gsub("-", "", forecast_date_char))
  
  version <- 3
  
  
  ## Version check
  # read.xlsx(LotsofmydatainExcel.xlsm,sheetName="Deaths",as.data.frame=TRUE)
  
  
  forecast_interval <- gsub(" ", "T", paste0(substr(forecast_start_utc, 1, nchar(forecast_start_utc)-3), "Z/", 
                                             substr(forecast_end_utc, 1, nchar(forecast_end_utc)-3),"Z"))
  current_time <- gsub(" ", "T", paste0(with_tz(Sys.time(), tzone = "CET"), "Z"))
  
  
  datau_w_prediction_forXML <- data.frame(Pos = 1:nrow(datau_w_prediction), Qty = datau_w_prediction$predicted_P)
  
  
  ## XML Construction
  doc <- newXMLDoc()
  root <- newXMLNode("ScheduleMessage",
                     attrs = c(DtdRelease = 3 , DtdVersion = 3), 
                     doc=doc)
  
  newXMLNode("MessageIdentification", attrs = c(v = "15X-K-KILOGA---O_PROD"), parent = root)
  newXMLNode("MessageVersion", attrs = c(v = version), parent = root) ## VERSION IS A PARAMETER !!
  newXMLNode("MessageType", attrs = c(v = "Z71"), parent = root)
  newXMLNode("ProcessType", attrs = c(v = "A17"), parent = root)
  newXMLNode("ScheduleClassificationType", attrs = c(v = "A01"), parent = root)
  newXMLNode("SenderIdentification", attrs = c(v = "15X-K-KILOGA---O", codingScheme = "A01"), parent = root)
  newXMLNode("SenderRole", attrs = c(v = "A20"), parent = root)
  newXMLNode("ReceiverIdentification", attrs = c(v = "15X-KATENERGIA-V", codingScheme = "A01"), parent = root)
  newXMLNode("ReceiverRole", attrs = c(v = "A06"), parent = root)
  newXMLNode("MessageDateTime", attrs = c(v = current_time), parent = root)  
  newXMLNode("ScheduleTimeInterval", attrs = c(v = forecast_interval), parent = root) 
  ScheduleTimeSeries <- newXMLNode("ScheduleTimeSeries", parent = root)  
  newXMLNode("SendersTimeSeriesIdentification", attrs = c(v = "KILOGAMM-NAP-31"), parent = ScheduleTimeSeries)
  newXMLNode("SendersTimeSeriesVersion", attrs = c(v = version), parent = ScheduleTimeSeries) # VERSION IS A PARAMETER !!
  newXMLNode("BusinessType", attrs = c(v = "A01"), parent = ScheduleTimeSeries) 
  newXMLNode("Product", attrs = c(v = "8716867000016"), parent = ScheduleTimeSeries)
  newXMLNode("ObjectAggregation", attrs = c(v = "A02"), parent = ScheduleTimeSeries)
  newXMLNode("InArea", attrs = c(v = "10YHU-MAVIR----U", codingScheme = "A01"), parent = ScheduleTimeSeries)
  newXMLNode("MeteringPointIdentification", attrs = c(v = "HU000120B11-U-NAPE-SZIGVA-0400-32", codingScheme = "A01"), parent = ScheduleTimeSeries)
  newXMLNode("InParty", attrs = c(v = "15X-K-KILOGA---O", codingScheme = "A01"), parent = ScheduleTimeSeries)
  newXMLNode("MeasurementUnit", attrs = c(v = "KWT"), parent = ScheduleTimeSeries)
  Period <- newXMLNode("Period", parent = ScheduleTimeSeries)  
  newXMLNode("TimeInterval", attrs = c(v = forecast_interval), parent = Period)
  newXMLNode("Resolution", attrs = c(v = "PT15M"), parent = Period)
  
  for (j in 1:nrow(datau_w_prediction)){
    IntervalNode = newXMLNode("Interval", parent=Period)  
    newXMLNode("Pos", attrs = c(v = datau_w_prediction_forXML$Pos[j]), parent=IntervalNode)  
    newXMLNode("Qty", attrs = c(v = datau_w_prediction_forXML$Qty[j]), parent=IntervalNode)  
  }  
  
  saveXML(doc, file = paste0(dir_upl, "Feltoltesre_Benedek/masverziok/15X-K-KILOGA---O_", 
                             gsub("-", "", forecast_date), "_", version,
                             ".xml"), encoding = "UTF-8", prefix = '<?xml version="1.0" encoding="UTF-8"?>\n')
  
}
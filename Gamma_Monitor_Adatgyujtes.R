# LIBRARIES
library(tidyverse)
library(lubridate)
library(XML)
library(data.table)
library(plyr)
library(DBI)
library(ggplot2)
library(dbplyr)
library(dplyr)
Sys.setenv(TZ='Europe/Budapest')
# FOLDER DEFINITIONS
dirs <- read.csv("C:/Users/B19883/Documents/R_AUTORUNS/SOURCE.csv", sep = ";", stringsAsFactors = F)
dir_gamma <- dirs[1, "Source"]
dir_upl <- dirs[2, "Source"]
dir_dashboard <- dirs[3, "Source"]
dir_projects <- dirs[4, "Source"]
dir_gamma_home <- paste0(dirs[5,"Source"], "2019_Q1_Menetrend_Gamma/")


source(paste0(dir_gamma_home, "Gamma_Functions.R"))


prod_raw <- read.csv(paste0(dir_gamma_home, "monitoring/Monitoring.csv"), sep = ";", dec = ",", skip = 12)

prod <- data.frame(datetime = prod_raw[, 1]) %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%Y.%m.%d. %H:%M", tz = "CET") - minutes(15),
         termeles_kW = prod_raw[, grep("Adat", colnames(prod_raw))[1]],
         eromu = "szigetvar_gamma")

###### Összelemzés
## mikor NEM futott le az enyém márciusban
Dates_NemAzEnyemLettBeadva <- as.Date(c("2019-03-01", "2019-03-02","2019-03-03","2019-03-04","2019-03-10","2019-03-11", "2019-03-22" ,"2019-03-28","2019-03-31", "2019-04-18"))

## ML Forecast Import
Forecast_ML <- DailyBenedekForecastMerge()

Forecast_ML <- Forecast_ML %>% dplyr::rename(Datetime = datetime, Sch_dayahead_kW_ML = Forecast_Benedek) %>% as.data.table()

################################### Original forecast import ###############################
library(readxl)
Original <- read_excel(paste0(dir_upl, "Betöltött/KILOGA_feltöltött.xlsx")) %>% tail(-2) %>% head(-2) %>%
  mutate(...2 = ...2 - minutes(15))
cols <- colnames(Original) %>% head(-1) %>% tail(-2)
Forecast_OR <- Original %>%
  mutate(times = substr(Original$...2, 12, 19)) %>%
  melt(measure.vars = cols) %>%
  mutate(variable = as.Date(as.numeric(variable), origin="2018-06-30", tz = "CET")) %>%
  mutate(Datetime = as.POSIXct(paste0(variable, times), format = "%Y-%m-%d%T", tz = "CET")) %>%
  select(Datetime, Sch_dayahead_kW_OR = value)


##### Original transform ends
joined <- join_all(list(prod, Forecast_OR, Forecast_ML)) %>%
  unique() %>%
  filter(!is.na(Datetime))

dataw <- joined %>%
  melt(measure.vars = c("Sch_dayahead_kW_OR", "Sch_dayahead_kW_ML")) %>%
  rename(c("variable" = "TypeOfForecast", "value" = "Sch_dayahead_kW")) %>%
  mutate(Production_kWh = Production_kW / 4,
         Sch_dayahead_kWh = Sch_dayahead_kW / 4)


# Bevételszámítás összehasonlításra
dataw <- dataw %>%
  mutate(Bevetel_alap_HUF = Production_kWh * 32.59,
         Bonus_HUF = ifelse((Production_kW * 0.5 <= Sch_dayahead_kW & Production_kW * 1.5 >= Sch_dayahead_kW),
                            ifelse(Production_kWh > Sch_dayahead_kWh, (Sch_dayahead_kWh - Production_kWh * 0.5) * 3,
                                   (Production_kWh * 1.5 - Sch_dayahead_kWh) * 3), 0),
         Bevetel_ossz_HUF = Bevetel_alap_HUF + Bonus_HUF,
         MLeBeadva = ifelse(as.Date(Datetime, tz = "CET") > "2019-02-28" &
                                   !(as.Date(Datetime, tz = "CET") %in% Dates_NemAzEnyemLettBeadva), 1, 0))


datawa <- joined %>% mutate(MLeBeadva = ifelse(as.Date(Datetime, tz = "CET") > "2019-02-28" &
                                                      !(as.Date(Datetime, tz = "CET") %in% Dates_NemAzEnyemLettBeadva), 1, 0)) %>%
  mutate(Sch_dayahead_kW = ifelse(as.Date(Datetime, tz = "CET") > "2019-02-28" &
                                    MLeBeadva == 1, Sch_dayahead_kW_OR, Sch_dayahead_kW_ML)) %>%
  mutate(Production_kWh = Production_kW / 4,
         Sch_dayahead_kWh = Sch_dayahead_kW / 4) %>%
  mutate(Bevetel_alap_HUF = Production_kWh * 32.59,
         Bonus_HUF = ifelse((Production_kW * 0.5 <= Sch_dayahead_kW & Production_kW * 1.5 >= Sch_dayahead_kW),
                            ifelse(Production_kWh > Sch_dayahead_kWh, (Sch_dayahead_kWh - Production_kWh * 0.5) * 3,
                                   (Production_kWh * 1.5 - Sch_dayahead_kWh) * 3), 0),
         Bevetel_ossz_HUF = Bevetel_alap_HUF + Bonus_HUF)


dataw %>%
  filter(as.Date(Datetime, tz = "CET") >= "2019-03-01") %>%
  group_by(TypeOfForecast, Date = lubridate::month(as.Date(Datetime))) %>%
  dplyr::summarize(SpecificBonus = sum(Bonus_HUF, na.rm = T) / sum(Production_kWh, na.rm = T)) %>%
  ggplot(aes(Date, SpecificBonus, fill = TypeOfForecast)) +
  geom_bar(stat = "identity", position = "dodge")



datawa %>%
  filter(as.Date(Datetime, tz = "CET") > "2018-03-04") %>%
  group_by(Date = as.factor(lubridate::month(Datetime))) %>%
  dplyr::summarize(SpecificBonus = sum(Bonus_HUF, na.rm = T) / sum(Production_kWh, na.rm = T)) %>%
  ggplot(aes(Date, SpecificBonus)) +
  geom_col()


joined %>%
  melt(measure.vars = c("Production_kW", "Sch_dayahead_kW_OR", "Sch_dayahead_kW_ML")) %>%
  dplyr::rename(TypeOfForecast = variable, KW = value) %>%
  filter(as.Date(Datetime, tz = "CET") > "2019-05-31") %>% 
  ggplot(aes(Datetime, KW, color = TypeOfForecast)) +
  geom_line()

dataw <- tbl_df(dataw)
dataw %>%
  filter(as.Date(Datetime, tz = "CET") > "2019-05-21") %>%
  group_by(TypeOfForecast, Date = as.Date(Datetime)) %>%
  dplyr::summarize(Bonus = sum(Bonus_HUF, na.rm = T))%>%
  ggplot(aes(Date, Bonus, fill = TypeOfForecast)) +
  geom_bar(stat = "identity", position = "dodge")
dataw %>%
  filter(as.Date(Datetime, tz = "CET") >= "2019-05-01") %>%
  group_by(TypeOfForecast, Date = as.Date(Datetime)) %>%
  dplyr::summarize(SpecificBonus = sum(Bonus_HUF, na.rm = T) / sum(Production_kWh, na.rm = T)) %>%
  ggplot(aes(Date, SpecificBonus, fill = TypeOfForecast)) +
  geom_bar(stat = "identity", position = "dodge")
Forecast_ML %>% group_by(as.Date(Datetime, tz = "CET")) %>% dplyr::summarize(n = n()) %>% filter(n < 96)

datefilt <- c("2019-03-10", "2019-03-11", "2019-03-26", "2019-03-27", "2019-04-03", "2019-04-04",
              2019-04-05, "2019-04-12", "2019-04-13", "2019-04-14",
              2019-04-15, "2019-04-19", "2019-05-20", "2019-05-22", "2019-05-25")
dataw <- dataw %>% na.omit()
dataw <- dataw %>%
  filter(!as.Date(Datetime, tz = "CET") %in% as.Date(datefilt, tz = "CET"))
a <- dataw %>%
  filter(as.Date(Datetime, tz = "CET") > "2019-03-5") %>%
  group_by(TypeOfForecast, Date = as.Date(Datetime, tz = "CET")) %>%
  dplyr::summarize(
    Production_kWh = sum(Production_kWh, na.rm = T))
dataw %>%
  filter(as.Date(Datetime, tz = "CET") > "2019-03-04") %>%
  group_by(TypeOfForecast, Date = as.factor(lubridate::month(Datetime))) %>%
  dplyr::summarize(SpecificBonus = sum(Bonus_HUF, na.rm = T) / sum(Production_kWh, na.rm = T)) %>%
  ggplot(aes(Date, SpecificBonus, fill = TypeOfForecast)) +
  geom_col() +
  facet_wrap (TypeOfForecast ~ .)
dataw %>%
  filter(as.Date(Datetime, tz = "CET") > "2019-01-01") %>%
  group_by(TypeOfForecast, Date = as.factor(lubridate::month(Datetime))) %>%
  dplyr::summarize(Bonus = sum(Bonus_HUF, na.rm = T)) %>%
  ggplot(aes(Date, Bonus, fill = TypeOfForecast)) +
  geom_bar(stat = "identity", position = "dodge")
dataw %>%
  filter(as.Date(Datetime, tz = "CET") > "2018-01-01") %>%
  group_by(TypeOfForecast, Date = as.factor(lubridate::month(Datetime))) %>%
  dplyr::summarize(SpecificBonus = sum(Bonus_HUF, na.rm = T) / sum(Production_kWh, na.rm = T)) %>%
  ggplot(aes(Date, SpecificBonus, fill = TypeOfForecast)) +
  geom_col() +
  facet_wrap (TypeOfForecast ~ .)
dataw %>%
  filter(as.Date(Datetime, tz = "CET") > "2019-03-04") %>%
  group_by(TypeOfForecast, Hour = lubridate::hour(Datetime), Month = lubridate::month(Datetime)) %>%
  dplyr::summarize(SpecificBonus = sum(Bonus_HUF, na.rm = T) / sum(Production_kWh, na.rm = T)) %>%
  ggplot(aes(Hour, SpecificBonus, fill = TypeOfForecast)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~Month)
dataw %>%
  filter(as.Date(Datetime, tz = "CET") > "2019-03-04") %>%
  group_by(TypeOfForecast, Hour = lubridate::hour(Datetime), Month = lubridate::month(Datetime)) %>%
  dplyr::summarize(Bonus = sum(Bonus_HUF, na.rm = T)) %>%
  ggplot(aes(Hour, Bonus, fill = TypeOfForecast)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Month)
con_db <- dbConnect(odbcodbc(), "EON_GUR", UID = "eonuser", PWD = "gur21eon")
daily <- datawa %>% group_by(as.Date(Datetime, tz = "CET")) %>% dplyr::summarize(mean(Sch_dayahead_kW))
unique(as.Date(dataw$Datetime, tz = "CET"))
daily <- dataw %>%
  filter(as.Date(Datetime, tz = "CET") > "2019-03-5") %>%
  group_by(TypeOfForecast, Date = as.Date(Datetime, tz = "CET")) %>%
  dplyr::summarize(
    Production_kWh = sum(Production_kWh, na.rm = T),
    Bonus = sum(Bonus_HUF, na.rm = T),
    SpecificBonus = sum(Bonus_HUF, na.rm = T) / sum(Production_kWh, na.rm = T))
daily <- daily %>% na.omit()
daily %>% ggplot(aes(Date, SpecificBonus, color = TypeOfForecast)) + geom_line(size = 2) + geom_point(size = 2)
daily %>% ggplot(aes(Production_kWh, SpecificBonus, color = TypeOfForecast)) + geom_point(size = 2) + geom_point(size = 2) +
  facet_grid(. ~ lubridate::month(Date)) +
  geom_smooth(method = "lm")
write.table(daily, "Robinak_Napi.csv", row.names = F, sep = ";", dec = ",")
weekly <- daily %>%
  select(-SpecificBonus) %>%
  filter(as.Date(Date, tz = "CET") > "2019-03-5") %>%
  group_by(TypeOfForecast, Week = lubridate::week(Date)) %>%
  dplyr::summarize(
    Date = min(Date, tz = "CET"),
    Production_kWh = sum(Production_kWh, na.rm = T),
    Bonus = sum(Bonus, na.rm = T),
    SpecificBonus = sum(Bonus, na.rm = T) / sum(Production_kWh, na.rm = T))
weekly <- weekly %>% na.omit()
weekly %>% ggplot(aes(Week, SpecificBonus, color = TypeOfForecast)) + geom_line(size = 2) + geom_point(size = 2) +
  geom_smooth(method = "lm")
write.table(weekly, "Robinak_Heti.csv", row.names = F, sep = ";", dec = ",")
##### Különbség plot
## Daily calculation
daily_ml <- daily %>% filter(TypeOfForecast == "ML") %>%
  ungroup() %>%
  dplyr::rename(Bonus_ML = Bonus, SpecificBonus_ML = SpecificBonus) %>%
  select(-TypeOfForecast)
daily_or <- daily %>% filter(TypeOfForecast == "Original") %>%
  ungroup() %>%
  dplyr::rename(Bonus_OR = Bonus, SpecificBonus_OR = SpecificBonus) %>%
  select(-TypeOfForecast)
daily_diff <- join(daily_ml, daily_or) %>%
  mutate(Bonus_Diff = Bonus_ML - Bonus_OR,
         SpecificBonus_Diff = SpecificBonus_ML - SpecificBonus_OR)
weekly_ml <- daily_ml %>%
  select(-SpecificBonus_ML) %>%
  group_by(Week = lubridate::week(Date)) %>%
  dplyr::summarize(
    Date = min(Date, tz = "CET"),
    Production_kWh = sum(Production_kWh, na.rm = T),
    Bonus_ML = sum(Bonus_ML, na.rm = T),
    SpecificBonus_ML = sum(Bonus_ML, na.rm = T) / sum(Production_kWh, na.rm = T))
weekly_or <- daily_or %>%
  select(-SpecificBonus_OR) %>%
  group_by(Week = lubridate::week(Date)) %>%
  dplyr::summarize(
    Date = min(Date, tz = "CET"),
    Production_kWh = sum(Production_kWh, na.rm = T),
    Bonus_OR = sum(Bonus_OR, na.rm = T),
    SpecificBonus_OR = sum(Bonus_OR, na.rm = T) / sum(Production_kWh, na.rm = T))
weekly_diff <- join(weekly_ml, weekly_or) %>%
  mutate(Bonus_Diff = Bonus_ML - Bonus_OR,
         SpecificBonus_Diff = SpecificBonus_ML - SpecificBonus_OR)
monthly_ml <- daily_ml %>%
  select(-SpecificBonus_ML) %>%
  group_by(Month = lubridate::month(Date)) %>%
  dplyr::summarize(
    Date = min(Date, tz = "CET"),
    Production_kWh = sum(Production_kWh, na.rm = T),
    Bonus_ML = sum(Bonus_ML, na.rm = T),
    SpecificBonus_ML = sum(Bonus_ML, na.rm = T) / sum(Production_kWh, na.rm = T))
monthly_or <- daily_or %>%
  select(-SpecificBonus_OR) %>%
  group_by(Month = lubridate::month(Date)) %>%
  dplyr::summarize(
    Date = min(Date, tz = "CET"),
    Production_kWh = sum(Production_kWh, na.rm = T),
    Bonus_OR = sum(Bonus_OR, na.rm = T),
    SpecificBonus_OR = sum(Bonus_OR, na.rm = T) / sum(Production_kWh, na.rm = T))
monthly_diff <- join(monthly_ml, monthly_or) %>%
  mutate(Bonus_Diff = Bonus_ML - Bonus_OR,
         SpecificBonus_Diff = SpecificBonus_ML - SpecificBonus_OR)
daily_diff %>% ggplot(aes(Date, Bonus_Diff)) + geom_line() + geom_smooth(method = "lm") +
  geom_line(size = 2, color = "red", alpha = 0.6) + geom_point(color = "yellow", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = F) + theme_bw() +
  ggtitle("Napi bónusz különbség az eredeti és az öntanuló algoritmusok között") +
  ylab("Napi bónusz különbség forintban") +
  xlab("Dátum")
daily_diff %>% ggplot(aes(Date, SpecificBonus_Diff)) + geom_line(size = 2, color = "red", alpha = 0.6) + geom_point(color = "yellow", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = F) + theme_bw() +
  ggtitle("Napi fajlagos bónusz különbség az eredeti és az öntanuló algoritmusok között") +
  ylab("Napi fajlagos bónusz különbség forint / kWh-ban") +
  xlab("Dátum")
daily_diff %>% ggplot(aes(Date, Bonus_Diff)) + geom_line() + geom_smooth(method = "lm") +
  geom_line(size = 2, color = "red", alpha = 0.6) + geom_point(color = "yellow", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = F) + theme_bw() +
  ggtitle("Napi bónusz különbség az eredeti és az öntanuló algoritmusok között") +
  ylab("Napi bónusz különbség forintban") +
  xlab("Dátum")
daily_diff %>% ggplot(aes(Date, SpecificBonus_Diff)) + geom_line(size = 2, color = "red", alpha = 0.6) + geom_point(color = "yellow", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = F) + theme_bw() +
  ggtitle("Napi fajlagos bónusz különbség az eredeti és az öntanuló algoritmusok között") +
  ylab("Napi fajlagos bónusz különbség forint / kWh-ban") +
  xlab("Dátum")
weekly_diff %>% ggplot(aes(Week, SpecificBonus_Diff)) + geom_line(size = 2, color = "red", alpha = 0.6) + geom_point(color = "yellow", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = F) + theme_bw() +
  ggtitle("Heti fajlagos bónusz különbség az eredeti és az öntanuló algoritmusok között") +
  ylab("Heti fajlagos bónusz különbség forint / kWh-ban") +
  xlab("Hét száma")
weekly_diff %>% ggplot(aes(Week, Bonus_Diff)) + geom_line(size = 2, color = "red", alpha = 0.6) + geom_point(color = "yellow", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = F) + theme_bw() +
  ggtitle("Heti bónusz különbség az eredeti és az öntanuló algoritmusok között") +
  ylab("Heti bónusz különbség forintban") +
  xlab("Hét száma")
monthly_diff %>% ggplot(aes(Month, SpecificBonus_Diff, fill = SpecificBonus_Diff > 0)) +
  geom_col(alpha = 0.7) +
  theme_bw() +
  scale_fill_manual(values = c("red", "green")) +
  theme(legend.position = "none") +
  ggtitle("Havi fajlagos bónusz különbség az eredeti és az öntanuló algoritmusok között") +
  ylab("Havi fajlagos bónusz különbség forint / kWh-ban") +
  xlab("Hónap")
monthly_diff %>% ggplot(aes(Month, Bonus_Diff, fill = SpecificBonus_Diff > 0)) +
  geom_col(alpha = 0.7) +
  theme_bw() +
  scale_fill_manual(values = c("red", "green")) +
  theme(legend.position = "none") +
  ggtitle("Havi bónusz különbség az eredeti és az öntanuló algoritmusok között") +
  ylab("Havi bónusz különbség forintban") +
  xlab("Hónap")
monthly_diff %>% ggplot(aes(Month, Bonus_Diff, fill = SpecificBonus_Diff > 0)) +
  geom_col(alpha = 0.7) +
  theme_bw() +
  scale_fill_manual(values = c("red", "green")) +
  theme(legend.position = "none") +
  ggtitle("Havi bónusz különbség az eredeti és az öntanuló algoritmusok között") +
  ylab("Havi bónusz különbség forintban") +
  
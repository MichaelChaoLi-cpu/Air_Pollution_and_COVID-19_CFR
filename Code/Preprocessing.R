library(readr)
library(tidyverse)
library("dplyr")
library(haven)

setwd(".\\Data\\")

#global AQI: https://aqicn.org/data-platform/covid19/verify/57c94d81-c396-481a-a12c-93a0065c705f
# 2020 mean
Wider_Single_Year_AQI <- function(input){
  input <- input %>% as.tibble()
  wider_global_aqi <- input %>% dplyr::select("Date","Country","City","Specie","median")
  wider_global_aqi <- wider_global_aqi %>% 
    pivot_wider(names_from = "Specie", values_from = "median")
  if ('temperature' %in% colnames(wider_global_aqi)){
    a = 0
  } else {
    wider_global_aqi <- wider_global_aqi %>% mutate(temperature = NA)
  }
  if ('humidity' %in% colnames(wider_global_aqi)){
    a = 0
  } else {
    wider_global_aqi <- wider_global_aqi %>% mutate(humidity = NA)
  }
  if ('pressure' %in% colnames(wider_global_aqi)){
    a = 0
  } else {
    wider_global_aqi <- wider_global_aqi %>% mutate(pressure = NA)
  }
  wider_global_aqi <- wider_global_aqi %>% dplyr::select("Date","Country","City","co",
                                                         "pm10","o3","so2","no2","pm25",
                                                         'aqi',
                                                         'temperature', 'humidity',
                                                         'pressure')
  return(wider_global_aqi)
}

Get_AQI_2020 <- function(){
  Global_AQI_2020Q1 <- read.csv(file = 'waqi-covid19-airqualitydata-2020Q1.csv', skip = 4, encoding = "UTF-8")
  Global_AQI_2020Q2 <- read.csv(file = 'waqi-covid19-airqualitydata-2020Q2.csv', skip = 4, encoding = "UTF-8")
  Global_AQI_2020Q3 <- read.csv(file = 'waqi-covid19-airqualitydata-2020Q3.csv', skip = 4, encoding = "UTF-8")
  Global_AQI_2020Q4 <- read.csv(file = 'waqi-covid19-airqualitydata-2020Q4.csv', skip = 4, encoding = "UTF-8")
  
  Global_AQI_2020Q1 <- Wider_Single_Year_AQI(Global_AQI_2020Q1)
  Global_AQI_2020Q2 <- Wider_Single_Year_AQI(Global_AQI_2020Q2)
  Global_AQI_2020Q3 <- Wider_Single_Year_AQI(Global_AQI_2020Q3)
  Global_AQI_2020Q4 <- Wider_Single_Year_AQI(Global_AQI_2020Q4)
  
  Global_AQI_2020 <- rbind(Global_AQI_2020Q1, Global_AQI_2020Q2, Global_AQI_2020Q3, Global_AQI_2020Q4)
  return(Global_AQI_2020)
}

Get_AQI_2019 <- function(){
  Global_AQI_2019Q4 <- read.csv(file = 'waqi-covid19-airqualitydata-2019Q4.csv', skip = 4, encoding = "UTF-8")
  Global_AQI_2019Q3 <- read.csv(file = 'waqi-covid19-airqualitydata-2019Q3.csv', skip = 4, encoding = "UTF-8")
  Global_AQI_2019Q2 <- read.csv(file = 'waqi-covid19-airqualitydata-2019Q2.csv', skip = 4, encoding = "UTF-8")
  Global_AQI_2019Q1 <- read.csv(file = 'waqi-covid19-airqualitydata-2019Q1.csv', skip = 4, encoding = "UTF-8")
  
  Global_AQI_2019Q1 <- Wider_Single_Year_AQI(Global_AQI_2019Q1)
  Global_AQI_2019Q2 <- Wider_Single_Year_AQI(Global_AQI_2019Q2)
  Global_AQI_2019Q3 <- Wider_Single_Year_AQI(Global_AQI_2019Q3)
  Global_AQI_2019Q4 <- Wider_Single_Year_AQI(Global_AQI_2019Q4)
  
  Global_AQI_2019 <- rbind(Global_AQI_2019Q1, Global_AQI_2019Q2, Global_AQI_2019Q3, Global_AQI_2019Q4)
  return(Global_AQI_2019)
}

Get_AQI_Dataset_5Years <- function(AQI_2019 = Get_AQI_2019(), AQI_2020 = Get_AQI_2020()){
  Global_AQI_2018H1 <- read.csv(file = 'waqi-covid19-airqualitydata-2018H1.csv', skip = 4, encoding = "UTF-8")
  Global_AQI_2017H1 <- read.csv(file = 'waqi-covid19-airqualitydata-2017H1.csv', skip = 4, encoding = "UTF-8")
  Global_AQI_2016H1 <- read.csv(file = 'waqi-covid19-airqualitydata-2016H1.csv', skip = 4, encoding = "UTF-8")
  Global_AQI_2015H1 <- read.csv(file = 'waqi-covid19-airqualitydata-2015H1.csv', skip = 4, encoding = "UTF-8")
  
  Global_AQI_2018H1 <- Wider_Single_Year_AQI(Global_AQI_2018H1)
  Global_AQI_2017H1 <- Wider_Single_Year_AQI(Global_AQI_2017H1)
  Global_AQI_2016H1 <- Wider_Single_Year_AQI(Global_AQI_2016H1)
  Global_AQI_2015H1 <- Wider_Single_Year_AQI(Global_AQI_2015H1)
  
  Global_AQI <- rbind(Global_AQI_2015H1, Global_AQI_2016H1, Global_AQI_2017H1,
                      Global_AQI_2018H1, AQI_2019, AQI_2020)
  return(Global_AQI)
}

Global_AQI <- Get_AQI_Dataset_5Years(AQI_2019 = Get_AQI_2019(), AQI_2020 = Get_AQI_2020())

Global_AQI <- Global_AQI %>%
  mutate(humidity = ifelse(humidity > 100, NA, humidity)) %>%
  mutate(pressure = ifelse(pressure < 500, NA, pressure))
Global_AQI <- Global_AQI %>%
  mutate(AQI_Value = (pmax((o3/100), (no2/90),(pm10/50), (so2/125), 
                           (co/10000), na.rm = T))*50)
Global_AQI <- Global_AQI %>%
  mutate(AQI_Value = ifelse(is.na(AQI_Value), aqi, AQI_Value))
Global_AQI <- Global_AQI %>%
  mutate(Poor_AQI = ifelse(AQI_Value > 50, 1, 0),
         Day_count = 1)

Dataset_1_Global_AQI <- 
  aggregate(Global_AQI,
            by = list(Global_AQI$Country, Global_AQI$City), 
            FUN = "mean", na.rm = T)%>%
  dplyr::select(-Date, -Country, -City) %>%
  rename(
    "Country" = "Group.1",
    "City" = "Group.2",
  ) 

Dataset_1_Global_AQI <- Dataset_1_Global_AQI %>%
  arrange(City)
Dataset_1_Global_AQI$City_Code <- c(1:618) 

Global_AQI_name <- Dataset_1_Global_AQI %>%
  dplyr::select(City_Code ,City, Country)

country_code <- Global_AQI_name %>%
  dplyr::select(Country)
country_code <- country_code %>% distinct()
country_code$num <- c(1:95)


rm(Get_AQI_2019)
rm(Get_AQI_2020)
rm(Get_AQI_Dataset_5Years)
rm(Wider_Single_Year_AQI)

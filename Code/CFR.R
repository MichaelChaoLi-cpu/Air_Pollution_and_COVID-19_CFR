library(COVID19)
library(lubridate)
library(maptools)
library(covid19.analytics)
library(raster)
library(st)
library(sf)
library(tidyverse)

setwd(".\\Data\\")
city_location <- read.csv("City_Location_levelnames.csv")
## the location data is obtained by Geocoding API
## the latest data is as of July 30th 2021

city_location <- city_location %>%
  dplyr::select(-X) %>%
  rename(
    City_Code = X0,
    Country = X1,
    City = X2,
    Latitude = X3,
    Longitude = X4,
    long_city_name = X5,
    short_city_name = X6,
    long_country_name = X7,
    short_country_name = X8
  ) 
# %>% dplyr::select(long_city_name, short_city_name, long_country_name, short_country_name)

proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#function 
Get_covid_CFR <- function(covid){
  if (nrow(covid) > 0) { 
    covid$id <- covid$id %>% as.factor()
    covid <- covid %>% as.tibble()
    # calucualte CFR
    covid_CFR <- covid %>%
      dplyr::select(id, date, confirmed, recovered, deaths, population)
    covid_CFR <- covid_CFR %>%
      filter(date == ymd("2021-06-30"))
    covid_CFR <- covid_CFR %>%
      mutate(CFR = deaths / confirmed * 100)
    # calcualte days of restriction
    restrictions <- covid %>%
      dplyr::select(id, gatherings_restrictions, transport_closing, stay_home_restrictions,
                    internal_movement_restrictions, international_movement_restrictions) %>%
      mutate(
        gatherings_restrictions = ifelse(gatherings_restrictions > 0, 1, 0),
        transport_closing = ifelse(transport_closing > 0, 1, 0),
        stay_home_restrictions = ifelse(stay_home_restrictions > 0, 1, 0),
        internal_movement_restrictions = ifelse(internal_movement_restrictions > 0, 1, 0),
        international_movement_restrictions = ifelse(international_movement_restrictions > 0, 1, 0)
      )
    restrictions <-
      aggregate(restrictions[,c(2:6)] ,by = list(restrictions$id), FUN = 'sum', na.rm = T)
    restrictions <- restrictions %>% mutate(id = Group.1)
    covid_CFR <- merge(covid_CFR, restrictions)
    #location
    location <- covid %>%
      dplyr::select(id, iso_alpha_3, iso_alpha_2, administrative_area_level_1, administrative_area_level_2,
                    administrative_area_level_3,
                    administrative_area_level, longitude, latitude)
    location <- unique(location)
    covid_CFR <- merge(covid_CFR, location)
    rm(covid)
    return(covid_CFR)
  }
}


Country_2 <- country_code[1,1] %>% as.character()
###TR Turkey
####--country 1 ------
covid <- covid19(country = Country_2, level = 1)
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                     proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == "TR")
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point_dataset <- city_point
city_point_dataset <- city_point_dataset %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
rm(covid)
####--country 1 ------

Country_2 <- country_code[2,1] %>% as.character()
#### Poland PL
####--country 2 ------
covid <- covid19(country = Country_2, level = 1)
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
rm(covid)
####--country 2 ------

Country_2 <- country_code[3,1] %>% as.character()
#### 
####--country 3 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
rm(covid)
####--country 3 ------

Country_2 <- country_code[4,1] %>% as.character()
####--country 4 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
rm(covid)
####--country 4 ------

Country_2 <- country_code[5,1] %>% as.character()
####--country 5 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
rm(covid)
####--country 5 ------

Country_2 <- country_code[6,1] %>% as.character()
####--country 6 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
rm(covid)
####--country 6 ------

Country_2 <- country_code[7,1] %>% as.character()
####--country 7 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
rm(covid)
####--country 7 ------

Country_2 <- country_code[8,1] %>% as.character()
####--country 8 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
rm(covid)
####--country 8 ------

Country_2 <- country_code[9,1] %>% as.character()
####--country 9 ------
covid <- covid19(country = Country_2, level = 2) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 1)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
city_point_dataset <- city_point_dataset[!duplicated(city_point_dataset$City_Code), ]
####--country 9 ------

Country_2 <- country_code[10,1] %>% as.character()
#### Mexico
####--country 10 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)
covid_CFR <- covid_CFR[!duplicated(covid_CFR$administrative_area_level_2), ]

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 10 ------

Country_2 <- country_code[11,1] %>% as.character()
####--country 11 ------
covid <- covid19(country = Country_2, level = 2) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

supply <- covid19.data() %>% filter(Country_Region == "Japan") %>%
  dplyr::select(Province_State, Lat, Long_) %>%
  rename(administrative_area_level_2 = Province_State,
         latitude= Lat,
         longitude = Long_)
covid_CFR <- covid_CFR %>% dplyr::select(-longitude, -latitude)
covid_CFR <- left_join(covid_CFR, supply)
rm(supply)
covid_CFR <- covid_CFR[!duplicated(covid_CFR$id), ]

mapbase <- raster::getData("GADM", country = Country_2, level = 1)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
city_point_dataset <- city_point_dataset[!duplicated(city_point_dataset$City_Code), ]
####--country 11 ------

Country_2 <- country_code[12,1] %>% as.character()
####--country 12 ------
covid <- covid19(country = Country_2, level = 3) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)
covid_CFR <- covid_CFR[!duplicated(covid_CFR$id), ]

mapbase <- raster::getData("GADM", country = Country_2, level = 2)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 12 ------

Country_2 <- country_code[13,1] %>% as.character()
####--country 13 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 13 ------

Country_2 <- country_code[14,1] %>% as.character()
####--country 14 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 14 ------

Country_2 <- country_code[15,1] %>% as.character()
####--country 15 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 15 ------

Country_2 <- country_code[16,1] %>% as.character() #FR level 2
#### France without long and lat
####--country 16 ------
covid <- covid19(country = Country_2, level = 3) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)
covid_CFR$CFR <- covid_CFR$deaths / (covid_CFR$deaths + covid_CFR$recovered)

mapbase <- raster::getData("GADM", country = Country_2, level = 2)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0, NAME_2)
mapbase@data <- right_join(covid_CFR, mapbase@data, by = c("administrative_area_level_3" =
                                                  "NAME_2"))
mapbase <- st_as_sf(mapbase)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3, 4, 5)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point <- city_point %>%
  dplyr::select(-longitude, -latitude) %>%
  rename(longitude = Longitude,
         latitude = Latitude)
city_point <- city_point %>%
  dplyr::select(colnames(city_point_dataset))
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 16 ------

Country_2 <- country_code[17,1] %>% as.character()
####--country 17 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 17 ------

Country_2 <- country_code[18,1] %>% as.character()
####--country 18 ------
covid <- covid19(country = Country_2, level = 2) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

supply <- covid19.data() %>% filter(Country_Region == "Netherlands") %>%
  dplyr::select(Province_State, Lat, Long_) %>%
  rename(administrative_area_level_2 = Province_State,
         latitude= Lat,
         longitude = Long_)
covid_CFR <- covid_CFR %>% dplyr::select(-longitude, -latitude)
covid_CFR <- left_join(covid_CFR, supply)
rm(supply)
covid_CFR <- covid_CFR[!duplicated(covid_CFR$id), ]

mapbase <- raster::getData("GADM", country = Country_2, level = 1)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 18 ------

Country_2 <- country_code[19,1] %>% as.character()
####--country 19 ------
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 19 ------

Country_2 <- country_code[20,1] %>% as.character()
#### Belgium is not so enough
####--country 20 ------
covid <- covid19(country = Country_2, level = 2) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 1)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 20 ------

Country_2 <- country_code[21,1] %>% as.character()
####--country 21 ------
covid <- covid19(country = Country_2, level = 2) #3, 2 ,1 test one by one
covid_CFR <- Get_covid_CFR(covid)

covid_CFR <- covid_CFR %>%
  mutate(deaths = ifelse(is.na(deaths), (confirmed - recovered), deaths)) %>%
  mutate(CFR = deaths / confirmed * 100)

mapbase <- raster::getData("GADM", country = Country_2, level = 1)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))

rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  filter(!(City == "Xining" & administrative_area_level_2 == "Gansu"))
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 21 ------

Country_2 <- country_code[22,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
####--country 22 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 22 ------

Country_2 <- country_code[23,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
#### Iran
####--country 23 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 23 ------

Country_2 <- country_code[24,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
####--country 24 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 24 ------

Country_2 <- country_code[26,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
####--country 26 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 26 ------

Country_2 <- country_code[27,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
####--country 27 ------
covid_CFR <- Get_covid_CFR(covid)
covid_CFR <- covid_CFR %>%
  mutate(longitude = 174.7645, latitude = -43.53202)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point <- city_point %>%
  dplyr::select(-(date:latitude))
city_point$key <- 1
covid_CFR$key <- 1
city_point <- merge(x = city_point, y = covid_CFR, by = "key")
city_point <- city_point %>%
  dplyr::select(colnames(city_point_dataset))
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
####--country 27 ------

Country_2 <- country_code[28,1] %>% as.character()
covid <- covid19(country = Country_2, level = 3) #3, 2 ,1 test one by one
####--country 28 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 2)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 28 ------

Country_2 <- country_code[29,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
####--country 29 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 29 ------

Country_2 <- country_code[30,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
#### Philippines
####--country 30 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point <- city_point %>%
  dplyr::select(-(date:latitude))
city_point$key <- 1
covid_CFR$key <- 1
city_point <- merge(x = city_point, y = covid_CFR, by = "key")
city_point <- city_point %>%
  dplyr::select(colnames(city_point_dataset))
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 30 ------

Country_2 <- country_code[31,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
####--country 31 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 31 ------

Country_2 <- country_code[32,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
####--country 32 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 32 ------

Country_2 <- country_code[33,1] %>% as.character()
covid <- covid19(country = Country_2, level = 3) #3, 2 ,1 test one by one
####--country 33 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 2)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0, NAME_2)
mapbase@data <- left_join(mapbase@data, covid_CFR, by = c("NAME_2" = "administrative_area_level_3"))
mapbase@data <- mapbase@data %>%
  dplyr::select(-NAME_2)
mapbase <- st_as_sf(mapbase)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point <- city_point %>%
  mutate(administrative_area_level_3 = City) %>%
  dplyr::select(colnames(city_point_dataset))
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
####--country 33 ------

Country_2 <- country_code[34,1] %>% as.character()
covid <- covid19(country = Country_2, level = 3) #3, 2 ,1 test one by one
####--country 34 ------
covid_CFR <- Get_covid_CFR(covid)
covid_CFR <- covid_CFR %>%
  mutate(administrative_area_level_3 = ifelse(administrative_area_level_3 == 
                                                "Bristol, City of",
                                              "Bristol", 
                                              administrative_area_level_3),
         administrative_area_level_3 = ifelse(administrative_area_level_3 == 
                                                "Manchester",
                                              "London", 
                                              administrative_area_level_3),
         )

mapbase <- raster::getData("GADM", country = Country_2, level = 2)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0, NAME_2)
mapbase <- st_as_sf(mapbase)

mapbase <- left_join(mapbase, covid_CFR, by = c("NAME_2" =
  "administrative_area_level_3"))
mapbase <- mapbase %>%
  dplyr::select(-NAME_2)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
mapbase <- st_as_sf(mapbase)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-longitude, -latitude, -GID_0)
xy <- city_location_this %>%
  dplyr::select(City_Code, Latitude, Longitude) %>%
  rename(longitude = Longitude,
         latitude = Latitude)
city_point <- left_join(city_point, xy)
city_point <- city_point %>%
  dplyr::select( -id, -Group.1, -geometry)
city_point <- city_point %>%
  mutate(administrative_area_level_3 = City) %>%
  dplyr::select(colnames(city_point_dataset))
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 34 ------

Country_2 <- country_code[35,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
####--country 35 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 35 ------

Country_2 <- country_code[36,1] %>% as.character()
covid <- covid19(country = Country_2, level = 2) #3, 2 ,1 test one by one
####--country 36 ------
covid_CFR <- Get_covid_CFR(covid)
covid_CFR <- covid_CFR %>%
  mutate(CFR = ifelse(confirmed == 0, 0, CFR))

mapbase <- raster::getData("GADM", country = Country_2, level = 1)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 36 ------

Country_2 <- country_code[37,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
####--country 37 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 37 ------

Country_2 <- country_code[38,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
####--country 38 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)
point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 38 ------

Country_2 <- country_code[39,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
####--country 39 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 39 ------

Country_2 <- country_code[40,1] %>% as.character()
covid <- covid19(country = Country_2, level = 1) #3, 2 ,1 test one by one
#### south Africa
####--country 40 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 0)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 40 ------

Country_2 <- country_code[41,1] %>% as.character()
covid <- covid19(country = Country_2, level = 2) #3, 2 ,1 test one by one
####--country 41 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 1)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point <- city_point %>%
  filter(CFR < 2.2)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 41 ------

Country_2 <- country_code[42,1] %>% as.character()
covid <- covid19(country = Country_2, level = 2) #3, 2 ,1 test one by one
####--country 42 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = 1)
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 42 ------

Country_2 <- country_code[43,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 43 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 43 ------

Country_2 <- country_code[44,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 44 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 44 ------

Country_2 <- country_code[45,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 45 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 45 ------

Country_2 <- country_code[46,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 46 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 46 ------

Country_2 <- country_code[47,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 47 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 47 ------

Country_2 <- country_code[48,1] %>% as.character()
a <- 3
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 48 ------
if (nrow(covid) > 0) { 
  covid$id <- covid$id %>% as.factor()
  covid <- covid %>% as.tibble() %>%
    mutate(deaths = ifelse(is.na(deaths), 0, deaths))
  # calucualte CFR
  covid_CFR <- covid %>%
    filter(date == ymd("2021-06-28"))
  covid_CFR <- covid_CFR %>%
    dplyr::select(id, date, confirmed, recovered, deaths, population)
  covid_CFR <- covid_CFR %>%
    mutate(CFR = deaths / confirmed * 100)
  # calcualte days of restriction
  restrictions <- covid %>%
    dplyr::select(id, gatherings_restrictions, transport_closing, stay_home_restrictions,
                  internal_movement_restrictions, international_movement_restrictions) %>%
    mutate(
      gatherings_restrictions = ifelse(gatherings_restrictions > 0, 1, 0),
      transport_closing = ifelse(transport_closing > 0, 1, 0),
      stay_home_restrictions = ifelse(stay_home_restrictions > 0, 1, 0),
      internal_movement_restrictions = ifelse(internal_movement_restrictions > 0, 1, 0),
      international_movement_restrictions = ifelse(international_movement_restrictions > 0, 1, 0)
    )
  restrictions <-
    aggregate(restrictions[,c(2:6)] ,by = list(restrictions$id), FUN = 'sum', na.rm = T)
  restrictions <- restrictions %>% mutate(id = Group.1)
  covid_CFR <- merge(covid_CFR, restrictions)
  #location
  location <- covid %>%
    dplyr::select(id, iso_alpha_3, iso_alpha_2, administrative_area_level_1, administrative_area_level_2,
                  administrative_area_level_3,
                  administrative_area_level, longitude, latitude)
  location <- location[!duplicated(location$id), ]
  covid_CFR <- merge(covid_CFR, location)
  rm(covid)
}
covid_CFR <- covid_CFR %>%
  dplyr::select(-longitude, -latitude)

city_location_this <- city_location %>% filter(Country == Country_2)
city_location_this <- city_location_this %>%
  dplyr::select("City_Code","Country","City", Longitude, Latitude) %>%
  rename(longitude = Longitude,
         latitude = Latitude)

city_point <- left_join(city_location_this, covid_CFR, by = c("City" = "administrative_area_level_3"))

city_point <- city_point %>%
  dplyr::select(-id)
city_point_dataset <- city_point_dataset %>% as.data.frame()
city_point <- city_point %>%
  mutate(administrative_area_level_3 = City,
         geometry = NA) %>%
  dplyr::select(colnames(city_point_dataset))
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
####--country 48 ------

Country_2 <- country_code[49,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 49 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(location)
rm(mapbase)
rm(point)
rm(restrictions)
####--country 49 ------

Country_2 <- country_code[50,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 50 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 50 ------

Country_2 <- country_code[51,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 51 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 51 ------

Country_2 <- country_code[52,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 52 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 52 ------

Country_2 <- country_code[53,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 53 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 53 ------

Country_2 <- country_code[54,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 54 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 54 ------

Country_2 <- country_code[55,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 55 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 55 ------

Country_2 <- country_code[56,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 56 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 56 ------

Country_2 <- country_code[58,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 58 ------
covid_CFR <- Get_covid_CFR(covid)

mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
mapbase@data <- mapbase@data %>%
  dplyr::select(GID_0)
mapbase <- st_as_sf(mapbase)

covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
xy <- covid_CFR %>% dplyr::select(longitude, latitude)
point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                proj4string = CRS(proj))
rm(xy)

point <- st_as_sf(point)
mapbase <- st_join(mapbase, point)

city_location_this <- city_location %>% filter(Country == Country_2)
xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                     proj4string = CRS(proj))
rm(xy)
city_point <- st_as_sf(city_point)
city_point <- st_join(city_point, mapbase)
city_point <- city_point %>%
  dplyr::select(-GID_0, -id, -Group.1, -geometry)
city_point_dataset <- rbind(city_point_dataset, city_point)
rm(city_location_this)
rm(city_point)
rm(covid_CFR)
rm(mapbase)
rm(point)
####--country 58 ------

Country_2 <- country_code[59,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
if (a>0) {
  ####--country 59 ------
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
  ####--country 59 ------
}


Country_2 <- country_code[60,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 60 ------
if (a>0) {
  
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0, NAME_1)
  mapbase@data <- left_join(mapbase@data, covid_CFR, 
                            by = c("NAME_1" = "administrative_area_level_2")) %>%
    rename(administrative_area_level_2 = NAME_1)
  mapbase <- st_as_sf(mapbase)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3, 4, 5)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry, -longitude, -latitude) %>%
    rename(latitude = Latitude,
           longitude = Longitude)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 60 ------

Country_2 <- country_code[61,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 61 ------
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 61 ------

Country_2 <- country_code[62,1] %>% as.character()
a <- 3
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 62 ------
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  covid_CFR <- covid_CFR %>%
    mutate(administrative_area_level_3 = ifelse(administrative_area_level_3 == "Graz(Stadt)",
                                                "Graz" ,administrative_area_level_3 ),
           administrative_area_level_3 = ifelse(administrative_area_level_3 == "Innsbruck-Stadt",
                                                "Innsbruck" ,administrative_area_level_3 ),
           administrative_area_level_3 = ifelse(administrative_area_level_3 == "Linz(Stadt)",
                                                "Linz" ,administrative_area_level_3 ),
           administrative_area_level_3 = ifelse(administrative_area_level_3 == "Salzburg(Stadt)",
                                                "Salzburg" ,administrative_area_level_3 ),
           administrative_area_level_3 = ifelse(administrative_area_level_3 == "Wiener Neustadt(Stadt)",
                                                "Vienna" ,administrative_area_level_3 )
    )
  city_location_this <- city_location %>% filter(Country == Country_2)
  
  covid_CFR <- covid_CFR %>%
    dplyr::select(-longitude, -latitude)
  city_location_this <- city_location_this %>%
    dplyr::select("City_Code","Country","City","Latitude","Longitude") %>%
    rename(
      latitude = Latitude,
      longitude = Longitude
           )
  city_point <- left_join(city_location_this, covid_CFR,
                          by = c("City" = "administrative_area_level_3"))
  city_point <- city_point %>%
    rename(administrative_area_level_3 = City)
  city_point <- city_point %>%
    mutate(City = administrative_area_level_3,
           geometry = NA) %>%
    dplyr::select(colnames(city_point_dataset))
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(mapbase)
  rm(point)
}
####--country 62 ------

Country_2 <- country_code[63,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 63 ------
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 63 ------

Country_2 <- country_code[64,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 64 ------
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 64 ------

Country_2 <- country_code[65,1] %>% as.character()
a <- 1
covid <- covid19(country = Country_2, level = a) #3, 2 ,1 test one by one
####--country 65 ------
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 65 ------

Country_2 <- country_code[67,1] %>% as.character()
a <- 2
####--country 66 ------
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 66 ------

Country_2 <- country_code[68,1] %>% as.character()
a <- 2
####--country 68 ------
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 68 ------

Country_2 <- country_code[69,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 69 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 69 ------

Country_2 <- country_code[70,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
####--country 70 ------
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 70 ------

Country_2 <- country_code[71,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 71 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  covid_CFR <- covid_CFR %>%
    mutate(administrative_area_level_3 = ifelse(
      administrative_area_level_2 == "Kauno apskr.", "Kauno",
      administrative_area_level_2
    ))
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0, NAME_1)
  mapbase <- st_as_sf(mapbase)
  
  mapbase <- left_join(mapbase, covid_CFR, by = c("NAME_1" = "administrative_area_level_3"))
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point <- city_point %>%
    mutate(administrative_area_level_3 = City) %>%
    dplyr::select(colnames(city_point_dataset))
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(mapbase)
}
####--country 71 ------

Country_2 <- country_code[72,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 72 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(mapbase)
  rm(point)
}
####--country 72 ------

Country_2 <- country_code[73,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 73 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 73 ------

Country_2 <- country_code[75,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 75 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 75 ------

Country_2 <- country_code[76,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 76 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 76 ------

Country_2 <- country_code[77,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 77 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 77 ------

Country_2 <- country_code[78,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
####--country 78 ------
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 78 ------

Country_2 <- country_code[79,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 79 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 79 ------

Country_2 <- country_code[80,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 80 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  covid_CFR <- covid_CFR %>%
    mutate(longitude = ifelse(id == "6e83383f", -46.63331, longitude),
           latitude = ifelse(id == "6e83383f", -23.55052, latitude))
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(mapbase)
  rm(point)
}
####--country 80 ------

Country_2 <- country_code[82,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 82 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 82 ------

Country_2 <- country_code[83,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 83 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 83 ------

Country_2 <- country_code[84,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 84 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 84 ------

Country_2 <- country_code[85,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 85 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 85 ------

Country_2 <- country_code[86,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 86 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 86 ------

Country_2 <- country_code[87,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 87 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 87 ------

Country_2 <- country_code[88,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 88 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 88 ------

Country_2 <- country_code[89,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 89 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 89 ------

Country_2 <- country_code[90,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 90 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 90 ------

Country_2 <- country_code[91,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 91 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 91 ------

Country_2 <- country_code[92,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 93 ------

Country_2 <- country_code[94,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 94 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(location)
  rm(mapbase)
  rm(point)
  rm(restrictions)
}
####--country 94 ------

Country_2 <- country_code[95,1] %>% as.character()
a <- 2
covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
####--country 95 ------
if(nrow(covid) == 0) {
  a <- 1
  covid <- covid19(country = Country_2, level = a)#3, 2 ,1 test one by one
}
if (a>0) {
  covid_CFR <- Get_covid_CFR(covid)
  
  mapbase <- raster::getData("GADM", country = Country_2, level = (a-1))
  mapbase@data <- mapbase@data %>%
    dplyr::select(GID_0)
  mapbase <- st_as_sf(mapbase)
  
  covid_CFR <- covid_CFR %>% filter(!is.na(longitude))
  xy <- covid_CFR %>% dplyr::select(longitude, latitude)
  point <- SpatialPointsDataFrame(coords = xy, data = covid_CFR,
                                  proj4string = CRS(proj))
  rm(xy)
  
  point <- st_as_sf(point)
  mapbase <- st_join(mapbase, point)
  
  city_location_this <- city_location %>% filter(Country == Country_2)
  xy <- city_location_this %>% dplyr::select(Longitude, Latitude)
  city_point <- SpatialPointsDataFrame(coords = xy, data = city_location_this[,c(1, 2, 3)],
                                       proj4string = CRS(proj))
  rm(xy)
  city_point <- st_as_sf(city_point)
  city_point <- st_join(city_point, mapbase)
  city_point <- city_point %>%
    dplyr::select(-GID_0, -id, -Group.1, -geometry)
  city_point_dataset <- rbind(city_point_dataset, city_point)
  rm(city_location_this)
  rm(city_point)
  rm(covid_CFR)
  rm(mapbase)
  rm(point)
}
####--country 95 ------



city_point_dataset_0701 <- city_point_dataset %>%
  dplyr::select(City_Code, Country, date:CFR, internal_movement_restrictions,
                international_movement_restrictions, administrative_area_level)
rm(city_point_dataset)

country_level <- city_point_dataset_0701 %>%
  dplyr::select(Country, administrative_area_level) %>%
  unique() %>%
  filter(!is.na(administrative_area_level))

library(lmtest)
library(ncdf4)


analysis.data <- left_join(Dataset_1_Global_AQI, city_point_dataset_0101, by = ("City_Code"))
analysis.data <- left_join(analysis.data, city_point_dataset_0701, by = ("City_Code"))
analysis.data <- left_join(analysis.data, city_location %>% dplyr::select(
  City_Code, Longitude, Latitude
), by = ("City_Code"))

analysis.data <- analysis.data %>%
  dplyr::select(-geometry) %>% as.tibble()
analysis.data <- analysis.data %>%
  dplyr::select(-geometry_0101) %>% as.tibble()

#add age data
xy <- analysis.data %>% dplyr::select(Longitude, Latitude)
city_point <- SpatialPointsDataFrame(coords = xy, data = analysis.data,
                                     proj4string = CRS(proj))
rm(xy)

city_buffer <- rgeos::gBuffer(city_point, byid = T, width = 0.25)

# here we require the researcher who want to run this code download the raster
#     at first to AGE_raster
# WorldPop: https://www.worldpop.org/geodata/listing?id=65
filelist <- list.files("\\AGE_raster\\")
filelist_tif <- c()
for (filename in filelist){
  if (str_sub(filename, -3, -1) == 'tif'){
    filelist_tif <- append(filelist_tif, filename)
  }  
}
rm(filelist)

dataset <- data.frame(Doubles=double(),
                      Ints=integer(),
                      Factors=factor(),
                      Logicals=logical(),
                      Characters=character(),
                      stringsAsFactors=FALSE)
for (filename in filelist_tif){
  test_tiff <- raster(filename)
  crs(test_tiff) <- proj
  gender <- str_sub(filename, 8, 8)
  if (str_sub(filename, 11, 11) != '_') {
    age <- str_sub(filename, 10, 11) %>% as.numeric()
  } else {
    age <- str_sub(filename, 10, 10) %>% as.numeric()
  }
  data_ext <- terra::extract(test_tiff, city_buffer, fun = mean, na.rm = TRUE) # use 2 degrees buffer
  city_buffer@data$pop <- data_ext
  pop_data <- city_buffer@data %>%
    dplyr::select(City_Code, pop)
  pop_data <- pop_data %>%
    mutate(Gender = gender,
           Age_Range = age)
  dataset <- rbind(dataset, pop_data)
}

dataset$pop <- dataset$pop %>% as.numeric()
dataset_gender <- dataset %>%
  pivot_wider(id_cols = City_Code, values_from = pop, names_from = Gender, values_fn = sum)
dataset_total <- dataset_gender %>%
  mutate(total = m + f) %>%
  dplyr::select(-m, -f)
dataset_gender <- dataset_gender %>%
  mutate(m_perc = m / (m + f) * 100) %>%
  dplyr::select(-m, -f)

dataset_age <- dataset %>%
  pivot_wider(id_cols = City_Code, values_from = pop, names_from = Age_Range, values_fn = sum)
dataset_age <- left_join(dataset_age, dataset_total)
rm(dataset_total)

dataset_age <- dataset_age %>%
  mutate(
    perc_15 = (`0` + `1` + `5` + `10`) / total * 100,
    perc_45 = (`15` + `20` + `25` + `30` + `35` + `40`) / total * 100,
    perc_65 = (`45` + `50` + `55` + `60`) / total * 100,
    perc_80 = (`65` + `70` + `75` + `80`) / total * 100
  ) %>%
  dplyr::select(City_Code, perc_15, perc_45, perc_65, perc_80, total)

analysis.data <- left_join(analysis.data, dataset_age)
analysis.data <- left_join(analysis.data, dataset_gender)
rm(dataset_age)
rm(dataset_gender)


###----------- GDP calculation --------
# download this file from Scientific Data: 10.1038/sdata.2018.4  
nc_data <- nc_open("GDP_per_capita_PPP_1990_2015_v2.nc")
lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude", verbose = F)
t <- ncvar_get(nc_data, "time")
ndvi.array <- ncvar_get(nc_data, "GDP_per_capita_PPP")
ndvi.slice <- ndvi.array[, , 26] 
r <- raster(t(ndvi.slice), xmn = min(lon), 
            xmx = max(lon), ymn = min(lat), ymx = max(lat),
            crs = CRS(proj))
data_ext <- terra::extract(r, city_buffer, fun = mean, na.rm = TRUE)
city_buffer@data$GDP_per_C <- data_ext
dataset_GDP <- city_buffer@data %>%
  dplyr::select(City_Code, GDP_per_C)
analysis.data <- left_join(analysis.data, dataset_GDP)
###----------- GDP calculation --------


analysis.data <- analysis.data %>% distinct()
analysis.data.1 <- analysis.data
analysis.data <- analysis.data.1[!duplicated(analysis.data.1$City_Code), ]


library(ggplot2)
library(sp)
library(raster)
library(tidyverse)
library(rgdal)
library(rgeos)
library(sf)
library(tmap)
library(tmaptools)
library(grid)
library("viridis") 
library("viridisLite")
library(wesanderson)


setwd(".\\Data\\")

boundary <- st_read('country.shp', stringsAsFactors = F)

CFR <- analysis.data %>%
  dplyr::select(CFR_0701, Longitude, Latitude) %>% na.omit()
xy <- CFR %>% dplyr::select(Longitude, Latitude)
CFR_point <- SpatialPointsDataFrame(coords = xy, data = CFR,
                                     proj4string = CRS(proj))
rm(xy)
#----------------------------------------
leg.tex.siz <- 2
leg.tit.siz <- 1.8 * 1.5
mai.tit.siz <- 3 * leg.tex.siz
point.siz <- 0.6
lab.siz <-  2
#-----------------------------------------

brk_cfr <- c(0, 0.5, 1, 1.5, 2, 2.5, 5)
plot_cfr_all <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz) + 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(CFR_point) +
  tm_dots("CFR_0701",size = point.siz, breaks = brk_cfr,
          palette = 'YlOrRd',
          midpoint = 2,
          legend.is.portrait = F,  style = 'cont',
          title = 'The City-level CFR of COVID-19 (%)',
          labels = c('0', '0.5', '1', '1.5', '2', '2.5', '5+')
          ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    )
plot_cfr_all
plot_cfr_all %>%
  tmap_save("01_CFR_all_AP_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)

rm(CFR)
rm(CFR_point)
#---------------------------------------------

no2 <- analysis.data %>%
  dplyr::select(no2, Longitude, Latitude) %>% na.omit()
xy <- no2 %>% dplyr::select(Longitude, Latitude)
no2_point <- SpatialPointsDataFrame(coords = xy, data = no2,
                                    proj4string = CRS(proj))
rm(xy)
#---------------------------------------------------Concentration-air-pollution-----------
brk_dot <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
plot_c1 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(no2_point) +
  tm_dots("no2",size = point.siz, breaks = brk_dot,
          midpoint = 25,
          palette = 'YlOrRd',
          legend.is.portrait = F,  style = 'cont',
          labels = c("0", "", "10", "", "20", "", "30", "", "40", "", "50", "", "60+"),
          title = expression(paste('The Average Concentration of ', NO[2]))
          ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_c1
plot_c1 %>%
  tmap_save("02_no2_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)



so2 <- analysis.data %>%
  dplyr::select(so2, Longitude, Latitude) %>% na.omit()
xy <- so2 %>% dplyr::select(Longitude, Latitude)
so2_point <- SpatialPointsDataFrame(coords = xy, data = so2,
                                    proj4string = CRS(proj))
rm(xy)
#---------------------------------------------------Concentration-air-pollution-----------
brk_dot <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
plot_c2 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(so2_point) +
  tm_dots("so2",size = point.siz, breaks = brk_dot,
          midpoint = 25,
          palette = 'YlOrRd',
          legend.is.portrait = F,  style = 'cont',
          labels = c("0", "", "10", "", "20", "", "30", "", "40", "", "50", "", "60+"),
          title = expression(paste('The Average Concentration of ', SO[2]))
          )+
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_c2
plot_c2 %>%
  tmap_save("02_so2_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)



o3 <- analysis.data %>%
  dplyr::select(o3, Longitude, Latitude) %>% na.omit()
xy <- o3 %>% dplyr::select(Longitude, Latitude)
o3_point <- SpatialPointsDataFrame(coords = xy, data = o3,
                                    proj4string = CRS(proj))
rm(xy)
#---------------------------------------------------Concentration-air-pollution-----------
brk_dot <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
plot_c3 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(o3_point) +
  tm_dots("o3",size = point.siz, breaks = brk_dot,
          palette = 'YlOrRd',
          legend.is.portrait = F,  style = 'cont',
          labels = c("0", "", "20", "", "40", "", "60", "", "80+"),
          title = expression(paste('The Average Concentration of ', O[3]))
          ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_c3
plot_c3 %>%
  tmap_save("02_o3_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)


pm25 <- analysis.data %>%
  dplyr::select(pm25, Longitude, Latitude) %>% na.omit()
xy <- pm25 %>% dplyr::select(Longitude, Latitude)
pm25_point <- SpatialPointsDataFrame(coords = xy, data = pm25,
                                     proj4string = CRS(proj))
rm(xy)
#---------------------------------------------------Concentration-air-pollution-----------
brk_dot <- c(0, 25, 50, 75, 100, 125, 150, 175, 200)
plot_c4 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(pm25_point) +
  tm_dots("pm25",size = point.siz, breaks = brk_dot,
          midpoint = 50,
          palette = 'YlOrRd',
          legend.is.portrait = F,  style = 'cont',
          labels = c("0", "", "50", "", "100", "", "150", "", "200"),
          title = expression(paste('The Average Concentration of ', PM[2.5]))
          ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_c4
plot_c4 %>%
  tmap_save("02_pm25_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)



pm10 <- analysis.data %>%
  dplyr::select(pm10, Longitude, Latitude) %>% na.omit()
xy <- pm10 %>% dplyr::select(Longitude, Latitude)
pm10_point <- SpatialPointsDataFrame(coords = xy, data = pm10,
                                     proj4string = CRS(proj))
rm(xy)
#---------------------------------------------------Concentration-air-pollution-----------
brk_dot <- c(0, 25, 50, 75, 100, 125, 150, 175, 200)
plot_c5 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(pm10_point) +
  tm_dots("pm10",size = point.siz, breaks = brk_dot,
          palette = 'YlOrRd',
          midpoint = 80,
          legend.is.portrait = F,  style = 'cont',
          labels = c("0", "", "50", "", "100", "", "150", "", "200"),
          title = expression(paste('The Average Concentration of ', PM[10]))
          ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_c5
plot_c5 %>%
  tmap_save("02_pm10_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)


AQI <- analysis.data %>%
  dplyr::select(AQI_Value, Longitude, Latitude) %>% na.omit()
xy <- AQI %>% dplyr::select(Longitude, Latitude)
AQI_point <- SpatialPointsDataFrame(coords = xy, data = AQI,
                                     proj4string = CRS(proj))
rm(xy)
#---------------------------------------------------Concentration-air-pollution-----------
brk_dot <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
plot_c6 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(AQI_point) +
  tm_dots("AQI_Value",size = point.siz, breaks = brk_dot,
          palette = 'YlOrRd',
          legend.is.portrait = F,  style = 'cont',
          labels = c("0", "", "20", "", "40", "", "60", "", "80", "", "100"),
          title = 'The Average AQI') +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_c6
plot_c6 %>%
  tmap_save("02_AQI_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)


Pr_AQI <- analysis.data %>%
  dplyr::select(Poor_AQI, Longitude, Latitude) %>% na.omit()
xy <- Pr_AQI %>% dplyr::select(Longitude, Latitude)
Pr_AQI_point <- SpatialPointsDataFrame(coords = xy, data = Pr_AQI,
                                    proj4string = CRS(proj))
rm(xy)
#---------------------------------------------------Concentration-air-pollution-----------
brk_dot <- c(0, 0.1, 0.2, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)
plot_c7 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(Pr_AQI_point) +
  tm_dots("Poor_AQI",size = point.siz, breaks = brk_dot,
          palette = 'YlOrRd',
          legend.is.portrait = F,  style = 'cont',
          labels = c("0", "", "20", "", "40", "", "60", "", "80", "", "100"),
          title = 'The Probability with Poor AQI (%)') +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_c7
plot_c7 %>%
  tmap_save("02_Poor_AQI_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)


#--------------GWR Result-------------------
brk_dot <- c(0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10)
gwr.model.city.no2$SDF@data <- gwr.model.city.no2$SDF@data %>%
  mutate(no2_sig_coef = ifelse(no2_sig_coef == 0, NA, no2_sig_coef))
plot_4.1 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(gwr.model.city.no2$SDF) +
  tm_dots("no2_sig_coef",size = point.siz, breaks = brk_dot,
          palette = 'YlOrRd',
          textNA = "Not Significant",
          legend.is.portrait = F,  style = 'cont',
          labels = c("0.04", "", "0.06", "", "0.08", "", "0.10"),
          title = expression(paste('The Spatial Distribution of ', NO[2],' Coefficient'))
          ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_4.1
plot_4.1 %>%
  tmap_save("03_no2_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)

#There is no significant value in SO2

gwr.model.city.o3$SDF@data <- gwr.model.city.o3$SDF@data %>%
  mutate(o3_sig_coef = ifelse(o3_sig_coef == 0, NA, o3_sig_coef))
brk_dot <- c(0.03, 0.04, 0.05, 0.06, 0.07, 0.08)
plot_4.2 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(gwr.model.city.o3$SDF) +
  tm_dots("o3_sig_coef",size = point.siz, breaks = brk_dot,
          palette = 'YlOrRd',
          textNA = "Not Significant",
          legend.is.portrait = F,  style = 'cont',
          labels = c("0.03", "0.04", "0.05", "0.06", "0.07", "0.08"),
          title = expression(paste('The Spatial Distribution of ', O[3], ' Coefficient'))
  ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_4.2
plot_4.2 %>%
  tmap_save("03_o3_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)


gwr.model.city.pm25$SDF@data <- gwr.model.city.pm25$SDF@data %>%
  mutate(pm25_sig_coef = ifelse(pm25_sig_coef == 0, NA, pm25_sig_coef))
brk_dot <- c(-0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04)
plot_4.3 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(gwr.model.city.pm25$SDF) +
  tm_dots("pm25_sig_coef",size = point.siz, breaks = brk_dot,
          midpoint = 0,
          palette = '-RdYlBu',
          textNA = "Not Significant",
          legend.is.portrait = F,  style = 'cont',
          labels = c("0.03", "0.04", "0.05", "0.06", "0.07", "0.08"),
          title = expression(paste('The Spatial Distribution of ', PM[2.5], ' Coefficient'))
  ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_4.3
plot_4.3 %>%
  tmap_save("03_pm25_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)


gwr.model.city.AQI_Value$SDF@data <- gwr.model.city.AQI_Value$SDF@data %>%
  mutate(AQI_Value_sig_coef = ifelse(AQI_Value_sig_coef == 0, NA, AQI_Value_sig_coef))
brk_dot <- c(-0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06)
plot_4.4 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(gwr.model.city.AQI_Value$SDF) +
  tm_dots("AQI_Value_sig_coef",size = point.siz, breaks = brk_dot,
          midpoint = 0,
          palette = '-RdYlBu',
          textNA = "Not Significant",
          legend.is.portrait = F,  style = 'cont',
          title = 'The Spatial Distribution of AQI Coefficient'
  ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_4.4
plot_4.4 %>%
  tmap_save("04_Figure\\03_AQI_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)

gwr.model.city.Poor_AQI$SDF@data <- gwr.model.city.Poor_AQI$SDF@data %>%
  mutate(Poor_AQI_sig_coef = ifelse(Poor_AQI_sig_coef == 0, NA, Poor_AQI_sig_coef))
brk_dot <- c(1, 2, 3, 4, 5, 6, 7)
plot_4.5 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(gwr.model.city.Poor_AQI$SDF) +
  tm_dots("Poor_AQI_sig_coef",size = point.siz, breaks = brk_dot,
          palette = 'YlOrRd',
          textNA = "Not Significant",
          legend.is.portrait = F,  style = 'cont',
          labels = c("1", "2", "3", "4", "5", "6", "7"),
          title = 'The Spatial Distribution of Probability with Poor AQI Coefficient'
  ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_4.5
plot_4.5 %>%
  tmap_save("03_PrAQI_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)

#--------------GWR Result-------------------

#----------------CFRR -------------------
brk_dot <- c(0, 5, 10, 15, 20, 25, 30, 35)
plot_5.1 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(gwr.model.city.no2$SDF) +
  tm_dots("CFR_0701R",size = point.siz, breaks = brk_dot,
          palette = 'YlOrRd',
          textNA = "Not Significant",
          legend.is.portrait = F,  style = 'cont',
          title = expression(paste('The CFRR of a 1-PPB Increase in ', NO[2]))
          ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_5.1
plot_5.1 %>%
  tmap_save("04_no2_CFRR_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)


brk_dot <- c(0, 5, 10, 15, 20, 25, 30, 35, 40)
plot_5.2 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(gwr.model.city.o3$SDF) +
  tm_dots("CFR_0701R",size = point.siz, breaks = brk_dot,
          palette = 'YlOrRd',
          legend.is.portrait = F,  style = 'cont',
          textNA = "Not Significant",
          labels = c("0", "5", "10", "15", "20", "25", "30", "35", "40 +"),
          title = expression(paste('The CFRR of a 1-PPB Increase in ', O[3]))
  ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_5.2
plot_5.2 %>%
  tmap_save("04_o3_CFRR_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)



brk_dot <- c(-3, 0, 3, 6, 9, 12, 15, 18)
plot_5.3 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(gwr.model.city.pm25$SDF) +
  tm_dots("CFR_0701R",size = point.siz, breaks = brk_dot,
          midpoint = 0,
          palette = '-RdYlBu',
          legend.is.portrait = F,  style = 'cont',
          textNA = "Not Significant",
          labels = c("-3", "0", "3", "6", "9", "12", "15", "18"),
          title = expression(paste('The CFRR of a 1-', mu,'g Increase in ', PM[2.5]))
  ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_5.3
plot_5.3 %>%
  tmap_save("04_pm25_CFRR_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)


brk_dot <- c(-6, -4, -2, 0, 2, 4, 6, 8)
plot_5.4 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(gwr.model.city.AQI_Value$SDF) +
  tm_dots("CFR_0701R",size = point.siz, breaks = brk_dot,
          palette = '-RdYlBu',
          legend.is.portrait = F,  style = 'cont',
          textNA = "Not Significant",
          labels = c("-3", "0", "3", "6", "9", "12", "15", "18"),
          title = expression(paste('The CFRR of a 1-unit Increase in AQI'))
  ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_5.4
plot_5.4 %>%
  tmap_save("04_AQI_CFRR_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)

brk_dot <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
plot_5.5 <- tm_shape(boundary) + 
  tm_borders(lwd = 1, alpha = .5) +
  tm_grid(labels.size = lab.siz)+ 
  tm_layout(inner.margins=c(0,0,.1,0)) +
  tm_shape(gwr.model.city.Poor_AQI$SDF) +
  tm_dots("CFR_0701R",size = point.siz, breaks = brk_dot,
          palette = 'YlOrRd',
          legend.is.portrait = F,  style = 'cont',
          textNA = "Not Significant",
          labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8" ,"9", '10+'),
          title = expression(paste('The CFRR of 1% Increase in Probability with Poor AQI'))
  ) +
  tm_layout(
    frame.lwd = 3,
    legend.show = T,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.text.size = leg.tex.siz ,
    legend.title.size = leg.tit.siz * 2,
    main.title.size = mai.tit.siz, 
    main.title.position = c("center", "center"),
    legend.width = 20,
    legend.height = 4
  )
plot_5.5
plot_5.5 %>%
  tmap_save("04_PrAQI_CFRR_dot.jpg" , width = 800, height = 450, units = 'mm', dpi = 300)

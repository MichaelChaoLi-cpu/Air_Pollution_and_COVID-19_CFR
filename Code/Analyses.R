library(broom)
library(lmtest)
library(car)
library(olsrr)
library(sandwich)
library(RFGLS)
library(spdep)
library(spgwr)
library(GWmodel)
library(tmap)

analysis.data$lg_CFR_0701 <- (analysis.data$CFR_0701 + 1) %>% log()  
analysis.data$lg_CFR_0101 <- (analysis.data$CFR_0101 + 1) %>% log() 

analysis.data$GDP_per_C_ori <- analysis.data$GDP_per_C
analysis.data$GDP_per_C <- analysis.data$GDP_per_C %>% log()
analysis.data$PI <- analysis.data$confirmed / analysis.data$population * 100

analysis.data$perc_6580 <- analysis.data$perc_65 + analysis.data$perc_80
analysis.data$Country <- analysis.data$Country %>% as.character()

setwd(".\\Data\\")
continent <- read.csv(file = 'Continent.csv')

analysis.data <- left_join(analysis.data, continent, by = ("Country"))

analysis.data$Country <- analysis.data$Country %>% as.factor()
analysis.data$OECD <- analysis.data$OECD %>% as.factor()
analysis.data$Continent <- analysis.data$Continent %>% as.factor()

analysis.data$lg_pop_den <- analysis.data$total %>% log()
analysis.data$Country <- analysis.data$Country %>% as.factor()
analysis.data <- within(analysis.data, sample <- factor(OECD:Continent))
#####


basic.lm <- lm(CFR_0101 ~ so2, data = analysis.data)
summary(basic.lm)

library(ggplot2)  # load the package

(prelim_plot <- ggplot(analysis.data, aes(x = so2, y = CFR_0101)) +
    geom_point() +
    geom_smooth(method = "lm"))
plot(basic.lm, which = 1)
plot(basic.lm, which = 2)
jpeg("04_Figure\\REV02\\box_continent.jpg", width = 3600, height = 2200, res = 300)
boxplot(CFR_0701 ~ Continent, data = analysis.data,
        ylab="CFR (%)")
dev.off()



(colour_plot <- ggplot(analysis.data, aes(x = so2, y = CFR_0701, colour = Continent)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

#plot
jpeg("CFR_SO2.jpg", width = 3600, height = 2200, res = 300)
ggplot(aes(x = so2, y = CFR_0701), data = analysis.data %>%
                        filter(!is.na(Continent))) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    facet_wrap(~ Continent) + # create a facet for each mountain range
    xlab("SO2") + 
    ylab("CFR")
dev.off()

#plot 

mixed.lmer <- function(vari_name){
  #  input a variable name
  func = paste('CFR_0701 ~', vari_name, " +  temperature + pressure + humidity + 
  internal_movement_restrictions_0701 + m_perc +
  lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C + 
  (1|Continent)") %>% as.formula()
  return(func)
}

# all test 0701
no2.mixed.lmer <- lmer(mixed.lmer("no2"), data = analysis.data)
so2.mixed.lmer <- lmer(mixed.lmer("so2"), data = analysis.data)
o3.mixed.lmer <- lmer(mixed.lmer("o3"), data = analysis.data)
pm25.mixed.lmer <- lmer(mixed.lmer("pm25"), data = analysis.data)
pm10.mixed.lmer <- lmer(mixed.lmer("pm10"), data = analysis.data)
AQI.mixed.lmer <- lmer(mixed.lmer("AQI_Value"), data = analysis.data)
PrPoor.mixed.lmer <- lmer(mixed.lmer("Poor_AQI"), data = analysis.data)
PrPoor.AQI.mixed.lmer <- lmer(mixed.lmer("Poor_AQI + AQI_Value + Poor_AQI * AQI_Value"),
                              data = analysis.data)
summary(PrPoor.AQI.mixed.lmer)

library(stargazer)
setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\06_Article\\")
stargazer(no2.mixed.lmer, so2.mixed.lmer, o3.mixed.lmer,
          pm25.mixed.lmer, pm10.mixed.lmer, AQI.mixed.lmer,
          PrPoor.mixed.lmer,
          title = "Table XXX: Mixed Effects Regression Results", type = "text", no.space = T,
          covariate.labels = c('Average Concentration of NO2',
                               'Average Concentration of SO2',
                               'Average Concentration of O3',
                               'Average Concentration of PM2.5',
                               'Average Concentration of PM10',
                               'Average AQI',
                               'Probability with Poor AQI',
                               'Average Temerature',
                               'Average Pressure',
                               'Average Humidity',
                               "Internal Movement Restrictions",
                               'Percentage of Male',
                               'Population Density',
                               'Percentage of Population 15 - 45',
                               'Percentage of Population 45 - 65',
                               'Percentage Of Population >= 65',
                               'GDP per Capita (PPP)'),
          dep.var.labels = "City-level COVID-19 CFR (%)",
          column.labels = c("NO2", "SO2", "O3", "PM2.5", "PM10", "AQI", "Pr. AQI"),
          out = '10_RegressionResult\\REV02\\MixedEffectsRegression.html')

stargazer(analysis.data %>% dplyr::select(CFR_0701, no2, so2, o3, pm25, pm10,
                                          AQI_Value, Poor_AQI, temperature , 
                                          pressure , humidity , internal_movement_restrictions_0701,
                                          m_perc ,lg_pop_den , perc_45 , perc_65 ,
                                          perc_80 , GDP_per_C) %>% data.frame(),
          title = "Table 1: Descriptive Statistics of Cities", type = "text", no.space = T,
          covariate.labels = c('CFR','Average Concentration of NO2',
                               'Average Concentration of SO2',
                               'Average Concentration of O3',
                               'Average Concentration of PM2.5',
                               'Average Concentration of PM10',
                               'Average AQI',
                               'Probability with Poor AQI',
                               'Average Temerature',
                               'Average Pressure',
                               'Average Humidity',
                               "Internal Movement Restrictions",
                               'Percentage of Male',
                               'Population Density (1000/km2)',
                               'Percentage of Population 15 - 45',
                               'Percentage of Population 45 - 65',
                               'Percentage Of Population >= 65',
                               'Logarithm of GDP per Capita (PPP)'),
          out = '10_RegressionResult\\REV02\\summary.html'
)
# all test 0701

plot(no2.mixed.lmer)  
qqnorm(resid(no2.mixed.lmer))
qqline(resid(no2.mixed.lmer))


analysis.data.level.no.country <- analysis.data %>%
  filter(administrative_area_level_0701 > 1)

no2.mixed.lmer <- lmer(mixed.lmer("no2"), data = analysis.data.level.no.country)
so2.mixed.lmer <- lmer(mixed.lmer("so2"), data = analysis.data.level.no.country)
o3.mixed.lmer <- lmer(mixed.lmer("o3"), data = analysis.data.level.no.country)
pm25.mixed.lmer <- lmer(mixed.lmer("pm25"), data = analysis.data.level.no.country)
pm10.mixed.lmer <- lmer(mixed.lmer("pm10"), data = analysis.data.level.no.country)
AQI.mixed.lmer <- lmer(mixed.lmer("AQI_Value"), data = analysis.data.level.no.country)
PrPoor.mixed.lmer <- lmer(mixed.lmer("Poor_AQI"), data = analysis.data.level.no.country)


a <- 1
if(a){
  
  all_no2 <- lm(
    CFR_0701 ~
    no2 +
      temperature + pressure + humidity + 
      internal_movement_restrictions_0701 + m_perc +
      lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
    data = analysis.data
  )
  
  all_so2 <- lm(
    CFR_0701 ~
      so2 +
      temperature + pressure + humidity + 
      internal_movement_restrictions_0701 + m_perc +
      lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
    data = analysis.data
  )
  
  all_o3 <- lm(
    CFR_0701 ~
      o3 +
      temperature + pressure + humidity + 
      internal_movement_restrictions_0701 + m_perc +
      lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
    data = analysis.data
  )
  
  all_pm25 <- lm(
    CFR_0701 ~
      pm25 +
      temperature + pressure + humidity + 
      internal_movement_restrictions_0701 + m_perc +
      lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
    data = analysis.data
  )
  
  all_pm10 <- lm(
    CFR_0701 ~
      pm10 +
      temperature + pressure + humidity + 
      internal_movement_restrictions_0701 + m_perc +
      lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
    data = analysis.data
  )
  
  all_aqi <- lm(
    CFR_0701 ~
      AQI_Value +
      temperature + pressure + humidity + 
      internal_movement_restrictions_0701 + m_perc +
      lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
    data = analysis.data
  )
  
  all_aqi_poor <- lm(
    CFR_0701 ~
      Poor_AQI +
      temperature + pressure + humidity + 
      internal_movement_restrictions_0701 + m_perc +
      lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
    data = analysis.data
  )
  
  library(stargazer)
  setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\06_Article\\")
  stargazer(all_no2, all_so2, all_o3, all_pm25, all_pm10, all_aqi, all_aqi_poor,
            title = "Table XXX: OLS Regression Results", type = "text", no.space = T,
            covariate.labels = c('Average Concentration of NO2',
                                 'Average Concentration of SO2',
                                 'Average Concentration of O3',
                                 'Average Concentration of PM2.5',
                                 'Average Concentration of PM10',
                                 'Average AQI',
                                 'Exposure to Poor Air Quality',
                                 'Average Temerature',
                                 'Average Pressure',
                                 'Average Humidity',
                                 "Internal Movement Restrictions",
                                 "International Movement Restrictions",
                                 'Percentage of Male',
                                 'Percentage Of Population 15-44',
                                 'Percentage Of Population 45-64', 
                                 'Percentage Of Population >= 65',
                                 'Life Expectancy',
                                 'GDP per Capita (PPP)',
                                 'Body Mass Index',
                                 'Coronary Heart Disease Mortality Rate',
                                 'Lung Cancer Mortality Rate',
                                 "Lung Disease Mortility Rate",
                                 'Diabetes Mellitus Mortality Rate',
                                 'Human Development Index'),
            dep.var.labels = "City-level COVID-19 CFR (%)",
            column.labels = c("NO2", "SO2", "O3", "PM2.5", "PM10", "AQI", "Pr. AQI"),
            out = '10_RegressionResult\\REV02\\Regression_city_ols_all_vari.html')
}

bptest(all_no2)
rse.no2.1 <- vcovHAC(all_no2)
rse.no2.2 <- sqrt(diag(rse.no2.1))
coeftest(all_no2, vcov = rse.no2.1)

bptest(all_so2)
rse.so2.1 <- vcovHAC(all_so2)
rse.so2.2 <- sqrt(diag(rse.so2.1))
coeftest(all_so2, vcov = rse.so2.1)


bptest(all_o3)
rse.o3.1 <- vcovHAC(all_o3)
rse.o3.2 <- sqrt(diag(rse.o3.1))
coeftest(all_o3, vcov = rse.o3.1)


bptest(all_pm25)
rse.pm25.1 <- vcovHAC(all_pm25)
rse.pm25.2 <- sqrt(diag(rse.pm25.1))
coeftest(all_pm25, vcov = rse.pm25.1)


bptest(all_pm10)
rse.pm10.1 <- vcovHAC(all_pm10)
rse.pm10.2 <- sqrt(diag(rse.pm10.1))
coeftest(all_pm10, vcov = rse.pm10.1)


library(stargazer)
stargazer(all_no2, all_so2, all_o3, all_pm25, all_pm10,
          se = list(rse.no2.2, rse.so2.2, rse.o3.2, rse.pm25.2, rse.pm10.2),
          title = "Table XXX: OLS Regression Results", type = "text", no.space = T,
          covariate.labels = c('Average Concentration of NO2',
                               'Average Concentration of SO2',
                               'Average Concentration of O3',
                               'Average Concentration of PM2.5',
                               'Average Concentration of PM10',
                               'Average Temerature',
                               'Average Pressure',
                               'Average Humidity',
                               "Internal Movement Restrictions",
                               "International Movement Restrictions",
                               'Percentage of Male',
                               'Percentage Of Population 15-44',
                               'Percentage Of Population 45-64', 
                               'Percentage Of Population >= 65',
                               'Life Expectancy',
                               'GDP per Capita (PPP)',
                               'Body Mass Index',
                               'Coronary Heart Disease Mortality Rate',
                               'Lung Cancer Mortality Rate',
                               "Lung Disease Mortility Rate",
                               'Diabetes Mellitus Mortality Rate',
                               'Human Development Index'),
          dep.var.labels = "City-level COVID-19 CFR (%)",
          column.labels = c("Model I", "Model II", "Model III", "Model IV", "Model V"),
          out = 'Regression_city_ols_use.html')

CFRR <- function(dataset, model){
  mean <- dataset$CFR_0701 %>% mean()
  coef <- model %>% coef()
  coef.use <- coef$Continent[2][[1]][1]
  CFRR.v <- coef.use/mean * 100
  ci95 <- confint(model)
  lower <- ci95[4]/mean * 100
  upper <- ci95[18]/mean * 100
  array <- c(CFRR.v, lower, upper)
  return(array)
}

CFRR.no2 <- CFRR(ana_no2, no2.mixed.lmer)
CFRR.so2 <- CFRR(ana_so2, so2.mixed.lmer)
CFRR.o3 <- CFRR(ana_o3, o3.mixed.lmer)
CFRR.pm25 <- CFRR(ana_pm25, pm25.mixed.lmer)
CFRR.pm10 <- CFRR(ana_pm10, pm10.mixed.lmer)
CFRR.AQI <- CFRR(ana_AQI_Value, AQI.mixed.lmer)
CFRR.Poor.AQI <- CFRR(ana_Poor_AQI, PrPoor.mixed.lmer)


count.country <- analysis.data %>%
  dplyr::select(Country.x, administrative_area_level_0701)
count.country$Country.x <- count.country$Country.x %>% as.factor()
count.country$num <- 1
count.country.num <- count.country %>%
  aggregate(by = list(count.country$Country.x), FUN = length)
level.country <- count.country %>%
  aggregate(by = list(count.country$Country.x), FUN = mean, na.rm = T)
Country.FullName <- read.csv('Country_CFR_Level.csv', header = T) %>%
  dplyr::select(-Data.Level)
level.country <- level.country %>%
  dplyr::select(Group.1, administrative_area_level_0701) %>%
  rename(Country = Group.1,
         Level = administrative_area_level_0701)
level.country <- left_join(level.country, Country.FullName)
count.country.num <- count.country.num %>%
  dplyr::select(Group.1, administrative_area_level_0701) %>%
  rename(Country = Group.1,
         City.Number = administrative_area_level_0701)
level.country <- left_join(level.country, count.country.num)
level.country <- level.country %>%
  dplyr::select(Full.Name, Country, Level, City.Number) %>%
  mutate(which.level = NA)
level.country <- level.country %>%
  mutate(which.level = ifelse(Level == 1, 'Country', which.level),
         which.level = ifelse(Level == 2, 'State/Province', which.level),
         which.level = ifelse(Level == 3, 'City', which.level)
         )
level.country %>% write.csv('level_country.csv')

library(GWmodel)


#------------------GWR no2---------------
ana_no2 <- analysis.data %>%
  dplyr::select(City_Code, CFR_0701, no2, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_no2 <-  ana_no2[!duplicated(ana_no2$City_Code),]

xy <- ana_no2 %>% dplyr::select(Longitude, Latitude)
ana_no2_point <- SpatialPointsDataFrame(coords = xy, data = ana_no2,
                                        proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         no2 + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_no2_point)
gwr.model.no2 = gwr.basic(CFR_0701  ~ 
                            no2 + 
                            temperature + pressure + humidity + 
                            internal_movement_restrictions_0701 + m_perc +
                            lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                          data = ana_no2_point,
                          bw  = GWRbandwidth) 
gwr.model.no2$SDF@data$no2_sig <- ifelse(abs(gwr.model.no2$SDF@data$no2_TV) > 1.96,
                                         1, 0)
gwr.model.no2$SDF@data$no2_sig_coef <- gwr.model.no2$SDF@data$no2 *
  gwr.model.no2$SDF@data$no2_sig
gwr.model.no2$SDF@data$CFR_0701R <- gwr.model.no2$SDF@data$no2 / ana_no2_point@data$CFR_0701 * 100

#------------------GWR so2---------------
ana_so2 <- analysis.data %>%
  dplyr::select(City_Code, CFR_0701, so2, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_so2 <-  ana_so2[!duplicated(ana_so2$City_Code),]

xy <- ana_so2 %>% dplyr::select(Longitude, Latitude)
ana_so2_point <- SpatialPointsDataFrame(coords = xy, data = ana_so2,
                                        proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         so2 + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_so2_point)
gwr.model.so2 = gwr.basic(CFR_0701  ~ 
                            so2 + 
                            temperature + pressure + humidity + 
                            internal_movement_restrictions_0701 + m_perc +
                            lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                          data = ana_so2_point,
                          bw  = GWRbandwidth) 
gwr.model.so2$SDF@data$so2_sig <- ifelse(abs(gwr.model.so2$SDF@data$so2_TV) > 1.96,
                                         1, 0)
gwr.model.so2$SDF@data$so2_sig_coef <- gwr.model.so2$SDF@data$so2 *
  gwr.model.so2$SDF@data$so2_sig
gwr.model.so2$SDF@data$CFR_0701R <- gwr.model.so2$SDF@data$so2 / ana_so2_point@data$CFR_0701 * 100

#------------------GWR o3---------------
ana_o3 <- analysis.data %>%
  dplyr::select(City_Code, CFR_0701, o3, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_o3 <-  ana_o3[!duplicated(ana_o3$City_Code),]

xy <- ana_o3 %>% dplyr::select(Longitude, Latitude)
ana_o3_point <- SpatialPointsDataFrame(coords = xy, data = ana_o3,
                                       proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         o3 + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_o3_point)
gwr.model.o3 = gwr.basic(CFR_0701  ~ 
                           o3 + 
                           temperature + pressure + humidity + 
                           internal_movement_restrictions_0701 + m_perc +
                           lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                         data = ana_o3_point,
                         bw  = GWRbandwidth) 
gwr.model.o3$SDF@data$o3_sig <- ifelse(abs(gwr.model.o3$SDF@data$o3_TV) > 1.96,
                                       1, 0)
gwr.model.o3$SDF@data$o3_sig_coef <- gwr.model.o3$SDF@data$o3 *
  gwr.model.o3$SDF@data$o3_sig
gwr.model.o3$SDF@data$CFR_0701R <- gwr.model.o3$SDF@data$o3 / ana_o3_point@data$CFR_0701 * 100


#------------------GWR pm25---------------
ana_pm25 <- analysis.data %>%
  dplyr::select(City_Code, CFR_0701, pm25, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_pm25 <-  ana_pm25[!duplicated(ana_pm25$City_Code),]

xy <- ana_pm25 %>% dplyr::select(Longitude, Latitude)
ana_pm25_point <- SpatialPointsDataFrame(coords = xy, data = ana_pm25,
                                         proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         pm25 + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_pm25_point)
gwr.model.pm25 = gwr.basic(CFR_0701  ~ 
                             pm25 + 
                             temperature + pressure + humidity + 
                             internal_movement_restrictions_0701 + m_perc +
                             lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                           data = ana_pm25_point,
                           bw  = GWRbandwidth) 
gwr.model.pm25$SDF@data$pm25_sig <- ifelse(abs(gwr.model.pm25$SDF@data$pm25_TV) > 1.96,
                                           1, 0)
gwr.model.pm25$SDF@data$pm25_sig_coef <- gwr.model.pm25$SDF@data$pm25 *
  gwr.model.pm25$SDF@data$pm25_sig
gwr.model.pm25$SDF@data$CFR_0701R <- gwr.model.pm25$SDF@data$pm25 / ana_pm25_point@data$CFR_0701 * 100

#------------------GWR pm10---------------
ana_pm10 <- analysis.data %>%
  dplyr::select(City_Code, CFR_0701, pm10, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_pm10 <-  ana_pm10[!duplicated(ana_pm10$City_Code),]

xy <- ana_pm10 %>% dplyr::select(Longitude, Latitude)
ana_pm10_point <- SpatialPointsDataFrame(coords = xy, data = ana_pm10,
                                         proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         pm10 + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_pm10_point)
gwr.model.pm10 = gwr.basic(CFR_0701  ~ 
                             pm10 + 
                             temperature + pressure + humidity + 
                             internal_movement_restrictions_0701 + m_perc +
                             lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                           data = ana_pm10_point,
                           bw  = GWRbandwidth) 
gwr.model.pm10$SDF@data$pm10_sig <- ifelse(abs(gwr.model.pm10$SDF@data$pm10_TV) > 1.96,
                                           1, 0)
gwr.model.pm10$SDF@data$pm10_sig_coef <- gwr.model.pm10$SDF@data$pm10 *
  gwr.model.pm10$SDF@data$pm10_sig
gwr.model.pm10$SDF@data$CFR_0701R <- gwr.model.pm10$SDF@data$pm10 / ana_pm10_point@data$CFR_0701 * 100


#------------------GWR AQI_Value---------------
ana_AQI_Value <- analysis.data %>%
  dplyr::select(City_Code, CFR_0701, AQI_Value, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_AQI_Value <-  ana_AQI_Value[!duplicated(ana_AQI_Value$City_Code),]

xy <- ana_AQI_Value %>% dplyr::select(Longitude, Latitude)
ana_AQI_Value_point <- SpatialPointsDataFrame(coords = xy, data = ana_AQI_Value,
                                              proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         AQI_Value + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_AQI_Value_point)
gwr.model.AQI_Value = gwr.basic(CFR_0701  ~ 
                                  AQI_Value + 
                                  temperature + pressure + humidity + 
                                  internal_movement_restrictions_0701 + m_perc +
                                  lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                                data = ana_AQI_Value_point,
                                bw  = GWRbandwidth) 
gwr.model.AQI_Value$SDF@data$AQI_Value_sig <- ifelse(abs(gwr.model.AQI_Value$SDF@data$AQI_Value_TV) > 1.96,
                                                     1, 0)
gwr.model.AQI_Value$SDF@data$AQI_Value_sig_coef <- gwr.model.AQI_Value$SDF@data$AQI_Value *
  gwr.model.AQI_Value$SDF@data$AQI_Value_sig
gwr.model.AQI_Value$SDF@data$CFR_0701R <- gwr.model.AQI_Value$SDF@data$AQI_Value / ana_AQI_Value_point@data$CFR_0701 * 100

#------------------GWR Poor_AQI---------------
ana_Poor_AQI <- analysis.data %>%
  dplyr::select(City_Code, CFR_0701, Poor_AQI, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_Poor_AQI <-  ana_Poor_AQI[!duplicated(ana_Poor_AQI$City_Code),]

xy <- ana_Poor_AQI %>% dplyr::select(Longitude, Latitude)
ana_Poor_AQI_point <- SpatialPointsDataFrame(coords = xy, data = ana_Poor_AQI,
                                             proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         Poor_AQI + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_Poor_AQI_point)
gwr.model.Poor_AQI = gwr.basic(CFR_0701  ~ 
                                 Poor_AQI + 
                                 temperature + pressure + humidity + 
                                 internal_movement_restrictions_0701 + m_perc +
                                 lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                               data = ana_Poor_AQI_point,
                               bw  = GWRbandwidth) 
gwr.model.Poor_AQI$SDF@data$Poor_AQI_sig <- ifelse(abs(gwr.model.Poor_AQI$SDF@data$Poor_AQI_TV) > 1.96,
                                                   1, 0)
gwr.model.Poor_AQI$SDF@data$Poor_AQI_sig_coef <- gwr.model.Poor_AQI$SDF@data$Poor_AQI *
  gwr.model.Poor_AQI$SDF@data$Poor_AQI_sig
gwr.model.Poor_AQI$SDF@data$CFR_0701R <- gwr.model.Poor_AQI$SDF@data$Poor_AQI / ana_Poor_AQI_point@data$CFR_0701 * 100

tm_shape(gwr.model.no2$SDF) + 
  tm_dots(col = "no2_sig_coef")


#------------------GWR city level no2---------------
ana_no2 <- analysis.data %>%
  filter(administrative_area_level_0701 > 1) %>%
  dplyr::select(City_Code, CFR_0701, no2, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_no2 <-  ana_no2[!duplicated(ana_no2$City_Code),]

xy <- ana_no2 %>% dplyr::select(Longitude, Latitude)
ana_no2_point <- SpatialPointsDataFrame(coords = xy, data = ana_no2,
                                        proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         no2 + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_no2_point)
gwr.model.city.no2 = gwr.basic(CFR_0701  ~ 
                                 no2 + 
                                 temperature + pressure + humidity + 
                                 internal_movement_restrictions_0701 + m_perc +
                                 lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                               data = ana_no2_point,
                               bw  = GWRbandwidth) 
gwr.model.city.no2$SDF@data$no2_sig <- ifelse(abs(gwr.model.city.no2$SDF@data$no2_TV) > 1.96,
                                              1, 0)
gwr.model.city.no2$SDF@data$no2_sig_coef <- gwr.model.city.no2$SDF@data$no2 *
  gwr.model.city.no2$SDF@data$no2_sig
gwr.model.city.no2$SDF@data$CFR_0701R <- gwr.model.city.no2$SDF@data$no2_sig_coef / ana_no2_point@data$CFR_0701 * 100

#------------------GWR city level so2---------------
ana_so2 <- analysis.data %>%
  filter(administrative_area_level_0701 > 1) %>%
  dplyr::select(City_Code, CFR_0701, so2, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_so2 <-  ana_so2[!duplicated(ana_so2$City_Code),]

xy <- ana_so2 %>% dplyr::select(Longitude, Latitude)
ana_so2_point <- SpatialPointsDataFrame(coords = xy, data = ana_so2,
                                        proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         so2 + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_so2_point)
gwr.model.city.so2 = gwr.basic(CFR_0701  ~ 
                                 so2 + 
                                 temperature + pressure + humidity + 
                                 internal_movement_restrictions_0701 + m_perc +
                                 lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                               data = ana_so2_point,
                               bw  = GWRbandwidth) 
gwr.model.city.so2$SDF@data$so2_sig <- ifelse(abs(gwr.model.city.so2$SDF@data$so2_TV) > 1.96,
                                              1, 0)
gwr.model.city.so2$SDF@data$so2_sig_coef <- gwr.model.city.so2$SDF@data$so2 *
  gwr.model.city.so2$SDF@data$so2_sig
gwr.model.city.so2$SDF@data$CFR_0701R <- gwr.model.city.so2$SDF@data$so2_sig_coef / ana_so2_point@data$CFR_0701 * 100

#------------------GWR city level o3---------------
ana_o3 <- analysis.data %>%
  filter(administrative_area_level_0701 > 1) %>%
  dplyr::select(City_Code, CFR_0701, o3, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_o3 <-  ana_o3[!duplicated(ana_o3$City_Code),]

xy <- ana_o3 %>% dplyr::select(Longitude, Latitude)
ana_o3_point <- SpatialPointsDataFrame(coords = xy, data = ana_o3,
                                       proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         o3 + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_o3_point)
gwr.model.city.o3 = gwr.basic(CFR_0701  ~ 
                                o3 + 
                                temperature + pressure + humidity + 
                                internal_movement_restrictions_0701 + m_perc +
                                lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                              data = ana_o3_point,
                              bw  = GWRbandwidth) 
gwr.model.city.o3$SDF@data$o3_sig <- ifelse(abs(gwr.model.city.o3$SDF@data$o3_TV) > 1.96,
                                            1, 0)
gwr.model.city.o3$SDF@data$o3_sig_coef <- gwr.model.city.o3$SDF@data$o3 *
  gwr.model.city.o3$SDF@data$o3_sig
gwr.model.city.o3$SDF@data$CFR_0701R <- gwr.model.city.o3$SDF@data$o3_sig_coef / ana_o3_point@data$CFR_0701 * 100


#------------------GWR city level pm25---------------
ana_pm25 <- analysis.data %>%
  filter(administrative_area_level_0701 > 1) %>%
  dplyr::select(City_Code, CFR_0701, pm25, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_pm25 <-  ana_pm25[!duplicated(ana_pm25$City_Code),]

xy <- ana_pm25 %>% dplyr::select(Longitude, Latitude)
ana_pm25_point <- SpatialPointsDataFrame(coords = xy, data = ana_pm25,
                                         proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         pm25 + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_pm25_point)
gwr.model.city.pm25 = gwr.basic(CFR_0701  ~ 
                                  pm25 + 
                                  temperature + pressure + humidity + 
                                  internal_movement_restrictions_0701 + m_perc +
                                  lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                                data = ana_pm25_point,
                                bw  = GWRbandwidth) 
gwr.model.city.pm25$SDF@data$pm25_sig <- ifelse(abs(gwr.model.city.pm25$SDF@data$pm25_TV) > 1.96,
                                                1, 0)
gwr.model.city.pm25$SDF@data$pm25_sig_coef <- gwr.model.city.pm25$SDF@data$pm25 *
  gwr.model.city.pm25$SDF@data$pm25_sig
gwr.model.city.pm25$SDF@data$CFR_0701R <- gwr.model.city.pm25$SDF@data$pm25_sig_coef / ana_pm25_point@data$CFR_0701 * 100

#------------------GWR city level pm10---------------
ana_pm10 <- analysis.data %>%
  filter(administrative_area_level_0701 > 1) %>%
  dplyr::select(City_Code, CFR_0701, pm10, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_pm10 <-  ana_pm10[!duplicated(ana_pm10$City_Code),]

xy <- ana_pm10 %>% dplyr::select(Longitude, Latitude)
ana_pm10_point <- SpatialPointsDataFrame(coords = xy, data = ana_pm10,
                                         proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         pm10 + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_pm10_point)
gwr.model.city.pm10 = gwr.basic(CFR_0701  ~ 
                                  pm10 + 
                                  temperature + pressure + humidity + 
                                  internal_movement_restrictions_0701 + m_perc +
                                  lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                                data = ana_pm10_point,
                                bw  = GWRbandwidth) 
gwr.model.city.pm10$SDF@data$pm10_sig <- ifelse(abs(gwr.model.city.pm10$SDF@data$pm10_TV) > 1.96,
                                                1, 0)
gwr.model.city.pm10$SDF@data$pm10_sig_coef <- gwr.model.city.pm10$SDF@data$pm10 *
  gwr.model.city.pm10$SDF@data$pm10_sig
gwr.model.city.pm10$SDF@data$CFR_0701R <- gwr.model.city.pm10$SDF@data$pm10_sig_coef / ana_pm10_point@data$CFR_0701 * 100


#------------------GWR city level AQI_Value---------------
ana_AQI_Value <- analysis.data %>%
  filter(administrative_area_level_0701 > 1) %>%
  dplyr::select(City_Code, CFR_0701, AQI_Value, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_AQI_Value <-  ana_AQI_Value[!duplicated(ana_AQI_Value$City_Code),]

xy <- ana_AQI_Value %>% dplyr::select(Longitude, Latitude)
ana_AQI_Value_point <- SpatialPointsDataFrame(coords = xy, data = ana_AQI_Value,
                                              proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         AQI_Value + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_AQI_Value_point)
gwr.model.city.AQI_Value = gwr.basic(CFR_0701  ~ 
                                       AQI_Value + 
                                       temperature + pressure + humidity + 
                                       internal_movement_restrictions_0701 + m_perc +
                                       lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                                     data = ana_AQI_Value_point,
                                     bw  = GWRbandwidth) 
gwr.model.city.AQI_Value$SDF@data$AQI_Value_sig <- ifelse(abs(gwr.model.city.AQI_Value$SDF@data$AQI_Value_TV) > 1.96,
                                                          1, 0)
gwr.model.city.AQI_Value$SDF@data$AQI_Value_sig_coef <- gwr.model.city.AQI_Value$SDF@data$AQI_Value *
  gwr.model.city.AQI_Value$SDF@data$AQI_Value_sig
gwr.model.city.AQI_Value$SDF@data$CFR_0701R <- gwr.model.city.AQI_Value$SDF@data$AQI_Value_sig_coef / ana_AQI_Value_point@data$CFR_0701 * 100

#------------------GWR city level Poor_AQI---------------
ana_Poor_AQI <- analysis.data %>%
  filter(administrative_area_level_0701 > 1) %>%
  dplyr::select(City_Code, CFR_0701, Poor_AQI, 
                temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, 
                Latitude, Longitude) %>% na.omit()
ana_Poor_AQI <-  ana_Poor_AQI[!duplicated(ana_Poor_AQI$City_Code),]

xy <- ana_Poor_AQI %>% dplyr::select(Longitude, Latitude)
ana_Poor_AQI_point <- SpatialPointsDataFrame(coords = xy, data = ana_Poor_AQI,
                                             proj4string = CRS(proj))
rm(xy)
GWRbandwidth <- bw.gwr(CFR_0701  ~ 
                         Poor_AQI + 
                         temperature + pressure + humidity + 
                         internal_movement_restrictions_0701 + m_perc +
                         lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                       data = ana_Poor_AQI_point)
gwr.model.city.Poor_AQI = gwr.basic(CFR_0701  ~ 
                                      Poor_AQI + 
                                      temperature + pressure + humidity + 
                                      internal_movement_restrictions_0701 + m_perc +
                                      lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C, 
                                    data = ana_Poor_AQI_point,
                                    bw  = GWRbandwidth) 
gwr.model.city.Poor_AQI$SDF@data$Poor_AQI_sig <- ifelse(abs(gwr.model.city.Poor_AQI$SDF@data$Poor_AQI_TV) > 1.96,
                                                        1, 0)
gwr.model.city.Poor_AQI$SDF@data$Poor_AQI_sig_coef <- gwr.model.city.Poor_AQI$SDF@data$Poor_AQI *
  gwr.model.city.Poor_AQI$SDF@data$Poor_AQI_sig
gwr.model.city.Poor_AQI$SDF@data$CFR_0701R <- gwr.model.city.Poor_AQI$SDF@data$Poor_AQI_sig_coef / ana_Poor_AQI_point@data$CFR_0701

tm_shape(gwr.model.city.Poor_AQI$SDF) + 
  tm_dots(col = "Poor_AQI_sig_coef")

## to answer reviewer's comments, we add this section to test the country-level regression
## country-level no2 test
no2.analysis.data <- analysis.data %>%
  dplyr::select(CFR_0701, no2, temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, Country.y) %>%
  na.omit()
no2.analysis.data <- no2.analysis.data %>%
  aggregate(by = list(no2.analysis.data$Country.y), FUN = "mean")

country.level.1 <- data.frame(Doubles=double(),
                              Ints=integer(),
                              Factors=factor(),
                              Logicals=logical(),
                              Characters=character(),
                              stringsAsFactors=FALSE)
for (country in no2.analysis.data$Group.1) {
  covid <- covid19(country = country, level = 1)
  covid_CFR <- covid %>%
    dplyr::select(id, date, confirmed, recovered, deaths, population)
  covid_CFR <- covid_CFR %>%
    filter(date == ymd("2021-06-30"))
  covid_CFR <- covid_CFR %>%
    mutate(CFR = deaths / confirmed * 100)
  covid_CFR$Group.1 <- country
  country.level.1 <- rbind(country.level.1, covid_CFR)
}

no2.analysis.data <- left_join(no2.analysis.data, country.level.1)
test.no2 <-
  lm(CFR_0701 ~ no2 +  temperature + pressure + humidity + 
       internal_movement_restrictions_0701 + m_perc +
       lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C,
     data = no2.analysis.data)

## country-level so2 test
so2.analysis.data <- analysis.data %>%
  dplyr::select(CFR_0701, so2, temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, Country.y) %>%
  na.omit()
so2.analysis.data <- so2.analysis.data %>%
  aggregate(by = list(so2.analysis.data$Country.y), FUN = "mean")

country.level.1 <- data.frame(Doubles=double(),
                              Ints=integer(),
                              Factors=factor(),
                              Logicals=logical(),
                              Characters=character(),
                              stringsAsFactors=FALSE)
for (country in so2.analysis.data$Group.1) {
  covid <- covid19(country = country, level = 1)
  covid_CFR <- covid %>%
    dplyr::select(id, date, confirmed, recovered, deaths, population)
  covid_CFR <- covid_CFR %>%
    filter(date == ymd("2021-06-30"))
  covid_CFR <- covid_CFR %>%
    mutate(CFR = deaths / confirmed * 100)
  covid_CFR$Group.1 <- country
  country.level.1 <- rbind(country.level.1, covid_CFR)
}

so2.analysis.data <- left_join(so2.analysis.data, country.level.1)
test.so2 <-
  lm(CFR_0701 ~ so2 +  temperature + pressure + humidity + 
       internal_movement_restrictions_0701 + m_perc +
       lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C,
     data = so2.analysis.data)
summary(test.so2)

## country-level o3 test
o3.analysis.data <- analysis.data %>%
  dplyr::select(CFR_0701, o3, temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, Country.y) %>%
  na.omit()
o3.analysis.data <- o3.analysis.data %>%
  aggregate(by = list(o3.analysis.data$Country.y), FUN = "mean")

country.level.1 <- data.frame(Doubles=double(),
                              Ints=integer(),
                              Factors=factor(),
                              Logicals=logical(),
                              Characters=character(),
                              stringsAsFactors=FALSE)
for (country in o3.analysis.data$Group.1) {
  covid <- covid19(country = country, level = 1)
  covid_CFR <- covid %>%
    dplyr::select(id, date, confirmed, recovered, deaths, population)
  covid_CFR <- covid_CFR %>%
    filter(date == ymd("2021-06-30"))
  covid_CFR <- covid_CFR %>%
    mutate(CFR = deaths / confirmed * 100)
  covid_CFR$Group.1 <- country
  country.level.1 <- rbind(country.level.1, covid_CFR)
}

o3.analysis.data <- left_join(o3.analysis.data, country.level.1)
test.o3 <-
  lm(CFR_0701 ~ o3 +  temperature + pressure + humidity + 
       internal_movement_restrictions_0701 + m_perc +
       lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C,
     data = o3.analysis.data)
summary(test.o3)

## country-level pm25 test
pm25.analysis.data <- analysis.data %>%
  dplyr::select(CFR_0701, pm25, temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, Country.y) %>%
  na.omit()
pm25.analysis.data <- pm25.analysis.data %>%
  aggregate(by = list(pm25.analysis.data$Country.y), FUN = "mean")

country.level.1 <- data.frame(Doubles=double(),
                              Ints=integer(),
                              Factors=factor(),
                              Logicals=logical(),
                              Characters=character(),
                              stringsAsFactors=FALSE)
for (country in pm25.analysis.data$Group.1) {
  covid <- covid19(country = country, level = 1)
  covid_CFR <- covid %>%
    dplyr::select(id, date, confirmed, recovered, deaths, population)
  covid_CFR <- covid_CFR %>%
    filter(date == ymd("2021-06-30"))
  covid_CFR <- covid_CFR %>%
    mutate(CFR = deaths / confirmed * 100)
  covid_CFR$Group.1 <- country
  country.level.1 <- rbind(country.level.1, covid_CFR)
}

pm25.analysis.data <- left_join(pm25.analysis.data, country.level.1)
test.pm25 <-
  lm(CFR_0701 ~ pm25 +  temperature + pressure + humidity + 
       internal_movement_restrictions_0701 + m_perc +
       lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C,
     data = pm25.analysis.data)
summary(test.pm25)

## country-level pm10 test
pm10.analysis.data <- analysis.data %>%
  dplyr::select(CFR_0701, pm10, temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, Country.y) %>%
  na.omit()
pm10.analysis.data <- pm10.analysis.data %>%
  aggregate(by = list(pm10.analysis.data$Country.y), FUN = "mean")

country.level.1 <- data.frame(Doubles=double(),
                              Ints=integer(),
                              Factors=factor(),
                              Logicals=logical(),
                              Characters=character(),
                              stringsAsFactors=FALSE)
for (country in pm10.analysis.data$Group.1) {
  covid <- covid19(country = country, level = 1)
  covid_CFR <- covid %>%
    dplyr::select(id, date, confirmed, recovered, deaths, population)
  covid_CFR <- covid_CFR %>%
    filter(date == ymd("2021-06-30"))
  covid_CFR <- covid_CFR %>%
    mutate(CFR = deaths / confirmed * 100)
  covid_CFR$Group.1 <- country
  country.level.1 <- rbind(country.level.1, covid_CFR)
}

pm10.analysis.data <- left_join(pm10.analysis.data, country.level.1)
test.pm10 <-
  lm(CFR_0701 ~ pm10 +  temperature + pressure + humidity + 
       internal_movement_restrictions_0701 + m_perc +
       lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C,
     data = pm10.analysis.data)
summary(test.pm10)

## country-level AQI_Value test
AQI_Value.analysis.data <- analysis.data %>%
  dplyr::select(CFR_0701, AQI_Value, temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, Country.y) %>%
  na.omit()
AQI_Value.analysis.data <- AQI_Value.analysis.data %>%
  aggregate(by = list(AQI_Value.analysis.data$Country.y), FUN = "mean")

country.level.1 <- data.frame(Doubles=double(),
                              Ints=integer(),
                              Factors=factor(),
                              Logicals=logical(),
                              Characters=character(),
                              stringsAsFactors=FALSE)
for (country in AQI_Value.analysis.data$Group.1) {
  covid <- covid19(country = country, level = 1)
  covid_CFR <- covid %>%
    dplyr::select(id, date, confirmed, recovered, deaths, population)
  covid_CFR <- covid_CFR %>%
    filter(date == ymd("2021-06-30"))
  covid_CFR <- covid_CFR %>%
    mutate(CFR = deaths / confirmed * 100)
  covid_CFR$Group.1 <- country
  country.level.1 <- rbind(country.level.1, covid_CFR)
}

AQI_Value.analysis.data <- left_join(AQI_Value.analysis.data, country.level.1)
test.AQI_Value <-
  lm(CFR_0701 ~ AQI_Value +  temperature + pressure + humidity + 
       internal_movement_restrictions_0701 + m_perc +
       lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C,
     data = AQI_Value.analysis.data)
summary(test.AQI_Value)

## country-level Poor_AQI test
Poor_AQI.analysis.data <- analysis.data %>%
  dplyr::select(CFR_0701, Poor_AQI, temperature, pressure, humidity, 
                internal_movement_restrictions_0701, m_perc,
                lg_pop_den, perc_45, perc_65, perc_80, GDP_per_C, Country.y) %>%
  na.omit()
Poor_AQI.analysis.data <- Poor_AQI.analysis.data %>%
  aggregate(by = list(Poor_AQI.analysis.data$Country.y), FUN = "mean")

country.level.1 <- data.frame(Doubles=double(),
                              Ints=integer(),
                              Factors=factor(),
                              Logicals=logical(),
                              Characters=character(),
                              stringsAsFactors=FALSE)
for (country in Poor_AQI.analysis.data$Group.1) {
  covid <- covid19(country = country, level = 1)
  covid_CFR <- covid %>%
    dplyr::select(id, date, confirmed, recovered, deaths, population)
  covid_CFR <- covid_CFR %>%
    filter(date == ymd("2021-06-30"))
  covid_CFR <- covid_CFR %>%
    mutate(CFR = deaths / confirmed * 100)
  covid_CFR$Group.1 <- country
  country.level.1 <- rbind(country.level.1, covid_CFR)
}

Poor_AQI.analysis.data <- left_join(Poor_AQI.analysis.data, country.level.1)
test.Poor_AQI <-
  lm(CFR_0701 ~ Poor_AQI +  temperature + pressure + humidity + 
       internal_movement_restrictions_0701 + m_perc +
       lg_pop_den + perc_45 + perc_65 + perc_80 + GDP_per_C,
     data = Poor_AQI.analysis.data)
summary(test.Poor_AQI)

setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\06_Article\\")
stargazer(test.no2, test.so2, test.o3,
          test.pm25, test.pm10, test.AQI_Value,
          test.Poor_AQI,
          title = "Table XXX: Country-level Regression Results", type = "text", no.space = T,
          covariate.labels = c('Average Concentration of NO2',
                               'Average Concentration of SO2',
                               'Average Concentration of O3',
                               'Average Concentration of PM2.5',
                               'Average Concentration of PM10',
                               'Average AQI',
                               'Probability with Poor AQI',
                               'Average Temerature',
                               'Average Pressure',
                               'Average Humidity',
                               "Internal Movement Restrictions",
                               'Percentage of Male',
                               'Population Density',
                               'Percentage of Population 15 - 45',
                               'Percentage of Population 45 - 65',
                               'Percentage Of Population >= 65',
                               'GDP per Capita (PPP)'),
          dep.var.labels = "City-level COVID-19 CFR (%)",
          column.labels = c("NO2", "SO2", "O3", "PM2.5", "PM10", "AQI", "Pr. AQI"),
          out = '10_RegressionResult\\REV02\\CountryLevelRegression.added.html')

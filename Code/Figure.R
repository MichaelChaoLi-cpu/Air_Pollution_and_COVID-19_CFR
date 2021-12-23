library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(cowplot)
library(dplyr)

# Figure 1
boundary.1 = boundary %>% st_as_sf() %>% dplyr::select(geometry)
sf::sf_use_s2(FALSE)
CFR <- analysis.data %>%
  dplyr::select(CFR_0701, Longitude, Latitude) %>% na.omit()


p <- ggplot() +
  geom_sf(data = boundary.1, color = "grey65", fill = "grey97", alpha = 0.5, size = 0.1) +
  geom_point(data = CFR, aes(x= Longitude, y = Latitude, color = CFR_0701)) +
  scale_color_gradient(low = "yellow", high = "red2",
                       breaks = seq(0,10,2) ) +
  coord_sf(expand = F, crs = 4326) +
  theme(axis.text.x = element_text(  colour = "black", size=12),
        axis.text.y = element_text( colour = "black", size=12),
        plot.title = element_text(size=16, face="bold", hjust=0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.8, linetype = 'solid',
                                        colour = "gray88"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = c(0.07, 0.1), legend.direction="horizontal"
        )  + 
  labs(color = element_blank()) + 
  annotate(geom="text", x = -175, y = -52, label="Legend:", hjust = 0, size = 6) +
  annotate(geom="text", x = -175, y = -59, label="The City-level CFR of COVID-19(%):", hjust = 0, size = 4)

ggsave("Figure_1.jpg", p, device = "jpeg", 
       path = "C:/Users/li.chao.987@s.kyushu-u.ac.jp/OneDrive - Kyushu University/06_Article/04_Figure/REV03",
       width = 297, height = 210, units = "mm", dpi = 500
)
# Figure 1 end

# Figure 2
# step 0 generate data
data.plot <- dplyr::full_join(no2, so2)
data.plot <- dplyr::full_join(data.plot, o3)
data.plot <- dplyr::full_join(data.plot, pm25)
data.plot <- dplyr::full_join(data.plot, pm10)
data.plot <- dplyr::full_join(data.plot, AQI)
data.plot <- dplyr::full_join(data.plot, Pr_AQI)
data.plot <- dplyr::full_join(data.plot, CFR)
data.plot <- data.plot %>% as.data.frame() %>%
  filter(!is.na(CFR_0701)) %>% 
  dplyr::select(-CFR_0701)

data.plot$ID <- 1:nrow(data.plot)

data.plot <- data.plot %>% 
  mutate(
    no2 = ifelse(is.na(no2), 0, no2),
    so2 = ifelse(is.na(so2), 0, so2),
    o3 = ifelse(is.na(o3), 0, o3),
    pm25 = ifelse(is.na(pm25), 0, pm25),
    pm10 = ifelse(is.na(pm10), 0, pm10),
    AQI_Value = ifelse(is.na(AQI_Value), 0, AQI_Value),
    Poor_AQI = ifelse(is.na(Poor_AQI), 0, Poor_AQI)
    )
data.plot <- data.plot %>% 
  mutate(
    no2.n = no2 / 5,
    no2.n = ifelse(no2.n > 5, 5, no2.n),
    so2.n = so2 / 4,
    so2.n = ifelse(so2.n > 5, 5, so2.n),
    o3.n = o3 / 8,
    o3.n = ifelse(o3.n > 5, 5, o3.n),
    pm25.n = pm25 / 20,
    pm25.n = ifelse(pm25.n > 5, 5, pm25.n),
    pm10.n = pm10 / 12,
    pm10.n = ifelse(pm10.n > 5, 5, pm10.n),
    AQI_Value.n = AQI_Value / 16,
    AQI_Value.n = ifelse(AQI_Value.n > 5, 5, AQI_Value.n),
    Poor_AQI.n = Poor_AQI / 0.12,
    Poor_AQI.n = ifelse(Poor_AQI.n > 5, 5, Poor_AQI.n)
    )
data.plot <- data.plot %>% 
  dplyr::select(-no2, -so2, -o3, -pm25, -pm10, -AQI_Value, -Poor_AQI)

data.plot <- data.plot %>%
  pivot_longer(cols = c(no2.n, so2.n, o3.n, pm25.n, pm10.n, AQI_Value.n, Poor_AQI.n), 
               names_to = "type", values_to = "value")
data.plot$type <- data.plot$type %>% as.factor()
data.plot$max.Radius <- 5

## test
#data.plot.t <- data.plot %>% filter(ID < 5)
#ggplot(data.plot.t,
#       aes(x = type, y = value, fill = type)) +
#  geom_col(position = "identity", width = 1) +
#  scale_fill_manual(values = c("red", "blue", "green", "yellow", "gray70", "orange", "magenta")) +
#  coord_polar() +
#  facet_grid(~ID) +
#  theme_void()

# step 0 end

# step 1 
df.grobs <- data.plot %>%
  group_by(ID, Longitude, Latitude, max.Radius) %>%
  do(subplots = ggplot(.,
                       aes(x = type, y = value, fill = type)) +
       geom_col(position = "identity", width = 1,
                alpha = 0.5,            # increase transparency to see map underneath
                show.legend = FALSE) +  # don't show legend for individual grobs
       scale_y_continuous(limits = c(0, unique(.$max.Radius))) + 
       scale_fill_manual(values = c("red", "blue", "green", "yellow", "gray34", "orange", "deepskyblue")) +
       coord_polar() +
       theme_void()) %>%
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = Longitude - 3,      # change from 1 to other 
                                           y = Latitude - 3,      # values if necessary,
                                           xmax = Longitude + 3,   # depending on the map's
                                           ymax = Latitude + 3))) # resolution.
# step 1 end

# step 2
boundary.1 = boundary %>% st_as_sf() %>% dplyr::select(geometry)
sf::sf_use_s2(FALSE)

p <- ggplot() +
  geom_sf(data = boundary.1, color = "grey65", fill = "grey97", alpha = 0.5, size = 0.1) +
  coord_sf(expand = F, crs = 4326) +
  theme_bw() 

p <- p + annotate(geom="text", x = -175, y = 15, label="Legend:",
                  hjust = 0) +
  annotate(geom="text", x = -175, y = 11.5, label="(Note: Data have been normalized)",
           hjust = 0, size = 2) +
  annotate(
    geom="text", x = -175, y = -65, 
    label = paste0(
      "Normalization Process:\nNorm(AQI) = AQI / 16, max[Norm(AQI)] = 5\n",
      "Norm(NO2) = NO2 / 5, max[Norm(NO2)] = 5\n",
      "Norm(O3) = O3 / 8, max[Norm(O3)] = 5\n",
      "Norm(PM10) = PM10 / 12, max[Norm(PM10)] = 5\n",
      "Norm(PM2.5) = PM2.5 / 20, max[Norm(PM2.5)] = 5\n",
      "Norm(AIQ(F)) = Probability with Poor AIQ / 0.12, max[Norm(AIQ(F))] = 5\n",
      "Norm(SO2) = SO2 / 4, max[Norm(SO2)] = 5\n"
      ),
    hjust = 0,
    size = 2
           )

# step 2 end

# step 3
p <- p + df.grobs$subgrobs

data.plot.l <- data.plot %>% filter(ID < 2)
data.plot.l$value <- c(1,2,3,4,5,3,4)
levels(data.plot.l$type) <- c("AQI", "NO2", "O3", "PM10", "PM2.5", "AQI(F)", "SO2")
legend.p <- ggplot(data.plot.l,
                   aes(x = type, y = value, fill = type)) +
  geom_col(position = "identity", width = 1, alpha = 0.5) +
  guides(fill=FALSE) +
  scale_fill_manual(values = c("red", "blue", "green", "yellow", "gray34", "orange", "deepskyblue")) +
  coord_polar() +
  theme_bw(base_size = 5)
legend.p

plot.with.insert <-
  ggdraw() +
  draw_plot(p) +
  draw_plot(legend.p, x = 0.03, y = .37, width = .2, height = .2)

#plot.with.insert

ggsave("Figure_2.jpg", plot.with.insert, device = "jpeg", 
       path = "C:/Users/li.chao.987@s.kyushu-u.ac.jp/OneDrive - Kyushu University/06_Article/04_Figure/REV03",
       width = 297, height = 210, units = "mm", dpi = 500
       )
# step 3 end

# Figure 3
gwr.no2.ggplot <- gwr.model.city.no2$SDF
gwr.no2.ggplot@data <- gwr.no2.ggplot@data %>%
  dplyr::select(no2_sig_coef)
gwr.no2.ggplot <- st_as_sf(gwr.no2.ggplot)
gwr.no2.ggplot <- cbind(gwr.no2.ggplot, as.data.frame(st_coordinates(gwr.no2.ggplot)) )
st_geometry(gwr.no2.ggplot) <- NULL

gwr.o3.ggplot <- gwr.model.city.o3$SDF
gwr.o3.ggplot@data <- gwr.o3.ggplot@data %>%
  dplyr::select(o3_sig_coef)
gwr.o3.ggplot <- st_as_sf(gwr.o3.ggplot)
gwr.o3.ggplot <- cbind(gwr.o3.ggplot, as.data.frame(st_coordinates(gwr.o3.ggplot)) )
st_geometry(gwr.o3.ggplot) <- NULL

gwr.pm25.ggplot <- gwr.model.city.pm25$SDF
gwr.pm25.ggplot@data <- gwr.pm25.ggplot@data %>%
  dplyr::select(pm25_sig_coef)
gwr.pm25.ggplot <- st_as_sf(gwr.pm25.ggplot)
gwr.pm25.ggplot <- cbind(gwr.pm25.ggplot, as.data.frame(st_coordinates(gwr.pm25.ggplot)) )
st_geometry(gwr.pm25.ggplot) <- NULL

gwr.AQI_Value.ggplot <- gwr.model.city.AQI_Value$SDF
gwr.AQI_Value.ggplot@data <- gwr.AQI_Value.ggplot@data %>%
  dplyr::select(AQI_Value_sig_coef)
gwr.AQI_Value.ggplot <- st_as_sf(gwr.AQI_Value.ggplot)
gwr.AQI_Value.ggplot <- cbind(gwr.AQI_Value.ggplot, as.data.frame(st_coordinates(gwr.AQI_Value.ggplot)) )
st_geometry(gwr.AQI_Value.ggplot) <- NULL

gwr.Poor_AQI.ggplot <- gwr.model.city.Poor_AQI$SDF
gwr.Poor_AQI.ggplot@data <- gwr.Poor_AQI.ggplot@data %>%
  dplyr::select(Poor_AQI_sig_coef)
gwr.Poor_AQI.ggplot <- st_as_sf(gwr.Poor_AQI.ggplot)
gwr.Poor_AQI.ggplot <- cbind(gwr.Poor_AQI.ggplot, as.data.frame(st_coordinates(gwr.Poor_AQI.ggplot)) )
st_geometry(gwr.Poor_AQI.ggplot) <- NULL

data.plot.f3 <- dplyr::full_join(gwr.no2.ggplot, gwr.o3.ggplot)
data.plot.f3 <- dplyr::full_join(data.plot.f3, gwr.pm25.ggplot)
data.plot.f3 <- dplyr::full_join(data.plot.f3, gwr.AQI_Value.ggplot)
data.plot.f3 <- dplyr::full_join(data.plot.f3, gwr.Poor_AQI.ggplot)
rm(gwr.no2.ggplot, gwr.o3.ggplot, gwr.pm25.ggplot, gwr.AQI_Value.ggplot, gwr.Poor_AQI.ggplot)

data.plot.f3 <- data.plot.f3[!(rowSums(is.na(data.plot.f3)) == 5), ]

data.plot.f3$ID <- 1:nrow(data.plot.f3)

data.plot.f3 <- data.plot.f3 %>% 
  mutate(
    no2_sig_coef = ifelse(is.na(no2_sig_coef), 0, no2_sig_coef),
    o3_sig_coef = ifelse(is.na(o3_sig_coef), 0, o3_sig_coef),
    pm25_sig_coef = ifelse(is.na(pm25_sig_coef), 0, pm25_sig_coef),
    AQI_Value_sig_coef = ifelse(is.na(AQI_Value_sig_coef), 0, AQI_Value_sig_coef),
    Poor_AQI_sig_coef = ifelse(is.na(Poor_AQI_sig_coef), 0, Poor_AQI_sig_coef),
    pm25_sig_coef_nega = ifelse(pm25_sig_coef < 0, -pm25_sig_coef, 0),
    pm25_sig_coef = ifelse(pm25_sig_coef < 0, 0, pm25_sig_coef),
    AQI_Value_sig_coef_nega = ifelse(AQI_Value_sig_coef < 0, -AQI_Value_sig_coef, 0),
    AQI_Value_sig_coef = ifelse(AQI_Value_sig_coef < 0, 0, AQI_Value_sig_coef)
  )
data.plot.f3 <- data.plot.f3 %>% 
  mutate(
    no2.n = ifelse(no2_sig_coef > 0, (no2_sig_coef - 0.04) / 0.012, 0),
    o3.n = ifelse(o3_sig_coef > 0, (o3_sig_coef - 0.03) / 0.01, 0),
    pm25.n = ifelse(pm25_sig_coef > 0, (pm25_sig_coef - 0.01) / 0.006, 0),
    AQI_Value.n = ifelse(AQI_Value_sig_coef > 0, (AQI_Value_sig_coef - 0.03) / 0.006, 0),
    Poor_AQI.n = ifelse(Poor_AQI_sig_coef > 0, (Poor_AQI_sig_coef - 1.5) / 1, 0),
    pm25.nega.n = ifelse(pm25_sig_coef_nega > 0, (pm25_sig_coef_nega - 0.01) / 0.006, 0),
    AQI_Value.nega.n = ifelse(AQI_Value_sig_coef_nega > 0, (AQI_Value_sig_coef_nega - 0.03) / 0.006, 0)
  )
data.plot.f3 <- data.plot.f3 %>% 
  dplyr::select(-no2_sig_coef, -o3_sig_coef, -pm25_sig_coef, -AQI_Value_sig_coef, -Poor_AQI_sig_coef,
                -pm25_sig_coef_nega, -AQI_Value_sig_coef_nega)
data.plot.f3 <- data.plot.f3 %>% 
  pivot_longer(cols = c(no2.n, o3.n, pm25.n, AQI_Value.n, Poor_AQI.n, pm25.nega.n, AQI_Value.nega.n), 
               names_to = "type", values_to = "value")
data.plot.f3$type <- data.plot.f3$type %>% as.factor()
data.plot.f3$max.Radius <- 5
colnames(data.plot.f3)[1] <- "Longitude"
colnames(data.plot.f3)[2] <- "Latitude"
# step 0 end

# step 1 
df.grobs <- data.plot.f3 %>%
  group_by(ID, Longitude, Latitude, max.Radius) %>%
  do(subplots = ggplot(.,
                       aes(x = type, y = value, fill = type)) +
       geom_col(position = "identity", width = 1,
                alpha = 0.5,            # increase transparency to see map underneath
                show.legend = FALSE) +  # don't show legend for individual grobs
       scale_y_continuous(limits = c(0, unique(.$max.Radius))) + 
       scale_fill_manual(values = c("red", "yellow", "blue", "green", "gray34", "deepskyblue", "orange")) +
       coord_polar() +
       theme_void()) %>%
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = Longitude - 3,      # change from 1 to other 
                                           y = Latitude - 3,      # values if necessary,
                                           xmax = Longitude + 3,   # depending on the map's
                                           ymax = Latitude + 3))) # resolution.
# step 1 end

# step 2
boundary.1 = boundary %>% st_as_sf() %>% dplyr::select(geometry)
sf::sf_use_s2(FALSE)

p <- ggplot() +
  geom_sf(data = boundary.1, color = "grey65", fill = "grey97", alpha = 0.5, size = 0.1) +
  coord_sf(expand = F, crs = 4326) +
  theme_bw() 

p <- p + annotate(geom="text", x = -175, y = 15, label="Legend:",
                  hjust = 0) +
  annotate(geom="text", x = -175, y = 11.5, label="(Note: Coefficients have been normalized)",
           hjust = 0, size = 2) +
  annotate(
    geom="text", x = -175, y = -57, 
    label = paste0(
      "Normalization Process of the Coefficients:\nNorm(AQI) = (|AQI| - 0.03) / 0.006\n",
      "Norm(NO2) = (NO2 - 0.04) / 0.012\n",
      "Norm(O3) = (O3 - 0.03) / 0.01\n",
      "Norm(PM2.5) = (|PM2.5| - 0.01) / 0.006\n",
      "Norm(AIQ(F)) = Probability with Poor AIQ - 1.5 / 1\n",
      "Not significant values have been ignored."
    ),
    hjust = 0,
    size = 2
  )

# step 2 end

# step 3
p <- p + df.grobs$subgrobs

data.plot.l <- data.plot.f3 %>% filter(ID < 2)
data.plot.l$value <- c(1,2,3,4,5,3,4)
levels(data.plot.l$type) <- c("AQI", "AQI(-)", "NO2", "O3", "PM2.5", "PM2.5(-)", "AQI(F)")
legend.p <- ggplot(data.plot.l,
                   aes(x = type, y = value, fill = type)) +
  geom_col(position = "identity", width = 1, alpha = 0.5) +
  guides(fill=FALSE) +
  scale_fill_manual(values = c("red", "yellow", "blue", "green", "gray34", "deepskyblue", "orange")) +
  coord_polar() +
  theme_bw(base_size = 5)
legend.p

plot.with.insert <-
  ggdraw() +
  draw_plot(p) +
  draw_plot(legend.p, x = 0.03, y = .37, width = .2, height = .2)

#plot.with.insert

ggsave("Figure_3.jpg", plot.with.insert, device = "jpeg", 
       path = "C:/Users/li.chao.987@s.kyushu-u.ac.jp/OneDrive - Kyushu University/06_Article/04_Figure/REV03",
       width = 297, height = 210, units = "mm", dpi = 500
)
# step 3 end

# Figure 4
gwr.no2.ggplot <- gwr.model.city.no2$SDF
gwr.no2.ggplot@data <- gwr.no2.ggplot@data %>%
  dplyr::select(CFR_0701R) %>% 
  rename(no2_sig_coef = CFR_0701R)
gwr.no2.ggplot <- st_as_sf(gwr.no2.ggplot)
gwr.no2.ggplot <- cbind(gwr.no2.ggplot, as.data.frame(st_coordinates(gwr.no2.ggplot)) )
st_geometry(gwr.no2.ggplot) <- NULL

gwr.o3.ggplot <- gwr.model.city.o3$SDF
gwr.o3.ggplot@data <- gwr.o3.ggplot@data %>%
  dplyr::select(CFR_0701R) %>% 
  rename(o3_sig_coef = CFR_0701R)
gwr.o3.ggplot <- st_as_sf(gwr.o3.ggplot)
gwr.o3.ggplot <- cbind(gwr.o3.ggplot, as.data.frame(st_coordinates(gwr.o3.ggplot)) )
st_geometry(gwr.o3.ggplot) <- NULL

gwr.pm25.ggplot <- gwr.model.city.pm25$SDF
gwr.pm25.ggplot@data <- gwr.pm25.ggplot@data %>%
  dplyr::select(CFR_0701R) %>% 
  rename(pm25_sig_coef = CFR_0701R)
gwr.pm25.ggplot <- st_as_sf(gwr.pm25.ggplot)
gwr.pm25.ggplot <- cbind(gwr.pm25.ggplot, as.data.frame(st_coordinates(gwr.pm25.ggplot)) )
st_geometry(gwr.pm25.ggplot) <- NULL

gwr.AQI_Value.ggplot <- gwr.model.city.AQI_Value$SDF
gwr.AQI_Value.ggplot@data <- gwr.AQI_Value.ggplot@data %>%
  dplyr::select(CFR_0701R) %>% 
  rename(AQI_Value_sig_coef = CFR_0701R)
gwr.AQI_Value.ggplot <- st_as_sf(gwr.AQI_Value.ggplot)
gwr.AQI_Value.ggplot <- cbind(gwr.AQI_Value.ggplot, as.data.frame(st_coordinates(gwr.AQI_Value.ggplot)) )
st_geometry(gwr.AQI_Value.ggplot) <- NULL

gwr.Poor_AQI.ggplot <- gwr.model.city.Poor_AQI$SDF
gwr.Poor_AQI.ggplot@data <- gwr.Poor_AQI.ggplot@data %>%
  dplyr::select(CFR_0701R) %>% 
  rename(Poor_AQI_sig_coef = CFR_0701R)
gwr.Poor_AQI.ggplot <- st_as_sf(gwr.Poor_AQI.ggplot)
gwr.Poor_AQI.ggplot <- cbind(gwr.Poor_AQI.ggplot, as.data.frame(st_coordinates(gwr.Poor_AQI.ggplot)) )
st_geometry(gwr.Poor_AQI.ggplot) <- NULL

data.plot.f4 <- dplyr::full_join(gwr.no2.ggplot, gwr.o3.ggplot)
data.plot.f4 <- dplyr::full_join(data.plot.f4, gwr.pm25.ggplot)
data.plot.f4 <- dplyr::full_join(data.plot.f4, gwr.AQI_Value.ggplot)
data.plot.f4 <- dplyr::full_join(data.plot.f4, gwr.Poor_AQI.ggplot)
rm(gwr.no2.ggplot, gwr.o3.ggplot, gwr.pm25.ggplot, gwr.AQI_Value.ggplot, gwr.Poor_AQI.ggplot)

data.plot.f4 <- data.plot.f4[!(rowSums(is.na(data.plot.f4)) == 5), ]

data.plot.f4$ID <- 1:nrow(data.plot.f4)

data.plot.f4 <- data.plot.f4 %>% 
  mutate(
    no2_sig_coef = ifelse(is.na(no2_sig_coef), 0, no2_sig_coef),
    o3_sig_coef = ifelse(is.na(o3_sig_coef), 0, o3_sig_coef),
    pm25_sig_coef = ifelse(is.na(pm25_sig_coef), 0, pm25_sig_coef),
    AQI_Value_sig_coef = ifelse(is.na(AQI_Value_sig_coef), 0, AQI_Value_sig_coef),
    Poor_AQI_sig_coef = ifelse(is.na(Poor_AQI_sig_coef), 0, Poor_AQI_sig_coef),
    pm25_sig_coef_nega = ifelse(pm25_sig_coef < 0, -pm25_sig_coef, 0),
    pm25_sig_coef = ifelse(pm25_sig_coef < 0, 0, pm25_sig_coef),
    AQI_Value_sig_coef_nega = ifelse(AQI_Value_sig_coef < 0, -AQI_Value_sig_coef, 0),
    AQI_Value_sig_coef = ifelse(AQI_Value_sig_coef < 0, 0, AQI_Value_sig_coef)
  )
data.plot.f4 <- data.plot.f4 %>% 
  mutate(
    no2.n = no2_sig_coef / 7,
    o3.n = o3_sig_coef / 10,
    pm25.n = pm25_sig_coef / 3.2,
    pm25.nega.n = pm25_sig_coef_nega / 3.2,
    AQI_Value.n = AQI_Value_sig_coef / 1.3,
    AQI_Value.nega.n = AQI_Value_sig_coef_nega / 1.3,
    Poor_AQI.n = Poor_AQI_sig_coef / 2.2
    )
)
data.plot.f4 <- data.plot.f4 %>% 
  dplyr::select(-no2_sig_coef, -o3_sig_coef, -pm25_sig_coef, -AQI_Value_sig_coef, -Poor_AQI_sig_coef,
                -pm25_sig_coef_nega, -AQI_Value_sig_coef_nega)
data.plot.f4 <- data.plot.f4 %>% 
  pivot_longer(cols = c(no2.n, o3.n, pm25.n, AQI_Value.n, Poor_AQI.n, pm25.nega.n, AQI_Value.nega.n), 
               names_to = "type", values_to = "value")
data.plot.f4$type <- data.plot.f4$type %>% as.factor()
data.plot.f4$max.Radius <- 5
colnames(data.plot.f4)[1] <- "Longitude"
colnames(data.plot.f4)[2] <- "Latitude"
# step 0 end

# step 1 
df.grobs <- data.plot.f4 %>%
  group_by(ID, Longitude, Latitude, max.Radius) %>%
  do(subplots = ggplot(.,
                       aes(x = type, y = value, fill = type)) +
       geom_col(position = "identity", width = 1,
                alpha = 0.5,            # increase transparency to see map underneath
                show.legend = FALSE) +  # don't show legend for individual grobs
       scale_y_continuous(limits = c(0, unique(.$max.Radius))) + 
       scale_fill_manual(values = c("red", "yellow", "blue", "green", "gray34", "deepskyblue", "orange")) +
       coord_polar() +
       theme_void()) %>%
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = Longitude - 3,      # change from 1 to other 
                                           y = Latitude - 3,      # values if necessary,
                                           xmax = Longitude + 3,   # depending on the map's
                                           ymax = Latitude + 3))) # resolution.
# step 1 end

# step 2
boundary.1 = boundary %>% st_as_sf() %>% dplyr::select(geometry)
sf::sf_use_s2(FALSE)

p <- ggplot() +
  geom_sf(data = boundary.1, color = "grey65", fill = "grey97", alpha = 0.5, size = 0.1) +
  coord_sf(expand = F, crs = 4326) +
  theme_bw() 

p <- p + annotate(geom="text", x = -175, y = 15, label="Legend:",
                  hjust = 0) +
  annotate(geom="text", x = -175, y = 11.5, label="(Note: CFRRs have been normalized)",
           hjust = 0, size = 2) +
  annotate(
    geom="text", x = -175, y = -57, 
    label = paste0(
      "Normalization Process of the CFRRs:\nNorm(AQI) = |CFRR of AQI| / 1.3\n",
      "Norm(NO2) = CFRR of NO2 / 7\n",
      "Norm(O3) = CFRR of O3 / 10\n",
      "Norm(PM2.5) = |CFRR of PM2.5| / 3.2\n",
      "Norm(AIQ(F)) = CFRR of Probability with Poor AIQ / 2.2\n",
      "Not significant values have been ignored."
    ),
    hjust = 0,
    size = 2
  )

# step 2 end

# step 3
p <- p + df.grobs$subgrobs

data.plot.l <- data.plot.f4 %>% filter(ID < 2)
data.plot.l$value <- c(1,2,3,4,5,3,4)
levels(data.plot.l$type) <- c("AQI", "AQI(-)", "NO2", "O3", "PM2.5", "PM2.5(-)", "AQI(F)")
legend.p <- ggplot(data.plot.l,
                   aes(x = type, y = value, fill = type)) +
  geom_col(position = "identity", width = 1, alpha = 0.5) +
  guides(fill=FALSE) +
  scale_fill_manual(values = c("red", "yellow", "blue", "green", "gray34", "deepskyblue", "orange")) +
  coord_polar() +
  theme_bw(base_size = 5)
legend.p

plot.with.insert <-
  ggdraw() +
  draw_plot(p) +
  draw_plot(legend.p, x = 0.03, y = .37, width = .2, height = .2)

#plot.with.insert

ggsave("Figure_4.jpg", plot.with.insert, device = "jpeg", 
       path = "C:/Users/li.chao.987@s.kyushu-u.ac.jp/OneDrive - Kyushu University/06_Article/04_Figure/REV03",
       width = 297, height = 210, units = "mm", dpi = 500
)
# step 3 end

#### adding new figure 12.23
table3 <- read.csv("C:/Users/li.chao.987@s.kyushu-u.ac.jp/Documents/Air_Pollution_and_COVID-19_CFR/Data/Table3.csv")
table3 <- table3 %>%
  mutate(Significance = ifelse(Significance == "-", " ", Significance))
table3$addingLabel <- paste0(as.character(table3$CFRR...), " ", table3$Significance)
figure_5 <- ggplot(data = table3, aes(x = Variable, y = CFRR...)) +
  geom_bar(stat = "identity", position=position_dodge(), aes(fill = Variable)) +
  geom_errorbar(aes(ymin = CILower, ymax = CIUpper),
                position=position_dodge(1), width = .5,
                size = 1, color = "gray34") +
  geom_text(aes(label = addingLabel), color="black",
            position = position_dodge(1), size = 7,
            vjust = -0.3) +
  scale_fill_brewer(palette = "Paired", 
                    labels = c(
                      "AQI (1-score)", "NO2 (PPB)", "O3 (PPB)",
                      "PM10 (ug/m3)", "PM2.5 (ug/m3)", "SO2 (PPB)",
                      "Pronability of Living with Poor AQI (%)"
  )) +
  scale_x_discrete(name = 'Air Pollution Type', labels = NULL) +
  scale_y_continuous(name = 'CFRR (%)') +
  labs(caption = "Note: *** p < 0.01, ** p < 0.05, * p < 0.10") +
  theme(axis.text.x = element_text(color = 'black', size = 14, angle = 90, vjust = 0.5),
        axis.text.y = element_text(color = 'black', size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        title = element_text(color = 'black', size = 22),
        legend.direction = "horizontal",
        legend.position = "bottom") 

figure_5
ggsave("Figure_5.jpg", figure_5, device = "jpeg", 
       path = "C:/Users/li.chao.987@s.kyushu-u.ac.jp/OneDrive - Kyushu University/06_Article/04_Figure/REV03",
       width = 297, height = 210, units = "mm", dpi = 500
)

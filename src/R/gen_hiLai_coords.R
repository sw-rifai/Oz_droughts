library(arrow); library(tidyverse); library(lubridate)
d2 <- read_parquet("../data_general/AVHRR_LAI_FAPAR_CDR_V5/Oz_AVHRR_LAI_CDR_1981_2019.parquet")

d2 %>% 
  filter(date >= ymd("1982-01-01")) %>% 
  filter(date <= ymd("2011-12-31")) %>% 
  group_by(lon,lat) %>%
  summarize(lai_u = mean(lai, na.rm=T)) %>% 
  ungroup() %>% 
  rename(longitude=lon, 
         latitude=lat) %>% 
  write_csv(., "data/Oz_avhrr_all_coords.csv")


tmp <- d2 %>% 
  filter(date >= ymd("1982-01-01")) %>% 
  filter(date <= ymd("2011-12-31")) %>% 
  mutate(month=month(date)) %>% 
  group_by(lon,lat,month) %>%
  summarize(lai_u = mean(lai, na.rm=T)) %>% 
  ungroup()


tmp %>% 
  ungroup() %>% 
  group_by(lon,lat) %>% 
  summarize(lai_min =min(lai_u,na.rm=T), 
            lai_max = max(lai_u, na.rm=T)) %>% 
  ungroup() %>% 
  filter(lai_max >= 1) %>% 
  dim()

lai_n <- tmp %>% 
  group_by(lon,lat) %>% 
  summarize(lai_avg = mean(lai_u, na.rm=T), 
            lai_min = min(lai_u, na.rm=T), 
            lai_max = max(lai_u, na.rm=T), 
            lai_amp = diff(range(lai_u))) %>% 
  ungroup()


# Coord set coords_EA_lai_amp0p5_min0p5: East Australia w/LAI_amp gte 0.5 & LAI_min gte 0.5 ----------
lai_n %>% 
  filter(lon>135) %>%
  filter(lai_amp > 0.5 & lai_min>=0.5) %>% 
  mutate(id = row_number()) %>% 
  rename(longitude=lon,latitude=lat) %>% 
  write_csv(., path="data/coords_set_EA_lai_amp0p5_min0p5.csv")
  


export_coords <- tmp %>% 
  ungroup() %>% 
  group_by(lon,lat) %>% 
  summarize(lai_min =min(lai_u,na.rm=T), 
            lai_max = max(lai_u, na.rm=T)) %>% 
  ungroup() %>% 
  filter(lai_min >= 0.5)
export_coords %>% 
  write_csv(., path = "data/Oz_coords_min_mon_lai_gte0p5.csv")

# Plot Min Monthly LAI ----------------------------------------------------
library(sf)
aus <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                   layer="gadm36_AUS_1")
# aus <- st_transform_proj(aus, st_crs(4326))
aus <- st_as_sf(aus)

tmp %>% 
  # filter(lon>=138 & lat > -45) %>% 
  group_by(lon,lat) %>% 
  summarize(lai_min = min(lai_u, na.rm=T)) %>% 
  ungroup() %>% 
  filter(lai_min >= 1) %>% 
  # rename(X=lon,Y=lat) %>% 
  ggplot(data=., aes(lon,lat,fill=lai_min))+
  geom_sf(data=aus, fill=NA, col='grey', inherit.aes = F,lwd=0.5)+
  geom_tile()+
  coord_sf(xlim = c(110,155), ylim = c(-10,-45), expand = FALSE)+
  scale_fill_viridis_c(expression(paste(LAI[min])),
                       option="D", 
                       direction=-1, begin=0,end=0.8)+
  labs(x=expression(paste(Longitude)), 
       y=expression(paste(Latitude)))+
  theme_linedraw()
ggsave(filename = "figures/Oz_30yr_min_mon_LAI_gte1.png")


tmp %>% 
  # filter(lon>=138 & lat > -45) %>% 
  group_by(lon,lat) %>% 
  summarize(lai_min = min(lai_u, na.rm=T), 
            lai_max = max(lai_u, na.rm=T)) %>% 
  ungroup() %>% 
  filter(lai_max >= 1) %>% 
  # rename(X=lon,Y=lat) %>% 
  ggplot(data=., aes(lon,lat,fill=lai_max))+
  geom_sf(data=aus, fill=NA, col='grey', inherit.aes = F,lwd=0.5)+
  geom_tile()+
  coord_sf(xlim = c(110,155), ylim = c(-10,-45), expand = FALSE)+
  scale_fill_viridis_c(expression(paste(LAI[min])),
                       option="D", 
                       direction=-1, begin=0,end=0.8)+
  labs(x=expression(paste(Longitude)), 
       y=expression(paste(Latitude)))+
  theme_linedraw()
ggsave(filename = "figures/Oz_30yr_min_mon_LAI_gte1.png")


vec_names <- c("Australian Capital Territory",
               "New South Wales", "Northern Territory",          
                "Queensland", "South Australia",     
                "Tasmania","Victoria","Western Australia")
ggplot()+
  geom_sf(data=aus %>% filter(NAME_1 %in% vec_names))

names(aus)
st_proj_info(aus)

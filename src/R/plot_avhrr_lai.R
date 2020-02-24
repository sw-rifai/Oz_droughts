library(arrow); library(tidyverse); library(lubridate)
library(sf)
aus <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                   layer="gadm36_AUS_1")
# aus <- st_transform_proj(aus, st_crs(4326))
aus <- st_as_sf(aus)


# Import and summarize ----------------------------------------------------
d2 <- read_parquet("../data_general/AVHRR_LAI_FAPAR_CDR_V5/Oz_AVHRR_LAI_CDR_1981_2019.parquet")

tmp <- d2 %>% 
  filter(date >= ymd("1982-01-01")) %>% 
  filter(date <= ymd("2011-12-31")) %>% 
  mutate(month=month(date)) %>% 
  group_by(lon,lat,month) %>%
  summarize(lai_u = mean(lai, na.rm=T)) %>% 
  ungroup()

lai_n <- tmp %>% 
  group_by(lon,lat) %>% 
  summarize(lai_avg = mean(lai_u, na.rm=T), 
            lai_min = min(lai_u, na.rm=T), 
            lai_max = max(lai_u, na.rm=T), 
            lai_amp = diff(range(lai_u))) %>% 
  ungroup()

lai_n$lai_amp %>% hist



# Plot LAI amplitude ----------------------------------------------------
p <- lai_n %>% 
  ggplot(data=., aes(lon,lat,fill=lai_amp))+
  geom_tile()+
  geom_sf(data=aus, fill=NA, col='grey', inherit.aes = F,lwd=0.25)+
  coord_sf(xlim = c(110,155), ylim = c(-10,-45), expand = FALSE)+
  scale_fill_viridis_c(expression(paste(LAI[amp])),
                       option="C", 
                       direction=-1, begin=0,end=1)+
  labs(x=expression(paste(Longitude)), 
       y=expression(paste(Latitude)))+
  theme_linedraw()
p
ggsave(plot = p, filename = "figures/Oz_30yr_amplitude_LAI.png", 
       device = "png", 
       type='cairo-png')
       

lai_n %>% 
  filter(lon>135) %>%
  filter(lai_amp > 0.5 & lai_min>=0.5) %>% 
  ggplot(data=., aes(lon,lat,fill=lai_amp))+
  geom_tile()+
  geom_sf(data=aus, fill=NA, col='black', inherit.aes = F,lwd=0.25)+
  coord_sf(xlim = c(135,155), 
           ylim = c(-10,-45),
           expand = T, ndiscr = 10)+
  scale_x_continuous(breaks=c(140,150))+
  scale_fill_viridis_c(expression(paste(LAI[amp])),
                       option="C", 
                       direction=-1, begin=0,end=1)+
  labs(x=expression(paste(Longitude)), 
       y=expression(paste(Latitude)))+
  theme_linedraw()
ggsave(filename = "figures/Oz_eastAus_AmpGte0p5_LaiMinGte0p5.png", 
       device = "png", 
       type='cairo-png')




s1 <- tmp %>% 
  rename(lai=lai_u) %>% 
  mutate(d_lat = cut_interval(lat, n=5), 
         d_lon = cut_interval(lon, n=5)) %>% 
  group_by(d_lat, d_lon, month) %>% 
  summarize(lai_u = mean(lai,na.rm=T), 
            lai_p05 = quantile(lai, 0.05), 
            lai_p95 = quantile(lai, 0.95)) %>% 
  ungroup()

s1 %>% 
  ggplot(data=., aes(month, lai_u))+
  geom_line()+
  scale_color_viridis_d(option='E')+
  scale_x_continuous(expand=c(0,0))+
  theme_linedraw()+
  facet_grid(rows = vars(d_lat), cols = vars(d_lon))


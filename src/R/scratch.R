d1 <- read_parquet("../data_general/AVHRR_EVI2_CDR_V5/Oz_AVHRR_EVI2_CDR_1981_2019.parquet"); 
d2 <- read_parquet("../data_general/AVHRR_LAI_FAPAR_CDR_V5/Oz_AVHRR_LAI_CDR_1981_2019.parquet")

d1 %>% 
  select(lon,lat) %>% 
  distinct()

unique(d1$lon) %in% unique(d2$lon)
unique(d1$lat) %in% unique(d2$lat)

d1 %>% dim

d1 %>%
  sample_n(100000) %>% 
  mutate(month=month(date)) %>% 
  group_by(lon,lat,month) %>% 
  summarize(val = mean(evi2,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(lon,lat,color=val))+
  geom_point(size=0.1)+
  scale_color_viridis_c()+
  coord_equal()+
  facet_wrap(~month)

d2 %>%
  sample_n(100000) %>% 
  mutate(month=month(date)) %>% 
  group_by(lon,lat,month) %>% 
  summarize(val = mean(lai,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(lon,lat,color=val))+
  geom_point(size=0.1)+
  scale_color_viridis_c()+
  coord_equal()+
  facet_wrap(~month)


# rsync -r z3530735@monsoon.ccrc.unsw.edu.au:/srv/ccrc/data41/z3530735/ERA5-Land/Oz Oz


arrow::write_arrow(data.frame(x=1:1e6), sink="delete_me.parquet")
file.remove("delete_me.parquet")




# 2020-02-27 --------------------------------------------------------------

# THIS IS A SUPER MEMORY HEAVY OPERATION -------------------------
library(tidyverse); library(arrow); library(lubridate);

avhrr <- read_parquet("../data_general/AVHRR_EVI2_CDR_V5/Oz_AVHRR_EVI2_CDR_1981_2019.parquet")
avhrr <- avhrr %>% filter(c(lat>= -15 & lon >= 145)==F); gc()

avhrr %>% 
  filter(date==min(date)) %>% 
  sample_n(10000) %>% 
  ggplot(data=., aes(lon,lat,fill=evi2))+
  geom_tile()+
  scale_fill_viridis_c()


coords %>% 
  ggplot(data=., aes(longitude,latitude,fill=lai_avg))+
  geom_tile()+
  scale_fill_viridis_c()+
  coord_sf(xlim = c(140,155))




d_train %>% 
  ggplot(data=., aes(evi2_u, evi2_anom))+
  ggpointdensity::geom_pointdensity()+
  geom_smooth(method='lm')+
  scale_color_viridis_c()

d_train %>% 
  ggplot(data=., aes(tree_cover, evi2_anom))+
  ggpointdensity::geom_pointdensity()+
  geom_smooth(method='lm')+
  scale_color_viridis_c()+
  facet_wrap(~month)

d_train %>% 
  ggplot(data=., aes(lon,lat,color=norm_lai_avg))+
  geom_point()+
  scale_color_viridis_c()

d_train %>% 
  mutate(tree_cover=round(tree_cover,digits = -1)) %>% 
  ggplot(data=., aes(mcwd5_anom_12mo, evi2_anom))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~tree_cover)

d_train %>% 
  mutate(precip_anom_12mo = precip_12mo-map, 
         precip_anom_24mo = precip_24mo-map*2, 
         precip_anom_36mo = precip_36mo-map*3) %>% 
  select(evi2_anom_sd, 
          # tree_cover, 
         vpd3pm_anom,  
         # tmax_anom, tmin_anom, cwd5_anom,
         precip_deriv,
         precip_anom,
         precip_anom_12mo,
         cwd5_anom, 
         mcwd5_anom_12mo,
         # precip_anom_24mo,
         # precip_anom_36mo,
         # precip_12mo, precip_24mo, precip_36mo
         vpd9am_anom) %>% 
  drop_na() %>% 
  cor() %>% 
  corrplot::corrplot(method='color', 
                     type='lower', 
                     # cl.pos = 'n', 
                     tl.pos = 'ld') #lt, ld, td, d, n

po_test %>% 
  mutate(year=year(date)) %>% 
  # filter(hydro_year %in% c(2009:2020)) %>% 
  filter(year>=2009) %>% 
  ggplot(data=., aes(lon,lat,color=evi2_anom_sd))+
  geom_point(size=0.25)+
  coord_equal()+
  scale_color_gradient2(limits=c(-3,3))+
  facet_wrap(~year, nrow=2)+
  theme(panel.background = element_rect(fill='black'), 
        panel.grid = element_blank())





inner_join(d %>% 
  filter(date==ymd("2019-02-01")) %>% 
  select(vpd3pm,id), 
  d2 %>% 
  filter(date==ymd("2019-02-01")) %>% 
  select(vpd3pm,id), 
  by='id', suffix=c("_o","_n")) %>% 
  ggplot(data=., aes(vpd3pm_o, vpd3pm_n))+
  geom_point()+
  geom_smooth(method='lm')

library(stars)
junk <- stars::read_stars("../data_general/MOD13Q1/MOD13Q1_NDVI_SEAUS_monmean_2002_2019-0000000000-0000005376.tif")
ggplot()+
  geom_stars(data=junk[,,,1])

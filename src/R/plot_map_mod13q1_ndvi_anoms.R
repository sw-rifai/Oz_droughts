library(tidyverse); library(lubridate); library(arrow)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE);
source("src/R/helper_funs_Oz_droughts.R")
o <- arrow::read_parquet("../data_general/MOD13Q1/MOD13Q1_NDVI_1km_SEAUS_monmean_2000_2019.parquet")
tmp <- lazy_dt(o)
# gc();

tmp_coarse <- o %>% 
  mutate(lon = 0.125*round(lon/0.125), 
         lat = 0.125*round(lat/0.125)) %>% 
  group_by(lon,lat,hydro_year,season) %>% 
  summarize(ndvi = mean(ndvi,na.rm=T)) %>% 
  ungroup() %>% 
  as_tibble()

tmp_norms <- o %>% 
  mutate(lon = 0.125*round(lon/0.125), 
         lat = 0.125*round(lat/0.125)) %>% 
  group_by(lon,lat,season) %>% 
  summarize(ndvi_u = mean(ndvi,na.rm=T), 
            ndvi_sd = sd(ndvi,na.rm=T)) %>% 
  ungroup() %>% 
  as_tibble()

tmp_coarse <- inner_join(tmp_coarse, 
           tmp_norms, by=c("lon","lat","season"))

vec_cols <- RColorBrewer::brewer.pal(n=7, "BrBG")
p <- tmp_coarse %>% 
  mutate(ndvi_anom = ndvi - ndvi_u) %>% 
  mutate(ndvi_anom_sd = ndvi_anom/ndvi_sd) %>% 
  filter(hydro_year %in% c(2002:2019)) %>% 
  ggplot(data=., aes(lon,lat,fill=ndvi_anom_sd))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(high=vec_cols[7], low=vec_cols[1], 
                       mid=vec_cols[4], 
                       limits=c(-3,3))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  facet_grid(season~hydro_year)+
  theme_linedraw()+
  blah_theme
ggsave(p, filename = paste0("figures/map_mod13q1_ndvi_anom_seasonal_SEOZ_",Sys.Date(),".png"), 
       width=42,height=12,units='cm',dpi='retina')

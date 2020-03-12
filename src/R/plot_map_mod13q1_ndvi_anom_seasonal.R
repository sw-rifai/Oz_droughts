library(tidyverse); library(lubridate); library(arrow); 
source("src/R/helper_funs_Oz_droughts.R")
mod <- read_parquet("../data_general/MOD13Q1/MOD13Q1_NDVI_Anom_seasonal_SEOz_2000-2019.parquet")


# norms %>% 
#   # filter(date==min(date)) %>% 
#   ggplot(data=., aes(lon,lat,fill=ndvi_u))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c()+
#   facet_wrap(~season)
vec_cols <- RColorBrewer::brewer.pal(n=7, "BrBG")
p <- mod %>% 
  mutate(lon = 0.125*round(lon/0.125), 
         lat = 0.125*round(lat/0.125)) %>% 
  group_by(lon,lat,hydro_year,season) %>% 
  summarize(ndvi_anom = mean(ndvi_anom,na.rm=T), 
            ndvi_anom_sd = mean(ndvi_anom_sd,na.rm=T)) %>% 
  ungroup() %>% 
  filter(hydro_year %in% c(2001:2019)) %>% 
  ggplot(data=., aes(lon,lat,fill=ndvi_anom_sd))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(
    expression(paste("NDVI Anom. (",sigma,")")),
    high=vec_cols[7],
    mid = vec_cols[4],
    low=vec_cols[1], 
    limits=c(-3,3), 
    oob=scales::squish)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  facet_grid(season~hydro_year)+
  theme_linedraw()+
  blah_theme
gc()
ggsave(p, filename = paste0("figures/map_mod13q1_ndvi_anom_seasonal_",Sys.Date(),".png"), 
       width=40, height=10, units='cm', dpi='retina', type='cairo')


unique(mod$lon) %>% length
unique(mod$lon) %>% round(./0.25) %>% unique() %>% length


(0.125*round(unique(mod$lon)/0.125)) %>% unique() %>% length

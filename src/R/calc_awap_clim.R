library(arrow); library(tidyverse); library(lubridate); 
clim <- read_parquet("../data_general/clim_grid/awap/parquet/awap_joined_monthly_1970_2019.parquet")
e5 <- read_parquet("../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_evaporation_1979_2019.parquet")

clim <- inner_join(clim, e5, by=c("id","date"))

norms_ma <- clim %>% 
  mutate(year=year(date)) %>% 
  filter(year >= 1982 & year <= 2011) %>% 
  group_by(id, year) %>% 
  summarize(ap = mean(precip, na.rm=T)*12, 
            atmax = mean(tmax, na.rm=T), 
            atmin = mean(tmin, na.rm=T), 
            aevap = mean(evap, na.rm=T)*12) %>% 
  ungroup() %>% 
  group_by(id) %>% 
    summarize(map = mean(ap), 
              ap_sd = sd(ap), 
              matmax = mean(atmax), 
              atmax_sd = sd(atmax), 
              matmin = mean(atmin), 
              atmin_sd = sd(atmin), 
              maevap = mean(aevap), 
              aevap_sd = sd(aevap)) %>% 
    ungroup()
  
tmp <- clim %>% 
  mutate(year=year(date)) %>% 
  mutate(month=month(date)) %>% 
  filter(year >= 1982 & year <= 2011) %>% 
  group_by(id, month) %>% 
  summarize(precip_u = mean(precip, na.rm=T), 
            tmax_u = mean(tmax, na.rm=T), 
            tmin_u = mean(tmin, na.rm=T), 
            evap_u = mean(evap, na.rm=T), 
            vpd9am_u = mean(vp9am, na.rm=T), 
            vpd3pm_u = mean(vpd3pm, na.rm=T),
            precip_sd = sd(precip, na.rm=T), 
            tmax_sd = sd(tmax, na.rm=T), 
            tmin_sd = sd(tmin, na.rm=T), 
            evap_sd = sd(evap, na.rm=T), 
            vpd9am_sd = sd(vp9am, na.rm=T), 
            vpd3pm_sd = sd(vpd3pm, na.rm=T),) %>% 
  ungroup()
clim2 <- tmp %>% 
  inner_join(clim %>% mutate(month=month(date)), 
             ., 
             by=c("id","month")) %>% 
  mutate(precip_anom = precip-precip_u, 
         tmax_anom = tmax - tmax_u, 
         tmin_anom = tmin - tmin_u, 
         evap_anom = evap - evap_u, 
         vpd9am_anom = vpd9am - vpd9am_u, 
         vpd3pm_anom = vpd3pm - vpd3pm_u) %>% 
  mutate(precip_anom_sd = precip_anom/precip_sd, 
         tmax_anom_sd = tmax_anom/tmax_sd, 
         tmin_anom_sd = tmin_anom/tmin_sd, 
         evap_anom_sd = evap_anom/evap_sd, 
         vpd9am_anom_sd = vpd9am_anom/vpd9am_sd, 
         vpd3pm_anom_sd = vpd3pm_anom/vpd3pm_sd)
  
clim2 <- clim2 %>% inner_join(., norms_ma, by=c('id'))

clim2 %>% 
  write_parquet(., 
                sink=paste0("../data_general/clim_grid/awap/parquet/awap_clim_",Sys.Date(),".parquet"))

# tmp <- clim %>% 
#   group_by(longitude, latitude) %>% 
#   summarize(missing = sum(is.na(precip)==T)) %>% 
#   ungroup()
# tmp %>% 
#   ggplot(data=., aes(longitude, latitude, fill=missing)) + 
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c(limits=c(0,1))

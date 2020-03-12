library(arrow); library(tidyverse); library(lubridate); 
library(RcppRoll)
library(sf)

#################################################################################################
#--- FUNCTIONS ---------------------------------------------------------------------------------#
#################################################################################################
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

f_cwd_v5 <- function(cwd_et,precip,et,map){
  # No reset during the wettest month of the year
  for(i in seq(2,length(precip))){
    
    cwd_et[i] <-  min(0, cwd_et[i-1] + (precip[i]) - max(et[i],1, na.rm=T), na.rm=T)
    cwd_et[i] <- ifelse(cwd_et[i] < -3000, -3000, cwd_et[i])
    cwd_et[i] <- ifelse(precip[i] > 0.333*map[i], 0, cwd_et[i])
    
  }
  cwd_et
}
#################################################################################################
#--- END SECTION-- -----------------------------------------------------------------------------#
#################################################################################################


# Load data ---------------------------------------------------------------
coords <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv")
coords <- coords %>% select(id, longitude, latitude) %>% 
  rename(lon=longitude, lat=latitude) %>% 
  filter(lon>=140)

d <- read_parquet(
    file = "../data_general/clim_grid/awap/parquet/awap_joined_monthly_1970_2019.parquet")
d <- d %>% 
  select(id,longitude,latitude,date,vp9am, vp3pm,tmin,tmax,vpd9am,vpd3pm,precip)
d <- d %>% rename(lon=longitude, lat=latitude)
d <- inner_join(coords, 
                d %>% select(-id), 
                by=c("lon","lat"))

e5 <- read_parquet("../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_evaporation_1979_2019.parquet")
d <- inner_join(d, e5, by=c('id','date'))
gc()

aus <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                   layer="gadm36_AUS_1")
# aus <- sf::st_transform_proj(aus, st_crs(4326))
aus <- st_as_sf(aus)


# CALCULATE NORMALS -------------------------------------------------------
clim <- d
clim <- clim %>% 
  mutate(hydro_year = year(date-months(6)))

norms_ma <- clim %>% 
  filter(hydro_year >= 1982 & hydro_year <= 2011) %>% 
  group_by(id, hydro_year) %>% 
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
  mutate(month=month(date)) %>% 
  filter(hydro_year >= 1982 & hydro_year <= 2011) %>% 
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



# Drop coordinates with missing precip or evap ----------------------------
d <- clim2; 
rm(clim); rm(clim2); gc(); 

# drop coordinates with missing data
vec_dates <- tibble(date=seq(min(d$date), 
                             max(d$date), by = '1 month'))
d <- left_join(vec_dates, d, by='date')
vec_drop <- d %>% 
  group_by(id) %>% 
  summarize(missing_evap = sum(is.na(evap)==T), 
            missing_precip = sum(is.na(precip))) %>% 
  ungroup()

vec_drop <- vec_drop %>% 
  mutate(bad = missing_evap+missing_precip) %>% 
  filter(bad >= 1) %>% 
  select(id)

d <- d %>% filter(!id %in% vec_drop$id) # drop coords with missing precip or evap


# CALCULATE DROUGHT METRICS -----------------------------------------------
spinup <- d %>% 
  select(id, date, precip_u, evap_u, map) %>% 
  mutate(cwd5 = NA) %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(cwd5 = f_cwd_v5(cwd_et = cwd5, precip = precip_u, et = evap_u, map=map)) %>% 
  ungroup()


tmp <- spinup %>% 
  filter(date==ymd('2003-06-01')) %>% 
  select(id, cwd5)
rm(spinup); 

d <- inner_join(d, tmp, by='id') %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(cwd5 = f_cwd_v5(cwd_et = cwd5, precip = precip_u, et = evap_u, map=map)) %>% 
  ungroup()

d <- d %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(mcwd5_12mo = roll_minr(cwd5, n=12, fill=NA), 
         mcwd5_24mo = roll_minr(cwd5, n=24, fill=NA), 
         mcwd5_36mo = roll_minr(cwd5, n=36, fill=NA)) %>% 
  ungroup()

d <- d %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(precip_3mo = roll_sumr(precip, n=3, fill=NA),
         precip_6mo = roll_sumr(precip, n=6, fill=NA),
         precip_12mo = roll_sumr(precip, n=12, fill=NA), 
         et_12mo = roll_sumr(evap, n=12, fill=NA), 
         precip_24mo = roll_sumr(precip, n=24, fill=NA), 
         et_24mo = roll_sumr(evap, n=24, fill=NA), 
         precip_36mo = roll_sumr(precip, n=36, fill=NA), 
         et_36mo = roll_sumr(evap, n=36, fill=NA)) %>% 
  ungroup() 

norms_cwd <- d %>% filter(date>=ymd('1982-01-01') & 
               date<=ymd('2010-12-31')) %>%
  group_by(lon,lat,month) %>% 
  summarize(cwd5_u = mean(cwd5, na.rm=T), 
            mcwd5_12mo_u = mean(mcwd5_12mo, na.rm=T), 
            cwd5_sd = sd(cwd5, na.rm=T), 
            mcwd5_sd = sd(mcwd5_12mo, na.rm=T)) %>% 
  ungroup()

d <- inner_join(d, norms_cwd, by=c("lon","lat","month"))

d <- d %>% 
  mutate(cwd5_anom = cwd5-cwd5_u, 
         mcwd5_anom_12mo = mcwd5_12mo - mcwd5_12mo_u) %>% 
  mutate(cwd5_anom_sd = cwd5_anom/cwd5_sd, 
         mcwd5_anom_sd = mcwd5_anom_12mo/mcwd5_sd)


# calculate derivative of precip ------------------------------------------
d <- d %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(precip_deriv = signal::sgolayfilt(precip, n=13, m=1)) %>% 
  ungroup()

d %>% 
  arrow::write_parquet(., sink=paste0("../data_general/clim_grid/awap/parquet/awap_EOZ_wDroughtMets_",Sys.Date(),'.parquet'))
# scratch -----------------------------------------------------------------

# d %>% 
#   filter(id %in% sample.int(4359, 50)) %>% 
#   ggplot(data=., aes(date, et_24mo, color=as.factor(id)))+
#   geom_line()+
#   theme(legend.position = 'none')
# 
# spinup %>% 
#   filter(id %in% sample.int(4359, 50)) %>% 
#   ggplot(data=., aes(date, cwd5, color=as.factor(id)))+
#   geom_line()+
#   theme(legend.position = 'none')
# 
# d %>% 
#   filter(date>=ymd('1983-01-01') & 
#            date<=ymd('1984-01-01')) %>% 
#   group_by(id) %>% 
#   filter(evap_u == max(evap_u)) %>% 
#   ungroup() %>% 
#   ggplot(data=., aes(lon,lat,fill=evap_u))+
#   geom_tile()+
#   scale_fill_viridis_c()
# spinup %>% 
#   filter(date==max(date)) %>% 
#   ggplot(data=., aes(evap_u, cwd5))+
#   geom_point()+
#   geom_smooth(k=30)
# 
# 
# d <- d %>% 
#   filter(is.na(precip)==F & is.na(evap)==F) %>% 
#   mutate(cwd5 = NA) %>% 
#   group_by(id) %>% 
#   arrange(date) %>% 
#   mutate(cwd5 = f_cwd_v5(cwd_et = cwd, precip = precip, et = evap)) %>% 
#   ungroup()




# # data prep --------------------------------------------------------------------
# d <- d %>% filter(c(lat> -15 & lon > 145)==F)
# d <- d %>% 
#   filter(is.na(precip)==F & is.na(evap)==F) %>% 
#   mutate(cwd5 = NA) %>% 
#   group_by(id) %>% 
#   arrange(date) %>% 
#   mutate(cwd5 = f_cwd_v5(cwd_et = cwd5, precip = precip, et = evap)) %>% 
#   ungroup()
# 
# d <- d %>% 
#   group_by(id) %>% 
#   arrange(date) %>% 
#   mutate(mcwd5_12mo = roll_minr(cwd5, n=12, fill=NA), 
#          mcwd5_24mo = roll_minr(cwd5, n=24, fill=NA), 
#          mcwd5_36mo = roll_minr(cwd5, n=36, fill=NA)) %>% 
#   ungroup()
# 
# d <- d %>% 
#   group_by(id) %>% 
#   arrange(date) %>% 
#   mutate(precip_12mo = roll_sumr(precip, n=12, fill=NA), 
#          et_12mo = roll_sumr(evap, n=12, fill=NA), 
#          precip_24mo = roll_sumr(precip, n=24, fill=NA), 
#          et_24mo = roll_sumr(evap, n=24, fill=NA), 
#          precip_36mo = roll_sumr(precip, n=36, fill=NA), 
#          et_36mo = roll_sumr(evap, n=36, fill=NA)) %>% 
#   ungroup() 
# 
# 
# norm_evi <- d %>% 
#   filter(date >= ymd('1982-01-01') & date <= ymd("2010-12-31")) %>% 
#   group_by(id, month) %>% 
#   summarize(evi2_u = mean(evi2, na.rm=T), 
#             evi2_sd = sd(evi2, na.rm=T)) %>% 
#   ungroup()
# d <- inner_join(d, norm_evi, by=c("id","month"))
# d <- d %>% mutate(evi2_anom = evi2 - evi2_u) %>% 
#   mutate(evi2_anom_sd = evi2_anom/evi2_sd)
# 

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
d <-  
  read_arrow(stream = "../data_general/Oz_misc_data/ahvrr_clim_2020-02-24.parquet")

aus <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                   layer="gadm36_AUS_1")
# aus <- sf::st_transform_proj(aus, st_crs(4326))
aus <- st_as_sf(aus)


# data prep --------------------------------------------------------------------
d <- d %>% filter(c(lat> -15 & lon > 145)==F)
d <- d %>% filter(is.nan(precip_u)==F) %>% 
  filter(is.nan(evap_u)==F)

d %>% 
  group_by(id) %>% 
  summarize(n_nan = sum(is.nan(evap_u)==T)) %>% 
  ungroup() %>% 
  pull(n_nan) %>% summary

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
  mutate(precip_12mo = roll_sumr(precip, n=12, fill=NA), 
         et_12mo = roll_sumr(evap, n=12, fill=NA), 
         precip_24mo = roll_sumr(precip, n=24, fill=NA), 
         et_24mo = roll_sumr(evap, n=24, fill=NA), 
         precip_36mo = roll_sumr(precip, n=36, fill=NA), 
         et_36mo = roll_sumr(evap, n=36, fill=NA)) %>% 
  ungroup() 

d %>% 
  arrow::write_parquet(., sink=paste0("../data_general/clim_grid/awap/parquet/awap_wDroughtMets_",Sys.Date(),'.parquet'))
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

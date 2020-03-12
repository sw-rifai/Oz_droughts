library(tidyverse); library(lubridate); library(arrow)

#***************************************************************************
# Import EVI2 and clim (ERA5-Land) -----------------------------------------
#***************************************************************************
avhrr <- read_parquet(file="../data_general/AVHRR_EVI2_CDR_V5/EOz_AVHRR_EVI2_LAI_1981_2019.parquet")
ex_pet <- read_parquet(file='../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_PET_1979_2019.parquet')
ex_vpd <- read_parquet(file="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_vpd_1979_2019.parquet")
ex_precip <- read_parquet(file='../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_precip_1979_2019.parquet')
ex <- inner_join(ex_pet %>% select(-days_in_month), 
                 ex_vpd, 
                 by=c("id","date"))
ex <- inner_join(ex, ex_precip %>% select(-days_in_month), 
                 by=c("id","date"))
d <- inner_join(ex, 
                avhrr, 
                by=c("id","date"))
d <- d %>% 
  mutate(quarter = quarter(date, fiscal_start = 11))
d <- d %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"OSN"))
d <- d %>% mutate(season = factor(q,
                                  levels=c("DJF","MAM","JJA","OSN"), 
                                  ordered=T))
d <- d %>% 
  mutate(hydro_year = year(date+months(1)))
ds <- d %>% 
  filter(hydro_year >= 1981 & hydro_year <= 2019) %>% 
  group_by(hydro_year, season,lon,lat) %>% 
  summarize(evi2 = mean(evi2, na.rm=T), 
            evi2_anom = mean(evi2_anom, na.rm=T), 
            evi2_anom_sd = mean(evi2_anom_sd, na.rm=T), 
            precip = sum(precip), 
            t2m = mean(t2m, na.rm = T), 
            vpd = mean(vpd,na.rm = T), 
            pet = mean(pet,na.rm=T)) %>% 
  ungroup()
ds_clim <- d %>% 
  filter(hydro_year >= 1982 & hydro_year <= 2010) %>% 
  group_by(season,lon,lat) %>% 
  summarize(evi2_u = mean(evi2, na.rm=T), 
            evi2_sd = sd(evi2, na.rm=T), 
            precip_u = mean(precip)*3, 
            precip_sd = sd(precip)*3, 
            t2m_u = mean(t2m, na.rm = T), 
            t2m_sd = sd(t2m, na.rm = T), 
            vpd_u = mean(vpd,na.rm = T),
            vpd_sd = sd(vpd,na.rm=T),
            pet_u = mean(pet,na.rm=T), 
            pet_sd = sd(pet,na.rm=T)) %>% 
  ungroup()

ds <- inner_join(ds, ds_clim, by=c("lon","lat","season"))
rm(d); 
gc();
#********************************************************************************

library(tidyverse);library(lubridate); library(arrow)
# ascending ~ 1:30am 
# descending ~ 1:30pm

vod <- read_parquet("../data_general/lpdr_test/VOD_LPDRv2_OZ.parquet")
# vod <- vod %>% inner_join(., 
#                    vod %>% 
#   group_by(lon,lat,month) %>% 
#   summarize(vod_asc_sd = sd(vod_asc, na.rm=T), 
#             vod_desc_sd = sd(vod_desc, na.rm=T)) %>% 
#   ungroup(), 
#   by=c("lon","lat","month")) %>% 
#   mutate(vod_asc_anom_sd = vod_asc_anom/vod_asc_sd, 
#          vod_desc_anom_sd = vod_desc_anom/vod_desc_sd)


# d_grid <- vod %>% select(lon,lat) %>% distinct() %>% 
#   sf::st_as_sf(d_grid, coords = c("lon","lat"))

vod <- vod %>% 
  inner_join(., 
             vod %>% 
               group_by(lon,lat,month) %>% 
               summarize(vod_asc_u = mean(vod_asc,na.rm=T), 
                         vod_desc_u = mean(vod_desc,na.rm=T), 
                         vod_asc_sd = sd(vod_asc, na.rm=T), 
                         vod_desc_sd = sd(vod_desc, na.rm=T),
                         vod_sigma_u = mean(vod_sigma,na.rm=T), 
                         vod_lambda_u = mean(vod_lambda,na.rm=T)) %>% 
               ungroup(), 
             by=c("lon","lat","month")) %>% 
  mutate(vod_asc_anom = vod_asc - vod_asc_u, 
         vod_desc_anom = vod_desc - vod_desc_u, 
         vod_sigma_anom = vod_sigma - vod_sigma_u, 
         vod_lamda_anom = vod_lambda - vod_lambda_u) %>% 
    mutate(vod_asc_anom_sd = vod_asc_anom/vod_asc_sd,
           vod_desc_anom_sd = vod_desc_anom/vod_desc_sd)
  

vod %>% 
  mutate(date=ymd(paste(year,month,1))) %>% 
  mutate(lat2 = round(lat, digits = -1)) %>% 
  mutate(lon2 = round(lon, digits = -1)) %>% 
  group_by(date,lon2,lat2) %>% 
  summarize(night = mean(vod_asc_anom,na.rm=T), 
            day = mean(vod_desc_anom,na.rm=T)) %>% 
  ungroup() %>% 
  pivot_longer(c(-date,-lon2,-lat2), #-lon2, -lat2, 
               names_to = "time", 
               values_to = 'vod') %>% 
  mutate(lat3 = factor(lat2,levels=as.character(c(-10,-20,-30,-40)), 
                       ordered = T)) %>% 
  group_by(lon2,lat3,time) %>%
  arrange(date) %>% 
  mutate(vod_3mo = RcppRoll::roll_meanr(vod, n=3, fill=NA)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(date, vod_3mo, color=time))+
  geom_hline(aes(yintercept=0),col='grey')+
  geom_line()+
  labs(x=NULL, y=NULL, title = "3-Month VOD Anomaly")+
  scale_color_viridis_d("Pass", option='B',begin = 0.25, end=0.75,direction = -1)+
  facet_grid(lat3~lon2, labeller = "label_value")+
  theme_linedraw()
ggsave("figures/timeseries_3monthVOD_EOZ.png", 
       width=20, height=10, units='cm', dpi='retina')



#***************************************************************************
# Import Clim, EVI2, and VOD -----------------------------------------
#***************************************************************************
vod_blocks <- vod %>% 
  mutate(date=ymd(paste(year,month,1))) %>% 
  mutate(lat2 = round(lat, digits = -1)) %>% 
  mutate(lon2 = round(lon, digits = -1)) %>% 
  group_by(date,lon2,lat2) %>% 
  summarize(night = mean(vod_asc_anom_sd,na.rm=T), 
            day = mean(vod_desc_anom_sd,na.rm=T)) %>% 
  ungroup() %>% 
  pivot_longer(c(-date,-lon2,-lat2), #-lon2, -lat2, 
               names_to = "time", 
               values_to = 'vod') %>% 
  mutate(lat3 = factor(lat2,levels=as.character(c(-10,-20,-30,-40)), 
                       ordered = T)) %>% 
  group_by(lon2,lat3,time) %>%
  arrange(date) %>% 
  mutate(vod_3mo = RcppRoll::roll_meanr(vod, n=3, fill=NA)) %>% 
  ungroup()
  

avhrr <- read_parquet(file="../data_general/AVHRR_EVI2_CDR_V5/EOz_AVHRR_EVI2_LAI_1981_2019.parquet")
ex_pet <- read_parquet(file='../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_PET_1979_2019.parquet')
ex_vpd <- read_parquet(file="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_vpd_1979_2019.parquet")
ex_precip <- read_parquet(file='../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_precip_1979_2019.parquet')
ex <- inner_join(ex_pet %>% select(-days_in_month), 
                 ex_vpd, 
                 by=c("id","date"))
ex <- inner_join(ex, ex_precip %>% select(-days_in_month), 
                 by=c("id","date"))
d <- inner_join(ex, 
                avhrr, 
                by=c("id","date"))

d_blocks <- d %>% 
  # mutate(date=ymd(paste(year,month,1))) %>% 
  mutate(lat2 = round(lat, digits = -1)) %>% 
  mutate(lon2 = round(lon, digits = -1)) %>% 
  group_by(date,lon2,lat2) %>% 
  summarize(evi2_anom_sd = mean(evi2_anom_sd,na.rm=T), 
            lai = mean(lai,na.rm=T)) %>% 
  ungroup() %>% 
  # pivot_longer(c(-date,-lon2,-lat2), #-lon2, -lat2, 
  #              names_to = "vi", 
  #              values_to = 'estimate') %>% 
  mutate(lat3 = factor(lat2,levels=as.character(c(-10,-20,-30,-40)), 
                       ordered = T)) %>% 
  group_by(lon2,lat3) %>%
  arrange(date) %>% 
  mutate(evi2_anom_sd_3mo = RcppRoll::roll_meanr(evi2_anom_sd, n=3, fill=NA)) %>% 
  ungroup()

d_blocks %>% 
  # filter(vi=='evi2_anom') %>% 
  filter(date>=ymd('2002-01-01')) %>% 
  ggplot(data=., aes(date, evi2_anom_sd_3mo))+
  geom_hline(aes(yintercept=0),col='grey')+
  geom_line()+
  labs(x=NULL, y=NULL, title = "3-Month VI")+
  scale_color_viridis_d("VI", option='B',begin = 0.25, end=0.75,direction = -1)+
  facet_grid(lat3~lon2, labeller = "label_value")+
  theme_linedraw()



d_blocks %>% 
  # filter(vi=='evi2_anom') %>% 
  filter(date>=ymd('2002-01-01')) %>% 
  inner_join(., vod_blocks, by=c("date","lat2","lon2","lat3")) %>% 
  ggplot(data=., aes(date, evi2_anom_sd_3mo))+
  geom_hline(aes(yintercept=0),col='grey')+
  geom_line()+
  geom_line(aes(date, vod_3mo,color=time))+
  labs(x=NULL, y=expression(paste(sigma)), 
       title = expression(paste("3-Month EVI & VOD Anomalies (z-score)")))+
  scale_color_viridis_d("VOD", option='B',begin = 0.25, end=0.75,direction = -1)+
  scale_y_continuous(expand=c(0,0), limits=c(-3,3))+
  facet_grid(lat3~lon2, labeller = "label_value")+
  theme_linedraw()
ggsave(filename = "figures/timeseries_3month_evi_vod_anom_zscore_2002_2019.png", 
       width=18, height=10, units='cm', dpi='retina')
#********************************************************************************



# Plot VOD seasonal daytime -----------------------------------------------
o <- arrow::read_parquet("../data_general/lpdr_test/lpdr_vodSeasonal_Oz2020-03-09.parquet")
o %>% 
  filter(hydro_year==2015) %>% 
  ggplot(data=., aes(x,y,fill=vod_day_avg))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()+
  facet_wrap(~season)

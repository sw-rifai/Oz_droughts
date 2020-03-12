library(arrow); library(tidyverse); library(lubridate); 
library(sf); library(lubridate)
# ascending ~ 1:30am 
# descending ~ 1:30pm
if( ((str_split(getwd(),"/",simplify = T) %>% last()) != "Oz_droughts") == F){
  setwd("/home/sami/srifai@gmail.com/work/research/Oz_droughts")
}

# Import ------------------------------------------------------------------
fp <- list.files("../data_general/lpdr_test/LPDR_VOD/", full.names = T, pattern='parquet')
library(foreach)
out <- foreach(i = 1:length(fp), 
               .packages = c("tidyverse","lubridate","arrow"),
               .combine=rbind) %do% {
                 o <- arrow::read_parquet(fp[i])
               }
out <- out %>% filter(band==5)
gc()

out <- out %>% 
  mutate(quarter = quarter(date, fiscal_start = 11))
out <- out %>% 
  mutate(q = case_when(quarter==1~"DJF",
                   quarter==2~"MAM",
                   quarter==3~"JJA",
                   quarter==4~"OSN"))
out <- out %>% mutate(season = factor(q,
                                         levels=c("DJF","MAM","JJA","OSN"), 
                                         ordered=T))
# fct_relevel(c("DJF","MAM","JJA","OSN"))
out$season %>% unique
out <- out %>% 
  mutate(hydro_year = year(date+months(1)))
out <- out %>% 
  filter(date>=ymd('2003-12-01') & date <= ymd('2019-12-01'))

tmp <- out %>% 
  group_by(x,y,season,hydro_year) %>% 
  summarize(vod_asc = mean(val_asc, na.rm=T), 
            vod_desc = mean(val_desc, na.rm = T), 
            vod_ddiff = mean(val_asc-val_desc, na.rm=T), 
            vod_sigma = coef(lm(val_desc~val_asc))[2], 
            vod_lambda = coef(lm(val_desc~val_asc))[1], 
            vod_nobs = sum(is.na(val_asc)==F & is.na(val_desc)==F)) %>%
  ungroup()
# tmp %>% 
#   feather::write_feather(., path='../data_general/Oz_misc_data/VOD_Seasonaly_LPDRv2.feather')

tmp %>% 
  arrow::write_parquet(., sink='../data_general/Oz_misc_data/VOD_Seasonaly_LPDRv2.parquet')

# tmp %>% 
#   filter(vod_nobs >= 10) %>% 
#   ggplot(data=., aes(x,y,fill=vod_ddiff))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c(
#                        # limits=c(0,2),
#                        oob=scales::squish)+
#   facet_wrap(~season)
# 
# 
# 
# out <- foreach(i = 1:length(fp), 
#                .packages = c("tidyverse","lubridate","arrow"),
#                .combine=rbind) %dopar% {
#                  o <- arrow::read_parquet(fp[i])
#                  test <- o %>% 
#                    mutate(month=month(date), 
#                           year=year(date)) %>% 
#                    filter(band==5) %>% 
#                    group_by(x,y,month,year) %>% 
#                    summarize(vod_asc = mean(val_asc, na.rm=T), 
#                              vod_desc = mean(val_desc, na.rm = T), 
#                              vod_ddiff = mean(val_asc-val_desc, na.rm=T), 
#                              vod_sigma = coef(lm(val_desc~val_asc))[2], 
#                              vod_lambda = coef(lm(val_desc~val_asc))[1], 
#                              vod_nobs = sum(is.na(val_asc)==F & is.na(val_desc)==F)) %>% 
#                    ungroup()
#                  test          
#                }
# 
# 

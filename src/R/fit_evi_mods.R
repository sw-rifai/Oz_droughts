library(tidyverse); library(lubridate); library(mgcv); library(RcppRoll)

d_coords <- read_csv("data/Oz_avhrr_lai_gte1_renamed.csv")
d_evi <- arrow::read_parquet("../data_general/AVHRR_EVI2_CDR_V5/Oz_AVHRR_EVI2_CDR_1981_2019.parquet")
d_precip <- arrow::read_parquet("../data_general/clim_grid/awap/parquet/awap_precip_monthly_1911_2019.parquet")
d_precip <- d_precip %>% inner_join(., d_coords, by='id')
d_precip <- d_precip %>% filter(date>=ymd('1981-06-01'))

d <- inner_join(d_evi, 
           d_precip %>% 
  rename(lon=longitude, 
         lat=latitude), 
  by=c("lon","lat","date"))

d_precip <- d %>% filter(is.na(precip)==F)
d_precip <- d_precip %>% mutate(month=month(date))
norms <- d_precip %>% 
  filter(date>=ymd("1982-01-01") & 
           date <= "2011-12-31") %>% 
  group_by(lon,lat,month) %>% 
  summarize(precip_u = mean(precip, na.rm=T), 
            precip_sd = sd(precip, na.rm=T)) %>% 
  ungroup()
norms_ma <- norms %>% group_by(lon,lat) %>% 
  summarize(map = sum(precip_u)) %>% 
  ungroup()
d_precip <- inner_join(d_precip, norms, by=c('lon','lat','month'))  
d_precip <- d_precip %>% 
  mutate(precip_anom = precip - precip_u) %>% 
  mutate(precip_anom_sigma = precip_anom/precip_sd)
d_precip <- inner_join(d_precip, norms_ma, by=c("lon","lat"))
d_precip <- d_precip %>% 
  group_by(lon,lat) %>% 
  arrange(date) %>% 
  mutate(precip_12mo = roll_sumr(precip, n=12, fill=NA)) %>% 
  ungroup()
d_precip <- d_precip %>% 
  mutate(precip_12mo_anom = precip_12mo - map)

d_train <- d_precip %>% 
  sample_n(10000)
d_test <- d_precip %>% 
  sample_n(10000)

f1 <- bam(evi2~s(lon,lat)+s(map), 
          data=d_train, 
          discrete=T)
summary(f1)
plot(f1)


f2 <- bam(evi2~s(lon,lat)+s(map)+s(precip_12mo_anom), 
          data=d_train, 
          discrete=T)
summary(f2); 
plot(f2); 

f3 <- bam(evi2~s(lon,lat)+
            s(map)+
            s(lai_min)+
            s(precip_12mo_anom), 
          data=d_train, 
          discrete=T)
summary(f3); 
plot(f3); 

d_precip %>% 
  sample_n(10000) %>% 
  lm(evi2~precip_anom, data=.) %>% 
  summary

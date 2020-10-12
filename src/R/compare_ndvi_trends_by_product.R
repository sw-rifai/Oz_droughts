library(stars); library(tidyverse); library(data.table); library(lubridate)
library(dtplyr, warn.conflicts = FALSE)
setDTthreads(threads=0)
library(mgcv)
library(RcppArmadillo)
#*******************************************************************************
# Import Koppen Climate Zone ------------------------------------------------------
# Load simplified BOM Koppen climate zones --------------------------------
kop <- arrow::read_parquet(file = '../data_general/Koppen_climate/BOM_Koppen_simplified7.parquet')

# Import NOAA AHVRR CDR --------------------------------------------------------
tmp <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_CDRv5_ndvi_median_nobs_stdDev_5000m_EastOz_1981_2019.tif",
                            proxy = F)
tmp_median <- tmp %>% 
  slice('band', seq(1,by=3,length.out = 461)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1981-08-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>% 
  set_names(c('ndvi_cdr'))
tmp_nobs <- tmp %>% 
  slice('band', seq(2,by=3,length.out = 461)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1981-08-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>% 
  set_names(c('nobs'))
tmp_sd <- tmp %>% 
  slice('band', seq(3,by=3,length.out = 461)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1981-08-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>% 
  set_names(c('sd'))
tmp_sza <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_CDRv5_sza_timeOfDay_median_5000m_EastOz_1981_2019.tif", 
                             proxy=F) %>% 
  slice('band', seq(1,by=2,length.out = 461)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1981-08-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>% 
  set_names("sza")
tmp_tod <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_CDRv5_sza_timeOfDay_median_5000m_EastOz_1981_2019.tif", 
                             proxy=F) %>% 
  slice('band', seq(2,by=2,length.out = 461)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1981-08-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>% 
  set_names("tod")

tmp <- c(tmp_median, tmp_nobs, tmp_sd, tmp_sza, tmp_tod)
d_cdr <- tmp %>% as.data.frame() %>% as.data.table()
d_cdr <- merge(d_cdr, as.data.table(kop))
d_cdr <- d_cdr[is.na(zone)==F & is.na(nobs)==F]
d_cdr <- d_cdr %>% mutate(month=month(date))

rm(tmp_median, tmp_nobs, tmp_sd, tmp_sza, tmp_tod)
gc()

d_cdr_norms <- d_cdr %>% 
  lazy_dt() %>%
  filter(date <= ymd("2017-01-01")) %>% 
  mutate(month=month(date)) %>% 
  group_by(x,y,month) %>% 
  summarize(ndvi_u = mean(ndvi_cdr,na.rm=TRUE), 
            sza_u = mean(sza,na.rm=TRUE), 
            tod_u = mean(tod, na.rm=TRUE), 
            ndvi_sd = sd(ndvi_cdr, na.rm=TRUE), 
            sza_sd = sd(sza,na.rm=TRUE), 
            tod_sd = sd(tod,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as.data.table()
d_cdr <- merge(d_cdr_norms, d_cdr, by=c("x","y","month"))
d_cdr <- d_cdr %>% 
  lazy_dt() %>% 
  mutate(ndvi_anom = ndvi_cdr - ndvi_u, 
         sza_anom = sza - sza_u, 
         tod_anom = tod - tod_u) %>% 
  mutate(ndvi_anom_sd = ndvi_anom/ndvi_sd, 
         sza_anom_sd = sza_anom/sza_sd, 
         tod_anom_sd = tod_anom/tod_sd) %>% 
  as.data.table()


# Filtering AVHRR CDR -----------------------------------------------------
d_cdr2 <- d_cdr %>% 
  lazy_dt() %>% 
  filter(nobs >= 3) %>% 
  filter(ndvi_cdr >= 0.1) %>% 
  mutate(cv_cdr = sd/ndvi_cdr) %>% 
  filter(cv_cdr < 0.25) %>% 
  filter(between(ndvi_anom_sd, -3.5,3.5)) %>% 
  filter(between(sza_anom_sd, -3.5,3.5)) %>% 
  filter(between(tod_anom_sd, -3.5,3.5)) %>% 
  as.data.table()

# f0 <- d_cdr[sample(.N, 1e6)] %>%
#   mutate(year=year(date)) %>% 
#   bam(ndvi_cdr ~ 
#         ti(sza_anom_sd,month) + 
#         ti(tod_anom_sd,month) + 
#        s(year,by=zone, bs='cs'), 
#       data=., 
#       discrete=T, 
#       select=T)
# summary(f0)
# f0 %>% plot
# 100*(38*4.111e-05)/0.3
# 
# expand_grid(date = seq(ymd("1982-01-01"),ymd("2019-01-01"),by='1 month'), 
#             zone = unique(kop$zone), 
#             sza_anom_sd = 0, 
#             tod_anom_sd = 0) %>% 
#   mutate(month=month(date), 
#          year=year(date)) %>% 
#   filter(is.na(zone)==F) %>% 
#   mutate(pred = predict(f0, newdata=.)) %>% 
#   ggplot(data=.,aes(date, pred, color=zone))+
#   geom_smooth()
# 
# expand_grid(date = seq(ymd("1982-01-01"),ymd("2019-01-01"),by='1 month'), 
#             zone = unique(kop$zone), 
#             sza_anom_sd = 0, 
#             tod_anom_sd = 0) %>% 
#   mutate(month=month(date), 
#          year=year(date)) %>% 
#   filter(is.na(zone)==F) %>% 
#   mutate(pred = predict(f0, newdata=.)) %>% 
#   group_by(zone) %>% 
#   summarize(rel_diff = -1 + max(pred)/min(pred))
# 
# d_cdr[sample(.N, 1e5)] %>% 
#   mutate(tod = tod*0.01 + 10) %>% 
#   pull(tod) %>% hist
# d_cdr[sample(.N, 1e5)] %>% 
#   ggplot(data=.,aes(date, sza,color=zone))+
#   geom_smooth()
# d_cdr2[sample(.N, 1e5)] %>% 
#   ggplot(data=.,aes(date, ndvi_anom_sd,color=zone))+
#   geom_smooth()
# 
# d_cdr %>% 
#   filter(between(date, ymd("2000-01-01"),ymd("2010-12-31"))) %>% 
#   select(x,y,sza,ndvi_cdr) %>% 
#   na.omit() %>% 
#   group_by(x,y) %>% 
#   summarize(rho = cor(sza,ndvi_cdr)) %>% 
#   ungroup() %>% 
#   ggplot(data=.,aes(x,y,fill=rho))+
#   geom_tile()+
#   scale_fill_viridis_c()+
#   coord_equal()

# Import MCD64  --------------------------------------------------------
tmp_fire <- stars::read_stars("../data_general/MCD64/MCD64_Oz/MCD64A1_BurnArea_reducedToAVHRRres_2001_2019_.tif",
                         proxy = F) 
tmp_fire <- tmp_fire %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2001-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>% 
  set_names("burn_area")
d_fire <- as_tibble(tmp_fire)
rm(tmp_fire); gc()

# Import MCD43 w/fire & deforestation mask --------------------------------------------------------
tmp <- stars::read_stars("../data_general/MCD43/MCD43A4_ndvi_median_count_stdDev_5000m_EastOz_mMean_maskFireDefor_redRes_2001-01-01_to_2019-12-31.tif",
                         proxy = F) 
tmp_median <- tmp %>% 
  slice('band', seq(1,by=3,length.out = 228)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2001-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>% 
  set_names(c('ndvi_mcd'))
tmp_nobs <- tmp %>% 
  slice('band', seq(2,by=3,length.out = 228)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2001-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>% 
  set_names(c('nobs_mcd'))
tmp_sd <- tmp %>% 
  slice('band', seq(3,by=3,length.out = 228)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2001-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>% 
  set_names(c('sd_mcd'))
tmp <- c(tmp_median, tmp_nobs, tmp_sd)
d_mcd <- tmp %>% as.data.frame() %>% as.data.table()
d_mcd <- merge(d_mcd, as.data.table(kop))
d_mcd <- d_mcd[is.na(zone)==F & is.na(nobs_mcd)==F]
d_mcd <- d_mcd[nobs_mcd>=3]
d_mcd <- d_mcd[sd_mcd/ndvi_mcd <= 0.25]
d_mcd <- d_mcd[,`:=`(month=month(date))]

dc <- merge(d_cdr, d_mcd, by=c("x","y","date"))
rm(tmp); gc()
rm(tmp_median, tmp_nobs, tmp_sd); gc()

# Import MCD43 no mask --------------------------------------------------------
tmp_nm <- stars::read_stars("../data_general/MCD43/MCD43A4_NDVI_5000m_EastOz_mMedian_noMasking_2001-01-01_to_2020-07-30.tif",
                         proxy = F) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2001-01-01"),ymd("2020-07-01"),by="1 month"), 
                    names = 'date') %>% 
  set_names(c('ndvi_mcd_nm'))


junk <- left_join(as_tibble(tmp_nm),d_mcd, by=c("x","y","date")) 
junk %>% select(ndvi_mcd, ndvi_mcd_nm) %>% summary

junk %>% 
  sample_n(10000) %>% 
  filter(ndvi_mcd > 0.1) %>% 
  inner_join(., kop) %>% 
  ggplot(data=., aes(ndvi_mcd_nm, ndvi_mcd))+
  geom_point()+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  facet_wrap(~zone)

junk %>% 
  sample_n(1e6) %>% 
  filter(ndvi_mcd > 0.1) %>% 
  inner_join(., kop) %>% 
  filter(is.na(zone)==F) %>% 
  select(date,zone,ndvi_mcd,ndvi_mcd_nm) %>% 
  gather(-date,-zone,key='key',value='value') %>% 
  ggplot(data=., aes(date,value,color=key))+
  geom_smooth(se=F)+
  scale_color_viridis_d(option='B',end=0.9)+
  facet_wrap(~zone,scales='free')


junk %>% 
  filter(near(x, 150.2866,tol=0.025)) %>% 
  filter(near(y, -33.4949,tol=0.025)) %>% 
  filter(date>=ymd("2015-01-01")) %>% 
  ggplot(data=.,aes(date, ndvi_mcd))+
  geom_point()+
  geom_point(aes(date,ndvi_mcd_nm),col='blue',size=0.5)+
  scale_color_viridis_c(option='B')



junk %>% 
  sample_n(1e6) %>% 
  filter(ndvi_mcd > 0.1) %>% 
  select(date, ndvi_mcd, ndvi_mcd_nm) %>% 
  gather(-date,key='key',value='value') %>% 
  ggplot(data=.,aes(date,value,color=key))+
  # geom_abline(aes(intercept=0,slope=1),col='#cf0000')+
  geom_smooth(se=F)+
  # geom_point()+
  scale_color_viridis_d(option='B',end=0.9,
                        labels=c("ndvi_mcd"='Disturbance Masked', 
                        "ndvi_mcd_nm"="No Masking"))+
  # facet_wrap(~zone,scales='free')+
  theme_linedraw()+
  theme(legend.position = c(1,0), 
        legend.justification = c(1,0))

# Calibrate AVHRR CDR to approximate MCD43 NDVI -----------------------------------------
# This is being done to counteract the effects of changes in solar zenith angle 
# and time of day. 
dc2 <- merge(d_mcd, d_cdr2, by=c("x","y","date","month"),all = TRUE)
dc2 <- merge(dc2, kop %>% select(x,y,zone), all=T, by=c("x","y"))
dc2_train <- dc2[is.na(ndvi_mcd)==F][between(date,ymd("2001-01-01"),ymd("2016-12-31"))==T][sample(.N, 1e6)]
dc2_test <- dc2[is.na(ndvi_mcd)==F][between(date,ymd("2001-01-01"),ymd("2016-12-31"))==T][sample(.N, 1e6)]

mc0 <- bam(ndvi_mcd ~ ndvi_cdr, data=dc2_train)
mc1 <- bam(ndvi_mcd ~ s(ndvi_cdr, bs='cs'), data=dc2_train, 
           discrete=T, 
           select=T)
mc2 <- bam(ndvi_mcd ~ s(ndvi_cdr, bs='cs')+
             s(x,y), data=dc2_train, 
           discrete=T, 
           select=T)
mc3 <- bam(ndvi_mcd ~ s(ndvi_cdr,x,y), data=dc2_train, 
           discrete=T, 
           select=T)
mc4 <- bam(ndvi_mcd ~ s(ndvi_cdr, bs='cs')+
             s(month)+
             s(x,y), data=dc2_train, 
           discrete=T, 
           select=T)
mc5 <- bam(ndvi_mcd ~ s(ndvi_cdr, bs='cs')+
             s(x,y,month), data=dc2_train, 
           discrete=T, 
           select=T)
mc6 <- bam(ndvi_mcd ~ s(ndvi_cdr, sza,x,y)+
                      s(month), data=dc2_train, 
           discrete=T, 
           select=T)
mc7 <- bam(ndvi_mcd ~ s(ndvi_cdr,sza,tod)+
             s(x,y,month), data=dc2_train, 
           discrete=T, 
           select=T)
mc8 <- bam(ndvi_mcd ~ s(ndvi_cdr,sza)+
             s(x,y,month), data=dc2_train, 
           discrete=T, 
           select=T)
mc9 <- bam(ndvi_mcd ~ s(ndvi_cdr, bs='cs')+
             s(month,sza,tod)+
             s(x,y), data=dc2_train, 
           discrete=T, 
           select=T)
mc10 <- bam(ndvi_mcd ~ s(ndvi_cdr,bs='cs')+
             s(month,bs='cs')+
             s(sza,bs='cs')+
             s(tod,bs='cs')+
             te(x,y), data=dc2_train, 
           discrete=T, 
           select=T)

bbmle::AICtab(mc0,mc1,mc2,mc3,mc4,mc5,mc6,mc7,mc8,mc9)
yardstick::rmse_vec(dc2_test$ndvi_mcd, predict(mc0,newdata=dc2_test))
yardstick::rmse_vec(dc2_test$ndvi_mcd, predict(mc1,newdata=dc2_test))
yardstick::rmse_vec(dc2_test$ndvi_mcd, predict(mc2,newdata=dc2_test))
yardstick::rmse_vec(dc2_test$ndvi_mcd, predict(mc3,newdata=dc2_test))
yardstick::rmse_vec(dc2_test$ndvi_mcd, predict(mc4,newdata=dc2_test))
yardstick::rmse_vec(dc2_test$ndvi_mcd, predict(mc5,newdata=dc2_test))
yardstick::rmse_vec(dc2_test$ndvi_mcd, predict(mc6,newdata=dc2_test))
yardstick::rmse_vec(dc2_test$ndvi_mcd, predict(mc7,newdata=dc2_test))
yardstick::rmse_vec(dc2_test$ndvi_mcd, predict(mc8,newdata=dc2_test))
yardstick::rmse_vec(dc2_test$ndvi_mcd, predict(mc9,newdata=dc2_test))
yardstick::rmse_vec(dc2_test$ndvi_mcd, predict(mc10,newdata=dc2_test))


gc()




# Apply Calibration prediction in parallel --------------------------------
library(foreach); library(doParallel)
n_cores <- 4
cl <- makeCluster(n_cores)
registerDoParallel(cl)
gc()
dc2 <- dc2 %>% mutate(year=year(date))
vec_years <- unique(dc2$year) %>% na.omit()

out <- foreach(i = 1:length(vec_years), 
               .packages = c("mgcv","data.table","tidyverse"),
               .combine=rbind) %dopar% {
                 out <- dc2[year==vec_years[i]][is.na(ndvi_cdr)==F] %>% 
                   mutate(ndvi_mcd_pred = predict(mc9, 
                                                  newdata=., 
                                                  newdata.guaranteed = T,
                                                  discrete = TRUE))
                 out
               } 

out[sample(.N, 2e6)] %>% 
  .[date<ymd("2001-01-01")] %>% 
  # filter(is.na(ndvi_mcd)==F) %>% 
  select(date,zone.y,ndvi_mcd, ndvi_mcd_pred,ndvi_cdr) %>% 
  gather(-date, -zone.y, key='key',value='value') %>% 
  ggplot(data=.,aes(date,value,color=key))+
  geom_smooth(method='lm')+
  facet_wrap(~zone.y)

coords_keep <- out[is.na(ndvi_mcd)==F] %>% 
  lazy_dt() %>% 
  group_by(x,y) %>% 
  summarize(nobs_mcd_2 = sum(is.na(ndvi_mcd)==F)) %>% 
  ungroup() %>% 
  as.data.table()


# Plot Annual Intercept & Trend Differences --------------------------------------
creg1 <- merge(coords_keep, out, by=c('x','y')) %>% 
  .[date>= ymd("2001-01-01") & date<= ymd("2016-12-31")] %>% 
  .[nobs_mcd_2 >= 100] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val_cdr=mean(ndvi_cdr,na.rm=TRUE), 
        val_mcd=mean(ndvi_mcd,na.rm=TRUE), 
        val_mcd_pred = mean(ndvi_mcd_pred,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,.(beta_cdr = list(unname(fastLm(
    X = cbind(1,year-2001), 
    y=val_cdr, data=.SD)$coefficients)), 
    beta_mcd = list(unname(fastLm(
      X = cbind(1,year-2001), 
      y=val_mcd, data=.SD)$coefficients)), 
    beta_cdr_pred = list(unname(fastLm(
      X = cbind(1,year-2001), 
      y=val_mcd_pred, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0_cdr=unlist(beta_cdr)[1], b1_cdr=unlist(beta_cdr)[2], 
          b0_mcd=unlist(beta_mcd)[1], b1_mcd=unlist(beta_mcd)[2], 
          b0_mcd_pred=unlist(beta_cdr_pred)[1], b1_mcd_pred=unlist(beta_cdr_pred)[2]), by=.(x,y)]

creg1 %>% 
  select(b0_mcd, b0_mcd_pred, b0_cdr) %>% 
  gather(-b0_mcd, key='key',value='value') %>% 
  ggplot(data=.,aes(b0_mcd, value, color=key))+
  geom_point(size=0.25,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  labs(x='Intercept MCD NDVI', 
       y='Intercept', 
       title='2001-2016',
       caption='Masked MCD43')+
  scale_color_viridis_d("",
                        option='B',end=0.8, 
                        labels=c("b0_cdr"="Intercept AVHRR CDR", 
                                 "b0_mcd_pred"="Intercept AVHRR CDR calibrated to MCD"))+
  theme_linedraw()+
  theme(legend.position = c(0.99,0.01), 
        legend.justification = c(0.99,0.01))
ggsave(filename = "figures/compare_lm_intercept_mcd_avhrr_recalibAvhrr.png", 
       width=15, height=12, units='cm')

creg1 %>% 
  select(b1_mcd, b1_mcd_pred, b1_cdr) %>% 
  gather(-b1_mcd, key='key',value='value') %>% 
  ggplot(data=.,aes(b1_mcd, value, color=key))+
  geom_point(size=0.25,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  labs(x='Trend MCD NDVI', 
       y='Trend', 
       subtitle = '2001-2016', 
      caption = 'Only 5x5 km grid cells with >= 100 masked MCD observations')+
  scale_color_viridis_d("",
                        option='B',end=0.8, 
                        labels=c("b1_cdr"="Trend AVHRR CDR", 
                                 "b1_mcd_pred"="Trend AVHRR CDR calibrated to MCD"))+
  theme_linedraw()+
  theme(legend.position = c(0.99,0.01), 
        legend.justification = c(0.99,0.01))
ggsave(filename = "figures/compare_lm_trend_mcd_avhrr_recalibAvhrr.png", 
       width=15, height=12, units='cm')

# Plot Seasonal Intercept & Trend Differences --------------------------------------
creg2 <- merge(coords_keep, out, by=c('x','y')) %>% 
  .[date>= ymd("2001-01-01") & date<= ymd("2016-12-31")] %>% 
  .[nobs_mcd_2 >= 100] %>% 
  mutate(month=month(date), 
         season = case_when(month%in%c(1,2,12)~'DJF',
                            month%in%c(3,4,5)~'MAM',
                            month%in%c(6,7,8)~'JJA',
                            month%in%c(9,10,11)~'SON')) %>% 
  mutate(season = factor(season,levels = c("SON","DJF","MAM","JJA"), 
                         ordered=TRUE)) %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val_cdr=mean(ndvi_cdr,na.rm=TRUE), 
        val_mcd=mean(ndvi_mcd,na.rm=TRUE), 
        val_mcd_pred = mean(ndvi_mcd_pred,na.rm=TRUE)), 
    keyby=.(x,y,year,season)] %>% 
  .[,.(beta_cdr = list(unname(fastLm(
    X = cbind(1,year-2001), 
    y=val_cdr, data=.SD)$coefficients)), 
    beta_mcd = list(unname(fastLm(
      X = cbind(1,year-2001), 
      y=val_mcd, data=.SD)$coefficients)), 
    beta_cdr_pred = list(unname(fastLm(
      X = cbind(1,year-2001), 
      y=val_mcd_pred, data=.SD)$coefficients))), 
    by=.(x,y,season)] %>% 
  .[,`:=`(b0_cdr=unlist(beta_cdr)[1], b1_cdr=unlist(beta_cdr)[2], 
          b0_mcd=unlist(beta_mcd)[1], b1_mcd=unlist(beta_mcd)[2], 
          b0_mcd_pred=unlist(beta_cdr_pred)[1], b1_mcd_pred=unlist(beta_cdr_pred)[2]), by=.(x,y)]

creg2 %>% 
  select(season,b0_mcd, b0_mcd_pred, b0_cdr) %>% 
  gather(-season,-b0_mcd, key='key',value='value') %>% 
  ggplot(data=.,aes(b0_mcd, value, color=key))+
  geom_point(size=0.25,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  labs(x='Intercept MCD NDVI', 
       y='Intercept', 
       caption='Masked MCD43')+
  scale_color_viridis_d("",
                        option='B',end=0.8, 
                        labels=c("b0_cdr"="Intercept AVHRR CDR", 
                                 "b0_mcd_pred"="Intercept AVHRR CDR calibrated to MCD"))+
  facet_wrap(~season)+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        legend.justification = c(0.99,0.01))
ggsave(filename = "figures/compare_lm_seasonal_intercept_mcd_avhrr_recalibAvhrr.png", 
       width=15, height=12, units='cm')

creg2 %>% 
  select(season,b1_mcd, b1_mcd_pred, b1_cdr) %>% 
  gather(-season,-b1_mcd, key='key',value='value') %>% 
  ggplot(data=.,aes(b1_mcd, value, color=key))+
  geom_point(size=0.5,alpha=0.15)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  labs(x='Trend MCD NDVI', 
       y='Trend', 
       subtitle = '2001-2016', 
       caption = 'Only 5x5 km grid cells with >= 100 masked MCD43 observations')+
  scale_color_viridis_d("",
                        option='B',end=0.8, 
                        labels=c("b1_cdr"="Trend AVHRR CDR", 
                                 "b1_mcd_pred"="Trend AVHRR CDR calibrated to MCD"))+
  facet_wrap(~season)+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        legend.justification = c(0.99,0.01))
ggsave(filename = "figures/compare_lm_seasonal_trend_mcd_avhrr_recalibAvhrr.png", 
       width=15, height=12, units='cm')

# Calibration model for AVHRR CDR -----------------------------------------
# Model fitting data is filtered for relatively high quality & consistency
d_calib <- dc %>% 
  .[nobs >=3 & sd < 0.05] %>% 
  .[nobs_mcd >=3 & sd_mcd < 0.05] %>% 
  .[ndvi_cdr > 0.1 & ndvi_cdr < 1] %>% 
  .[date>= ymd("2001-01-01") & date<= ymd("2016-12-31")]
d_calib %>% dim
d_train <- d_calib %>% sample_n(1e6)
d_test <- anti_join(d_calib, d_train, by=c("x","y","date","month")) %>% 
  sample_n(1e6)

cm0 <- bam(ndvi_cdr ~ ndvi_mcd, data=d_train, 
           discrete=T)
cm1 <- bam(ndvi_cdr ~ s(ndvi_mcd, bs='cs'), data=d_train, 
           discrete=T, 
           select=T)
cm2 <- bam(ndvi_cdr ~ s(ndvi_mcd, bs='cs')+
                      s(x,y), data=d_train, 
           discrete=T, 
           select=T)
cm3 <- bam(ndvi_cdr ~ s(ndvi_mcd,x,y), data=d_train, 
           discrete=T, 
           select=T)
cm4 <- bam(ndvi_cdr ~ s(ndvi_mcd, bs='cs')+
             s(month)+
             s(x,y), data=d_train, 
           discrete=T, 
           select=T)
cm5 <- bam(ndvi_cdr ~ s(ndvi_mcd, bs='cs')+
             s(x,y,month), data=d_train, 
           discrete=T, 
           select=T)

bbmle::AICtab(cm0, cm1, cm2, cm3, cm4)
mgcViz::getViz(cm0) %>% plot(., allTerms=T)
mgcViz::getViz(cm1) %>% plot(., allTerms=T)
mgcViz::getViz(cm2) %>% plot(., allTerms=T)
mgcViz::getViz(cm3) %>% plot(., allTerms=T)
mgcViz::getViz(cm4) %>% plot(., allTerms=T)

yardstick::rmse_vec(d_train$ndvi_cdr, predict(cm0,newdata=d_train))
yardstick::rmse_vec(d_train$ndvi_cdr, predict(cm1,newdata=d_train))
yardstick::rmse_vec(d_train$ndvi_cdr, predict(cm2,newdata=d_train))
yardstick::rmse_vec(d_train$ndvi_cdr, predict(cm3,newdata=d_train))
yardstick::rmse_vec(d_train$ndvi_cdr, predict(cm4,newdata=d_train))
yardstick::rmse_vec(d_train$ndvi_cdr, predict(cm5,newdata=d_train))

system.time(
dc <- dc %>% 
  mutate(ndvi_cdr_pred = predict(cm4, newdata=., newdata.guaranteed = T, discrete = TRUE))
)

jc <- dc %>% 
  .[date>= ymd("2001-01-01") & date<= ymd("2016-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val_cdr=mean(ndvi_cdr,na.rm=TRUE), 
        val_mcd=mean(ndvi_mcd,na.rm=TRUE), 
        val_cdr_pred = mean(ndvi_cdr_pred,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,.(beta_cdr = list(unname(fastLm(
    X = cbind(1,year-2001), 
    y=val_cdr, data=.SD)$coefficients)), 
    beta_mcd = list(unname(fastLm(
      X = cbind(1,year-2001), 
      y=val_mcd, data=.SD)$coefficients)), 
    beta_cdr_pred = list(unname(fastLm(
      X = cbind(1,year-2001), 
      y=val_cdr_pred, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0_cdr=unlist(beta_cdr)[1], b1_cdr=unlist(beta_cdr)[2], 
          b0_mcd=unlist(beta_mcd)[1], b1_mcd=unlist(beta_mcd)[2], 
          b0_cdr_pred=unlist(beta_cdr_pred)[1], b1_cdr_pred=unlist(beta_cdr_pred)[2]), by=.(x,y)]


jc[b0_mcd>0.1 & b0_cdr>0.1] %>% 
  ggplot(data=.,aes(b0_mcd,b0_cdr))+
  geom_point(size=0.5,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')

jc[b0_mcd>0.1 & b0_cdr>0.1] %>% 
  .[b1_mcd> -0.05 & b1_mcd<0.05] %>% 
  .[b1_cdr> -0.05 & b1_cdr<0.05] %>% 
  ggplot(data=.,aes(b1_mcd,b1_cdr))+
  geom_point(size=0.5,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')


jc[b0_mcd>0.1 & b0_cdr>0.1] %>% 
  .[b1_mcd> -0.05 & b1_mcd<0.05] %>% 
  .[b1_cdr> -0.05 & b1_cdr<0.05] %>% 
  ggplot(data=.,aes(b0_cdr_pred,b0_cdr))+
  geom_point(size=0.5,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')
jc[b0_mcd>0.1 & b0_cdr>0.1] %>% 
  .[b1_mcd> -0.05 & b1_mcd<0.05] %>% 
  .[b1_cdr> -0.05 & b1_cdr<0.05] %>% 
  ggplot(data=.,aes(b1_cdr_pred,b1_cdr))+
  geom_point(size=0.5,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')

# filtering for AVHRR
jc_1 <- dc %>% 
  .[nobs >=3 & sd < 0.05] %>% 
  .[ndvi_cdr > 0.1 & ndvi_cdr < 1] %>% 
  .[date>= ymd("2001-01-01") & date<= ymd("2016-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val_cdr=mean(ndvi_cdr,na.rm=TRUE), 
        val_mcd=mean(ndvi_mcd,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[val_cdr < 1 & val_mcd <1] %>% 
  .[val_cdr > 0.1 & val_mcd >0.1] %>% 
  .[,.(beta_cdr = list(unname(fastLm(
    X = cbind(1,year-2001), 
    y=val_cdr, data=.SD)$coefficients)), 
    beta_mcd = list(unname(fastLm(
      X = cbind(1,year-2001), 
      y=val_mcd, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0_cdr=unlist(beta_cdr)[1], b1_cdr=unlist(beta_cdr)[2], 
          b0_mcd=unlist(beta_mcd)[1], b1_mcd=unlist(beta_mcd)[2]), by=.(x,y)]

jc_1[b0_mcd>0.1 & b0_cdr>0.1] %>% 
  .[b1_mcd> -0.05 & b1_mcd<0.05] %>% 
  .[b1_cdr> -0.05 & b1_cdr<0.05] %>% 
  ggplot(data=.,aes(b0_mcd,b0_cdr))+
  geom_point(size=0.5,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')

jc_1[b0_mcd>0.1 & b0_cdr>0.1] %>% 
  .[b1_mcd> -0.05 & b1_mcd<0.05] %>% 
  .[b1_cdr> -0.05 & b1_cdr<0.05] %>% 
  ggplot(data=.,aes(b1_mcd,b1_cdr))+
  geom_point(size=0.5,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')


# filtering for both AVHRR & MODIS
jc_2 <- dc %>% 
  .[nobs >=3 & sd < 0.05] %>% 
  .[nobs_mcd >=3 & sd_mcd < 0.05] %>% 
  .[ndvi_cdr > 0.1 & ndvi_cdr < 1] %>% 
  .[date>= ymd("2001-01-01") & date<= ymd("2016-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val_cdr=mean(ndvi_cdr,na.rm=TRUE), 
        val_mcd=mean(ndvi_mcd,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[val_cdr < 1 & val_mcd <1] %>% 
  .[val_cdr > 0.1 & val_mcd >0.1] %>% 
  .[,.(beta_cdr = list(unname(fastLm(
    X = cbind(1,year-2001), 
    y=val_cdr, data=.SD)$coefficients)), 
    beta_mcd = list(unname(fastLm(
      X = cbind(1,year-2001), 
      y=val_mcd, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0_cdr=unlist(beta_cdr)[1], b1_cdr=unlist(beta_cdr)[2], 
          b0_mcd=unlist(beta_mcd)[1], b1_mcd=unlist(beta_mcd)[2]), by=.(x,y)]

jc_2[b0_mcd>0.1 & b0_cdr>0.1] %>% 
  .[b1_mcd> -0.05 & b1_mcd<0.05] %>% 
  .[b1_cdr> -0.05 & b1_cdr<0.05] %>% 
  ggplot(data=.,aes(b0_mcd,b0_cdr))+
  geom_point(size=0.5,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')

jc_2[b0_mcd>0.1 & b0_cdr>0.1] %>% 
  .[b1_mcd> -0.05 & b1_mcd<0.05] %>% 
  .[b1_cdr> -0.05 & b1_cdr<0.05] %>% 
  ggplot(data=.,aes(b1_mcd,b1_cdr))+
  geom_point(size=0.5,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')


# no pre-processing
f_0 <- jc[b0_mcd>0.1 & b0_cdr>0.1] %>% 
  .[b1_mcd> -0.05 & b1_mcd<0.05] %>% 
  .[b1_cdr> -0.05 & b1_cdr<0.05] %>%
  lm(b1_mcd~b1_cdr, data=.)
f_0 %>% summary

f_1 <- jc_1[b0_mcd>0.1 & b0_cdr>0.1] %>% 
  .[b1_mcd> -0.05 & b1_mcd<0.05] %>% 
  .[b1_cdr> -0.05 & b1_cdr<0.05] %>% 
  lm(b1_mcd~b1_cdr, data=.)
f_1 %>% summary


#
#
#





























dat_p33 <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_CDRv5_ndvi_p33__5000m_EastOz_1981_2019.tif",
                            proxy = F) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1981-08-01"),ymd("2019-11-01"),by="1 month"), 
                    names = 'date') %>% 
    set_names(c('ndvi_p33'))
dat_cdr <- c(dat_sd, dat_p33) %>% as_tibble() %>% as.data.table()
dat_cdr <- merge(dat_cdr, as.data.table(kop))


# Import NOAA AVHRR CDR  ------------------------------------------------------
dat_red <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif",
                             proxy=F) %>%
  slice('band', seq(1,by=2,length.out = 456)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1982-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>% 
  set_names(c("x","y","date","red"))
dat_nir <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif", 
                             proxy=F) %>%
  slice('band', seq(2,by=2,length.out = 456)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1982-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>% 
  set_names(c("x","y","date","nir"))
gc() 
dat_red <- dat_red[is.na(red)==F]
dat_nir <- dat_nir[is.na(nir)==F]
gc()
dat_cdr <- dat_red[dat_nir,on=.(x,y,date)]
dat_cdr <- dat_cdr[,`:=`(ndvi_cdr=(nir-red)/(nir+red))] %>%
  .[,`:=`(year=year(date))]
dat_cdr <- dat_cdr %>% lazy_dt() %>%
  filter(is.na(ndvi_cdr)==F & red > 0 & nir > 0) %>%
  as.data.table()
rm(dat_red, dat_nir); gc()




dat_g <- stars::read_stars("../data_general/Oz_misc_data/GIMMS_ndvi_5000m_EastOz_1981_2013.tif", 
                            proxy = F) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1981-07-01"),ymd("2013-12-01"),by="1 month"),
                    names = 'date') %>% 
  as_tibble() %>% 
  set_names(c("x","y","date","ndvi_g")) %>% 
  filter(between(ndvi_g,0,1))

# MCD43  ------------------------------------------------------------
o_mcd <- stars::read_stars("../data_general/MCD43/MCD43A4_NDVI_5000m_EastOz_mMean_maskFireDefor_2001-01-01_to_2020-07-30.tif", 
                  proxy=F) %>% 
         st_set_dimensions(., 3, 
                  values=seq(ymd("2001-01-01"),ymd("2020-07-01"),by="1 month"), 
                  names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>%
  set_names(c("x","y","date","ndvi"))

o1 <- stars::read_stars("../data_general/MCD43/MCD43A4_red_nir_5000m_EastOz_mMean_maskFireDefor_2001-01-01_to_2020-07-30.tif", 
                        proxy=F) %>% 
  slice('band', seq(1,by=2,to = 470)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2001-01-01"),ymd("2020-07-01"),by="1 month"), 
                    names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>%
  set_names(c("x","y","date","nir"))
o2 <- stars::read_stars("../data_general/MCD43/MCD43A4_red_nir_5000m_EastOz_mMean_maskFireDefor_2001-01-01_to_2020-07-30.tif", 
                        proxy=F) %>% 
  slice('band', seq(2,by=2,to = 470)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2001-01-01"),ymd("2020-07-01"),by="1 month"), 
                    names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>%
  set_names(c("x","y","date","red"))

o <- o1[o2,on=.(x,y,date)]
rm(o1,o2); gc()

o <- o[,`:=`(ndvi_mcd=(nir-red)/(nir+red))]
o <- o[,`:=`(evi2_mcd=2.5*(nir-red)/(nir+2.4*red+1))]
o <- o[,`:=`(nirv_mcd=ndvi_mcd*nir)]
o <- o[,`:=`(year=year(date))]
o <- o %>% lazy_dt() %>% 
  filter(is.na(ndvi_mcd)==F & red > 0 & nir > 0) %>% 
  as.data.table()
dat_mcd <- o %>% lazy_dt() %>% select(x,y,date,ndvi_mcd,evi2_mcd,nirv_mcd) %>% as.data.table()
# END MCD15 FPAR ***************************************************************

# # Import MOD13A2 Terra -----------------------------------------------------------
o1 <- stars::read_stars("../data_general/MOD13A2/MOD13A2_SR_5km_EastOz_mmean_NVISmask_2003_2019.tif", 
                        proxy = F) %>%
  slice('band', seq(1,by=2,to = 408)) %>%
  st_set_dimensions(., 3,
                    values=seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"),
                    names = 'date') %>%
  as_tibble() %>%
  as.data.table() %>%
  set_names(c("x","y","date","red"))
o2 <- stars::read_stars("../data_general/MOD13A2/MOD13A2_SR_5km_EastOz_mmean_NVISmask_2003_2019.tif", 
                        proxy=F) %>%
  slice('band', seq(2,by=2,to = 408)) %>%
  st_set_dimensions(., 3,
                    values=seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"),
                    names = 'date') %>%
  as_tibble() %>%
  as.data.table() %>%
  set_names(c("x","y","date","nir"))

o <- o1[o2,on=.(x,y,date)]
rm(o1,o2); gc()

o <- o[,`:=`(ndvi_mod=(nir-red)/(nir+red))] %>%
     .[,`:=`(year=year(date))]
o <- o %>% lazy_dt() %>%
  filter(is.na(ndvi_mod)==F & red > 0 & nir > 0) %>%
  as.data.table()
mod <- o %>% lazy_dt() %>% select(x,y,date,ndvi_mod) %>% as.data.table()


# # MYD13A2 Aqua ------------------------------------------------------------
o1 <- stars::read_stars("../data_general/MYD13A2/MYD13A2_SR_5km_EastOz_mmean_NVISmask_2003_2019.tif", 
                        proxy=F) %>%
  slice('band', seq(1,by=2,to = 408)) %>%
  st_set_dimensions(., 3,
                    values=seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"),
                    names = 'date') %>%
  as_tibble() %>%
  as.data.table() %>%
  set_names(c("x","y","date","red"))
o2 <- stars::read_stars("../data_general/MYD13A2/MYD13A2_SR_5km_EastOz_mmean_NVISmask_2003_2019.tif", 
                        proxy=F) %>%
  slice('band', seq(2,by=2,to = 408)) %>%
  st_set_dimensions(., 3,
                    values=seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"),
                    names = 'date') %>%
  as_tibble() %>%
  as.data.table() %>%
  set_names(c("x","y","date","nir"))
o <- o1[o2,on=.(x,y,date)]
rm(o1,o2); gc()

o <- o[,`:=`(ndvi_myd=(nir-red)/(nir+red))]
o <- o[,`:=`(year=year(date))]
o <- o %>% lazy_dt() %>%
  filter(is.na(ndvi_myd)==F & red > 0 & nir > 0) %>%
  as.data.table()
myd <- o %>% lazy_dt() %>% select(x,y,date,ndvi_myd) %>% as.data.table()


vv  <- inner_join(out, dat_g, by=c("x","y","date"))

vv %>% 
  sample_n(10000) %>% 
  ggplot(data=.,aes(ndvi_mcd, ndvi_g))+
  geom_point()+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  labs(x='mcd',y='gimms')

vv %>% 
  sample_n(1000) %>% 
  ggplot(data=.,aes(ndvi, ndvi_g))+
  geom_point()+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  labs(x='avhrr cdr',y='gimms')


rr  <- inner_join(dat_g1, dat_g2, by=c("x","y","date"),suffix=c("_o","_m"))

rr %>% 
  sample_n(10000) %>% 
  ggplot(data=.,aes(ndvi_g_o, ndvi_g_m))+
  geom_point()+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  labs(x='original',y='masked')

rr %>% 
  sample_n(10000) %>% 
  select(ndvi_g_o, ndvi_g_m) %>% 
  summary

nobs <- vv %>% 
  filter(between(date,ymd("2001-01-01"),ymd("2013-12-31"))) %>% 
  group_by(x,y) %>% 
  summarize(nobs_g = sum(is.na(ndvi_g)==F), 
            nobs_m = sum(is.na(ndvi_mcd)==F)) %>% 
  ungroup()
jm <- vv %>%
  as.data.table() %>% 
  .[date>= ymd("2003-01-01") & date<= ymd("2013-12-31")] %>% 
  # .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
  .[is.na(ndvi_mcd)==F] %>% 
  .[, .(val=mean(ndvi_mcd,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,.(beta = list(unname(fastLm(
    X = cbind(1,year-2001), 
    y=val, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]

jg <- dat_g %>% 
  as.data.table() %>% 
  merge(., kop, by=c("x","y")) %>% 
  .[date>= ymd("2003-01-01") & date<= ymd("2013-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val=mean(ndvi_g,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,.(beta = list(unname(fastLm(
    X = cbind(1,year-2001), 
    y=val, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]

jmod <- mod %>% 
  as.data.table() %>% 
  merge(., kop, by=c("x","y")) %>% 
  .[date>= ymd("2003-01-01") & date<= ymd("2013-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[is.na(ndvi_mod)==F] %>% 
  .[, .(val=mean(ndvi_mod,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,.(beta = list(unname(fastLm(
    X = cbind(1,year-2003), 
    y=val, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]

jmyd <- myd %>% 
  as.data.table() %>% 
  merge(., kop, by=c("x","y")) %>% 
  .[date>= ymd("2003-01-01") & date<= ymd("2013-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[is.na(ndvi_myd)==F] %>% 
  .[, .(val=mean(ndvi_myd,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,.(beta = list(unname(fastLm(
    X = cbind(1,year-2003), 
    y=val, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]

jmcd <- dat_mcd %>% 
  merge(., kop, by=c("x","y")) %>% 
  .[date>= ymd("2003-01-01") & date<= ymd("2013-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[is.na(ndvi_mcd)==F] %>% 
  .[, .(val=mean(ndvi_mcd,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,.(beta = list(unname(fastLm(
    X = cbind(1,year-2003), 
    y=val, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]

jcdr <- dat_cdr[sd < 0.05] %>% 
  merge(., kop, by=c("x","y")) %>% 
  .[date>= ymd("2003-01-01") & date<= ymd("2013-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val=mean(ndvi_p33,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,.(beta = list(unname(fastLm(
    X = cbind(1,year-2001), 
    y=val, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]


jg$b1 %>% summary
jmod$b1 %>% summary
jmyd$b1 %>% summary
jmcd$b1 %>% summary
jcdr$b1 %>% summary

# compare CDR with MCD
merge(jmcd,jcdr,suffix=c("_mcd","_cdr")) %>% 
  inner_join(., kop) %>% 
  # inner_join(., nobs) %>% 
  # filter(nobs_m > 80) %>% 
  # filter(nobs_g > 80) %>% 
  filter(between(b1_mcd,-0.025,0.025)) %>% 
  filter(between(b1_cdr,-0.025,0.025)) %>% 
  ggplot(data=.,aes(b0_mcd,b0_cdr))+
  geom_point(size=0.1,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  geom_smooth(color='black')+
  scale_color_viridis_c()

merge(jg,jcdr,suffix=c("_g","_c")) %>% 
  inner_join(., kop) %>% 
  # inner_join(., nobs) %>% 
  # filter(nobs_m > 80) %>% 
  # filter(nobs_g > 80) %>% 
  filter(between(b1_g,-0.01,0.01)) %>% 
  filter(between(b1_c,-0.01,0.01)) %>% 
  ggplot(data=.,aes(b0_g,b0_c))+
  geom_point(size=0.1,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  scale_color_viridis_c()

# compare GIMMS with CDR
merge(jg,jcdr,suffix=c("_g","_c")) %>% 
  inner_join(., kop) %>% 
  # inner_join(., nobs) %>% 
  # filter(nobs_m > 80) %>% 
  # filter(nobs_g > 80) %>% 
  filter(between(b1_g,-0.025,0.025)) %>% 
  filter(between(b1_c,-0.025,0.025)) %>% 
  ggplot(data=.,aes(b1_g,b1_c))+
  geom_point(size=0.1,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  geom_smooth(color='black')+
  scale_color_viridis_c()

merge(jg,jcdr,suffix=c("_g","_c")) %>% 
  inner_join(., kop) %>% 
  ggplot(data=.,aes(x,y,fill=b0_g-b0_c))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(limits=c(-0.25,0.25),oob=scales::squish)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='gray50'),
        panel.grid = element_blank())

merge(jg,jcdr,suffix=c("_g","_c")) %>% 
  inner_join(., kop) %>% 
  ggplot(data=.,aes(x,y,fill=b1_g-b1_c))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(limits=c(-0.01,0.01),oob=scales::squish)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='gray50'),
        panel.grid = element_blank())

merge(jm,ja,suffix=c("_m","_a")) %>% 
  inner_join(., vc) %>% 
  inner_join(., nobs) %>% 
  filter(nobs_m > 80) %>% 
  filter(nobs_g > 80) %>% 
  filter(between(b1_m,-0.01,0.01)) %>% 
  filter(between(b1_a,-0.01,0.01)) %>% 
  ggplot(data=.,aes(b0_a,b0_m))+
  geom_point(size=0.1,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  scale_color_viridis_c()
merge(jm,ja,suffix=c("_m","_a")) %>% 
  inner_join(., vc) %>% 
  inner_join(., nobs) %>% 
  filter(nobs_m > 80) %>% 
  filter(nobs_g > 80) %>% 
  filter(between(b1_m,-0.01,0.01)) %>% 
  filter(between(b1_a,-0.01,0.01)) %>% 
  ggplot(data=.,aes(b1_a,b1_m))+
  geom_point(size=0.1,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  scale_color_viridis_c()

merge(jmod,jmyd,suffix=c("_t","_a")) %>% 
  inner_join(., jg %>% rename(b0_g=b0, b1_g=b1)) %>% 
  # inner_join(., vc) %>% 
  # inner_join(., nobs) %>% 
  # filter(nobs_m > 80) %>% 
  # filter(nobs_g > 80) %>% 
  filter(between(b1_t,-0.03,0.03)) %>%
  filter(between(b1_a,-0.03,0.03)) %>%
  ggplot(data=.,aes(b1_a,b1_t))+
  geom_point(size=0.1,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  scale_color_viridis_c()+
  labs(x="Aqua",y='Terra', title='MODIS platform trend comparison')+
  theme_linedraw()


merge(jmod,jmyd,suffix=c("_t","_a")) %>% 
  as_tibble() %>% 
  inner_join(., jg %>% rename(b0_g=b0, b1_g=b1)) %>% 
  select(contains("b0")) %>% cor

merge(jmod,jmyd,suffix=c("_t","_a")) %>% 
  as_tibble() %>% 
  inner_join(., jg %>% rename(b0_g=b0, b1_g=b1)) %>% 
  select(contains("b1")) %>% cor

merge(jmod,jmyd,suffix=c("_t","_a")) %>% 
  inner_join(., jg %>% rename(b0_g=b0, b1_g=b1)) %>% 
  # inner_join(., vc) %>% 
  # inner_join(., nobs) %>% 
  # filter(nobs_m > 80) %>% 
  # filter(nobs_g > 80) %>% 
  filter(between(b1_t,-0.03,0.03)) %>%
  filter(between(b1_a,-0.03,0.03)) %>%
  ggplot(data=.,aes(b1_g,b1_t))+
  geom_point(size=0.1,alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  scale_color_viridis_c()+
  labs(x="Aqua",y='Terra', title='MODIS platform trend comparison')+
  theme_linedraw()

jmod$b0 %>% sum
jmyd$b0 %>% sum
jmod$b0 %>% sd
jmyd$b0 %>% sd

merge(jmod,jmyd,suffix=c("_t","_a")) %>% 
  select(b1_t, b1_a) %>% cor

library(mgcv); library(mgcViz); library(gratia)
as_g <- dat_g %>% 
  as.data.table() %>% 
  merge(., kop, by=c("x","y")) %>% 
  .[date>= ymd("2003-01-01") & date<= ymd("2013-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val=mean(ndvi_g,na.rm=TRUE)), 
    keyby=.(x,y,zone,year)] %>% 
  .[,`:=`(year=year-2003)]
fg <- bam(val~s(x,y,by=year), data=as_g, 
          discrete = T)

mgcViz::getViz(fg) %>% plot
v_fg <- mgcViz::getViz(fg)


as_mt <- mod %>% 
  as.data.table() %>% 
  merge(., kop, by=c("x","y")) %>% 
  .[date>= ymd("2003-01-01") & date<= ymd("2013-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val=mean(ndvi_mod,na.rm=TRUE)), 
    keyby=.(x,y,zone,year)] %>% 
  .[,`:=`(year=year-2003)]
fmod <- bam(val~s(x,y,by=year), data=as_mt, 
          discrete = T)
mgcViz::getViz(fmod) %>% plot
v_fmod <- mgcViz::getViz(fmod)

as_ma <- myd %>% 
  as.data.table() %>% 
  merge(., kop, by=c("x","y")) %>% 
  .[date>= ymd("2003-01-01") & date<= ymd("2013-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val=mean(ndvi_myd,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,`:=`(year=year-2003)]
fmyd <- bam(val~s(x,y,by=year), data=as_ma, 
          discrete = T)
mgcViz::getViz(fmyd) %>% plot
v_fmyd <- mgcViz::getViz(fmyd)

as_cdr <- dat_cdr %>% 
  as.data.table() %>% 
  merge(., kop, by=c("x","y")) %>% 
  .[date>= ymd("2003-01-01") & date<= ymd("2013-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val=mean(ndvi_cdr,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,`:=`(year=year-2003)]
fcdr <- bam(val~s(x,y,by=year), data=as_cdr, 
            discrete = T)
mgcViz::getViz(fcdr) %>% plot
v_fcdr <- mgcViz::getViz(fcdr)

inner_join(
  gratia::evaluate_smooth(fg,"s(x,y"),
  gratia::evaluate_smooth(fcdr,"s(x"), 
  by=c("x","y","smooth","by_variable"), 
  suffix=c("_gimms","_cdr")) %>% 
  ggplot(data=.,aes(x,y,fill=est_gimms-est_cdr))+
  geom_tile()+
  scale_fill_gradient2()+
  coord_equal()

inner_join(
gratia::evaluate_smooth(fg,"s(x,y"),
gratia::evaluate_smooth(fmod,"s(x"), 
by=c("x","y","smooth","by_variable"), 
suffix=c("_gimms","_mod")) %>% 
  ggplot(data=.,aes(x,y,fill=est_gimms-est_mod))+
  geom_tile()+
  scale_fill_gradient2()+
  coord_equal()

inner_join(
  gratia::evaluate_smooth(fmod,"s(x,y"),
  gratia::evaluate_smooth(fmyd,"s(x"), 
  by=c("x","y","smooth","by_variable"), 
  suffix=c("_mod","_myd")) %>% 
  ggplot(data=.,aes(x,y,fill=est_mod-est_myd))+
  geom_tile()+
  scale_fill_gradient2()+
  coord_equal()



summary(fg)
summary(fmod)
summary(fmyd)





o_mcd[is.na(ndvi)==F] %>% 
  inner_join(mod) %>% 
  sample_n(1e5) %>% 
  ggplot(data=.,aes(ndvi, ndvi_mod))+
  geom_point()+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')

o_mcd[is.na(ndvi)==F] %>% 
  inner_join(mod) %>% 
  inner_join(kop) %>% 
  sample_n(2e5) %>% 
  mutate(month=month(date)) %>% 
  ggplot(data=.,aes(ndvi, ndvi_mod, color=zone))+
  geom_point(size=0.5)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm',se=F)+
  # geom_smooth(method='lm',inherit.aes = F,aes(ndvi,ndvi_myd))+
  scale_fill_viridis_d(end=0.9)+
  facet_grid(zone~month)

o_mcd[is.na(ndvi)==F] %>% 
  inner_join(myd) %>% 
  inner_join(kop) %>% 
  sample_n(2e5) %>% 
  mutate(month=month(date)) %>% 
  ggplot(data=.,aes(ndvi, ndvi_myd, color=zone))+
  geom_point(size=0.5)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm',se=F)+
  # geom_smooth(method='lm',inherit.aes = F,aes(ndvi,ndvi_myd))+
  scale_fill_viridis_d(end=0.9)+
  facet_grid(zone~month)

o_mcd[is.na(ndvi)==F] %>% 
  inner_join(dat_g) %>% 
  inner_join(kop) %>% 
  sample_n(2e5) %>% 
  mutate(month=month(date)) %>% 
  ggplot(data=.,aes(ndvi, ndvi_g, color=zone))+
  geom_point(size=0.5)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm',se=F)+
  # geom_smooth(method='lm',inherit.aes = F,aes(ndvi,ndvi_myd))+
  scale_fill_viridis_d(end=0.9)+
  facet_grid(zone~month)

o_mcd[is.na(ndvi)==F] %>% 
  inner_join(dat_cdr) %>% 
  inner_join(kop) %>% 
  sample_n(2e5) %>% 
  filter(sd < 0.05) %>% 
  mutate(month=month(date)) %>% 
  ggplot(data=.,aes(ndvi, ndvi_p33, color=sd))+
  geom_point(size=0.5)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm',se=F)+
  # geom_smooth(method='lm',inherit.aes = F,aes(ndvi,ndvi_myd))+
  scale_x_continuous(limits=c(0,1))+
  scale_y_continuous(limits=c(0,1))+
  scale_color_viridis_c(end=0.9)+
  # stat_regline_equation(label.x = 0, label.y = 0.8, color='black')+
  facet_grid(zone~month)


dat_cdr[sd < 0.05] %>% 
  .[date>= ymd("2003-01-01") & date<= ymd("2013-12-31")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val=mean(ndvi_cdr,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,`:=`(year=year-2003)]



# joint comparison of 
dcp <- dc %>% lazy_dt() %>% 
  mutate(ndvi_hyb = coalesce(ndvi_cdr_pred, ndvi_cdr)) %>% 
  as.data.table()


dcp <- bind_rows(
  d_cdr %>% 
  .[nobs >=3 & sd <= 0.05] %>% 
  .[date>= ymd("1981-09-01") & date<= ymd("2000-12-30")] , 
  dc %>% 
  .[nobs_mcd >=3 & sd_mcd <= 0.05] %>% 
  .[date>= ymd("1981-09-01") & date<= ymd("2019-09-30")] %>% 
  select(x,y,date,zone,ndvi_cdr,ndvi_cdr_pred)) %>% 
  lazy_dt() %>% 
  mutate(ndvi_hyb = coalesce(ndvi_cdr_pred, ndvi_cdr)) %>% 
  as.data.table()


jcp <- dcp %>% 
  .[nobs >=3 & sd <= 0.05] %>% 
  .[date>= ymd("1981-09-01") & date<= ymd("2019-09-30")] %>% 
  .[,`:=`(year=year(date))] %>% 
  .[, .(val_cdr=mean(ndvi_cdr,na.rm=TRUE), 
        # val_mcd=mean(ndvi_mcd,na.rm=TRUE), 
        val_cdr_pred = mean(ndvi_hyb,na.rm=TRUE)), 
    keyby=.(x,y,year)] %>% 
  .[,.(beta_cdr = list(unname(fastLm(
    X = cbind(1,year-2001), 
    y=val_cdr, data=.SD)$coefficients)), 
    # beta_mcd = list(unname(fastLm(
    #   X = cbind(1,year-2001), 
    #   y=val_mcd, data=.SD)$coefficients)), 
    beta_cdr_pred = list(unname(fastLm(
      X = cbind(1,year-2001), 
      y=val_cdr_pred, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0_cdr=unlist(beta_cdr)[1], b1_cdr=unlist(beta_cdr)[2], 
          # b0_mcd=unlist(beta_mcd)[1], b1_mcd=unlist(beta_mcd)[2], 
          b0_cdr_pred=unlist(beta_cdr_pred)[1], b1_cdr_pred=unlist(beta_cdr_pred)[2]), by=.(x,y)]

jcp$b0_cdr %>% summary
jcp$b0_cdr_pred %>% summary
jcp$b1_cdr %>% summary
jcp$b1_cdr_pred

jcp %>% 
  as_tibble() %>% 
  filter(b0_cdr_pred > 0.1 & b0_cdr_pred < 1) %>% 
  filter(b1_cdr_pred > -0.05 & b1_cdr_pred < 0.05) %>% 
  rowwise() %>% 
  mutate(val = 100*(b1_cdr_pred*38)/b0_cdr_pred) %>% 
  ggplot(data=.,aes(x,y,fill=val))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(limits=c(-30,30), oob=scales::squish)


dcp %>% 
  # .[nobs >=3 & sd <= 0.05] %>% 
  .[date>= ymd("1981-09-01") & date<= ymd("2019-09-30")] %>% 
  .[sample(.N, 3e5)] %>%  # pull(date) %>% max
  select(zone,date,ndvi_cdr, ndvi_hyb) %>% 
  gather(-zone,-date,key = 'key',value='value') %>% 
  ggplot(data=.,aes(date, value, color=key))+
  geom_smooth()+
  facet_wrap(~zone, scales='free')
  

library(brms)
library(mgcv); library(gratia); library(nls.multstart)
library(tidyverse)
library(data.table); setDTthreads(threads = 8)
library(lubridate); 
library(dtplyr);
options(mc.cores=parallel::detectCores()-3) 

# IMPORT ###################################################################
source("src/R/extract_awap_rad.R")
frac <- stars::read_ncdf("../data_general/Oz_misc_data/csiro_FC.v310.MCD43A4_0p5_2001_2019.nc")
frac[,,,1] %>% as_tibble() %>% filter(is.na(soil)==F)
eoz <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_Tree_Cover_5000m_East_Oz_noMask_2000_2019.tif",
                         RasterIO = list(bands=1))
frac <- stars::st_warp(src=frac, dest=eoz, use_gdal = F)
frac <- frac %>% as.data.table() %>% lazy_dt() %>% 
  rename(date=time) %>%
  mutate(date=as.Date(date)) %>% 
  filter(is.na(npv)==F) %>% 
  as.data.table()

vi <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet", 
                          col_select = c("x","y","date",
                                         "ndvi_c","ndvi_m","ndvi_hyb", 
                                         "evi2_hyb","evi2_m")) %>% 
  as.data.table()
vi <- frac[vi,on=.(x,y,date)]
vi[is.na(npv)==F]
vi[date==ymd("2001-03-01")&is.na(npv)==F] %>% ggplot(data=.,aes(x,y,fill=npv))+
  geom_tile()+coord_equal()+scale_fill_viridis_c()
vi <- merge(frac, 
             vi,
             by=c("x","y","date"), 
             all=TRUE,allow.cartesian=TRUE)

tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id","season",
                             "precip",  
                             "precip_anom",
                             "precip_anom_3mo",
                             "precip_anom_36mo",
                             "precip_anom_12mo",
                             "map", 
                             "precip_12mo",
                             # "precip_36mo",
                             "tmax","tmax_u",
                             "tmax_anom","tmax_anom_sd", "matmax",
                             "tmin","tmin_anom",
                             "vpd15","vpd15_anom","vpd15_anom_sd","mavpd15",
                             # "vpd15_u",
                             "pet","mapet",
                             "pet_anom","pet_anom_3mo","pet_u","pet_sd",
                             "pet_anom_sd",
                             "pet_12mo",
                             # "pet_36mo",
                             "pe","mape",
                             # "ndvi_u",
                             # "ndvi_anom",
                             # "ndvi_anom_12mo",
                             "ndvi_anom_sd",
                             # "ndvi_mcd",
                             'vc','veg_class',
                             'month',
                             "x", "y", "year")) %>% 
  as.data.table() %>% 
  .[is.infinite(mape)==F] %>% 
  .[,`:=`(p_anom_12mo_frac = precip_anom_12mo/map)]

tmp <- merge(tmp, 
             arad %>% select(x_vi,y_vi,time,rad) %>% 
               rename(x=x_vi,y=y_vi,date=time) %>% 
               mutate(date=as.Date(date)),
             by=c("x","y","date"), 
             all=TRUE,allow.cartesian=TRUE)


oo <- tmp[,.(rad_u = mean(rad, na.rm=TRUE)), by=.(x,y,month)]
tmp <- oo[tmp,on=.(x,y,month)]
rm(oo)
tmp <- tmp[,`:=`(rad_anom = rad-rad_u)]

# unique(tmp[,.(vc,veg_class)]) %>% View
# tmp <- tmp[order(x,y,date)][,tmean := (tmax+tmin)/2]
tmp <- tmp[order(x,y,date)][, tmax_3mo := frollmean(tmax,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmax_u_3mo := frollmean(tmax_u,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, rad_3mo := frollmean(rad,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, rad_12mo := frollmean(rad,n = 12,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmax_anom_3mo := frollmean(tmax_anom,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, rad_anom_3mo := frollmean(rad_anom,n = 3,fill = NA,align='center'), by=.(x,y)]

# tmp <- tmp[order(x,y,date)][, tmean_3mo := frollmean(tmean,n = 3,fill = NA,align='center'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, tmin_3mo := frollmean(tmin,n = 3,fill = NA,align='center'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, tmax_12mo := frollmean(tmax,n = 12,fill = NA,align='right'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, pet_3mo := frollmean(pet,n = 3,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, precip_3mo := frollmean(precip,n = 3,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, vpd15_3mo := frollmean(vpd15,n = 3,fill = NA,align='right'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, vpd15_anom_3mo := frollmean(vpd15_anom,n = 3,fill = NA,align='right'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, pet_24mo := frollmean(pet,n = 24,fill = NA,align='right'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, precip_24mo := frollmean(precip,n = 24,fill = NA,align='right'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, pet_48mo := frollmean(pet,n = 48,fill = NA,align='right'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, precip_48mo := frollmean(precip,n = 48,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmax_anom_12mo := frollmean(tmax_anom,n = 12,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, rad_anom_12mo := frollmean(rad_anom,n = 12,fill = NA,align='right'), by=.(x,y)]

tmp <- tmp[,`:=`(#pe_3mo = precip_3mo/pet_3mo, 
  pe_12mo = precip_12mo/pet_12mo 
  # pe_24mo = precip_24mo/pet_24mo, 
  # pe_36mo = precip_36mo/pet_36mo, 
  # pe_48mo = precip_48mo/pet_48mo
)]
dim(tmp)
tmp <- merge(tmp, 
             vi,
             by=c("x","y","date"), 
             all=TRUE,allow.cartesian=TRUE)
tmp <- tmp[order(x,y,date)][, ndvi_3mo := frollmean(ndvi_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]

rm(vi,arad); gc(full=TRUE)

mlo <- readr::read_table("../data_general/CO2_growth_rate/co2_mm_mlo_20200405.txt", 
                         skip = 72, col_names = F) %>% 
  set_names(
    c("year","month","ddate","co2_avg","co2_int","co2_trend","ndays")
  ) %>% 
  mutate(date = ymd(paste(year,month,1))) %>% 
  select(date,co2_int,co2_trend) %>% 
  as.data.table()
tmp <- merge(mlo,tmp,by="date")
tmp <- tmp[is.na(ndvi_3mo)==F & is.na(co2_int)==F]
center_co2 <- mean(tmp$co2_int)
tmp <- tmp[,`:=`(cco2=co2_int-center_co2)]
gc()
#*******************************************************************************

tmp[,`:=`(pe_anom_12mo = pe_12mo - mape)]
train_dat <- tmp[season=='SON'][mape<2][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 100000)]
test_dat <- tmp[season=='SON'][mape<2][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 100000)]

tmp[hydro_year>2016][pe_12mo<2][sample(.N,10000)] %>% 
  ggplot(data=., aes(pe_12mo, ndvi_3mo,color=vpd15_anom))+
  geom_point(size=0.5)+
  geom_smooth(color='yellow')+
  scale_color_gradient2(high='red',low='blue')+
  # scico::scale_color_scico(palette = 'vik')+
  facet_wrap(~season)+
  theme_black()

fit0 <- bam(ndvi_3mo ~ te(pe_12mo,bs='cs'), 
           data=train_dat, 
           select=TRUE, 
           discrete = TRUE)
fit1 <- bam(ndvi_3mo ~ te(pe_12mo,cco2,bs='cs'), 
           data=train_dat, 
           select=TRUE, 
           discrete = TRUE)
fit2 <- bam(ndvi_3mo ~ te(pe_12mo,co2_trend,bs='cs')+
                       s(tmax_anom_sd,bs='cs'),
            data=train_dat, 
            select=TRUE, 
            discrete = TRUE)
fit3 <- bam(ndvi_3mo ~ te(pe_12mo,cco2,bs='cs')+
                       te(matmax, tmax_anom_3mo,bs='cs'), 
            data=train_dat, 
            select=TRUE, 
            discrete = TRUE)
fit4 <- bam(ndvi_3mo ~ te(pe_12mo,cco2,bs='cs')+
              te(mape, tmax_anom_3mo,bs='cs'), 
            data=train_dat, 
            select=TRUE, 
            discrete = TRUE)
fit5 <- bam(ndvi_3mo ~ te(pe_12mo,cco2,bs='cs')+
              te(mapet, tmax_anom_3mo,bs='cs'), 
            data=train_dat, 
            select=TRUE, 
            discrete = TRUE)
fit6 <- bam(ndvi_3mo ~ s(pe_12mo,bs='cs')+
              te(matmax, tmax_anom_3mo,bs='cs'), 
            data=train_dat, 
            select=TRUE, 
            discrete = TRUE)
fit7 <- bam(ndvi_3mo ~ s(cco2,by=pe_12mo, bs='cs'),
            data=train_dat, 
            select=TRUE, 
            discrete = TRUE)
fit8 <- bam(ndvi_3mo ~ te(pe_12mo,cco2,bs='cs')+
              te(matmax, tmax_anom_3mo,bs='cs'), 
            data=train_dat, 
            family=Gamma(link='log'),
            select=TRUE, 
            discrete = TRUE)
fit9 <- bam(ndvi_3mo ~ te(pe_12mo,cco2,bs='cs')+
              te(mape, rad_anom_3mo,bs='cs'), 
            data=train_dat, 
            family=Gamma(link='log'),
            select=TRUE, 
            discrete = TRUE)
fit10 <- bam(ndvi_3mo ~ te(pe_12mo,tmax_anom_12mo,bs='cs'),
            data=train_dat, 
            select=TRUE, 
            discrete = TRUE)
fit11 <- bam(ndvi_3mo ~ te(pe_12mo,tmax_anom_3mo,bs='cs')+
                        te(mape,cco2,bs='cs'),
             data=train_dat, 
             select=TRUE, 
             discrete = TRUE)
fit12 <- bam(ndvi_3mo ~ te(pe_12mo,cco2,bs='cs')+
               te(matmax,vpd15_anom,bs='cs'),
             data=train_dat, 
             select=TRUE, 
             discrete = TRUE)
fit13 <- bam(ndvi_3mo ~ te(pe_12mo,cco2,bs='cs')+
              te(mape, tmax_anom,bs='cs'), 
            data=train_dat, 
            select=TRUE, 
            discrete = TRUE)
fit14 <- bam(ndvi_3mo ~ te(pe_12mo,cco2,bs='cs')+
               te(tmax,bs='cs'), 
             data=train_dat, 
             select=TRUE, 
             discrete = TRUE)

bbmle::AICtab(fit0, fit1, fit2, fit3, fit4, fit5, fit6,fit7,fit8,fit9,fit10, 
              fit11, fit12,fit13,fit14)

summary(fit10)
summary(fit1)
summary(fit11)
plot(fit11,scheme=2)
gratia::evaluate_smooth(fit2, smooth="te(pe_12mo") %>% 
  filter(pe_12mo < 2) %>% 
  ggplot(data=., aes(pe_12mo, est,color=co2_trend,group=co2_trend))+
geom_line(alpha=0.5)+
  scale_color_viridis_c()
gratia::evaluate_smooth(fit2, smooth="s(I(tmax_3mo/matmax") %>% 
  ggplot(data=., aes(matmax, est,color=tmax_anom_3mo,group=tmax_anom_3mo))+
  geom_line(alpha=0.5)+
  scale_color_viridis_c()

gratia::evaluate_smooth(fit4, smooth="te(pe_12mo,cco2)") %>% 
  filter(pe_12mo < 2) %>% 
  ggplot(data=., aes(pe_12mo, est,color=cco2,group=cco2))+
  geom_line(alpha=0.5)+
  scale_color_gradient2()
gratia::evaluate_smooth(fit4, smooth="te(mape,tmax_anom_3mo)") %>% 
  ggplot(data=., aes(mape, est,color=tmax_anom_3mo,group=tmax_anom_3mo))+
  geom_line(alpha=0.5)+
  scale_color_gradient2()

gratia::evaluate_smooth(fit12, smooth="te(pe_12mo,vpd15_anom)") %>% 
  ggplot(data=., aes(pe_12mo, est,color=vpd15_anom,group=vpd15_anom))+
  geom_line(alpha=0.5)+
  scale_color_gradient2()+
  theme_black()

gratia::draw(fit)

gratia::evaluate_smooth(fit4, smooth="te(pe_12mo,cco2)") %>% 
  filter(pe_12mo < 2) %>% 
  ggplot(data=., aes(pe_12mo, est,color=cco2,group=cco2))+
  geom_line(alpha=0.5)+
  scale_color_viridis_c()

gratia::evaluate_smooth(fit4, smooth="te(mape,tmax_anom_3mo)",
                        n=200) %>% 
  # filter(pe_12mo < 2) %>% 
  ggplot(data=., aes(mape, est,color=tmax_anom_3mo,
                     group=tmax_anom_3mo))+
  geom_line(alpha=0.75)+
  scale_color_gradient2(high='red',low='navy')+
  theme_black()



gratia::appraise(fit5)



# Broken stick linear model -----------------------------------------------
train_dat %>% 
  lazy_dt() %>% 
  filter(mape < 2) %>% 
  mutate(mape_d = cut_width(mape,width = 0.1)) %>%  
  mutate(co2_d = cut_interval(co2_trend, 3)) %>% 
  as_tibble() %>% 
  filter(between(pe_anom_12mo,-0.05,0.05)) %>% 
  ggplot(data=., aes(pe_12mo,ndvi_3mo, 
                     color=co2_d))+
  geom_smooth(method='lm',se=F)+
  scale_color_viridis_d(option='B',end=0.9)+
  facet_wrap(~mape_d,scales = 'free')

tmp %>% 
  lazy_dt() %>% 
  filter(mape < 2) %>% 
  mutate(mape_d = cut_width(mape,width = 0.1)) %>%  
  mutate(co2_d = cut_interval(co2_trend, 4)) %>% 
  filter(pe_anom_12mo>-0.05) %>% 
  filter(pe_anom_12mo<0.05) %>% 
  as.data.table() %>% 
  ggplot(data=., aes(pe_12mo,ndvi_3mo, 
                     color=co2_d))+
  geom_smooth(method='lm',se=F)+
  scale_color_viridis_d(option='B',end=0.9)+
  facet_wrap(~mape_d,scales = 'free')+
  theme_linedraw()


tmp %>% 
  lazy_dt() %>% 
  filter(mape < 2) %>% 
  mutate(mape_d = cut_width(mape,width = 0.1)) %>%  
  mutate(co2_d = cut_interval(co2_trend, 4)) %>% 
  filter(pe_anom_12mo>-0.05) %>% 
  filter(pe_anom_12mo<0.05) %>% 
  as.data.table() %>% 
  ggplot(data=., aes(pe_12mo,ndvi_3mo, 
                     color=co2_d))+
  geom_smooth(se=F)+
  scale_color_viridis_d(option='B',end=0.9)+
  facet_wrap(~mape_d,scales = 'free')+
  theme_linedraw()

o <- tmp %>% 
  lazy_dt() %>% 
  sample_n(1000) %>% 
  filter(is.na(pe_12mo)==F) %>% 
  group_by(hydro_year) %>% 
  summarize(fit = list(unname(
           RcppArmadillo::fastLm(y=ndvi_3mo,X=cbind(1,pe_12mo),data=.SD)
           ))) %>% 
  ungroup() %>% 
  as_tibble()

library(RcppArmadillo)
o <- tmp[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
  .[date>= ymd("1982-01-01") & date<= ymd("2019-09-30")] %>% 
  .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
  .[is.na(val)==F] %>% 
  .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
    by=.(season,hydro_year)]

o <- tmp[is.na(veg_class)==F][is.na(pe_12mo)==F][date <= ymd('2019-09-01')] %>% 
  .[,.(beta = list(unname(fastLm(X = cbind(1,hydro_year), 
                                 y=ndvi_3mo, data=.SD)$coefficients))), 
    by=.(season,hydro_year)] %>% 
  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(season,hydro_year)] %>% 
  .[,.(season,hydro_year,b0,b1)]
o <- tmp[sample(.N,10000)] %>% 
         .[is.na(veg_class)==F] %>% .[is.na(pe_12mo)==F] %>% 
        .[is.na(hydro_year)==F] %>% .[date <= ymd('2019-09-01')] %>% 
  .[,.(beta = list(unname(lm(ndvi_3mo~pe_12mo,data=.SD)$coefficients))),
    by=.(season, hydro_year)] %>% 
  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(season,hydro_year)] %>% 
  .[,.(season,hydro_year,b0,b1)]

tmp[sample(.N,10000)] %>% 
  .[is.na(veg_class)==F] %>% .[is.na(pe_12mo)==F] %>% 
  .[is.na(hydro_year)==F] %>% .[date <= ymd('2019-09-01')] %>% 
  .[,.(beta = list(unname(fastLm(ndvi_3mo~pe_12mo,data=.SD)$coefficients))),
    by=.(season, hydro_year)] %>% 
  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(season, hydro_year)] %>% 
  .[,.(season, hydro_year,b0,b1)] %>% 
  ggplot(data=.,aes(hydro_year,b0,color=season))+geom_point()+geom_line()+
  geom_smooth(method='lm',se=F)

o[,.(beta = list(unname(fastLm(X = cbind(1,hydro_year), 
                                 y=ndvi_3mo, data=.SD)$coefficients))), 
    by=hydro_year] 

  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(season,hydro_year)] %>% 
  .[,.(season,hydro_year,b0,b1)]
fastLm(X=cbind(1,o$hydro_year),y=o$ndvi_3mo,data=o)

o <- tmp %>% 
  sample_n(10000) %>% 
  as_tibble() %>% 
  group_by(hydro_year,season) %>% 
  summarize(fit = coef(lm(ndvi_3mo~pe_12mo, data=.))[2]) %>% 
  ungroup()
o
summary(lm(ndvi_3mo~pe_12mo, data=tmp[sample(.N, 10000)]))
summary(lm(ndvi_3mo~log(pe_12mo), data=tmp[sample(.N, 10000)]))

# Richards function wCO2 v1 ----------------------------------------------------
n_ric_x2 <- nls_multstart(ndvi_3mo ~ 
      (Asym+Asym2*co2_trend) * (1+exp(((xmid+xmid2*co2_trend) - pe_12mo)/(scal)))^(-exp(-(lpow))),
                  # grad_ric_x2_6p(x = pe_12mo,x2=cco2, 
                  #                   Asym=Asym,Asym2=Asym2, 
                  #                   xmid=xmid, xmid2=xmid2,
                  #                   scal=scal,
                  #                   lpow=lpow),
                  data=train_dat,
                  iter=100,
                  supp_errors = 'Y',
                  control=nls.control(maxiter=100),
                  start_upper = c(Asym=0.7561,Asym2=5e-4,
                            xmid=0.27,xmid2=3e-4,
                            scal=0.28,
                            lpow=-0.28), 
                  start_lower=c(0.5, -0.01, 
                          -0.5, -0.01, 
                          0.15,  
                          -0.9))
summary(n_ric_x2)

test_dat %>% 
  as_tibble() %>% 
  mutate(pred=predict(n_ric_x2,newdata=.)) %>% 
  filter(is.na(pred)==F) %>% 
  summarize(rmse=mean((pred-ndvi_3mo)**2, na.rm=TRUE), 
            r2 = cor(pred,ndvi_3mo)**2)
train_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_ric_x2,newdata=.), 
         # pred0=predict(n_gs_2,newdata=.)
         ) %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(pred,ndvi_3mo))+
  geom_point(alpha=0.5,color='blue')+
  geom_smooth()+
  # geom_point(aes(pred0,ndvi_3mo),color='green',alpha=0.1)+
  # geom_smooth(aes(pred0,ndvi_3mo),color='green')+
  geom_abline(aes(intercept=0,slope=1),color='red')+
  theme_black()

# Richards function wCO2 & temp v1 ----------------------------------------------------
n_ric_co2_tmax <- nls_multstart(ndvi_3mo ~ 
    grad_ric_x2_8p(x=mape,x2=cco2,x3=pe_12mo, 
                   Asym=Asym,Asym2=Asym2, 
                   xmid=xmid,xmid2=xmid2,
                   scal=scal,
                   b1=b1),
    data=train_dat,
    iter=100,
    supp_errors = 'Y',
    control=nls.control(maxiter=100),
    start_lower=c(Asym=0.5, Asym2=-5e-4, 
               xmid=0, xmid2=-0.0001, 
               scal=0.15, 
               b1=0),
    start_upper = c(Asym=1,Asym2=5e-4,
                    xmid=0.27,xmid2=3e-4,
                    scal=0.28,
                    b1=0.1) 
        )
summary(n_ric_co2_tmax)

sqrt(mean((predict(n_ric_co2_tmax, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_ric_co2_tmax, newdata=test_dat),test_dat$ndvi_3mo)**2

test_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_ric_co2_tmax, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')


test_dat %>% 
  as_tibble() %>% 
  mutate(pred=predict(n_ric_co2_tmax,newdata=.)) %>% 
  filter(is.na(pred)==F) %>% 
  summarize(rmse=mean((pred-ndvi_3mo)**2, na.rm=TRUE), 
            r2 = cor(pred,ndvi_3mo)**2)
train_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_ric_co2_tmax,newdata=.), 
         # pred0=predict(n_gs_2,newdata=.)
  ) %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(pred,ndvi_3mo))+
  geom_point(alpha=0.5,color='blue')+
  geom_smooth()+
  # geom_point(aes(pred0,ndvi_3mo),color='green',alpha=0.1)+
  # geom_smooth(aes(pred0,ndvi_3mo),color='green')+
  geom_abline(aes(intercept=0,slope=1),color='red')+
  theme_black()

expand_grid(map = 1200,
            mape=seq(0.1,2,length.out=50),
            tmax_anom_3mo = c(0),
            cco2 = c(-40,0,40),
            pe_anom_12mo=c(-0.5,0,0.5),
            # pe_12mo = seq(0.1,2,length.out=50)
) %>% 
  mutate(pe_12mo = mape) %>% 
  mutate(matmax = predict(f_matmax, newdata=. )) %>% 
  mutate(tmax_3mo = predict(f_tmax_3mo, newdata=.)) %>% 
  mutate(vpd15_3mo = predict(f_vpd15_3mo, newdata=.)) %>% 
  mutate(co2_trend = cco2+center_co2) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  mutate(pred=predict(n_ric_co2_tmax,newdata=.)) %>% 
  ggplot(data=., aes(mape, pred, 
                     color=as_factor(co2),
                     # group=cco2, 
                     linetype=as_factor(pe_anom_12mo)))+
  geom_line()+
  scale_color_viridis_d(option='B', end=0.8)+
  scale_x_continuous(expand=c(0,0))+
  facet_wrap(~pe_anom_12mo, scales = 'fixed',nrow = 1)+
  theme_linedraw()+
  theme(panel.grid = element_blank())



# Exponential Function ----------------------------------------------------
n_exp <- nls.multstart::nls_multstart(ndvi_3mo ~ 
                  A/exp(B*pe_12mo) + C,
                data=train_dat, 
                iter = 100,
                supp_errors = 'Y',
                control=nls.control(maxiter=100),
                start_lower = c(A=0,    B=0.5, C=0), 
                start_upper = c(A=0.25, B=1, C=1)) 
summary(n_exp)
train_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_exp,newdata=.)) %>% 
  select(pred,ndvi_3mo) %>% 
  na.omit() %>% 
  cor()


# Exponential Function w/CO2 ----------------------------------------------------
n_exp_x2 <- nls_multstart(ndvi_3mo ~ 
              (A*co2_trend)/exp(B*pe_12mo*co2_trend) + C*co2_trend,
                    data=train_dat, 
                    iter = 100,
                    supp_errors = 'Y',
                    control=nls.control(maxiter=200),
                    start_lower = c(A=-0.1,    B=0.5, C=0), 
                    start_upper = c(A=0.1, B=1, C=0.1)) 
summary(n_exp_x2)

sqrt(mean((predict(n_exp_x2, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_exp_x2, newdata=test_dat),test_dat$ndvi_3mo)**2

# Exponential Function w/CO2 ----------------------------------------------------
n_exp_x2_6p <- nls_multstart(ndvi_3mo ~ 
          (A+A1*co2_trend)/exp(B*pe_12mo+B1*co2_trend) + (C+C1*co2_trend),
        data=train_dat, 
        iter = 100,
        supp_errors = 'Y',
        control=nls.control(maxiter=200),
        start_lower = c(A=-0.1,A1=0, B=0.5,B1=0, C=0, C1=0), 
        start_upper = c(A=0.1,A1=0.1,  B=1,B1=0.1, C=1, C1=0.1)) 
summary(n_exp_x2_6p)

sqrt(mean((predict(n_exp_x2_6p, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_exp_x2_6p, newdata=test_dat),test_dat$ndvi_3mo)**2

# Exponential Function w/tmax ----------------------------------------------------
n_exp_x2_6p_tmax <- nls_multstart(ndvi_3mo ~ 
       (A+A1*tmax_3mo)/exp(B*pe_12mo+B1*tmax_3mo) + (C+C1*tmax_3mo),
     data=train_dat, 
     iter = 100,
     supp_errors = 'Y',
     control=nls.control(maxiter=200),
     start_lower = c(A=-0.1,A1=0, B=0.5,B1=0, C=0, C1=0), 
     start_upper = c(A=0.1,A1=0.1,  B=1,B1=0.1, C=1, C1=0.1)) 
summary(n_exp_x2_6p_tmax)

sqrt(mean((predict(n_exp_x2_6p_tmax, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_exp_x2_6p_tmax, newdata=test_dat),test_dat$ndvi_3mo)**2

# Exponential Function w/co2 & tmax v1 ----------------------------------------------------
n_exp_co2_tmax <- nls_multstart(ndvi_3mo ~ 
    (A+A1*tmax_3mo+A2*co2_trend)/exp(B*pe_12mo) + (C+C1*tmax_3mo+C2*co2_trend),
    data=train_dat %>% mutate(tmax_frac_anom = tmax_anom_3mo/matmax), 
    iter = 100,
    supp_errors = 'Y',
    control=nls.control(maxiter=200),
    start_lower = c(A=-0.1,A1=0,A2=0, B=0.5, C=0, C1=0, C2=0), 
    start_upper = c(A=0.1,A1=0.1,A2=0.01, B=1, C=1, C1=0.1, C2=0.01)) 
summary(n_exp_co2_tmax)

sqrt(mean((predict(n_exp_co2_tmax, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_exp_co2_tmax, newdata=test_dat),test_dat$ndvi_3mo)**2

test_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_exp_co2_tmax, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')

# Exponential Function w/co2 & tmax v2 ----------------------------------------------------
n_exp_co2_tmax_2 <- nls_multstart(ndvi_3mo ~ 
      (A+A1*tmax_anom_3mo*matmax+A2*co2_trend)/exp(B*pe_12mo) + 
        (C+C1*tmax_anom_3mo*matmax+C2*co2_trend),
    data=train_dat, 
    iter = 100,
    supp_errors = 'Y',
    control=nls.control(maxiter=200),
    start_lower = c(A=-0.1,A1=0,A2=0, B=0.5, C=0, C1=0, C2=0), 
    start_upper = c(A=0.1,A1=0.1,A2=0.01, B=1, C=1, C1=0.1, C2=0.01))
summary(n_exp_co2_tmax_2)

sqrt(mean((predict(n_exp_co2_tmax_2, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_exp_co2_tmax_2, newdata=test_dat),test_dat$ndvi_3mo)**2

# Exponential Function w/co2 & tmax v3 ----------------------------------------------------
n_exp_co2_tmax_3 <- nls_multstart(ndvi_3mo ~ 
    (A+A1*tmax_3mo+A2*co2_trend)/exp(B*pe_12mo + B1*co2_trend) + 
      (C+C1*tmax_anom_3mo+C2*co2_trend),
        data=train_dat, 
        iter = 100,
        supp_errors = 'Y',
        control=nls.control(maxiter=200),
        start_lower = c(A=-0.1,A1=0,A2=0, B=0.5,B1=-0.001, C=0, C1=0,C2=0), 
        start_upper = c(A=0.1,A1=0.1,A2=0.001,  B=1,B1=0.001, C=1, C1=0.1,C2=0.001)) 
summary(n_exp_co2_tmax_3)

sqrt(mean((predict(n_exp_co2_tmax_3, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_exp_co2_tmax_3, newdata=test_dat),test_dat$ndvi_3mo)**2

# Exponential Function w/co2 & tmax v4 ----------------------------------------------------
n_exp_co2_tmax_4 <- nls_multstart(ndvi_3mo ~ 
      (A+A2*co2_trend)/exp(B*pe_12mo + B1*co2_trend) + 
      (C+C2*co2_trend)+
        D1*tanh(D2*vpd15_anom_sd),
    data=train_dat, 
    iter = 50,
    supp_errors = 'Y',
    control=nls.control(maxiter=500),
    start_lower = c(A=-0.1,A2=0, 
                    B=0.5,B1=-0.001, 
                    C=0, C2=0,
                    D1=0,D2=-1), 
    start_upper = c(A=0.1,A2=0.001,
                    B=1,B1=0.001, 
                    C=1, C2=0.001,
                    D1=1,D2=1)) 
summary(n_exp_co2_tmax_4)

sqrt(mean((predict(n_exp_co2_tmax_4, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_exp_co2_tmax_4, newdata=test_dat),test_dat$ndvi_3mo)**2

test_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_exp_co2_tmax_4, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')

fn_logistic <- function(x,x0,L,M,k){M+(L)/(1+exp(-k*(x-x0)))}
curve(fn_logistic(x,0,L=0.2,M=-0.2,k=2),-2,2)

curve(2.083e+00*tanh(1.041e-02*x),-4,4)

# Exponential Function w/co2 & tmax v5 ----------------------------------------------------
# nonlinear: CO2, linear: tmax
n_exp_co2_tmax_5 <- nls_multstart(ndvi_3mo ~ 
                (A+A2*co2_trend)/exp(B*pe_12mo + B1*co2_trend) + 
                (C+C2*co2_trend)+D1*matmax+(D2*vpd15_anom_sd),
              data=train_dat, 
              iter = 100,
              supp_errors = 'Y',
              control=nls.control(maxiter=200),
              start_lower = c(A=-0.1,A2=0, 
                              B=0.5,B1=-0.001, 
                              C=0, C2=0,
                              D1=0,D2=-1), 
              start_upper = c(A=0.1,A2=0.001,
                              B=1,B1=0.001, 
                              C=1, C2=0.001,
                              D1=1,D2=1)) 
summary(n_exp_co2_tmax_5)

sqrt(mean((predict(n_exp_co2_tmax_5, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_exp_co2_tmax_5, newdata=test_dat),test_dat$ndvi_3mo)**2

test_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_exp_co2_tmax_5, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')


# Exponential Function w/co2 & tmax v6 ----------------------------------------------------
# nonlinear additive: tmax, nonlinear additive: co2
n_exp_co2_tmax_6 <- nls_multstart(ndvi_3mo ~ 
            (A+A2*tmax_anom_3mo)/exp(B*pe_12mo + B1*tmax_anom_3mo) + 
            (C+C2*tmax_anom_3mo)+
            D1*tanh(D2*cco2),
          data=train_dat, 
          iter = 100,
          supp_errors = 'Y',
          control=nls.control(maxiter=200),
          start_lower = c(A=-0.1,A2=0, 
                          B=0.5,B1=-0.001, 
                          C=0, C2=0,
                          D1=0,D2=-1), 
          start_upper = c(A=0.1,A2=0.001,
                          B=1,B1=0.001, 
                          C=1, C2=0.001,
                          D1=1,D2=1)) 

n_exp_co2_tmax_6 <- nls_multstart(ndvi_3mo ~ 
            A0+(B0*co2_trend)+
            (A1+A5*co2_trend)*tanh(A2*pe_12mo+A3*co2_trend+A4*tmax_anom_3mo)+ 
            D0*matmax,
          data=train_dat, 
          iter = 100,
          supp_errors = 'Y',
          control=nls.control(maxiter=100),
          start_lower = c(A0=0,A=-0.1,A2=0,A3=-0.001,A4=-0.001,A5=0,
                          B0=0,
                          # B=0.5,B1=-0.001, 
                          # C=0, C2=0,
                          D0=0),
                          # D1=0,D2=-1), 
          start_upper = c(A0=1,A=0.1,A2=0.001,A3=0.001,A4=0.001,A5=0.001,
                          B0=0.001,
                          # B=1,B1=0.001, 
                          # C=1, C2=0.001,
                          D0=1))
                          # D1=1,D2=1)) 
summary(n_exp_co2_tmax_6)
sqrt(mean((predict(n_exp_co2_tmax_6, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_exp_co2_tmax_6, newdata=test_dat),test_dat$ndvi_3mo)**2

test_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_exp_co2_tmax_6, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')

# gs model v1 ----------------------------------------------------------------
n_gs_1 <- nls_multstart(ndvi_3mo ~ 
                      g3*(1+g1/sqrt(vpd15))*pe_12mo*rad/co2_trend+g0,
                    data=train_dat, 
                    iter = 10,
                    supp_errors = 'Y',
                    control=nls.control(maxiter=200),
                    start_lower = c(g0=0,g1=0,g3=0), 
                    start_upper = c(g0=1,g1=10,g3=10)) 
summary(n_gs_1)
train_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_gs_1,newdata=.)) %>% 
  summarize(mean((pred-ndvi_3mo)**2, na.rm=TRUE))
train_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_gs_1,newdata=.)) %>% 
  ggplot(data=., aes(ndvi_3mo,pred))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')

# gs model v2 ----------------------------------------------------------------
n_gs_2 <- nls_multstart(ndvi_3mo ~ 
            (a0)*tanh(a1*mape)+
            g3*(1 + g1/(vpd15_3mo**0.5))*(log(precip_12mo)/(co2_trend)),
            data=train_dat, 
            iter = 10,
            supp_errors = 'Y',
            control=nls.control(maxiter=100),
            start_lower = c(a0=0,a1=0,
                            g1=0,g3=0), 
            start_upper = c(a0=1,a1=1,
                            g1=10,g3=10)) 
summary(n_gs_2)
test_dat %>% 
  as_tibble() %>% 
  mutate(pred=predict(n_gs_2,newdata=.)) %>% 
  summarize(rmse=sqrt(mean((pred-ndvi_3mo)**2, na.rm=TRUE)), 
            r2 = cor(pred,ndvi_3mo)**2)
train_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_gs_2,newdata=.), 
         # pred0=predict(n_exp_co2_tmax_5,newdata=.)
         ) %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(pred,ndvi_3mo))+
  geom_point(alpha=0.1,color='blue')+
  geom_smooth()+
  # geom_point(aes(pred0,ndvi_3mo),color='green',alpha=0.1)+
  # geom_smooth(aes(pred0,ndvi_3mo),color='green')+
  geom_abline(aes(intercept=0,slope=1),color='red')+
  theme_black()

train_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_gs_2,newdata=.), 
         pred0=predict(n_exp_co2_tmax_5,newdata=.)) %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(pe_12mo,ndvi_3mo))+
  geom_point()+
  geom_point(aes(pe_12mo, pred),color='blue')+
  geom_point(aes(pe_12mo, pred0),color='red')


expand_grid(map = 1200,
            # mape=seq(0.1,2,length.out=50),
            tmax_anom_3mo = c(0),
            cco2 = c(-40,0,40),
            precip_12mo=1000,
            pe_anom_12mo=c(-0.5,0,0.5),
            pe_12mo = seq(0.1,2,length.out = 50),
            # pe_12mo = seq(0.1,2,length.out=50)
) %>% 
  mutate(mape=pe_12mo) %>% 
  mutate(matmax = predict(f_matmax, newdata=. )) %>% 
  mutate(tmax_3mo = predict(f_tmax_3mo, newdata=.)) %>% 
  mutate(vpd15_3mo = predict(f_vpd15_3mo, newdata=.)) %>% 
  mutate(co2_trend = cco2+center_co2) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  mutate(pred=predict(n_gs_2,newdata=.)) %>% 
  ggplot(data=., aes(pe_12mo, pred, 
                     color=as_factor(co2),
                     # group=cco2, 
                     linetype=as_factor(pe_anom_12mo)))+
  geom_line()+
  scale_color_viridis_d(option='B', end=0.8)+
  scale_x_continuous(expand=c(0,0))+
  facet_wrap(~pe_anom_12mo, scales = 'fixed',nrow = 1)+
  theme_linedraw()+
  theme(panel.grid = element_blank())


# gs model v3 ----------------------------------------------------------------
n_gs_3 <- nls_multstart(ndvi_3mo ~ 
    # g0+g3*(1 + g1*sqrt(mape)/(pe_12mo**0.5))*(((exp(log(1+pe_12mo/mape)))/(co2_trend**-0.5))),
      # g0+g3*(1 + g1*sqrt(mape)/(pe_12mo**0.5))*(pe_12mo**g4/(co2_trend**g5)),
       # g0+g3*(1 + g1*sqrt(mape)/(pe_12mo**0.787672))*(pe_12mo**g4/(co2_trend**g5)),
        #A/exp(B*pe_12mo) + C
        # (precip_12mo*g3)/(g0+g2*(1+g1/sqrt(vpd15_3mo))*pet_12mo/(co2_trend)),
        # A+(precip_12mo)/(g0+g2*(1+g1/sqrt(vpd15_3mo))*precip_12mo/(co2_trend)),
        data=train_dat, 
        iter = 10,
        supp_errors = 'Y',
        control=nls.control(maxiter=100),
       start_lower = c(g0=-10,g1=0,g3=8), 
       start_upper = c(g0=1,g1=10,g3=10)) 
summary(n_gs_3)
test_dat %>% 
  as_tibble() %>% 
  mutate(pred=predict(n_gs_3,newdata=.)) %>% 
  filter(is.na(pred)==F) %>% 
  summarize(rmse=mean((pred-ndvi_3mo)**2, na.rm=TRUE), 
            r2 = cor(pred,ndvi_3mo)**2)
train_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_gs_3,newdata=.), 
         pred0=predict(n_gs_2,newdata=.)) %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(pred,ndvi_3mo))+
  geom_point(alpha=0.5,color='blue')+
  geom_smooth()+
  # geom_point(aes(pred0,ndvi_3mo),color='green',alpha=0.1)+
  # geom_smooth(aes(pred0,ndvi_3mo),color='green')+
  geom_abline(aes(intercept=0,slope=1),color='red')+
  theme_black()


expand_grid(map = 1200,
            # mape=seq(0.1,2,length.out=50),
            tmax_anom_3mo = c(0),
            cco2 = c(-40,0,40),
            pe_anom_12mo=c(-0.5,0,0.5),
            pe_12mo = seq(0.1,2,length.out = 50),
            # pe_12mo = seq(0.1,2,length.out=50)
    ) %>% 
  mutate(mape=pe_12mo) %>% 
  mutate(matmax = predict(f_matmax, newdata=. )) %>% 
  mutate(tmax_3mo = predict(f_tmax_3mo, newdata=.)) %>% 
  mutate(vpd15_3mo = predict(f_vpd15_3mo, newdata=.)) %>% 
  mutate(co2_trend = cco2+center_co2) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  mutate(pred=predict(n_gs_3,newdata=.)) %>% 
  ggplot(data=., aes(pe_12mo, pred, 
                     color=as_factor(co2),
                     # group=cco2, 
                     linetype=as_factor(pe_anom_12mo)))+
  geom_line()+
  scale_color_viridis_d(option='B', end=0.8)+
  scale_x_continuous(expand=c(0,0))+
  facet_wrap(~pe_anom_12mo, scales = 'fixed',nrow = 1)+
  theme_linedraw()+
  theme(panel.grid = element_blank())



# Four Param Logistic Function --------------------------------------------
n_fpl <- nls.multstart::nls_multstart(ndvi_3mo ~ 
                  SSfpl(pe_12mo, A, B, xmid, scal),
                  data=train_dat, 
                  iter = 100,
                  supp_errors = 'Y',
                  control=nls.control(maxiter=100),
                  start_lower = c(A=0,    B=0.5, xmid=0.1, scal=0), 
                  start_upper = c(A=0.25, B=1, xmid=1, scal=1)) 
summary(n_fpl)





n1 <- nls.multstart::nls_multstart(ndvi_3mo ~ NLS.L4(pe_12mo, b, c, d, e),
                             data=train_dat, 
                             iter = 100,
                             supp_errors = 'Y',
                             control=nls.control(maxiter=1000),
                             start_lower = c(b=2, c=-1, d=0, e=0), 
                             start_upper = c(b=3, c=0.25, d=1, e=1)) 
n1
summary(n1)
D(expression(a - (a - b) * exp (- c * X)), "a")

drc::W1.2
library(aomisc)
aomisc::NLS.L2

fn_l2 <- function(x,b,c,d,e){
  c + (d-c)/(1+exp(-b*(x-e)))
}
plot(ndvi_3mo~pe_12mo, train_dat[sample(.N, 1000)])
lines(predict(n1, newdata=data.frame(pe_12mo=seq(0,2,length.out = 100)))~
       seq(0,2,length.out = 100), type='l',col='red',lwd=2)
curve(fn_l2(x,b = 2.4,c=-0.17,d=0.76,e=0.23), 0,2,col='red',lwd=2)
curve(fn_l2(x,b = 2.4,c=-0.17,d=0.76,e=0.23), 0,2,col='red',add=TRUE,lwd=2)

plot(ndvi_3mo~pe_12mo, train_dat[sample(.N, 1000)])



n_ric <- nls.multstart::nls_multstart(ndvi_3mo ~ 
                    mod_grad_ric_x2(x = pe_12mo,x2=cco2, 
                                    Asym=Asym,Asym2=Asym2, 
                                    xmid=xmid, xmid2=xmid2,
                                    scal=scal,scal2=scal2,
                                    lpow=lpow, lpow2=lpow2),
                  data=train_dat, 
                  iter=100,
                  supp_errors = 'Y',
                  control=nls.control(maxiter=1000),
                  start_upper = c(Asym=0.7561,Asym2=5e-4,
                            xmid=0.27,xmid2=3e-4,
                            scal=0.28,scal2=0.01,
                            lpow=-0.28,lpow2=0.01), 
                  start_lower=c(0.5, -0.01, 
                          -0.5, -0.01, 
                          0.15, -0.001, 
                          -0.9, -0.01), 
                  algorithm = 'port')
summary(n_ric)

# Richards Function w/6 params --------------------------------------
n_ric_6p <- nls.multstart::nls_multstart(ndvi_3mo ~ 
                   grad_ric_x2_6p(x = pe_12mo,x2=cco2, 
                                Asym=Asym,Asym2=Asym2, 
                                xmid=xmid, xmid2=xmid2,
                                scal=scal,
                                lpow=lpow),
              data=train_dat, 
              iter=100,
              # supp_errors = 'Y',
              control=nls.control(maxiter=100),
              start_upper = c(Asym=0.7561,Asym2=5e-4,
                              xmid=0.27,xmid2=3e-4,
                              scal=0.28,
                              lpow=-0.28), 
              start_lower=c(0.5, -0.01, 
                            0, -0.01, 
                            0.15, 
                            -0.9), 
              algorithm = 'port')
summary(n_ric_6p)

n_ric_off <- nls.multstart::nls_multstart(ndvi_3mo ~ 
                                        grad_ric_offset(x = pe_12mo,x2=cco2,
                                                        offset=offset,
                                                        offset2=offset2,
                                                        Asym=Asym, 
                                                        xmid=xmid,
                                                        scal=scal,
                                                        lpow=lpow),
                                      data=train_dat, 
                                      iter=100,
                                      supp_errors = 'Y',
                                      control=nls.control(maxiter=100),
                                      start_upper = c(offset=1, 
                                                      offset2=1, 
                                                      Asym=0.7561,
                                                      xmid=0.27,
                                                      scal=0.2,
                                                      lpow=-0.28), 
                                      start_lower=c(-1,
                                                    -1, 
                                                    0.5, 
                                                    -0.5, 
                                                    0.15, 
                                                    -0.9), 
                                      algorithm = 'port')
summary(n_ric_off)


n1 <-  train_dat %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
                                 SSlogis(pe_12mo, Asym, xmid, scal),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01),
                start_upper = c(Asym=1, xmid=1, scal=1),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(n1)



train_dat %>% 
  mutate(pred = predict(n1)) %>% 
  mutate(res = pred - ndvi_3mo) %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(pred, ndvi_3mo,color=tmax_anom_3mo))+
  geom_point(size=1, alpha=1)+
  geom_smooth()+
  scale_color_gradient2(limits=c(-2,2),oob=scales::squish)+
  theme_black()
cbind(predict(n1),train_dat$ndvi_3mo) %>% cor


n2 <-  train_dat %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
                                 SSweibull(pe_12mo, Asym, Drop, lrc ,pwr),
                               data = .,
                               iter = 10,
                               start_lower = c(Asym=0.0, Drop=-1,lrc=0,pwr=-1),
                               start_upper = c(Asym=1, Drop=1,lrc=1,pwr=1),
                               # supp_errors = 'Y',
                               na.action = na.omit)
summary(n2)
train_dat %>% 
  mutate(pred = predict(n2, newdata=.)) %>% 
  mutate(res = pred - ndvi_3mo) %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(pe_12mo, pred))+
  geom_point(size=1, alpha=1,color='yellow')+
  geom_point(aes(pe_12mo,ndvi_3mo,color=tmax_anom_3mo))+
  # geom_smooth()+
  scale_color_gradient2(limits=c(-2,2),oob=scales::squish)+
  theme_black()


n3 <-  train_dat %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
            grad_weibull_x2(pe_12mo,cco2, Asym,Asym2, Drop, lrc ,pwr),
               data = .,
               iter = 100,
               start_lower = c(Asym=0.0,Asym2=-0.5, Drop=-1,lrc=0,pwr=-1),
               start_upper = c(Asym=1,  Asym2=0.5, Drop=1,lrc=1,pwr=1),
               supp_errors = 'Y',
               na.action = na.omit)
summary(n3)
train_dat %>% 
  mutate(pred = predict(n3, newdata=.)) %>% 
  mutate(res = pred - ndvi_3mo) %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(pe_12mo, pred))+
  geom_point(size=1, alpha=1,color='yellow')+
  geom_point(aes(pe_12mo,ndvi_3mo,color=tmax_anom_3mo))+
  # geom_smooth()+
  scale_color_gradient2(limits=c(-2,2),oob=scales::squish)+
  theme_black()

n4 <- nls.multstart::nls_multstart(ndvi_3mo ~ 
    grad_weibull_x3(x = pe_12mo,x2=cco2,x3=tmax_anom_3mo,
                    Asym=Asym,Asym2=Asym2,Asym3=Asym3,
                    Drop=Drop, lrc=lrc , pwr=pwr),
             data = train_dat,
             iter = 100,
             start_lower = c(Asym=0.0,Asym2=-0.05,Asym3=-0.5, 
                             Drop=-1,lrc=0,pwr=-1),
             start_upper = c(Asym=1,  Asym2=0.05,Asym3=0.5, 
                             Drop=1,lrc=1,pwr=1),
             supp_errors = 'Y',
             na.action = na.omit)
summary(n4)

n5 <- nls.multstart::nls_multstart(ndvi_3mo ~ 
        grad_weibull_x2_tmax(x = pe_12mo,x2=cco2,
                           tmax_anom_3mo=tmax_anom_3mo,
                           matmax=matmax, 
                           b_tmax=b_tmax,
                           Asym=Asym,Asym2=Asym2,
                           Drop=Drop, lrc=lrc , pwr=pwr),
         data = train_dat,
         iter = 1000,
         control=nls.control(maxiter=100),
         start_lower = c(Asym=0.5,Asym2=-0.05,b_tmax=-0.5, 
                         Drop=-1,lrc=0,pwr=-1),
         start_upper = c(Asym=1,  Asym2=0.05,b_tmax=0.5, 
                         Drop=1,lrc=1,pwr=1),
         supp_errors = 'Y',
         na.action = na.omit)
summary(n5)

# nonlinear co2 + map + matmax v1 -----------------------------------------------
# a3*cco2+
#   a0*tanh(a1*pe_12mo+a2*cco2)+
#   b0+b1*matmax+b2*log(map)+b3*tmax_anom_3mo**2,

n_co2_topt <- nls_multstart(ndvi_3mo ~ 
                  a3*cco2+
                  (a0+a4*cco2)*tanh(a1*pe_anom_12mo + b1*mape + a2*cco2)+
                  b0*matmax + b2*tmax_3mo + b3*tmax_3mo**2,
                  # a3*cco2+
                  # a0*tanh(a1*pe_12mo+a2*cco2)+
                  # b0*tanh(b1*mape+a4*cco2)+
                  # b2*matmax+b3*tmax_anom_3mo**2,
                  data=train_dat,
                  iter=5,
                  supp_errors = 'Y',
                  control=nls.control(maxiter=100),
                  start_lower = c(a0=0, a1=0,a2=0,a3=0,a4=0,
                                  b0=0, b1=0,b2=0,b3=0), 
                  start_upper = c(a0=1, a1=1, a2=0.001,a3=0.001,a4=0.001,
                                  b0=0.8,b1=0.5,b2=1, b3=1))
summary(n_co2_topt)
sqrt(mean((predict(n_co2_topt, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_co2_topt, newdata=test_dat),test_dat$ndvi_3mo)**2

curve(-2.687e+00*tanh(-2.378e-04*x), -50,50)
train_dat %>% sample_n(10000) %>% ggplot(data=., aes(tmax_anom_3mo,ndvi_3mo))+geom_point()+geom_smooth()


test_dat %>% as_tibble() %>% 
  sample_n(10000) %>% 
  mutate(pred=predict(n_co2_topt, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')


expand_grid(matmax = c(25),
            map = 1200,
            pe_12mo=0.5,
            pe_anom_12mo=0,
            tmax_3mo=c(20,25,30),
            tmax_anom_3mo = c(0,-4),
            cco2 = c(-40,0,40),
            mape = seq(0.1,2,length.out=50)) %>% 
  mutate(pred=predict(n_co2_topt,newdata=.)) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  ggplot(data=., aes(mape, pred, 
                     color=as_factor(co2),
                     # group=cco2, 
                     linetype=as_factor(tmax_3mo)))+
  geom_line()+
  scale_color_viridis_d(option='B', end=0.8)+
  scale_x_continuous(expand=c(0,0))+
  facet_wrap(~matmax, scales = 'free')+
  theme_linedraw()+
  theme(panel.grid = element_blank())


#-----------------------------------------------------------
n_co2_tmax <- nls_multstart(ndvi_3mo ~ 
  (a0+a1*cco2+a2*tmax_3mo/tmax_u_3mo)*
  tanh(a3*pe_12mo + a4*cco2 +a5*tmax_3mo/tmax_u_3mo+a6*pe_anom_12mo),
                            data=train_dat,
                            iter=5,
                            supp_errors = 'Y',
                            control=nls.control(maxiter=100),
                            start_lower = c(a0=0, 
                                            a1=0,
                                            a2=0, 
                                            a3=0,
                                            a4=0,
                                            a5=0, 
                                            a6=0
                                            # b0=0, 
                                            # b1=0, 
                                            # b2=0,
                                            # b3=0
                                            ),
                            # d0=0,d1=0,d2=0), 
                            start_upper = c(a0=1, 
                                            a1=1,
                                            a2=1, 
                                            a3=1,
                                            a4=1,
                                            a5=1,
                                            a6=1
                                            # b0=1, 
                                            # b1=1, 
                                            # b2=0.1, 
                                            # b3=1
                                            ))

n_co2_tmax <- nls_multstart(ndvi_3mo ~ 
      (a3*cco2)+
      a5*(cco2*(pe_12mo/mape))+
      (a0+a4*cco2+b1*pe_anom_12mo) * tanh(a1*pe_12mo + a2*cco2) +
      b0*(pe_12mo/mape) + 
      b2*(tmax_anom_3mo/tmax_u_3mo) + 
      b3*(tmax_anom_3mo/tmax_u_3mo)^2,
      data=train_dat,
      iter=5,
      supp_errors = 'Y',
      control=nls.control(maxiter=100),
    start_lower = c(a0=0, 
                    a1=0,
                    a2=0, 
                      a3=0,
                      a4=0,
                      a5=0,
                      b0=0, 
                      b1=0, 
                      b2=0,
                      b3=0),
      # d0=0,d1=0,d2=0), 
      start_upper = c(a0=1, 
                      a1=1,
                      a2=1, 
                      a3=1,
                      a4=1,
                      a5=1,
                      b0=1, 
                      b1=1, 
                      b2=0.1, 
                      b3=1))
summary(n_co2_tmax)
sqrt(mean((predict(n_co2_tmax, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_co2_tmax, newdata=test_dat),test_dat$ndvi_3mo)**2
curve(coef(n_co2_tmax)["b0"]*25 + coef(n_co2_tmax)["b2"]*x + coef(n_co2_tmax)["b3"]*x**2, 20,36)

test_dat %>% as_tibble() %>% 
  sample_n(10000) %>% 
  mutate(pred=predict(n6, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')

f_matmax <- lm(matmax~mape, data=train_dat)
f_tmax_3mo <- lm(tmax_3mo~mape+pe_anom_12mo, data=train_dat)
f_vpd15_3mo <- lm(vpd15_3mo~mape+pe_anom_12mo, data=train_dat)


expand_grid(map = 1200,
            mape=seq(0.1,2,length.out=50),
            tmax_anom_3mo = c(0),
            tmax_u_3mo=25,
            cco2 = c(-40,0,40),
            pe_anom_12mo=c(-0.5,0,0.5),
            # pe_12mo = seq(0.1,2,length.out=50)
  ) %>% 
  mutate(pe_12mo = mape) %>% 
  mutate(matmax = predict(f_matmax, newdata=. )) %>% 
  mutate(tmax_3mo = predict(f_tmax_3mo, newdata=.)) %>% 
  mutate(vpd15_3mo = predict(f_vpd15_3mo, newdata=.)) %>% 
  mutate(co2_trend = cco2+center_co2) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  mutate(pred=predict(n_co2_tmax,newdata=.)) %>% 
  ggplot(data=., aes(mape, pred, 
                     color=as_factor(co2),
                     # group=cco2, 
                     linetype=as_factor(pe_anom_12mo)))+
  geom_line()+
  scale_color_viridis_d(expression(paste(CO[2]~ppm)), 
                        option='B', end=0.75)+
  scale_x_continuous(expand=c(0,0))+
  labs(x="Mean Annual P:PET")+
  facet_wrap(~pe_anom_12mo, scales = 'fixed',nrow = 1,labeller = label_both)+
  theme_linedraw()+
  theme(panel.grid = element_blank())


expand_grid(map = 1200,
            mape=seq(0.1,2,length.out=50),
            tmax_anom_3mo = c(-3,0,3),
            tmax_u_3mo=25,
            cco2 = c(-40,0,40),
            pe_anom_12mo=c(-0.35,0,0.35),
            pe_12mo = seq(0.1,2,length.out=50)) %>%
  mutate(pe_12mo = mape) %>% 
  mutate(matmax = predict(f_matmax, newdata=. )) %>% 
  mutate(tmax_3mo = predict(f_tmax_3mo, newdata=.)) %>% 
  mutate(vpd15_3mo = predict(f_vpd15_3mo, newdata=.)) %>% 
  mutate(co2_trend = cco2+center_co2) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  mutate(pred=predict(n_co2_tmax,newdata=.)) %>% 
  rename(`3-mo Tmax Anom. (C)` = tmax_anom_3mo, 
         `12-mo P:PET Anom.` = pe_anom_12mo) %>% 
  ggplot(data=., aes(mape, pred, 
                     color=as_factor(co2)))+
  geom_line()+
  scale_color_viridis_d(expression(paste(CO[2]~ppm)), 
                        option='B', end=0.75)+
  scale_x_continuous(expand=c(0,0))+
  labs(x="Mean Annual P:PET", y='NDVI')+
  facet_grid(`12-mo P:PET Anom.`~`3-mo Tmax Anom. (C)`, scales = 'fixed',labeller = label_both)+
  theme_linedraw()


#-----------------------------------------------------------
n_mape_co2_tmax <- nls_multstart(ndvi_3mo ~ 
        (a0)*tanh(a1*mape)+
        (a2*cco2)+
        (a3*pe_anom_12mo)+
        (a4*tmax_anom_12mo),
      data=train_dat,
                            iter=5,
                            supp_errors = 'Y',
                            control=nls.control(maxiter=100),
                            start_lower = c(a0=0, 
                                            a1=0,
                                          a2=0, 
                                            a3=0,
                                            a4=0                                            # a5=0, 
                                            # a6=0
                                            # b0=0, 
                                            # b1=0, 
                                            # b2=0,
                                            # b3=0
                            ),
                            # d0=0,d1=0,d2=0), 
                            start_upper = c(a0=1, 
                                            a1=1,
                                            a2=1, 
                                            a3=1,
                                            a4=1
                                            # a5=1,
                                            # a6=1
                                            # # b0=1, 
                                            # b1=1, 
                                            # b2=0.1, 
                                            # b3=1
                            ))
summary(n_mape_co2_tmax)
sqrt(mean((predict(n_mape_co2_tmax, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_mape_co2_tmax, newdata=test_dat),test_dat$ndvi_3mo)**2

expand_grid(mape=seq(0.1,2,length.out=50),
            tmax_anom_12mo = c(-3,0,3),
            tmax_u_3mo=25,
            cco2 = c(-40,0,40),
            pe_anom_12mo=c(-0.35,0,0.35)) %>%
  mutate(pe_12mo = mape) %>% 
  mutate(matmax = predict(f_matmax, newdata=. )) %>% 
  mutate(tmax_3mo = predict(f_tmax_3mo, newdata=.)) %>% 
  mutate(vpd15_3mo = predict(f_vpd15_3mo, newdata=.)) %>% 
  mutate(co2_trend = cco2+center_co2) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  mutate(pred=predict(n_mape_co2_tmax,newdata=.)) %>% 
  rename(`12-mo Tmax Anom. (C)` = tmax_anom_12mo, 
         `12-mo P:PET Anom.` = pe_anom_12mo) %>% 
  ggplot(data=., aes(mape, pred, 
                     color=as_factor(co2)))+
  geom_line()+
  geom_hline(aes(yintercept=0.8),lty=3)+
  scale_color_viridis_d(expression(paste(CO[2]~ppm)), 
                        option='B', end=0.75)+
  scale_x_continuous(expand=c(0,0))+
  labs(x="Mean Annual P:PET", y='NDVI')+
  facet_grid(`12-mo P:PET Anom.`~`12-mo Tmax Anom. (C)`, scales = 'fixed',labeller = label_both)+
  theme_linedraw()
ggsave("figures/n_mape_co2_tmax_wLinAnom_facet.png", 
       width=20, height=18, units='cm',dpi=350,type='cairo')



# tmax peak model --------------------------------------------------------------
tmp[,`:=`(pe_anom_12mo = pe_12mo - mape)]
train_dat <- tmp[season=='SON'][mape<2][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 100000)]
test_dat <- tmp[season=='SON'][mape<2][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 100000)]

fn5 <- function(x,x2,a,s,s2) exp(-exp(a*(x-s+s2*matmax)**2))
curve(exp(-exp(-1.443*x)), 0,2)
curve(exp(log(exp(x)-1)), -3,3)
curve(-log(x-log(x)),0,10)
curve((x**2),-5,5)


n_fn5 <- nls_multstart(ndvi_3mo ~ #exp(-exp(a*(tmax_anom_3mo-s)**2)) + 
                         # exp(-exp(-s3*pe_anom_12mo - s2*mape - s*cco2)) - 0.3,
         exp(-exp(-s3*pe_anom_12mo - s2*mape - s*cco2)) - 
           s4*co2_trend,
                                 data=train_dat,
                                 iter=10,
                                 supp_errors = 'Y',
                                 control=nls.control(maxiter=100),
                                 start_lower = c(#a=0, 
                                                 s=0,
                                                 s2=0 ,
                                                 s3=0,
                                                 s4=0
                                                 # a6=0
                                                 # b0=0, 
                                                 # b1=0, 
                                                 # b2=0,
                                                 # b3=0
                                 ),
                                 # d0=0,d1=0,d2=0), 
                                 start_upper = c(#a=1, 
                                                 s=0.01,
                                                 s2=1,
                                                 s3=1,
                                                 s4=0.1
                                                 # a6=1
                                                 # # b0=1, 
                                                 # b1=1, 
                                                 # b2=0.1, 
                                                 # b3=1
                                 ))
summary(n_fn5)
sqrt(mean((predict(n_fn5, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_fn5, newdata=test_dat),test_dat$ndvi_3mo)**2

train_dat %>% as_tibble() %>%  
  sample_n(10000) %>% 
  mutate(val = tmax_anom_3mo/mape) %>% ggplot(data=., aes(val,ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method='nls', se=F, 
              formula = y~exp(-exp(a*x-1)), 
              method.args=list(start=c("a"=0.1)), 
              color='red')

test_dat %>% as_tibble() %>% 
  sample_n(10000) %>% 
  mutate(pred=predict(n_fn5, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo,color=tmax_anom_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')+
  scico::scale_colour_scico(limits=c(-2,2),oob=scales::squish, 
                            palette = 'vik')+
  theme_black()

expand_grid(mape=seq(0.1,2,length.out=100),
            pe_anom_12mo = c(0),
            cco2=c(-40,0,40),
            tmax_u_3mo=c(15,25,35), 
            tmax_anom_3mo=c(0)) %>% 
  mutate(co2_trend = cco2+center_co2) %>% 
  mutate(pred = predict(n_fn5,newdata=.)) %>% 
  ggplot(data=., aes(mape, pred,color=cco2,group=cco2))+
  geom_line()+
  geom_point(data=train_dat[sample(.N,100)],aes(mape,ndvi_3mo))+
  scale_color_viridis_c(end=0.9)

expand_grid(tmax_anom_3mo=seq(-8,8,length.out=100), 
            tmax_u_3mo=c(15,25,35), 
            mape=0.5) %>% 
  mutate(pred = predict(n_fn5,newdata=.)) %>% 
  ggplot(data=., aes(tmax_anom_3mo, pred,color=tmax_u_3mo,group=tmax_u_3mo))+
  geom_line()+
  geom_point(data=train_dat[sample(.N,100)],aes(tmax_anom_3mo,ndvi_3mo))+
  scale_color_viridis_c(end=0.9)

expand_grid(tmax_anom_3mo=c(-5,0,5), 
            tmax_u_3mo=seq(10,35,length.out = 100)) %>% 
  mutate(pred = predict(n_fn5,newdata=.)) %>% 
  ggplot(data=., aes(tmax_u_3mo, pred,color=tmax_anom_3mo,group=tmax_anom_3mo))+
  geom_line()+
  geom_point(data=train_dat[sample(.N,200)],aes(tmax_u_3mo,ndvi_3mo))+
  scale_color_viridis_c(end=0.9)




# Compare nonlinear models ------------------------------------------------
bbmle::AICtab(n_ric)
bbmle::AICtab(# n_ric, n3,n4,n5, n_ric_off, n_ric_6p, 
              n_ric_x2,
              n_fpl,
              n_exp,
              n_exp_x2, 
              n_exp_x2_6p, 
              # n_exp_x2_6p_tmax, 
              n_exp_co2_tmax, 
              n_exp_co2_tmax_2, 
              n_exp_co2_tmax_3, n_exp_co2_tmax_4, n_exp_co2_tmax_5, 
              n_gs_1, n_gs_2, n_gs_3)

train_dat %>% 
  mutate(pred_ric = predict(n_ric, newdata=.),
         pred6 = predict(n_exp_co2_tmax, newdata=.),
         pred3 = predict(n3, newdata=.), 
         pred4 = predict(n4, newdata=.), 
         pred5 = predict(n5, newdata=.)) %>% 
  # mutate(res = pred - ndvi_3mo) %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(pe_12mo, ndvi_3mo))+
  geom_smooth(aes(pe_12mo, pred_ric),color='blue',se=F)+
  geom_smooth(aes(pe_12mo, pred3),color='white',se=F)+
  geom_smooth(aes(pe_12mo, pred4),color='yellow',se=F)+
  geom_smooth(aes(pe_12mo, pred5),color='red',se=F)+
  geom_smooth(aes(pe_12mo, pred6),color='green',se=F)+
  theme_black()

train_dat %>% 
  mutate(pred_ric = predict(n_ric, newdata=.), 
         pred3 = predict(n3, newdata=.), 
         pred4 = predict(n4, newdata=.), 
         pred5 = predict(n5, newdata=.)) %>% 
  # mutate(res = pred - ndvi_3mo) %>% 
  sample_n(1000) %>% 
  ggplot(data=., )+
  geom_smooth(aes(pred_ric,ndvi_3mo),color='green',se=F)+
  geom_smooth(aes(pred3,ndvi_3mo),color='white',se=F)+
  geom_smooth(aes(pred4,ndvi_3mo),color='yellow',se=F)+
  geom_smooth(aes(pred5,ndvi_3mo),color='red',se=F)+
  geom_abline(aes(intercept=0,slope=1),color='blue')+
  theme_black()


train_dat %>% 
  as_tibble() %>% 
  mutate(pred_ric = predict(n_ric, newdata=.),
         pred3 = predict(n3, newdata=.), 
         pred4 = predict(n4, newdata=.), 
         pred5 = predict(n5, newdata=.), 
         pred6 = predict(n_exp_co2_tmax, newdata=.)) %>% 
  summarize(rmse_ric = sqrt(mean((pred_ric-ndvi_3mo)**2,na.rm=TRUE)), 
            rmse3 = sqrt(mean((pred3-ndvi_3mo)**2,na.rm=TRUE)), 
            rmse4 = sqrt(mean((pred4-ndvi_3mo)**2,na.rm=TRUE)), 
            rmse5 = sqrt(mean((pred5-ndvi_3mo)**2,na.rm=TRUE)), 
            rmse6 = sqrt(mean((pred6-ndvi_3mo)**2,na.rm=TRUE)))
















tmp[,`:=`(pe_anom_12mo = pe_12mo - mape)]
train_dat <- tmp[season=='SON'][mape<2][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 100000)]
test_dat <- tmp[season=='SON'][mape<2][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 100000)]

tmp[sample(.N,10000)] %>% 
  ggplot(data=., aes(date, npv))+
  # geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~vc, scales = 'free')


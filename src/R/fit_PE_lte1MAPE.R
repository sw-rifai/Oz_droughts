library(brms)
library(mgcv); library(gratia); library(nls.multstart)
library(tidyverse)
library(data.table); setDTthreads(threads = 8)
library(lubridate); 
library(dtplyr);
library(broom)
options(mc.cores=parallel::detectCores()-3) 
set.seed(333)
# IMPORT ###################################################################
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
                                         "nirv_m","nirv_hyb",
                                         "evi2_hyb","evi2_m")) %>% 
  as.data.table()
vi <- frac[vi,on=.(x,y,date)]

lvi <- lazy_dt(vi)

lvi %>% group_by(x,y) %>% summarize(ndvi_u =mean(ndvi_hyb,na.rm=TRUE)) %>% show_query()

norms_vi <- vi[,`:=`(month=month(date))] %>% 
  .[,.(ndvi_u = mean(ndvi_hyb,na.rm=TRUE), 
       ndvi_sd = sd(ndvi_hyb,na.rm=TRUE)),keyby=.(x,y,month)]

vi <- norms_vi[vi,on=.(x,y,month)] %>% 
  .[,`:=`(ndvi_anom = ndvi_hyb - ndvi_u)] %>% 
  .[,`:=`(ndvi_anom_sd = ndvi_anom/ndvi_sd)]

dat <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
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
                             # "ndvi_anom_sd",
                             # "ndvi_mcd",
                             'vc','veg_class',
                             'month',
                             "x", "y", "year")) %>% 
  as.data.table() %>% 
  .[is.infinite(mape)==F] %>% 
  .[,`:=`(p_anom_12mo_frac = precip_anom_12mo/map)]


# unique(dat[,.(vc,veg_class)]) %>% View
# dat <- dat[order(x,y,date)][,tmean := (tmax+tmin)/2]
dat <- dat[order(x,y,date)][, tmax_3mo := frollmean(tmax,n = 3,fill = NA,align='center'), by=.(x,y)]
dat <- dat[order(x,y,date)][, tmax_u_3mo := frollmean(tmax_u,n = 3,fill = NA,align='center'), by=.(x,y)]
dat <- dat[order(x,y,date)][, tmax_anom_3mo := frollmean(tmax_anom,n = 3,fill = NA,align='center'), by=.(x,y)]

# dat <- dat[order(x,y,date)][, tmean_3mo := frollmean(tmean,n = 3,fill = NA,align='center'), by=.(x,y)]
# dat <- dat[order(x,y,date)][, tmin_3mo := frollmean(tmin,n = 3,fill = NA,align='center'), by=.(x,y)]
# dat <- dat[order(x,y,date)][, tmax_12mo := frollmean(tmax,n = 12,fill = NA,align='right'), by=.(x,y)]
# dat <- dat[order(x,y,date)][, pet_3mo := frollmean(pet,n = 3,fill = NA,align='right'), by=.(x,y)]
dat <- dat[order(x,y,date)][, precip_3mo := frollmean(precip,n = 3,fill = NA,align='right'), by=.(x,y)]
dat <- dat[order(x,y,date)][, vpd15_3mo := frollmean(vpd15,n = 3,fill = NA,align='right'), by=.(x,y)]
# dat <- dat[order(x,y,date)][, vpd15_anom_3mo := frollmean(vpd15_anom,n = 3,fill = NA,align='right'), by=.(x,y)]
# dat <- dat[order(x,y,date)][, pet_24mo := frollmean(pet,n = 24,fill = NA,align='right'), by=.(x,y)]
# dat <- dat[order(x,y,date)][, precip_24mo := frollmean(precip,n = 24,fill = NA,align='right'), by=.(x,y)]
# dat <- dat[order(x,y,date)][, pet_48mo := frollmean(pet,n = 48,fill = NA,align='right'), by=.(x,y)]
# dat <- dat[order(x,y,date)][, precip_48mo := frollmean(precip,n = 48,fill = NA,align='right'), by=.(x,y)]
dat <- dat[order(x,y,date)][, tmax_anom_12mo := frollmean(tmax_anom,n = 12,fill = NA,align='right'), by=.(x,y)]

dat <- dat[,`:=`(#pe_3mo = precip_3mo/pet_3mo, 
  pe_12mo = precip_12mo/pet_12mo 
  # pe_24mo = precip_24mo/pet_24mo, 
  # pe_36mo = precip_36mo/pet_36mo, 
  # pe_48mo = precip_48mo/pet_48mo
)]
dim(dat)
dat <- merge(dat, 
             vi,
             by=c("x","y","date"), 
             all=TRUE,allow.cartesian=TRUE)
dat <- dat[order(x,y,date)][, ndvi_3mo := frollmean(ndvi_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]
dat <- dat[order(x,y,date)][, evi2_3mo := frollmean(evi2_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]
dat <- dat[order(x,y,date)][, nirv_3mo := frollmean(nirv_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]

rm(vi); gc(full=TRUE)

mlo <- readr::read_table("../data_general/CO2_growth_rate/co2_mm_mlo_20200405.txt", 
                         skip = 72, col_names = F) %>% 
  set_names(
    c("year","month","ddate","co2_avg","co2_int","co2_trend","ndays")
  ) %>% 
  mutate(date = ymd(paste(year,month,1))) %>% 
  select(date,co2_int,co2_trend) %>% 
  as.data.table()
dat <- merge(mlo,dat,by="date")
dat <- dat[is.na(ndvi_3mo)==F & is.na(co2_int)==F]
center_co2 <- mean(dat$co2_int)
dat <- dat[,`:=`(cco2=co2_int-center_co2)]
gc()
dat <- dat[is.na(vc)==F]
dat <- dat[str_detect(vc,"Forests") | 
             str_detect(vc, "Eucalypt") |
             str_detect(vc, "Rainforests")]
dat <- dat[ndvi_m>0][ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5]
#*******************************************************************************

#split test & train ------------------------------------------------------------
dat[,`:=`(pe_anom_12mo = pe_12mo - mape)]
train_dat <- dat[season=='SON'][mape<1.5][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 3e6)]
test_dat <- dat[season=='SON'][mape<1.5][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 1e6)]
#*******************************************************************************

# l_ndvi <- lm(ndvi_m~co2_trend*I(pe_anom_12mo/mape) + mape, 
#                  data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5])
# summary(l_ndvi)
l_ndvi <- lm(ndvi_hyb~scale(log(co2_trend))*I(pe_anom_12mo/mape) + scale(mape) + vc, 
                 data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5])
summary(l_ndvi)
l_evi2 <- lm(evi2_hyb~I(pe_anom_12mo/mape) +scale(cco2)*scale(mape) + vc, 
                 data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5])
summary(l_evi2)
l_nirv <- lm(nirv_hyb~I(pe_anom_12mo/mape) + scale(cco2)*scale(mape) + vc, 
                 data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5])
summary(l_nirv)
l_gv <- lm(gv~I(pe_anom_12mo/mape) + scale(cco2)*scale(mape) + vc, 
             data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5])
summary(l_gv)

expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10), 
            vc=dat$vc[100000],
            # pe_anom_12mo = c(-0.15,0,0.15)
            percent_ppet_anom = c(-50,0,50)
  ) %>% 
  mutate(cco2 = co2_trend - center_co2) %>% 
  mutate(pe_anom_12mo = mape*percent_ppet_anom/100) %>% 
  mutate(pe_12mo = mape+pe_anom_12mo) %>% 
  filter(pe_12mo > 0) %>%
  mutate(pred_ndvi = predict(l_ndvi, newdata=.)) %>% 
  mutate(pred_evi2 = predict(l_evi2, newdata=.)) %>% 
  mutate(pred_nirv = predict(l_nirv, newdata=.)) %>% 
  mutate(pred_gv = predict(l_gv, newdata=.)) %>% 
  select(pred_ndvi, pred_evi2, pred_nirv,pred_gv,co2_trend,mape,percent_ppet_anom) %>% 
  gather(-co2_trend, -mape,-percent_ppet_anom, key='VI', value='value') %>% 
  ggplot(data=.,aes(mape, value,color=co2_trend,group=co2_trend))+
  geom_line()+
  scale_color_viridis_c(option='B')+
  facet_grid(VI~percent_ppet_anom,labeller = label_both,scales='free')


train_dat[mape<1] %>% sample_n(10000) %>% 
  mutate(anom = pe_anom_12mo/pe_12mo) %>% 
  ggplot(data=.,aes(mape,npv))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method='lm',se=F,color='red')

train_dat[mape<1] %>% sample_n(50000) %>% 
  mutate(anom = pe_anom_12mo/mape) %>% 
  mutate(mape_d = cut_interval(mape,n = 4)) %>% 
  ggplot(data=.,aes(precip_anom_12mo, anom))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method='lm',se=F,color='red')+
  facet_wrap(~mape_d)




  
expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10), 
            vc=dat$vc[100000],
            # pe_anom_12mo = c(-0.15,0,0.15)
            vpd15_anom_sd = c(-1.5,0,1.5)
) %>% 
mutate(cco2 = co2_trend - center_co2) %>% 
# mutate(pe_anom_12mo = mape*percent_ppet_anom/100) %>% 
# mutate(pe_12mo = mape+pe_anom_12mo) %>% 
# filter(pe_12mo > 0) %>%
mutate(pred_evi2 = predict(l_evi2_2, newdata=.)) %>% 
ggplot(data=.,aes(mape, pred_evi2,color=co2_trend,group=co2_trend))+
geom_line()+
scale_color_viridis_c(option='B')+
  facet_wrap(~vpd15_anom_sd)





l_evi2_2 <- bam(evi2_m~ 
                  mape+
                  I(pe_anom_12mo/mape)+
                  s(vc,bs='re'), 
                data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5], 
                select=TRUE, discrete = TRUE)
l_evi2_3 <- bam(evi2_m~ 
                  mape*cco2+
                  I(pe_anom_12mo/mape)+
                  s(vc,bs='re'), 
                data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5], 
                select=TRUE, discrete = TRUE)
l_evi2_4 <- bam(evi2_m~ 
                  mape+
                  I(pe_anom_12mo/mape)*cco2+
                  s(vc,bs='re'), 
                data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5], 
                select=TRUE, discrete = TRUE)
l_evi2_5 <- bam(evi2_m~ 
                  mape*cco2+
                  I(pe_anom_12mo/mape)*cco2+
                  s(vc,bs='re'), 
                data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5], 
                select=TRUE, discrete = TRUE)
l_evi2_5 <- bam(evi2_m~ 
                  scale(pet_anom_3mo)+
                  precip_anom_3mo+
                  mape*cco2+
                  I(pe_anom_12mo/mape)*cco2+
                  s(vc,bs='re'), 
                data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5], 
                select=TRUE, discrete = TRUE)

bbmle::AICtab(l_evi2_2, l_evi2_3, l_evi2_4, l_evi2_5)

predict(l_evi2_5, newdata=test_dat)
predict(l_evi2_5, exclude="scale(pet_anom_3mo)", newdata=test_dat)



summary(l_evi2_2)
plot(l_evi2_2,scheme = 2)
o <- gratia::evaluate_smooth(l_evi2_2, 's(mape')
o %>% 
  # mutate(pe_anom_12mo_d = cut_interval(pe_anom_12mo, 6)) %>% 
  ggplot(data=.,aes(mape,est))+
  geom_point()+
  geom_smooth(method='lm')
  # facet_wrap(~pe_anom_12mo_d)
  
train_dat %>% 
  sample_n(1000) %>% 
  mutate(map_d = cut_interval(map,n=4)) %>% 
  ggplot(data=.,aes(co2_trend,pet_anom_3mo,color=map_d))+
  geom_point()+
  geom_smooth(method='lm',se=F)



l_ndvi <- lm(ndvi_hyb~scale(log(co2_trend))*I(pe_anom_12mo/mape) + scale(mape) + vc, 
             data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5])
summary(l_ndvi)


train_dat %>% 
  sample_n(1e4) %>% 
  mutate(mape_d = cut_width(mape, 0.1)) %>% 
  group_by(mape_d) %>% 
  summarize(fit = lm(ndvi_3mo ~ scale(log(co2_trend))*I(pe_anom_12mo/mape) + scale(mape) + vc)) %>% 
  ungroup()

dat[is.na(season)==FALSE][mape<1.5][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5] %>% 
  as_tibble() %>% 
  mutate(mape_d = cut_width(mape, 0.15)) %>% 
  nest(data = c(-mape_d,-season)) %>% 
  mutate(fit = map(data, 
         ~lm(evi2_3mo~scale(co2_trend)+scale(pe_anom_12mo)+scale(vpd15_anom_sd)+vc, 
       # ~lm(ndvi_3mo~scale(co2_trend)+scale(I(pe_anom_12mo/mape)), 
           data=.x)), 
         tidied = map(fit, tidy)) %>% 
  unnest(tidied) %>% 
  filter(term!="(Intercept)") %>%
  filter(str_detect(term, 'co2') | 
           str_detect(term, 'tmax_anom_3mo')|
           str_detect(term, 'pe_anom_12mo')) %>% 
  ggplot(data=., aes(mape_d, estimate))+
  geom_col()+
  geom_errorbar(aes(ymin=estimate-2*std.error, 
                    ymax=estimate+2*std.error,
                    color=p.value),
                width=0.2)+
  scale_color_viridis_c(option='B',direction=-1,begin = 0.1,end=0.9, 
                        limits=c(0,0.1),oob=scales::squish)+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))+
  facet_grid(season~term, scales='free')

dat[is.na(season)==FALSE][mape<1.5][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5] %>% 
  as_tibble() %>% 
  mutate(mape_d = cut_width(mape, 0.15)) %>% 
  mutate(season = factor(season, levels=c("SON","DJF","MAM","JJA"),ordered = T)) %>% 
  nest(data = c(-mape_d,-season)) %>% 
  mutate(fit = map(data, 
       ~lm(ndvi_3mo~scale(co2_trend)*scale(I(pe_anom_12mo/mape))+vc, 
           # ~lm(ndvi_3mo~scale(co2_trend)+scale(I(pe_anom_12mo/mape)), 
           data=.x)), 
         tidied = map(fit, tidy)) %>% 
  unnest(tidied) %>% 
  filter(str_detect(term,"vc")==F) %>%
  filter(str_detect(term,"Intercept")==F) %>%
  # filter(str_detect(term, 'co2') | 
  #          str_detect(term, 'tmax_anom_3mo')|
  #          str_detect(term, 'pe_anom_12mo')) %>% 
  ggplot(data=., aes(mape_d, estimate))+
  geom_col()+
  geom_errorbar(aes(ymin=estimate-2*std.error, 
                    ymax=estimate+2*std.error),
                width=0.2)+
  # scale_fill_viridis_c(option='B',direction=1,begin = 0.1,end=0.9, 
  #                       limits=c(0,0.05),oob=scales::squish)+
  # scale_color_viridis_c(option='B',direction=1,begin = 0.1,end=0.9, 
  #                       limits=c(0,0.05),oob=scales::squish)+
  scale_x_discrete(guide = guide_axis(n.dodge = 1,angle = 90))+
  labs(x='Mean Annual P:PET Range', 
       title='NDVI Linear Model Effects', 
       subtitle = 'approx. 19 million obs')+
  facet_grid(season~term, scales = 'free_y', 
             labeller = labeller(
    term=c("scale(co2_trend)"="CO2", 
           "scale(co2_trend):scale(I(pe_anom_12mo/mape))"="CO2 x P:PET_anom", 
           "scale(I(pe_anom_12mo/mape))"="P:PET anom")))+
  theme_linedraw()
ggsave(filename = 'figures/big_linearModel_by_MAPPET_range.png', 
       height=14, width=16, units='cm')


# Asym-Drop*exp(-exp(lrc)*x^pwr)
curve(SSweibull(x,Asym = 0.9,Drop = 0.8,lrc=0.25,pwr=1),0,2)
curve(SSweibull(x,Asym = 0.9,Drop = 0.8,lrc=0.25,pwr=0.5),0,2,add=T,col='red')
curve(SSweibull(x,Asym = 0.9,Drop = 0.8,lrc=0.25,pwr=1.5),0,2,add=T,col='blue')
curve(SSweibull(x,Asym = 0.74,Drop = 0.613,lrc=0.57,pwr=1.45),0,2,add=T,col='purple',lwd=2)
train_dat[sample(.N,1000)] %>% points(ndvi_3mo~mape,data=.,pch=20,col='gray')

n1 <-  train_dat[sample(.N,1e5)] %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
                                 SSweibull(mape, Asym, Drop, lrc ,pwr),
                               data = .,
                               iter = 3,
                               start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0),
                               start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2),
                               # supp_errors = 'Y',
                               na.action = na.omit)
summary(n1)

n2 <-  train_dat[sample(.N,1e5)] %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
             Asym-Drop*exp(-exp(lrc)*mape^pwr) + B1*(pe_anom_12mo/mape),
           data = .,
           iter = 3,
           start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5),
           start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5),
           # supp_errors = 'Y',
           na.action = na.omit)
summary(n2)

n3 <- train_dat[sample(.N,1e5)] %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
               Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
               B1*(pe_anom_12mo/mape) + B2*(tmax_anom_3mo),
               data = .,
               iter = 3,
               start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0),
               start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001),
               # supp_errors = 'Y',
               na.action = na.omit)
summary(n3)

n4 <- train_dat[sample(.N,1e5)] %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
             Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
             B1*(pe_anom_12mo/mape) + B2*(cco2) + B3*(cco2*pe_anom_12mo/mape),
           data = .,
           iter = 3,
           start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0),
           start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001),
           # supp_errors = 'Y',
           na.action = na.omit)
summary(n4)

n5 <- train_dat[sample(.N,1e5)] %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
           Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
           B1*(pe_anom_12mo/mape) + B2*(tmax_anom_3mo) + B3*(cco2) + B4*(cco2*mape),
           data = .,
           iter = 3,
           start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0,B4=0),
           start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001,B5=0.001),
           # supp_errors = 'Y',
           na.action = na.omit)
summary(n5)

n6 <- train_dat[sample(.N,1e5)] %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
       Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
     B1*(pe_anom_12mo/mape) + B2*(tmax_anom_3mo) + B3*(cco2) + B4*(cco2*mape)+B5*(cco2*tmax_anom_3mo),
     data = .,
     iter = 1,
     start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0,B4=0,B5=0),
     start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001,B4=0.001,B5=0.001),
     # supp_errors = 'Y',
     na.action = na.omit)
summary(n6)

n7 <- train_dat[sample(.N,1e6)] %>%
  mutate(p_anom_frac = precip_anom_12mo/map, 
         pet_anom_frac = pet_anom_3mo/pet_u) %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
     Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
     B1*(precip_anom_12mo/map) + B2*(pet_anom_3mo/pet_u) + 
     B3*(cco2) + B4*(cco2*precip_anom_12mo/map)+B5*(cco2*pet_anom_3mo/pet_u),
   data = .,
   iter = 1,
   start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0,B4=0,B5=0),
   start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001,B4=0.001,B5=0.001),
   # supp_errors = 'Y',
   na.action = na.omit)
summary(n7)

# n7 <- train_dat[sample(.N,1e6)] %>%
#   mutate(p_anom_frac = precip_anom_12mo/map, 
#          pet_anom_frac = pet_anom_3mo/pet_u) %>% 
#   nls.multstart::nls_multstart(ndvi_3mo ~ 
#        Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
#        B1*(p_anom_frac) + B2*(pet_anom_frac) + 
#        B3*(cco2) + B4*(cco2*p_anom_frac)+B5*(cco2*pet_anom_frac),
#      data = .,
#      iter = 1,
#      start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0,B4=0,B5=0),
#      start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001,B4=0.001,B5=0.001),
#      # supp_errors = 'Y',
#      na.action = na.omit)
# summary(n7)

n8 <- train_dat[sample(.N,5e5)] %>%
  mutate(p_anom_frac = precip_anom_12mo/map, 
         pet_anom_frac = pet_anom_3mo/pet_u) %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
       co2_trend*Asym-co2_trend*Drop*exp(-exp(lrc)*mape^pwr) + 
       B1*(precip_anom_12mo/map) + B2*(pet_anom_3mo/pet_u) + 
       B3*(cco2) + B4*(cco2*precip_anom_12mo/map)+B5*(cco2*pet_anom_3mo/pet_u),
     data = .,
     iter = 1,
     start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0,B4=0,B5=0),
     start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001,B4=0.001,B5=0.001),
     # supp_errors = 'Y',
     na.action = na.omit)
summary(n8)

yardstick::rsq_trad_vec(test_dat$ndvi_3mo,estimate=predict(n1,newdata=test_dat))
yardstick::rsq_trad_vec(test_dat$ndvi_3mo,estimate=predict(n2,newdata=test_dat))
yardstick::rsq_trad_vec(test_dat$ndvi_3mo,estimate=predict(n3,newdata=test_dat))
yardstick::rsq_trad_vec(test_dat$ndvi_3mo,estimate=predict(n4,newdata=test_dat))
yardstick::rsq_trad_vec(test_dat$ndvi_3mo,estimate=predict(n5,newdata=test_dat))
yardstick::rsq_trad_vec(test_dat$ndvi_3mo,estimate=predict(n6,newdata=test_dat))
yardstick::rsq_trad_vec(test_dat$ndvi_3mo,estimate=predict(n7,newdata=test_dat))
yardstick::rsq_trad_vec(test_dat$ndvi_3mo,estimate=predict(n8,newdata=test_dat))

yardstick::rmse_vec(test_dat$ndvi_3mo,estimate=predict(n1,newdata=test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo,estimate=predict(n2,newdata=test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo,estimate=predict(n3,newdata=test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo,estimate=predict(n4,newdata=test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo,estimate=predict(n5,newdata=test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo,estimate=predict(n6,newdata=test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo,estimate=predict(n7,newdata=test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo,estimate=predict(n8,newdata=test_dat))



o_preds <- expand_grid(season=unique(train_dat$season),
  co2 = seq(min(dat$co2_int),max(dat$co2_int),length.out=50),
  mape = seq(0.05,1.5,length.out = 200), 
  pct_anom = c(-33,0,33)) %>% 
  mutate(pe_anom_12mo = 0.01*pct_anom*mape+mape) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(n6, newdata=.))


o_preds %>% 
  ggplot(data=., aes(mape,pred,color=(co2), group=co2))+
  geom_line(alpha=1)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0,1.5),
                     breaks=c(0,0.5,1,1.5),
                     labels = c(0,0.5,1,1.5),
                     expand=c(0,0)
                     # guide = guide_axis(n.dodge=1, angle=0,check.overlap = TRUE)
  )+
  scale_y_continuous(#limits=c(0,0.9), 
                     expand=c(0,0))+
  labs(x=expression(paste("Mean Annual P:PET")),
       #x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  facet_grid(pct_anom~vpd15_anom)+
  theme_linedraw()+
  guides(color=guide_colorbar(title.position = 'top'))+
  theme(#panel.grid = element_blank(),
        # panel.spacing.x = unit(6, "mm"),
        axis.text = element_text(size=10),
        # axis.text.x = element_text(angle=45, vjust=-0.5),
        legend.position = c(0.85,0.1), 
        legend.key.width = unit(0.65,'cm'),
        legend.direction = 'horizontal', 
        legend.background = element_rect(fill=NA))




 # Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
 # B1*(precip_anom_12mo/map) + B2*(pet_anom_3mo/pet_u) + 
 # B3*(cco2) + B4*(cco2*precip_anom_12mo/map)+B5*(cco2*pet_anom_3mo/pet_u),

train_dat %>% 
  sample_n(10000) %>% 
  lm(map~mape,data=.) %>% summary
ggplot(data=.,aes(mape,pet_u))+
  geom_point()

o_preds <- expand_grid(season=unique(train_dat$season),
             co2 = seq(min(dat$co2_int),max(dat$co2_int),length.out=50),
             mape = seq(0.05,1.5,length.out = 200), 
             x_mape = c(-33,0,33), 
             x_map = c(-50,0,50), 
             x_pet = c(-33,0,33)) %>%
  mutate(map=333+828*mape, 
         pet_u = 217 - 63*mape) %>% 
  mutate(pe_anom_12mo = 0.01*x_mape*mape) %>% 
  mutate(precip_anom_12mo = 0.01*x_map*map) %>% 
  mutate(pet_anom_3mo = 0.01*x_pet*pet_u) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(n7, newdata=.))

o_preds %>% 
  filter(x_mape == 0) %>% 
  ggplot(data=., aes(mape,pred,color=(co2), group=co2))+
  geom_line(alpha=1)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0,1.5),
                     # breaks=c(0,0.5,1,1.5),
                     # labels = c(0,0.5,1,1.5),
                     expand=c(0,0)
                     # guide = guide_axis(n.dodge=1, angle=0,check.overlap = TRUE)
  )+
  scale_y_continuous(#limits=c(0,0.9), 
    expand=c(0,0))+
  labs(x=expression(paste("Mean Annual P:PET")),
       #x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  facet_grid(x_map~x_pet, labeller = label_both)+
  theme_linedraw()+
  guides(color=guide_colorbar(title.position = 'top'))+
  theme(#panel.grid = element_blank(),
    # panel.spacing.x = unit(6, "mm"),
    axis.text = element_text(size=10),
    # axis.text.x = element_text(angle=45, vjust=-0.5),
    legend.position = c(0.85,0.1), 
    legend.key.width = unit(0.65,'cm'),
    legend.direction = 'horizontal', 
    legend.background = element_rect(fill=NA))


test_dat %>% 
  mutate(pred=predict(n7,newdata=.)) %>% 
  sample_n(10000) %>% 
  ggplot(data=.,aes(pred,ndvi_3mo))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(se=F)+
  geom_abline(aes(intercept=0,slope=1),col='red')

test_dat %>% 
  mutate(pred=predict(n7,newdata=.)) %>% 
  sample_n(10000) %>% 
  ggplot(data=.,aes(mape,ndvi_3mo))+
  geom_point()+
  geom_point(aes(mape,pred),color='blue')


fn <- function(x,y) {
  co2_trend <- x
  cco2 <- x-center_co2
  d <- dat[1000,] %>% 
    as_tibble() %>% 
    select(-co2_trend, -cco2)
  d$co2_trend <- x
  d$cco2 <- cco2
  out <- predict(n8,newdata=d)
  return(out)}
fn(40)
fn(420)-fn(340)
fn <- Vectorize(fn)
curve(fn(x), 
      340,440)

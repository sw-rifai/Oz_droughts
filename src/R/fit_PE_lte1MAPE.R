library(brms)
library(mgcv); library(gratia); library(nls.multstart)
library(tidyverse)
library(data.table); setDTthreads(threads = 8)
library(lubridate); 
library(dtplyr);
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
                             # "ndvi_anom_sd",
                             # "ndvi_mcd",
                             'vc','veg_class',
                             'month',
                             "x", "y", "year")) %>% 
  as.data.table() %>% 
  .[is.infinite(mape)==F] %>% 
  .[,`:=`(p_anom_12mo_frac = precip_anom_12mo/map)]


# unique(tmp[,.(vc,veg_class)]) %>% View
# tmp <- tmp[order(x,y,date)][,tmean := (tmax+tmin)/2]
tmp <- tmp[order(x,y,date)][, tmax_3mo := frollmean(tmax,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmax_u_3mo := frollmean(tmax_u,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmax_anom_3mo := frollmean(tmax_anom,n = 3,fill = NA,align='center'), by=.(x,y)]

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
tmp <- tmp[order(x,y,date)][, evi2_3mo := frollmean(evi2_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, nirv_3mo := frollmean(nirv_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]

rm(vi); gc(full=TRUE)

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
tmp <- tmp[is.na(vc)==F]
tmp <- tmp[str_detect(vc,"Forests") | 
             str_detect(vc, "Eucalypt") |
             str_detect(vc, "Rainforests")]
tmp <- tmp[ndvi_m>0][ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5]
#*******************************************************************************

#split test & train ------------------------------------------------------------
tmp[,`:=`(pe_anom_12mo = pe_12mo - mape)]
train_dat <- tmp[season=='SON'][mape<2][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 3e6)]
test_dat <- tmp[season=='SON'][mape<2][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 1e6)]
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
            vc=tmp$vc[100000],
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
            vc=tmp$vc[100000],
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

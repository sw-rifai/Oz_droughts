library(qgam); library(tidyverse); library(lubridate); 
library(colorspace)
library(mgcv); library(gratia); library(nls.multstart)
library(tidyverse)
library(data.table); setDTthreads(threads = 8)
library(lubridate); 
library(dtplyr);
library(nls.multstart)
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

vi <- arrow::read_parquet("../data_general/MCD43/MCD43_AVHRR_NDVI_hybrid_2020-08-08.parquet", 
                          col_select = c("x","y","date",
                                         "ndvi_c","ndvi_mcd","ndvi_hyb", 
                                         "nirv_c","nirv_mcd","nirv_hyb",
                                         "evi2_mcd","evi2_hyb",
                                         'fpar','fpar_hyb')) %>% 
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
dat <- dat[x>= 140] # FILTER TO LON >= 140
dat <- dat[ndvi_hyb>0][ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5]
dat[,`:=`(pe_anom_12mo = pe_12mo - mape)]
dat[,`:=`(epoch = ifelse(date<ymd("2000-12-31"),'avhrr','modis'))]
dat <- dat %>% mutate(epoch = as_factor(epoch), 
                      season = factor(season, levels=c("SON","DJF","MAM","JJA")))
#*******************************************************************************

#split test & train ------------------------------------------------------------
train_dat <- dat[season=='SON'][mape<1.5][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 1e6)]
test_dat <- dat[season=='SON'][mape<1.5][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 1e6)]
gc(full = T)
#*******************************************************************************

quSeq <- c(0.10, 0.5, 0.975)
fit <- mqgam(ndvi_3mo ~ 
               ti(cco2,k=3,bs='cs')+
               ti(mape,bs='cs',k=3) +
               ti(co2_trend,mape,k=3,bs='cs')+
               s(I(pe_anom_12mo/mape),k=3,bs='cs')+
               # s(I(pe_anom_12mo/mape),bs='cs',k=5) + 
               epoch, 
            data=train_dat[sample(.N, 100000)], 
            multicore=TRUE,
            ncores=10,
            qu = quSeq)
qdo(fit,0.975,plot, scale=0,scheme=2)
qdo(fit,quSeq[1],summary, scale=0)
qdo(fit,quSeq[2],summary, scale=0)
qdo(fit,quSeq[3],summary, scale=0)

# 0.74, 0.78, 0.71

zz <- expand_grid(season=unique(train_dat$season),
            co2=c(340,380,420),
            # co2 = seq(min(dat$co2_trend),max(dat$co2_trend),length.out=3),
            mape = seq(0.05,1.5,length.out = 100), 
            epoch=train_dat$epoch[1],
            pct_anom = c(-66,0,33), 
            iq = quSeq) %>% 
  mutate(co2_trend=co2) %>% 
  mutate(pe_anom_12mo = 0.01*pct_anom*mape) %>%
  mutate(pe_12mo = pe_anom_12mo+mape) %>%
  mutate(cco2 = co2-center_co2)

zz <- zz %>% 
  mutate(pred_high = qdo(fit, qu=quSeq[3], predict, newdata=.)) %>% 
  mutate(pred_med = qdo(fit, qu=quSeq[2], predict, newdata=.)) %>% 
  mutate(pred_low = qdo(fit, qu=quSeq[1], predict, newdata=.))

d_mean <- train_dat %>% summarize(value=mean(ndvi_3mo),
                        mape=mean(mape))


p_q <- zz %>% 
  select(co2,mape,pct_anom,pred_high, pred_med, pred_low) %>% 
  gather(-co2,-mape,-pct_anom, key='key',value='value') %>% 
  mutate(key=factor(key,levels = c('pred_high','pred_med','pred_low'),ordered = T)) %>% 
  ggplot(data=., aes(mape, value,color=co2,group=co2))+
  # geom_hline(aes(yintercept=c(0.1)),color='red',lty=3)+
  # geom_hline(aes(yintercept=c(0.9)),color='red',lty=3)+
  # geom_vline(aes(xintercept=0.25),color='blue',lty=3)+
  # geom_vline(aes(xintercept=0.5),color='blue',lty=3)+
  # geom_vline(aes(xintercept=0.75),color='blue',lty=3)+
  # geom_vline(aes(xintercept=1),color='blue',lty=3)+
  geom_line()+
  # scale_color_discrete_sequential('ag_GrnYl',rev = TRUE)+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)),
                        end=0.8,option='B',direction = 1)+
  scale_y_continuous(limits=c(0,1),expand=c(0,0.01))+
  scale_x_continuous(limits=c(0,1.5),
                     expand=c(0,0.1), 
                     breaks=seq(0,1.5,by=0.25),
                     minor_breaks = NULL)+
  labs(x="Mean Annual P:PET", 
       y="NDVI")+
  facet_grid(key~pct_anom,
             labeller = labeller(key=c("pred_high"="Quantile 97.5%", 
                                       "pred_med"="Quantile 50%",
                                       "pred_low"="Quantile 10%"), 
                                 pct_anom=c("-66"="-66% P:PET anom.", 
                                            "0"="0% P:PET anom.",
                                            "33"="33% P:PET anom.")))+
  theme_linedraw()+
  theme(panel.background = element_blank(), 
        panel.grid.minor.y = element_blank()
        #panel.grid = element_blank()
        ); p_q
ggsave(p_q, filename = "figures/quantile_reg_ndvi_ppet_x_co2.png", 
       width=20, height=14, units='cm', dpi=350, type='cairo')


zz %>% 
  select(co2,mape,pct_anom,pred_high, pred_med, pred_low) %>% 
  gather(-co2,-mape,-pct_anom, key='key',value='value') %>% 
  mutate(key=factor(key,levels = c('pred_high','pred_med','pred_low'),ordered = T)) %>% 
  ggplot(data=., aes(mape, value,color=key,group=key))+
  geom_hline(aes(yintercept=c(0.1)),color='red',lty=3)+
  geom_hline(aes(yintercept=c(0.9)),color='red',lty=3)+
  geom_vline(aes(xintercept=0.25),color='blue',lty=3)+
  geom_vline(aes(xintercept=0.5),color='blue',lty=3)+
  geom_vline(aes(xintercept=1),color='blue',lty=3)+
  geom_line()+
  # scale_color_discrete_sequential('ag_GrnYl',rev = TRUE)+
  scale_color_viridis_d(end=0.7,option='D',direction = -1)+
  scale_y_continuous(limits=c(0,1),expand=c(0,0.01))+
  facet_grid(pct_anom~co2,labeller = label_both)+
  theme_linedraw()+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank())

train_dat %>% 
  filter(epoch=='avhrr') %>% 
  lazy_dt() %>% 
  mutate(ppet_anom_frac = pe_anom_12mo/mape) %>% 
  mutate(mape_d = cut_number(pe_12mo,50), 
         co2_d = cut_number(co2_trend,3), 
         p_d = cut_number(ppet_anom_frac,3)) %>% 
  group_by(mape_d,co2_d,p_d) %>% 
  summarize(p99 = quantile(ndvi_3mo,0.99),
            p50=quantile(ndvi_3mo,0.5),
            p10=quantile(ndvi_3mo,0.1),
            mappet = mean(mape,na.rm=TRUE), 
            co2 = mean(co2_trend,na.rm=TRUE), 
            p_anom = mean(ppet_anom_frac,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  gather(-mape_d,-mappet,-co2_d,-co2,-p_d,-p_anom,key='key',value='value') %>%
  ggplot(data=.,aes(mappet, value,color=co2_d,group=co2_d))+
  geom_line()+
  scale_color_viridis_d(option='B',end=0.8)+
  facet_grid(key~p_d)+
  theme_linedraw()



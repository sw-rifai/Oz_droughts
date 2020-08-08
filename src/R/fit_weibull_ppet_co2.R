library(brms)
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
                             "vpd15_anom_3mo",
                             "vpd15_u",
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
dat <- dat[x>=140] # Filter to Lon >= 140
dat <- dat[ndvi_m>0][ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5]
#*******************************************************************************

#split test & train ------------------------------------------------------------
dat[,`:=`(pe_anom_12mo = pe_12mo - mape)]
train_dat <- dat[season=='SON'][mape<1.5][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 1e6)]
test_dat <- dat[season=='SON'][mape<1.5][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 1e6)]
gc(reset = T, full = T)
#*******************************************************************************

test_dat %>% mutate(val = pe_anom_12mo/mape) %>% pull(val) %>% quantile(., c(0.1,0.5,0.9))


w4 <- train_dat[sample(.N,5e5)] %>% 
  nls_multstart(ndvi_3mo ~ 
                  (Asym+B1*cco2)-(Drop)*exp(-exp(lrc+B2*cco2)*mape^(pwr)), 
                # (Asym+B1*cco2)-(Drop+B2*cco2)*exp(-exp(lrc+B3*cco2)*pe_12mo^(pwr)), 
                data = .,
                iter = 1,
                start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=0,B2=0),
                start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.001,B2=0),
                # lower = c(Asym=0.5, Drop=0.4, lrc=0.1, pwr=0.1,B3=-0.01),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(w4)


n4 <- train_dat %>% 
  nls.multstart::nls_multstart(ndvi_3mo ~ 
         Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
         B1*(pe_anom_12mo/mape) + B2*(cco2) + B3*(cco2*pe_anom_12mo/mape),
       data = .,
       iter = 1,
       start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0),
       start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001),
       # supp_errors = 'Y',
       na.action = na.omit)
summary(n4)

n4_evi2 <- train_dat %>% 
  nls.multstart::nls_multstart(evi2_3mo ~ 
                                 Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
                                 B1*(pe_anom_12mo/mape) + B2*(cco2) + B3*(cco2*pe_anom_12mo/mape),
                               data = .,
                               iter = 1,
                               start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0),
                               start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001),
                               # supp_errors = 'Y',
                               na.action = na.omit)
summary(n4)

n4_nirv <- train_dat %>% 
  nls.multstart::nls_multstart(nirv_3mo ~ 
                                 Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
                                 B1*(pe_anom_12mo/mape) + B2*(cco2) + B3*(cco2*pe_anom_12mo/mape),
                               data = .,
                               iter = 1,
                               start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0),
                               start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001),
                               # supp_errors = 'Y',
                               na.action = na.omit)
summary(n4)

n5 <- train_dat %>% 
  nls_multstart(evi2_3mo ~ 
                  Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
                  B1*(vpd15_anom_3mo) + B2*(cco2) + B3*(vpd15_anom_3mo*cco2),
                data = .,
                iter = 3,
                start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0),
                start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(n5)

n6 <- train_dat %>% 
   nls_multstart(evi2_3mo ~ 
     Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
     B1*(tmax_anom_12mo) + B2*(cco2) + B3*(tmax_anom_12mo*cco2),
   data = .,
   iter = 3,
   start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0),
   start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001),
   # supp_errors = 'Y',
   na.action = na.omit)
summary(n6)


n7 <- train_dat %>% 
  nls_multstart(evi2_3mo ~ 
                  Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
                  B1*(vpd15_anom_3mo) + B2*(cco2) + B3*(vpd15_anom_3mo*cco2) + 
                  B4*(cco2*pe_anom_12mo/mape),
                data = .,
                iter = 3,
                start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0,B4=0),
                start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001,B4=0.001),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(n7)

n8 <- train_dat %>% 
  nls_multstart(evi2_3mo ~ 
                  Asym-Drop*exp(-exp(lrc)*mape^pwr) + 
                  B1*(vpd15_anom_3mo) + B2*(cco2) + B3*(vpd15_anom_3mo*cco2) + 
                  B4*(cco2*(precip_anom_12mo/map)),
                data = .,
                iter = 1,
                start_lower = c(Asym=0.0, Drop=0.6,lrc=0,pwr=0,B1=-0.5,B2=0,B3=0,B4=0),
                start_upper = c(Asym=1, Drop=1,lrc=1,pwr=2,B1=0.5,B2=0.001,B3=0.001,B4=0.001),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(n8)


yardstick::rsq_trad_vec(test_dat$evi2_3mo,estimate=predict(n4,newdata=test_dat))
yardstick::rsq_trad_vec(test_dat$evi2_3mo,estimate=predict(n5,newdata=test_dat))
yardstick::rsq_trad_vec(test_dat$evi2_3mo,estimate=predict(n7,newdata=test_dat))
yardstick::rsq_trad_vec(test_dat$evi2_3mo,estimate=predict(n8,newdata=test_dat))

yardstick::rmse_vec(test_dat$evi2_3mo,estimate=predict(n4,newdata=test_dat))
yardstick::rmse_vec(test_dat$evi2_3mo,estimate=predict(n5,newdata=test_dat))
yardstick::rmse_vec(test_dat$evi2_3mo,estimate=predict(n7,newdata=test_dat))
yardstick::rmse_vec(test_dat$evi2_3mo,estimate=predict(n8,newdata=test_dat))

bbmle::AICtab(n4,n5,n6,n7,n8)


o_preds <- expand_grid(season=unique(train_dat$season),
                       co2 = seq(min(dat$co2_int),max(dat$co2_int),length.out=50),
                       mape = seq(0.05,1.5,length.out = 200), 
                       pct_anom = c(-50,0,50)) %>% 
  mutate(pe_anom_12mo = 0.01*pct_anom*mape) %>% 
  mutate(pe_12mo = pe_anom_12mo+mape) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(w4, newdata=.))


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
  facet_grid(~pct_anom)+
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

# n4_preds NDVI ----------------------------------------------------------------
n4_preds <- expand_grid(season=unique(train_dat$season),
                        co2 = seq(min(dat$co2_int),max(dat$co2_int),length.out=50),
                        mape = seq(0.05,1.5,length.out = 200), 
                        pct_anom = c(-50,0,50)) %>% 
  mutate(pe_anom_12mo = 0.01*pct_anom*mape) %>%
  # mutate(pe_12mo = pe_anom_12mo+mape) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(n4, newdata=.))


vec_labels <- c("-50"="-50% P:PET Anom.",
                "0"='0% P:PET Anom.',
                "50"="50% P:PET Anom.")
p4_ndvi <- n4_preds %>% 
  ggplot(data=., aes(mape,pred,color=(co2), group=co2))+
  geom_line(alpha=1)+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0.08,1.5),
                     breaks=c(0,0.5,1,1.5),
                     labels = c(0,0.5,1,1.5),
                     expand=c(0,0),
                     guide = guide_axis(n.dodge=1, angle=0,check.overlap = TRUE)
  )+
  scale_y_continuous(#limits=c(0,0.9), 
    expand=c(0,0))+
  labs(x=expression(paste("Mean Annual P:PET")),
       y=expression(paste(NDVI["3 mo"])))+
  facet_grid(~pct_anom, labeller = labeller(pct_anom=vec_labels))+
  theme_linedraw()+
  guides(color=guide_colorbar(title.position = 'top'))+
  theme(#panel.grid = element_blank(),
    # panel.spacing.x = unit(6, "mm"),
    axis.text = element_text(size=10),
    # axis.text.x = element_text(angle=45, vjust=-0.5),
    legend.position = c(0.525,0.175), 
    legend.key.width = unit(0.65,'cm'),
    legend.direction = 'horizontal', 
    legend.background = element_rect(fill=NA)); p4_ndvi
ggsave(filename = 'figures/n4_weibull_ppet_x_co2.png',
       width = 16, height = 8, units='cm', dpi=350, type='cairo')

# n4_preds EVI2 ----------------------------------------------------------------
n4_preds_evi2 <- expand_grid(season=unique(train_dat$season),
                        co2 = seq(min(dat$co2_int),max(dat$co2_int),length.out=50),
                        mape = seq(0.05,1.5,length.out = 200), 
                        pct_anom = c(-50,0,50)) %>% 
  mutate(pe_anom_12mo = 0.01*pct_anom*mape) %>%
  # mutate(pe_12mo = pe_anom_12mo+mape) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(n4_evi2, newdata=.))


vec_labels <- c("-50"="-50% P:PET Anom.",
                "0"='0% P:PET Anom.',
                "50"="50% P:PET Anom.")
p4_evi2 <- n4_preds_evi2 %>% 
  ggplot(data=., aes(mape,pred,color=(co2), group=co2))+
  geom_line(alpha=1)+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0.08,1.5),
                     breaks=c(0,0.5,1,1.5),
                     labels = c(0,0.5,1,1.5),
                     expand=c(0,0),
                     guide = guide_axis(n.dodge=1, angle=0,check.overlap = TRUE)
  )+
  scale_y_continuous(#limits=c(0,0.9), 
    expand=c(0,0))+
  labs(x=expression(paste("Mean Annual P:PET")),
       y=expression(paste(EVI2["3 mo"])))+
  facet_grid(~pct_anom, labeller = labeller(pct_anom=vec_labels))+
  theme_linedraw()+
  guides(color=guide_colorbar(title.position = 'top'))+
  theme(#panel.grid = element_blank(),
    # panel.spacing.x = unit(6, "mm"),
    axis.text = element_text(size=10),
    # axis.text.x = element_text(angle=45, vjust=-0.5),
    legend.position = c(0.525,0.175), 
    legend.key.width = unit(0.65,'cm'),
    legend.direction = 'horizontal', 
    legend.background = element_rect(fill=NA)); p4_evi2
ggsave(filename = 'figures/n4_evi2_weibull_ppet_x_co2.png',
       width = 16, height = 8, units='cm', dpi=350, type='cairo')

# n4_preds NIRV ----------------------------------------------------------------
n4_preds_nirv <- expand_grid(season=unique(train_dat$season),
                        co2 = seq(min(dat$co2_int),max(dat$co2_int),length.out=50),
                        mape = seq(0.05,1.5,length.out = 200), 
                        pct_anom = c(-50,0,50)) %>% 
  mutate(pe_anom_12mo = 0.01*pct_anom*mape) %>%
  # mutate(pe_12mo = pe_anom_12mo+mape) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(n4_nirv, newdata=.))


vec_labels <- c("-50"="-50% P:PET Anom.",
                "0"='0% P:PET Anom.',
                "50"="50% P:PET Anom.")
p4_nirv <- n4_preds_nirv %>% 
  ggplot(data=., aes(mape,pred,color=(co2), group=co2))+
  geom_line(alpha=1)+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0.08,1.5),
                     breaks=c(0,0.5,1,1.5),
                     labels = c(0,0.5,1,1.5),
                     expand=c(0,0),
                     guide = guide_axis(n.dodge=1, angle=0,check.overlap = TRUE)
  )+
  scale_y_continuous(#limits=c(0,0.9), 
    expand=c(0,0))+
  labs(x=expression(paste("Mean Annual P:PET")),
       y=expression(paste(NIR["V 3 mo"])))+
  facet_grid(~pct_anom, labeller = labeller(pct_anom=vec_labels))+
  theme_linedraw()+
  guides(color=guide_colorbar(title.position = 'top'))+
  theme(#panel.grid = element_blank(),
    # panel.spacing.x = unit(6, "mm"),
    axis.text = element_text(size=10),
    # axis.text.x = element_text(angle=45, vjust=-0.5),
    legend.position = c(0.535,0.16), 
    legend.key.width = unit(0.65,'cm'),
    legend.direction = 'horizontal', 
    legend.background = element_rect(fill=NA)); p4_nirv
ggsave(filename = 'figures/n4_nirv_weibull_ppet_x_co2.png',
       width = 16, height = 8, units='cm', dpi=350, type='cairo')


# Stack n4 VI preds -------------------------------------------------------
i1 <- magick::image_read("figures/n4_weibull_ppet_x_co2.png")
i2 <- magick::image_read("figures/n4_evi2_weibull_ppet_x_co2.png")
i3 <- magick::image_read("figures/n4_nirv_weibull_ppet_x_co2.png")
magick::image_append(c(i1,i2,i3),stack=T)

ggsave(p4_ndvi/p4_evi2/p4_nirv, 
        filename = 'figures/n4_multi_VI_weibull_ppet_x_co2.png',
        width = 20, height = 24, units='cm', dpi=350, type='cairo')
       
# n5_preds ----------------------------------------------------------------
n5_preds <- expand_grid(season=unique(train_dat$season),
                        co2 = seq(min(dat$co2_int),max(dat$co2_int),length.out=50),
                        mape = seq(0.05,1.5,length.out = 200), 
                        # pct_anom = c(-50,0,50), 
                        vpd15_anom_3mo = c(-0.5,0,0.5)) %>% 
  # mutate(pe_anom_12mo = 0.01*pct_anom*mape) %>% 
  # mutate(pe_12mo = pe_anom_12mo+mape) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(n5, newdata=.))


n5_preds %>% 
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
  facet_grid(~vpd15_anom_3mo)+
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

# n6_preds ----------------------------------------------------------------
n6_preds <- expand_grid(season=unique(train_dat$season),
                       co2 = seq(min(dat$co2_int),max(dat$co2_int),length.out=50),
                       mape = seq(0.05,1.5,length.out = 200), 
                       # pct_anom = c(-50,0,50), 
                       tmax_anom_12mo = c(-1,0,1)) %>% 
  # mutate(pe_anom_12mo = 0.01*pct_anom*mape) %>% 
  # mutate(pe_12mo = pe_anom_12mo+mape) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(n6, newdata=.))


n6_preds %>% 
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
  facet_grid(~tmax_anom_12mo)+
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



# n7_preds ----------------------------------------------------------------
n7_preds <- expand_grid(season=unique(train_dat$season),
                        co2 = seq(min(dat$co2_int),max(dat$co2_int),length.out=50),
                        mape = seq(0.05,1.5,length.out = 200), 
                        pct_anom = c(-50,0,50),
                        vpd15_anom_3mo = c(-0.5,0,0.5)) %>% 
  mutate(pe_anom_12mo = 0.01*pct_anom*mape) %>%
  mutate(pe_12mo = pe_anom_12mo+mape) %>%
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(n7, newdata=.)) 

n7_preds %>% 
  filter(!(vpd15_anom_3mo==-0.5 & pct_anom==-50))


n7_preds %>% 
  filter(!(vpd15_anom_3mo==-0.5 & pct_anom==-50)) %>% 
  filter(!(vpd15_anom_3mo==0.5 & pct_anom==50)) %>% 
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
  facet_grid(pct_anom~vpd15_anom_3mo, labeller = label_both)+
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

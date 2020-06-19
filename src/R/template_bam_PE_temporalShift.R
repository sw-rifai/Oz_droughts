#*******************************************************************************
#* Description:
#* Fit the NDVI~Precip:PET models with GAMs. This is meant to supplement to 
#* provide an phenomenological confirmation of the choice of non-linear logistic
#* functional form.
#* Extract the splines and plot the shift in optimal P:PET through time. 
#*
#*
#*

library(tidyverse)
library(data.table); setDTthreads(threads = 8)
library(lubridate); 
library(mgcv); #library(mgcViz); 
library(gratia)
library(dtplyr); 
library(nls.multstart)
# IMPORT DATA ###################################################################
# vegetation index record
vi <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet", 
                          col_select = c("x","y","date",
                                         "ndvi_c","ndvi_m","ndvi_hyb", 
                                         "evi2_hyb","evi2_m"))

# climate records
tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id","season",
                             "precip",  "precip_anom", 
                             "precip_anom_3mo","precip_anom_36mo",
                             "precip_anom_12mo","map", 
                             "precip_12mo","precip_36mo",
                             "tmax","tmax_anom","tmax_anom_sd", "matmax",
                             "tmin",
                             "vpd15","vpd15_anom","vpd15_anom_sd","mavpd15",
                             "vpd15_u",
                             "pet","mapet","pet_anom","pet_anom_3mo","pet_u","pet_sd",
                             "pet_anom_sd", "pet_12mo","pet_36mo",
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
  .[is.infinite(mape)==F]
# unique(tmp[,.(vc,veg_class)]) %>% View
tmp <- tmp[order(x,y,date)][,tmean := (tmax+tmin)/2]
tmp <- tmp[order(x,y,date)][, tmax_3mo := frollmean(tmax,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmean_3mo := frollmean(tmean,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmin_3mo := frollmean(tmin,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmax_12mo := frollmean(tmax,n = 12,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pet_3mo := frollmean(pet,n = 3,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, precip_3mo := frollmean(precip,n = 3,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, vpd15_3mo := frollmean(vpd15,n = 3,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, vpd15_anom_3mo := frollmean(vpd15_anom,n = 3,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pet_24mo := frollmean(pet,n = 24,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, precip_24mo := frollmean(precip,n = 24,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pet_48mo := frollmean(pet,n = 48,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, precip_48mo := frollmean(precip,n = 48,fill = NA,align='right'), by=.(x,y)]

tmp <- tmp[,`:=`(pe_3mo = precip_3mo/pet_3mo, 
                 pe_12mo = precip_12mo/pet_12mo, 
                 pe_24mo = precip_24mo/pet_24mo, 
                 pe_36mo = precip_36mo/pet_36mo, 
                 pe_48mo = precip_48mo/pet_48mo)]
dim(tmp)

# merge VI and Clim
tmp <- merge(tmp, 
             vi,
             by=c("x","y","date"), 
             all=TRUE,allow.cartesian=TRUE)
tmp <- tmp[order(x,y,date)][, ndvi_3mo := frollmean(ndvi_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]
rm(vi); gc(full=TRUE)

# CO2 record
mlo <- read_table("../data_general/CO2_growth_rate/co2_mm_mlo_20200405.txt", 
                  skip = 72, col_names = F) %>% 
  set_names(
    c("year","month","ddate","co2_avg","co2_int","co2_trend","ndays")
  ) %>% 
  mutate(date = ymd(paste(year,month,1))) %>% 
  select(date,co2_int,co2_trend) %>% 
  as.data.table()
tmp <- merge(mlo,tmp,by="date")
# END Prep data # **************************************************************


# Cross-Val BAM -----------------------------------------------------------
fn_bam <- function(dat){
  train_dat <- dat[sample(.N, .N * 0.5)]
  test_dat <- fsetdiff(dat, train_dat, all = F)
  fit <- bam(ndvi_3mo~
               s(mape,k=4)+
               s(vpd15_anom_3mo,k=4) +
               s(precip_anom_3mo,k=4) +
               s(pe_48mo,k=4),
             # s(pe_12mo,fac_hy,bs=c('fs'),xt='gp',
             #   m=c(3,0.5), k=5), 
             data=train_dat, 
             select=TRUE, discrete = T, nthreads = 8)
  
  bfit <- broom::tidy(fit)
  bfit$hydro_year <- unique(dat$hydro_year); 
  bfit$season <- unique(dat$season)
  gof <- test_dat %>% 
    lazy_dt() %>% 
    mutate(pred = predict(fit,newdata=.)) %>% 
    mutate(res = pred-ndvi_3mo) %>% 
    select(pred,res,ndvi_3mo) %>% 
    na.omit() %>% 
    summarize(rmse = sqrt(mean((pred-ndvi_3mo)**2)), 
              r2 = cor(pred,ndvi_3mo)**2) %>% 
    as.data.table()
  bfit$rmse_out <- gof$rmse
  bfit$r2_out <- gof$r2
  
  gof <- train_dat %>% 
    lazy_dt() %>% 
    mutate(pred = predict(fit,newdata=.)) %>% 
    mutate(res = pred-ndvi_3mo) %>% 
    select(pred,res,ndvi_3mo) %>% 
    na.omit() %>% 
    summarize(rmse = sqrt(mean((pred-ndvi_3mo)**2)), 
              r2 = cor(pred,ndvi_3mo)**2) %>% 
    as.data.table()
  bfit$rmse_in <- gof$rmse
  bfit$r2_in <- gof$r2
  bfit$model[1] <- list(fit)
  
  return(bfit)  
}

# dat <- tmp[hydro_year %in% c(2009)] %>% 
#   # .[season=="DJF"] %>%
#   .[season=="DJF"] %>%
#   .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
#   .[ndvi_hyb>0] %>%
#   .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
#   .[is.na(veg_class)==F]


how_dat <- tmp[hydro_year %in% c(1985:2019)] %>% 
  # .[season=="DJF"] %>%
  .[season=="DJF"] %>%
  .[is.na(pe_48mo)==F] %>% 
  .[mape <= 3] %>% 
  .[is.na(ndvi_3mo)==F] %>% 
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_hyb>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  # .[sample(.N,50000)] %>%
  .[,fn_bam(.SD),by=.(hydro_year)]
how_dat

how_dat %>% 
  ggplot(data=., aes(hydro_year,r2_out))+geom_point()+
  geom_point(aes(hydro_year,r2_in),color='red')
how_dat %>% 
  ggplot(data=., aes(hydro_year,rmse_out))+geom_point()+
  geom_point(aes(hydro_year,rmse_in),color='red')

how_dat$model[[1]] %>% 
  gratia::draw()

how_dat %>% 
  filter(hydro_year==1990)



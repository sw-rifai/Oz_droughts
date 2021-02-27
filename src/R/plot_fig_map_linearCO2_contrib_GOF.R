# 
#*******************************************************************************
#* Description:
#* Estimate the CO2 contribution to the greening trend
#*
#*
#*
library(tidyverse); library(sf)
library(data.table); setDTthreads(threads = 0)
library(lubridate); 
library(mgcv); #library(mgcViz); 
library(dtplyr); 
library(RcppArmadillo); library(patchwork)
library(stars)
library(foreach); library(doParallel)
library(zyp); library(nls.multstart)
options(pillar.sigfig = 5)
# IMPORT DATA ###################################################################

# load("data/gridCell_lm_ndvi_clim.Rdata") # grid cell linear regressions
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)

# vegetation index record
vi <- arrow::read_parquet("../data_general/MCD43/MCD43_AVHRR_NDVI_hybrid_2020-10-11.parquet" 
                          # col_select = c("x","y","date",
                          #                "ndvi_c","ndvi_mcd","ndvi_hyb", 
                          #                "evi2_hyb","evi2_mcd","sz")
) %>% 
  as.data.table()
vi <- vi %>% lazy_dt() %>% 
  mutate(ndvi_hyb_e1 = coalesce(ndvi_mcd_nm_pred, NA_real_),
         ndvi_hyb_e2 = coalesce(ndvi_mcd, NA_real_)) %>% 
  mutate(ndvi_hyb = coalesce(ndvi_hyb_e2, ndvi_hyb_e1)) %>% 
  mutate(ndvi_hyb = ifelse(between(ndvi_hyb,0,1),ndvi_hyb,NA_real_)) %>% 
  as.data.table()
norms_vi <- vi[,`:=`(month=month(date))] %>% 
  .[,.(ndvi_u = mean(ndvi_hyb,na.rm=TRUE), 
       ndvi_sd = sd(ndvi_hyb,na.rm=TRUE)),keyby=.(x,y,month)]
vi <- norms_vi[vi,on=.(x,y,month)] %>% 
  .[,`:=`(ndvi_anom = ndvi_hyb - ndvi_u)] %>% 
  .[,`:=`(ndvi_anom_sd = ndvi_anom/ndvi_sd)]

# vi <- mcf[,.(x,y,date,soil,gv,npv)][vi,on=.(x,y,date)]

# Load climate data 
dat <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id",
                             # "season",
                             "precip",  "precip_anom", 
                             "precip_anom_3mo","precip_anom_36mo",
                             "precip_anom_12mo",
                             "map",
                             "precip_12mo","precip_36mo",
                             "tmax","tmax_anom","tmax_anom_sd", "matmax",
                             "tmin","tmin_anom",
                             "vpd15","vpd15_anom","vpd15_anom_sd","mavpd15",
                             "vpd15_12mo",
                             "vpd15_u",
                             "pet","mapet","pet_anom","pet_anom_3mo","pet_u","pet_sd",
                             "pet_anom_sd", "pet_12mo","pet_36mo",
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
  .[is.infinite(mape)==F]
dat <- dat[order(x,y,date)][, vpd15_12mo := frollmean(vpd15,n = 12,fill = NA,align='right',na.rm=TRUE), by=.(x,y)]
dat <- dat[,`:=`(pe = precip/pet, 
                 pe_12mo = precip_12mo/pet_12mo)]
dat <- merge(dat, 
             vi,
             by=c("x","y","date"), 
             all=TRUE,allow.cartesian=TRUE)
dat <- dat[order(x,y,date)][, ndvi_3mo := frollmean(ndvi_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]
rm(vi); gc(full=TRUE)

# Attach season
dat[,`:=`(year=year(date),month=month(date))] %>%
  .[,`:=`(season = case_when(month%in%c(3:5)~'MAM',
                             month%in%c(6:8)~'JJA',
                             month%in%c(9:11)~'SON',
                             month%in%c(12,1,2)~'DJF'))]
dat[,`:=`(season = factor(season, levels = c('SON','DJF','MAM','JJA'),ordered = TRUE))]
dat[,`:=`(hydro_year=year(date+months(1)))]

# FILTER TO LON >= 140 !!! **********
dat <- dat[x>=140]

coords_keep <- dat %>% lazy_dt() %>% 
  group_by(x,y) %>% 
  summarize(nobs_total = sum(is.na(ndvi_hyb)==F)) %>% 
  ungroup() %>% 
  as.data.table()
dat <- merge(dat, coords_keep, by=c("x","y"))

# Load Mauna Loa CO2 record
mlo <- readr::read_table("../data_general/CO2_growth_rate/co2_mm_mlo_20200405.txt", 
                         skip = 72, col_names = F) %>% 
  set_names(
    c("year","month","ddate","co2_avg","co2_int","co2_trend","ndays")
  ) %>% 
  mutate(date = ymd(paste(year,month,1))) %>% 
  select(date,co2_int,co2_trend) %>% 
  as.data.table()
dat <- merge(mlo,dat,by="date",all = TRUE)

ldat <- dat %>% lazy_dt()
# END Load awap clim dat *****************************************************************



# Load simplified BOM Koppen climate zones --------------------------------
ref_grid <- stars::read_stars('../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif', 
                              RasterIO = list(bands=1))
bom <- stars::read_stars("../data_general/Koppen_climate/BOM/kpngrp.txt")
bom <- st_warp(src=bom, dest=ref_grid[,,], use_gdal = F)
bom <- set_names(bom, 'koppen') %>% as_tibble()
bom <- left_join(ref_grid %>% as_tibble() %>% select(x,y), 
                 bom)

coords <- dat %>% select(x,y) %>% distinct() %>% filter(x>=140) 

g_map <- ldat %>% 
  filter(date>=ymd("1982-01-01")&date<=ymd("2010-12-31")) %>% 
  group_by(x,y) %>% 
  summarize(map = mean(precip,na.rm=TRUE)*12) %>% 
  ungroup() %>% 
  as_tibble()

kop <- left_join(coords, bom, by=c("x","y")) %>% 
  inner_join(., g_map, by=c("x","y")) %>% 
  as_tibble() %>% 
  mutate(zone = case_when(between(koppen,0,11) ~ 'Temperate', 
                          (y <= -40) ~ 'Tasmania',
                          between(koppen, 12,21)~'GD_temp', # Grassland
                          between(koppen, 22,30)~'GD_temp', # Desert
                          between(koppen, 31,34)~'Subtropical',
                          between(koppen, 35,40)~'Tropical', 
                          koppen >= 41 ~ 'Equatorial')) %>% 
  mutate(zone = ifelse(y < -40, 'Temperate Tas.', zone)) %>% #pull(zone) %>% table
  mutate(zone = ifelse(zone == "GD_temp" & map < 500, 'Arid',zone)) %>%   
  mutate(zone = ifelse(zone == "GD_temp" & map >= 500, 'Grassland',zone)) %>%   
  mutate(zone = factor(zone, levels = c("Equatorial","Tropical",
                                        "Subtropical","Grassland","Arid",
                                        "Temperate","Temperate Tas."), ordered = T))
kop <- kop %>% mutate(cz=zone)
kop <- as.data.table(kop)
# arrow::write_parquet(kop, sink='../data_general/Koppen_climate/BOM_Koppen_simplified7.parquet')
#*** End Kop zone load ********************************************************

# add the NVIS vegetation classes
# base <- stars::read_stars('../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif', 
#                           RasterIO = list(bands=1))
base <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_Tree_Cover_5000m_East_Oz_noMask_2000_2019.tif", 
                          RasterIO = list(bands=1))
nvis <- stars::read_stars("../data_general/NVIS/nvis51_majorVegClass_0p05.tif")
nvis2 <- st_warp(src=nvis, dest=base[,,], use_gdal = T)
names(nvis2) <- "veg_class"
nvis <- nvis2 %>% as_tibble() %>% as.data.table()
codes <- readr::read_fwf("../data_general/NVIS/nvis51_majorVegClass_codes.txt", 
                         fwf_widths(c(2,100)), skip = 1) %>% 
  set_names(c("veg_class","veg_class_descrip")) %>% 
  mutate(vc = as.factor(veg_class_descrip)) 
vc <- left_join(nvis, codes, by='veg_class')

# mod <- svi[mod, on=.(x,y,year)]
# end section ******************************************************************
# END DATA IMPORT SECTION ******************************************************


tmp_ndvi <- dat %>% 
  .[date >= ymd("1981-11-01") & date <= ymd("2019-08-30")] %>% 
  .[hydro_year %in% c(1982:2019)] %>% 
  .[,`:=`(hydro_year_c = hydro_year-1982, 
          epoch = ifelse(date<=ymd("2000-12-31"),0,1),
          frac_p = precip_12mo/map,
          frac_p_anom = precip_anom_12mo/map,
          frac_pet_anom = (pet_12mo-mapet)/mapet,
          frac_vpd_anom = vpd15_anom/mavpd15,
          frac_ppet_anom = (pe_12mo-mape)/mape)] %>% 
  .[,.(ndvi_hyb = mean(ndvi_hyb,na.rm=TRUE), 
       co2 = mean(co2_trend,na.rm=TRUE),
       epoch=mean(epoch,na.rm=TRUE), 
       nobs = sum(is.na(ndvi_hyb)==F), 
       p_anom = mean(precip_anom_12mo,na.rm=TRUE),
       pet_anom = mean(pet_12mo-mapet,na.rm=TRUE),
       ppet_anom = mean(pe_12mo,na.rm=TRUE),
       frac_p = mean(frac_p, na.rm=TRUE),
       frac_p_anom = mean(frac_p_anom,na.rm=TRUE), 
       frac_vpd_anom = mean(frac_vpd_anom,na.rm=TRUE),
       frac_pet_anom = mean(frac_pet_anom, na.rm=TRUE),
       frac_ppet_anom = mean(frac_ppet_anom,na.rm=TRUE)),
    by=.(x,y,hydro_year, hydro_year_c)] %>% 
  group_by(x,y) %>% 
  mutate(id = cur_group_id()) %>%
  ungroup() %>% 
  as.data.table()
tmp_annual_nobs <- tmp_ndvi %>%
  mutate(epoch=round(epoch)) %>% 
  lazy_dt() %>% 
  group_by(id,epoch) %>% 
  summarize(obs_annual = sum(nobs >= 4)) %>% 
  as.data.table()
tmp_epoch_nobs <- tmp_annual_nobs %>% group_by(id) %>% 
  summarize(obs_epoch = sum(obs_annual > 5)) %>% 
  ungroup()
# pull(obs_epoch) %>% table
# tmp_epoch_nobs %>% 
#   inner_join(., tmp_ndvi %>% select(x,y,id) %>% distinct(), by='id') %>% 
#   ggplot(data=.,aes(x,y,fill=obs_epoch))+geom_tile()+scale_fill_viridis_c()
vec_ids <- tmp_epoch_nobs %>% 
  filter(obs_epoch >= 1.9) %>% 
  pull(id)

# Percent increase in NDVI after effects of ppet were removed
min_co2 <- min(tmp_ndvi[hydro_year==1982]$co2)
max_co2 <- max(tmp_ndvi[hydro_year==2019]$co2)
mid_co2 <- max(tmp_ndvi[hydro_year==2000]$co2)



fits <-  tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  .[,.(beta = list(coef(MASS::rlm(
    ndvi_hyb~
      scale(co2_start,center=TRUE,scale=F)+
      ppet_anom+
      p_anom+
      pet_anom+
      jitter(epoch)))))
    ,
    by=.(x,y)] %>%
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2], 
          b2=unlist(beta)[3], 
          b3=unlist(beta)[4],
          b4=unlist(beta)[5], 
          b5=unlist(beta)[6]
  ), by=.(x,y)]


gofs <- tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  merge(., fits, by=c("x","y")) %>% 
  # head() %>% 
  lazy_dt() %>% 
  mutate(pred_full = b0+(b1*(co2_start-35))+b2*ppet_anom+b3*p_anom+b4*pet_anom+b5*epoch, 
         no_co2 = b0+b2*ppet_anom+b3*p_anom+b4*pet_anom+b5*epoch) %>% 
  as.data.table() %>% 
  # head() %>% 
  .[,.(r2_full = yardstick::rsq_vec(ndvi_hyb,pred_full), 
       r2_no_co2 = yardstick::rsq_vec(ndvi_hyb,no_co2), 
       rmse_full = yardstick::rmse_vec(ndvi_hyb, pred_full), 
       rmse_no_co2 = yardstick::rmse_vec(ndvi_hyb, no_co2)), by=.(x,y)] 
gofs$r2_full %>% hist
gofs$r2_no_co2 %>% hist

p1 <- gofs %>% 
  # select(r2_full, r2_no_co2) %>% 
  mutate(diff = r2_full - r2_no_co2) %>% 
  ggplot(data=.,aes(x,y,fill=diff))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(limits=c(0,0.5), oob=scales::squish,
                       option='A',
                       na.value = 'red')+
  labs(title='CO2 contribution to R2')

p2 <- gofs %>% 
  # select(r2_full, r2_no_co2) %>% 
  mutate(diff = 100*(rmse_no_co2-rmse_full)/rmse_no_co2 ) %>% 
  ggplot(data=.,aes(x,y,fill=diff))+
  geom_tile()+
  coord_equal()+
  # scico::scale_fill_scico(palette = 'batlow',
  #                         limits=c(0,50),
  #                         oob=scales::squish)
  scale_fill_viridis_c('% reduction of RMSE', 
                       limits=c(0,50),
                       option='A',
                       na.value = 'red', 
                       oob=scales::squish)+
  labs(title='CO2 attributed reduction of RMSE')
p1+p2



# KEEP! % improvement of rmse
bad_pix <- gofs[r2_no_co2 < 0.001][order(r2_no_co2)][,.(x,y)]
bad_fits <- tmp_ndvi[id%in%vec_ids] %>% 
  as.data.table() %>% 
  .[x%in%bad_pix$x[1] & y%in%bad_pix$y] %>% 
  # .[x==bad_pix$x[1] & y==bad_pix$y[1]] %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  .[,.(beta = list(coef(MASS::rlm(
    ndvi_hyb~
      scale(co2_start,center=TRUE,scale=F)+
      ppet_anom+
      p_anom+
      pet_anom+
      jitter(epoch)))))
    ,
    by=.(x,y)] %>%
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2], 
          b2=unlist(beta)[3], 
          b3=unlist(beta)[4],
          b4=unlist(beta)[5], 
          b5=unlist(beta)[6]
  ), by=.(x,y)]


fn_rlm <- function(din){
  fit <- MASS::rlm(ndvi_hyb~
              scale(co2_start,center=TRUE,scale=F)+
              ppet_anom+
              p_anom+
              pet_anom+
              jitter(epoch), maxit=25,data=din)
  vec_coefs <- coef(fit)
  return(c(vec_coefs,fit$converged))
}

find_bad <- tmp_ndvi[id%in%vec_ids] %>% 
  as.data.table() %>% 
  # .[x%in%bad_pix$x[1] & y%in%bad_pix$y] %>%
  # .[x==bad_pix$x[1] & y==bad_pix$y[1]] %>%
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  .[,.(beta = list(fn_rlm(.SD))),by=.(x,y)] %>% 
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2], 
          b2=unlist(beta)[3], 
          b3=unlist(beta)[4],
          b4=unlist(beta)[5], 
          b5=unlist(beta)[6],
          converged=unlist(beta)[7]
  ), by=.(x,y)]
find_bad$converged %>% table

MASS::rlm(mpg~cyl,data=mtcars)$converged

scico::scico_palette_show()

# KEEP! % more variation explained with CO2
gofs %>% 
  # select(r2_full, r2_no_co2) %>% 
  mutate(diff = 100*(r2_full-r2_no_co2)) %>% 
  ggplot(data=.,aes(x,y,fill=diff))+
  geom_tile()+
  coord_equal()+
  # scico::scale_fill_scico(palette = 'batlow',
  #                         limits=c(0,50),
  #                         oob=scales::squish)
  scale_fill_viridis_c(limits=c(0,100),
                       option='A',
                       na.value = 'red', 
                       oob=scales::squish)


# DUMP EVERYTHING BENEATH ------------------------------------------------------

fits <-  tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  .[,.(beta = list(coef(MASS::rlm(
    ndvi_hyb~
      scale(co2_start,center=TRUE,scale=F)+
      ppet_anom+
      p_anom+
      pet_anom+
      jitter(epoch)))))
    ,
    by=.(x,y)] %>%
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2], 
          b2=unlist(beta)[3], 
          b3=unlist(beta)[4],
          b4=unlist(beta)[5], 
          b5=unlist(beta)[6]
  ), by=.(x,y)]

fits_lm <-  tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  .[,.(beta = list(coef(lm(
    ndvi_hyb~
      scale(co2_start,center=TRUE,scale=F)+
      ppet_anom+
      p_anom+
      pet_anom+
      jitter(epoch)))))
    ,
    by=.(x,y)] %>%
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2], 
          b2=unlist(beta)[3], 
          b3=unlist(beta)[4],
          b4=unlist(beta)[5], 
          b5=unlist(beta)[6]
  ), by=.(x,y)]

library(robustbase)
fits_lmrob <-  tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  # .[x==bad_pix$x[1] & y==bad_pix$y[1]] %>%
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  .[,.(beta = list(coef(robustbase::lmrob(
    ndvi_hyb~
      scale(co2_start,center=TRUE,scale=F)+
      ppet_anom+
      p_anom+
      pet_anom+
      jitter(epoch),
    control = lmrob.control(refine.tol = 1e-06),
    setting='KS2014'))))
    ,
    by=.(x,y)] %>%
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2], 
          b2=unlist(beta)[3], 
          b3=unlist(beta)[4],
          b4=unlist(beta)[5], 
          b5=unlist(beta)[6]
  ), by=.(x,y)]
fits_lm[x==bad_pix$x[1] & y==bad_pix$y[1]]


gofs_lm <- tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  merge(., fits_lm, by=c("x","y")) %>% 
  # head() %>% 
  lazy_dt() %>% 
  mutate(pred_full = b0+(b1*(co2_start-35))+b2*ppet_anom+b3*p_anom+b4*pet_anom+b5*epoch, 
         no_co2 = b0+b2*ppet_anom+b3*p_anom+b4*pet_anom+b5*epoch) %>% 
  as.data.table() %>% 
  # head() %>% 
  .[,.(r2_full = yardstick::rsq_vec(ndvi_hyb,pred_full), 
       r2_no_co2 = yardstick::rsq_vec(ndvi_hyb,no_co2), 
       rmse_full = yardstick::rmse_vec(ndvi_hyb, pred_full), 
       rmse_no_co2 = yardstick::rmse_vec(ndvi_hyb, no_co2)), by=.(x,y)] 

gofs <- tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  merge(., fits, by=c("x","y")) %>% 
  # head() %>% 
  lazy_dt() %>% 
  mutate(pred_full = b0+(b1*(co2_start-35))+b2*ppet_anom+b3*p_anom+b4*pet_anom+b5*epoch, 
         no_co2 = b0+b2*ppet_anom+b3*p_anom+b4*pet_anom+b5*epoch) %>% 
  as.data.table() %>% 
  # head() %>% 
  .[,.(r2_full = yardstick::rsq_vec(ndvi_hyb,pred_full), 
       r2_no_co2 = yardstick::rsq_vec(ndvi_hyb,no_co2), 
       rmse_full = yardstick::rmse_vec(ndvi_hyb, pred_full), 
       rmse_no_co2 = yardstick::rmse_vec(ndvi_hyb, no_co2)), by=.(x,y)] 

gofs$r2_full %>% summary
gofs_lm$r2_full %>% summary

gofs$rmse_full %>% summary
gofs_lm$rmse_full %>% summary

find_bad %>% 
  ggplot(data=.,aes(x,y,fill=converged))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()










































# SM Fig 14: NDVI Beta CO2 comparison by epoch ----------------------------
## RLM on annual ndvi -----------------------------------------------
tmp <- dat %>% 
  .[date >= ymd("1981-11-01") & date <= ymd("2019-08-31")] %>% 
  .[hydro_year %in% c(1982:2019)] %>% 
  .[,`:=`(hydro_year_c = hydro_year-1982, 
          epoch = ifelse(date<=ymd("2000-12-31"),0,1))] %>% 
  .[is.na(ndvi_hyb)==F] %>% 
  .[,.(ndvi_hyb = mean(ndvi_hyb,na.rm=TRUE), 
       epoch=mean(epoch,na.rm=TRUE)),
    by=.(x,y,hydro_year_c)] %>% 
  .[is.na(ndvi_hyb)==F] %>% 
  lazy_dt() %>% 
  select(x,y,hydro_year_c,ndvi_hyb,epoch) %>% 
  as.data.table() %>% 
  group_by(x,y) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup()
tmp <- tmp %>% distinct()  
tmp <- tmp %>% as.data.table()

# filter out pixels that only have data for one satellite epoch
vec_rlm <- tmp %>% group_by(x,y,id) %>% 
  summarize(nobs=n()) %>% 
  ungroup() %>% 
  filter(nobs > 19)
tmp <- tmp[id %in% unique(vec_rlm$id)]
rlm_ndvi_annual <-  tmp %>% 
  as.data.table() %>% 
  .[,.(beta = list(coef(MASS::rlm(ndvi_hyb~hydro_year_c+epoch)))),by=.(x,y)] %>%
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2], 
          b2=unlist(beta)[3]), by=.(x,y)]
rlm_ndvi_annual


# Summarize annual data by epoch -------------------------------
tmp_ndvi_e1 <- dat %>% 
  .[date >= ymd("1981-12-01") & date <= ymd("2000-11-30")] %>% 
  .[,`:=`(hydro_year_c = hydro_year-1982, 
          epoch = ifelse(date<=ymd("2000-12-31"),0,1),
          frac_p = precip_12mo/map,
          frac_p_anom = precip_anom_12mo/map,
          frac_pet_anom = (pet_12mo-mapet)/mapet,
          frac_vpd_anom = vpd15_anom/mavpd15,
          frac_ppet_anom = (pe_12mo-mape)/mape)] %>% 
  .[,.(ndvi_hyb = mean(ndvi_hyb,na.rm=TRUE), 
       co2 = mean(co2_trend,na.rm=TRUE),
       epoch=mean(epoch,na.rm=TRUE), 
       nobs = sum(is.na(ndvi_hyb)==F), 
       p_anom = mean(precip_anom_12mo,na.rm=TRUE),
       pet_anom = mean(pet_12mo-mapet,na.rm=TRUE),
       ppet_anom = mean(pe_12mo,na.rm=TRUE),
       frac_p = mean(frac_p, na.rm=TRUE),
       frac_p_anom = mean(frac_p_anom,na.rm=TRUE), 
       frac_vpd_anom = mean(frac_vpd_anom,na.rm=TRUE),
       frac_pet_anom = mean(frac_pet_anom, na.rm=TRUE),
       frac_ppet_anom = mean(frac_ppet_anom,na.rm=TRUE)),
    by=.(x,y,hydro_year, hydro_year_c)] %>% 
  group_by(x,y) %>% 
  mutate(id = cur_group_id()) %>%
  ungroup() %>% 
  as.data.table()

tmp_ndvi_e2 <- dat %>% 
  .[date >= ymd("2001-01-01") & date <= ymd("2019-08-30")] %>% 
  .[,`:=`(hydro_year_c = hydro_year-2001, 
          epoch = ifelse(date<=ymd("2000-12-31"),0,1),
          frac_p = precip_12mo/map,
          frac_p_anom = precip_anom_12mo/map,
          frac_pet_anom = (pet_12mo-mapet)/mapet,
          frac_vpd_anom = vpd15_anom/mavpd15,
          frac_ppet_anom = (pe_12mo-mape)/mape)] %>% 
  .[,.(ndvi_hyb = mean(ndvi_hyb,na.rm=TRUE), 
       co2 = mean(co2_trend,na.rm=TRUE),
       epoch=mean(epoch,na.rm=TRUE), 
       nobs = sum(is.na(ndvi_hyb)==F), 
       p_anom = mean(precip_anom_12mo,na.rm=TRUE),
       pet_anom = mean(pet_12mo-mapet,na.rm=TRUE),
       ppet_anom = mean(pe_12mo,na.rm=TRUE),
       frac_p = mean(frac_p, na.rm=TRUE),
       frac_p_anom = mean(frac_p_anom,na.rm=TRUE), 
       frac_vpd_anom = mean(frac_vpd_anom,na.rm=TRUE),
       frac_pet_anom = mean(frac_pet_anom, na.rm=TRUE),
       frac_ppet_anom = mean(frac_ppet_anom,na.rm=TRUE)),
    by=.(x,y,hydro_year, hydro_year_c)] %>% 
  group_by(x,y) %>% 
  mutate(id = cur_group_id()) %>%
  ungroup() %>% 
  as.data.table()

tmp_ndvi <- dat %>% 
  .[date >= ymd("1981-11-01") & date <= ymd("2019-08-30")] %>% 
  .[hydro_year %in% c(1982:2019)] %>% 
  .[,`:=`(hydro_year_c = hydro_year-1982, 
          epoch = ifelse(date<=ymd("2000-12-31"),0,1),
          frac_p = precip_12mo/map,
          frac_p_anom = precip_anom_12mo/map,
          frac_pet_anom = (pet_12mo-mapet)/mapet,
          frac_vpd_anom = vpd15_anom/mavpd15,
          frac_ppet_anom = (pe_12mo-mape)/mape)] %>% 
  .[,.(ndvi_hyb = mean(ndvi_hyb,na.rm=TRUE), 
       co2 = mean(co2_trend,na.rm=TRUE),
       epoch=mean(epoch,na.rm=TRUE), 
       nobs = sum(is.na(ndvi_hyb)==F), 
       p_anom = mean(precip_anom_12mo,na.rm=TRUE),
       pet_anom = mean(pet_12mo-mapet,na.rm=TRUE),
       ppet_anom = mean(pe_12mo,na.rm=TRUE),
       frac_p = mean(frac_p, na.rm=TRUE),
       frac_p_anom = mean(frac_p_anom,na.rm=TRUE), 
       frac_vpd_anom = mean(frac_vpd_anom,na.rm=TRUE),
       frac_pet_anom = mean(frac_pet_anom, na.rm=TRUE),
       frac_ppet_anom = mean(frac_ppet_anom,na.rm=TRUE)),
    by=.(x,y,hydro_year, hydro_year_c)] %>% 
  group_by(x,y) %>% 
  mutate(id = cur_group_id()) %>%
  ungroup() %>% 
  as.data.table()
tmp_annual_nobs <- tmp_ndvi %>%
  mutate(epoch=round(epoch)) %>% 
  lazy_dt() %>% 
  group_by(id,epoch) %>% 
  summarize(obs_annual = sum(nobs >= 4)) %>% 
  as.data.table()
tmp_epoch_nobs <- tmp_annual_nobs %>% group_by(id) %>% 
  summarize(obs_epoch = sum(obs_annual > 5)) %>% 
  ungroup()
# pull(obs_epoch) %>% table
# tmp_epoch_nobs %>% 
#   inner_join(., tmp_ndvi %>% select(x,y,id) %>% distinct(), by='id') %>% 
#   ggplot(data=.,aes(x,y,fill=obs_epoch))+geom_tile()+scale_fill_viridis_c()
vec_ids <- tmp_epoch_nobs %>% 
  filter(obs_epoch >= 1.9) %>% 
  pull(id)

# Percent increase in NDVI after effects of ppet were removed
min_co2 <- min(tmp_ndvi[hydro_year==1982]$co2)
max_co2 <- max(tmp_ndvi[hydro_year==2019]$co2)
mid_co2 <- max(tmp_ndvi[hydro_year==2000]$co2)

rlm_ndvi_annual_co2_ppet_epoch <-  tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  .[,.(beta = list(coef(MASS::rlm(
    ndvi_hyb~
      scale(co2_start,center=TRUE,scale=F)+
      ppet_anom+
      p_anom+
      pet_anom+
      jitter(epoch)))))
    ,
    by=.(x,y)] %>%
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2], 
          b2=unlist(beta)[3], 
          b3=unlist(beta)[4],
          b4=unlist(beta)[5], 
          b5=unlist(beta)[6]
  ), by=.(x,y)]

rlm_ndvi_annual_co2_ppet_epoch %>% 
  mutate(val =1 - (abs(b2)+abs(b3)+abs(b4)+abs(b5))/
           (abs(b1)+abs(b2)+abs(b3)+abs(b4)+abs(b5))) %>% 
  ggplot(data=.,aes(x,y,fill=val))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(option='B')
rlm_ndvi_annual_co2_ppet_epoch %>% 
  ggplot(data=.,aes(b0,b1,color=b5))+
  geom_point()+
  geom_smooth(method='lm')+
  scico::scale_color_scico(palette='tokyo',begin=0.1,end=0.9)
scico::scico_palette_show()


rlm_ndvi_annual_co2_ppet_e1 <-  tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[hydro_year %in% 1982:2000] %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  .[,.(beta = list(coef(MASS::rlm(
    ndvi_hyb~
      co2_start+
      scale(ppet_anom)+
      scale(p_anom)+
      scale(pet_anom)))))
    ,
    by=.(x,y)] %>%
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2], 
          b2=unlist(beta)[3], 
          b3=unlist(beta)[4],
          b4=unlist(beta)[5]
  ), by=.(x,y)]

rlm_ndvi_annual_co2_ppet_e2 <-  tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[hydro_year %in% 2001:2019] %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - mid_co2)] %>% 
  .[,.(beta = list(coef(MASS::rlm(
    ndvi_hyb~
      co2_start+
      scale(ppet_anom)+
      scale(p_anom)+
      scale(pet_anom)))))
    ,
    by=.(x,y)] %>%
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2], 
          b2=unlist(beta)[3], 
          b3=unlist(beta)[4],
          b4=unlist(beta)[5]
  ), by=.(x,y)]



# ndvi_hyb~
#   co2_start+
#   scale(ppet_anom)+
#   scale(p_anom)+
#   scale(pet_anom)+
#   jitter(epoch)))))
rlm_ndvi_annual_co2_ppet_epoch %>% 
  ggplot(data=.,aes(x,y,fill=b1))+
  coord_equal()+
  geom_tile()+
  # scale_fill_gradient2()
  scico::scale_fill_scico(palette='batlow',limits=c(0,0.003),na.value='black')

rlm_ndvi_annual_co2_ppet_epoch %>% 
  select(starts_with('b'),-beta) %>% 
  gather(key='key',value='value') %>% 
  ggplot(data=.,aes(value,fill=key,color=key))+
  geom_density(alpha=0.25)+
  scale_x_continuous(limits=c(-0.01,0.01))



test_co2 <-  tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  .[,.(beta = list(coef(MASS::rlm(
    ndvi_hyb~
      co2_start+
      ppet_anom+
      p_anom+
      pet_anom+
      jitter(epoch)))))
    ,
    by=.(x,y)] %>%
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2], 
          b2=unlist(beta)[3], 
          b3=unlist(beta)[4],
          b4=unlist(beta)[5], 
          b5=unlist(beta)[6]
  ), by=.(x,y)]

gofs <- tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  merge(., test_co2, by=c("x","y")) %>% 
  # head() %>% 
  lazy_dt() %>% 
  mutate(pred_full = b0+b1*co2_start+b2*ppet_anom+b3*p_anom+b4*pet_anom+b5*epoch, 
         no_co2 = b0+b2*ppet_anom+b3*p_anom+b4*pet_anom+b5*epoch) %>% 
  as.data.table() %>% 
  # head() %>% 
  .[,.(r2_full = yardstick::rsq_vec(ndvi_hyb,pred_full), 
       r2_no_co2 = yardstick::rsq_vec(ndvi_hyb,no_co2)), by=.(x,y)] 
gofs$r2_full %>% hist
gofs$co2_contrib %>% summary
gofs %>% 
  # select(r2_full, r2_no_co2) %>% 
  mutate(diff = (r2_full - r2_no_co2)/(r2_full)) %>% 
  ggplot(data=.,aes(x,y,fill=diff))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(limits=c(0,1),na.value = 'red')

rlm_ndvi_annual_co2_ppet_epoch



bind_rows(as_tibble(rlm_ndvi_annual_co2_ppet_epoch) %>% mutate(epoch='Merged 1982-2019'), 
          as_tibble(rlm_ndvi_annual_co2_ppet_e1) %>% mutate(epoch='AVHRR 1982-2000'), 
          as_tibble(rlm_ndvi_annual_co2_ppet_e2) %>% mutate(epoch='MODIS 2001-2019')) %>% 
  inner_join(., as_tibble(kop %>% select(x,y,zone)), by=c("x","y")) %>% 
  mutate(epoch=factor(epoch, ordered = T, levels=rev(c("Merged 1982-2019",
                                                       "AVHRR 1982-2000",
                                                       "MODIS 2001-2019")))) %>% 
  ggplot(data=.,aes(b1, zone, color=zone, fill=epoch))+
  geom_vline(aes(xintercept=0),color='grey')+
  geom_boxplot(outlier.colour = NA)+
  scale_color_viridis_d('Climate', option='B', end=0.85)+
  scale_fill_manual('Epoch', values=c(
    "Merged 1982-2019"='white',
    "AVHRR 1982-2000"='grey80', 
    "MODIS 2001-2019"='grey30'))+
  scale_x_continuous(limits=c(-0.0025,0.005))+
  scale_y_discrete(limits=rev(structure(c(1L,2L,3L,4L,5L,6L,7L),# c(5L, 4L, 6L, 2L, 1L, 3L, 7L), 
                                        .Label = c("Equatorial",
                                                   "Tropical", "Subtropical", "Grassland", "Arid", "Temperate",
                                                   "Temperate Tas."), class = c("ordered", "factor"))))+
  labs(y=NULL,
       x=expression(paste(Delta,"NDVI"~CO[2]~'ppm'**-1)))+
  theme_linedraw()+
  theme(panel.grid.minor = element_blank())
ggsave(filename = 'figures/SM_fig_14_rlm_CO2_effect_by_epoch.png',
       width=16, height = 12, units='cm',type='cairo')
# End section ******************************************************************















gofs <- tmp_ndvi[id%in%vec_ids] %>% # filter out pixels that only have data for one satellite epoch
  as.data.table() %>% 
  .[is.na(ndvi_hyb)==F & is.na(frac_p_anom)==F] %>% 
  .[,`:=`(co2_start = co2 - min_co2)] %>% 
  merge(., test, by=c("x","y")) %>% 
  # head() %>% 
  lazy_dt() %>% 
  mutate(pred_full = b0+(b1*(co2_start-35))+b2*ppet_anom+b3*p_anom+b4*pet_anom+b5*epoch, 
         no_co2 = b0+b2*ppet_anom+b3*p_anom+b4*pet_anom+b5*epoch) %>% 
  as.data.table() %>% 
  # head() %>% 
  .[,.(r2_full = yardstick::rsq_vec(ndvi_hyb,pred_full), 
       r2_no_co2 = yardstick::rsq_vec(ndvi_hyb,no_co2), 
       rmse_full = yardstick::rmse_vec(ndvi_hyb, pred_full), 
       rmse_no_co2 = yardstick::rmse_vec(ndvi_hyb, no_co2)), by=.(x,y)] 
gofs$r2_full %>% hist
gofs$r2_no_co2 %>% hist
gofs$co2_contrib %>% summary
gofs %>% 
  # select(r2_full, r2_no_co2) %>% 
  mutate(diff = (r2_no_co2)/(r2_full)) %>% 
  ggplot(data=.,aes(x,y,fill=diff))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(limits=c(0,1),na.value = 'red')

# KEEP! % improvement of rmse
gofs %>% 
  # select(r2_full, r2_no_co2) %>% 
  mutate(diff = 100*(rmse_no_co2-rmse_full)/rmse_no_co2 ) %>% 
  ggplot(data=.,aes(x,y,fill=diff))+
  geom_tile()+
  coord_equal()+
  # scico::scale_fill_scico(palette = 'batlow',
  #                         limits=c(0,50),
  #                         oob=scales::squish)
  scale_fill_viridis_c(limits=c(0,50),
                       option='A',
                       na.value = 'red', 
                       oob=scales::squish)
scico::scico_palette_show()

# KEEP! % more variation explained with CO2
gofs %>% 
  # select(r2_full, r2_no_co2) %>% 
  mutate(diff = 100*(r2_full-r2_no_co2)) %>% 
  ggplot(data=.,aes(x,y,fill=diff))+
  geom_tile()+
  coord_equal()+
  # scico::scale_fill_scico(palette = 'batlow',
  #                         limits=c(0,50),
  #                         oob=scales::squish)
  scale_fill_viridis_c(limits=c(0,100),
                       option='A',
                       na.value = 'red', 
                       oob=scales::squish)



colorspace::hcl_palettes() %>% plot

gofs %>% 
  select(rmse_full, rmse_no_co2) %>% 
  gather() %>% 
  ggplot(data=.,aes(value,fill=key))+
  geom_density(alpha=0.5)

gofs %>% 
  select(r2_full, r2_no_co2) %>% 
  gather() %>% 
  ggplot(data=.,aes(value,fill=key))+
  geom_density(alpha=0.5)

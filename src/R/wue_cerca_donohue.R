library(mgcv); library(gratia); library(nls.multstart)
library(tidyverse)
library(data.table); setDTthreads(threads = 8)
library(lubridate); 
library(dtplyr);
library(nls.multstart); library(RcppArmadillo)
library(sf); library(stars)
library(patchwork)
set.seed(333)
# IMPORT ###################################################################
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
dat_clim <- dat[,.(x,y,date,hydro_year,month,vpd15,pe_12mo,mape,map,precip_anom_12mo)]
dat <- merge(dat, 
             vi,
             by=c("x","y","date","month","year"), 
             all=TRUE,allow.cartesian=TRUE)
dat <- dat[order(x,y,date)][, ndvi_3mo := frollmean(ndvi_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]
# dat <- dat[order(x,y,date)][, evi2_3mo := frollmean(evi2_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]
# dat <- dat[order(x,y,date)][, nirv_3mo := frollmean(nirv_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]

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
kop <- arrow::read_parquet("../data_general/Koppen_climate/BOM_Koppen_simplified7.parquet")
dat <- merge(dat, kop %>% select(x,y,zone), by=c("x","y"))
#*******************************************************************************


coords_keep <- dat %>% 
  lazy_dt() %>% 
  group_by(x,y,hydro_year) %>% 
  summarize(nobs = sum(is.na(ndvi_hyb)==F)) %>% 
  ungroup() %>% 
  as.data.table()
coords_keep <- coords_keep %>% 
  filter(nobs >= 6) %>% 
  group_by(x,y) %>% 
  summarize(nobs_annual = n()) %>% 
  ungroup()


dat_train_a <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
  .[date>= ymd("1981-12-01") & date<= ymd("2019-08-30")] %>% 
  # .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
  .[,`:=`(epoch=ifelse(hydro_year < 2001,0,1))] %>% 
  .[is.na(ndvi_hyb)==F] %>% 
  .[is.na(pe_anom_12mo)==F] %>% 
  inner_join(., coords_keep,by=c('x','y')) %>% 
  .[,`:=`(hydro_year_c = hydro_year-1982, 
          frac_p_anom = precip_anom_12mo/map, 
          frac_ppet_anom = pe_anom_12mo/mape)] %>% 
  .[,.(x,y,date,ndvi_hyb,
       hydro_year,hydro_year_c,frac_p_anom,frac_ppet_anom,epoch, 
       nobs_annual)] %>% 
  .[,.(ndvi_hyb = mean(ndvi_hyb,na.rm=TRUE), 
       frac_p_anom = mean(frac_p_anom,na.rm=TRUE), 
       frac_ppet_anom = mean(frac_ppet_anom,na.rm=TRUE), 
       epoch = mean(epoch, na.rm=TRUE)), by=.(x,y,hydro_year_c)]
dat_train_a <- dat_train_a %>% mutate(epoch=as.numeric(epoch))
dat_train_a %>% select(ndvi_hyb,hydro_year_c,frac_p_anom,frac_ppet_anom,epoch, x,y) %>% dim
dat_train_a %>% select(ndvi_hyb,hydro_year_c,frac_p_anom,frac_ppet_anom,epoch, x,y) %>% 
  distinct() %>% dim


# NDVI Regression ---------------------------------------------------------
# robust regression
system.time(
lt_ndvi_year <-  dat_train_a %>% #[nobs_annual >= 10] %>% 
  .[,.(beta = list(unname(lm(ndvi_hyb~hydro_year_c+frac_p_anom+frac_ppet_anom+epoch, 
                                    data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2],b2=unlist(beta)[3],b3=unlist(beta)[4]
  ), by=.(x,y)]
)


library(RcppArmadillo)
# ols regression
system.time(
  lt_ndvi_year_o <- dat_train[nobs_annual >= 20] %>% 
    .[,.(beta = list(unname(fastLm(
      X = cbind(1,hydro_year_c,frac_p_anom,frac_ppet_anom,epoch), 
      y=ndvi_hyb, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2],b2=unlist(beta)[3],b3=unlist(beta)[4]
    ), by=.(x,y)]
)

inner_join(lt_ndvi_year, lt_ndvi_year_o, by=c("x","y"),suffix=c("_rlm","_lm")) %>% 
  ggplot(data=.,aes(b0_lm,b0_rlm))+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_point()
inner_join(lt_ndvi_year, lt_ndvi_year_o, by=c("x","y"),suffix=c("_rlm","_lm")) %>% 
  ggplot(data=.,aes(b1_lm,b1_rlm))+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_point()
inner_join(lt_ndvi_year, lt_ndvi_year_o, by=c("x","y"),suffix=c("_rlm","_lm")) %>% 
  ggplot(data=.,aes(b2_lm,b2_rlm))+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_point()
inner_join(lt_ndvi_year, lt_ndvi_year_o, by=c("x","y"),suffix=c("_rlm","_lm")) %>% 
  ggplot(data=.,aes(b3_lm,b3_rlm))+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_point()

# system.time(
#   lt_ndvi_year2 <- 
#     dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
#     .[date>= ymd("1981-09-01") & date<= ymd("2019-08-30")] %>%
#     # .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>%
#     .[,`:=`(epoch=ifelse(hydro_year < 2001,0,1))] %>%
#     .[is.na(ndvi_hyb)==F] %>%
#     .[is.na(pe_anom_12mo)==F] %>%
#     .[,.(beta = list(unname(MASS::rlm(
#       x = cbind(1,hydro_year-1982,precip_anom_12mo/map,pe_anom_12mo/mape,epoch),
#       y=ndvi_hyb, data=.SD)$coefficients))),
#       by=.(x,y)] %>%
#     .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2],b2=unlist(beta)[3]#,b3=unlist(beta)[4]
#             ), by=.(x,y)]
# )

# VPD Regression ---------------------------------------------------------
system.time(
  lt_v_o <- dat_clim[date>=ymd("1982-01-01")][date<=ymd("2019-09-30")] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,hydro_year-1982), 
                                   y=vpd15, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)

system.time(
  lt_v <- dat_clim[date>=ymd("1981-11-01")][date<=ymd("2019-11-30")] %>%  
    .[,`:=`(hydro_year_c=hydro_year-1982)] %>% 
    .[,.(vpd15=mean(vpd15,na.rm=TRUE)),by=.(x,y,hydro_year_c)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,hydro_year_c), 
                                   y=vpd15, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)
system.time(
  lt_v_rlm <- dat[date>=ymd("1981-12-01")][date<=ymd("2019-08-30")] %>% 
    .[,`:=`(hydro_year_c = hydro_year-1982)] %>% 
    .[is.na(vpd15)==F] %>%
    .[,.(vpd15=mean(vpd15,na.rm=TRUE)),by=.(x,y,hydro_year_c)] %>% 
    .[,.(beta = list(unname(MASS::rlm(vpd15~hydro_year_c, 
                                      data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]
)

library(zyp)
system.time(
lt_v_sen <- dat_clim[date>=ymd("1981-12-01")][date<=ymd("2019-11-30")] %>% 
  .[,`:=`(hydro_year_c = hydro_year-1982)] %>% 
  .[is.na(vpd15)==F] %>% 
  .[,.(vpd15 = mean(vpd15,na.rm=TRUE)), by=.(x,y,hydro_year_c)] %>% 
  .[,.(beta = list(coef(zyp.sen(vpd15~hydro_year_c)))),by=.(x,y)] %>%
  .[,`:=`(b0=unlist(beta)[1], 
          b1=unlist(beta)[2]), by=.(x,y)]
)

inner_join(lt_v_sen, lt_v, by=c("x","y"),suffix=c("_sen","_lm")) %>% 
  ggplot(data=.,aes(b1_lm,b1_sen))+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_point()+
  geom_smooth(method='lm')



# Precip Regression ---------------------------------------------------------
system.time(
  lt_p <- dat_clim[date>=ymd("1982-01-01")][date<=ymd("2019-12-31")] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,hydro_year-1982), 
                                   y=precip_anom_12mo/map, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)

system.time(
  lt_p_sen <- dat_clim[date>=ymd("1981-12-01")][date<=ymd("2019-11-30")] %>% 
    .[,`:=`(hydro_year_c = hydro_year-1982)] %>% 
    .[is.na(precip_anom_12mo)==F] %>%
    .[,.(p_tot = mean(precip_anom_12mo+map,na.rm=TRUE)), 
      by=.(x,y,hydro_year_c)] %>% 
    .[,.(beta = list(coef(zyp.sen(p_tot~hydro_year_c)))),by=.(x,y)] %>%
    .[,`:=`(b0=unlist(beta)[1], 
            b1=unlist(beta)[2]), by=.(x,y)]
)


# Percent increase of VPD
dVPD_VPD <- mean(38*lt_v$b1,na.rm=TRUE)/mean(lt_v$b0,na.rm=TRUE)
dVPD_VPD_sen <- mean(38*lt_v_sen$b1,na.rm=TRUE)/mean(lt_v_sen$b0,na.rm=TRUE)

# Percent increase in NDVI
dNDVI_NDVI <- mean(38*lt_ndvi_year_o$b1,na.rm=TRUE)/mean(lt_ndvi_year_o$b0,na.rm=TRUE)
dNDVI_NDVI_sen <- mean(38*lt_ndvi_year$b1,na.rm=TRUE)/mean(lt_ndvi_year$b0,na.rm=TRUE)



# Percent increase in Ca
dCa_Ca <- 
  diff(range(mlo[date>=ymd("1982-01-01") & date <= ymd("2019-09-30")]$co2_trend))/
  mean(mlo[date>=ymd("1982-01-01") & date <= ymd("1983-01-01")]$co2_trend)

# Expected WUE related increase (Donohue 2013)
0.5*(dCa_Ca - 0.5*dVPD_VPD)
0.5*(dCa_Ca - 0.5*dVPD_VPD_sen)


# Actual percent relative increase in NDVI
dNDVI_NDVI
dNDVI_NDVI_sen


p_ndvi <- lt_ndvi_year %>% 
  as_tibble() %>% 
  # filter(between(b1,-0.1,0.1)) %>% 
  filter(b0 > 0.1) %>%
  ggplot(data=.,aes(x,y,fill=100*38*b1/b0))+
  geom_sf(data=oz_poly, inherit.aes = F)+
  geom_tile()+
  scale_x_continuous(breaks=seq(140,154,by=5))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scale_fill_viridis_c(expression(paste(Delta,"NDVI(%)")),
                       option='D', limits=c(0,20), oob=scales::squish)+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=8),
        legend.position = c(1,1), 
        legend.justification = c(1,1)); p_ndvi

p_p_sen <- lt_p_sen %>% 
  as_tibble() %>% 
  # filter(between(b1,-0.1,0.1)) %>% 
  filter(b0 > 0) %>% 
  ggplot(data=.,aes(x,y,fill=100*38*b1/b0))+
  geom_sf(data=oz_poly, inherit.aes = F, fill='gray40',color='black')+
  geom_tile()+
  scale_x_continuous(breaks=seq(140,154,by=5))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scico::scale_fill_scico(expression(paste(Delta,"P(%)")),
                          palette ='vikO', 
                          direction=-1,
                          limits=c(-80,80),
                          oob=scales::squish)+
  # scale_fill_viridis_c(expression(paste(Delta,"VPD(%)")),
  #   option='A', limits=c(0,20), oob=scales::squish)+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=8),
        legend.position = c(1,1), 
        legend.justification = c(1,1)); p_p_sen

p_vpd_sen <- lt_v_sen %>% 
  as_tibble() %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  filter(b0 > 0) %>% 
  ggplot(data=.,aes(x,y,fill=100*38*b1/b0))+
  geom_sf(data=oz_poly, inherit.aes = F)+
  geom_tile()+
  scale_x_continuous(breaks=seq(140,154,by=5))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scico::scale_fill_scico(expression(paste(Delta,"VPD(%)")),
          palette ='romaO', direction=-1,
          limits=c(-20,20), 
          oob=scales::squish)+
  # scale_fill_viridis_c(expression(paste(Delta,"VPD(%)")),
  #   option='A', limits=c(0,20), oob=scales::squish)+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=8),
        legend.position = c(1,1), 
        legend.justification = c(1,1)); p_vpd_sen

p_vpd <- lt_v %>% 
  as_tibble() %>% 
  filter(between(b1,-0.2,0.2)) %>%
  filter(b0>0.1) %>% 
  mutate(val = 100*38*(b1/b0)) %>% 
  filter(between(val,-50,50)) %>% 
  ggplot(data=.,aes(x,y,fill=val))+
  geom_sf(data=oz_poly, inherit.aes = F)+
  geom_tile()+
  scale_x_continuous(breaks=seq(140,154,by=5))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scale_fill_viridis_c(expression(paste(Delta,"VPD(%)")),
                       option='A',
                       # limits=c(0,20),
                       oob=scales::squish)+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=8),
        legend.position = c(1,1), 
        legend.justification = c(1,1)); p_vpd

p_vpd_o <- lt_v_o %>% 
  as_tibble() %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  ggplot(data=.,aes(x,y,fill=100*38*b1/b0))+
  geom_sf(data=oz_poly, inherit.aes = F)+
  geom_tile()+
  scale_x_continuous(breaks=seq(140,154,by=5))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scale_fill_viridis_c(expression(paste(Delta,"VPD(%)")),
                       option='A', limits=c(0,20), oob=scales::squish)+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=8),
        legend.position = c(1,1), 
        legend.justification = c(1,1)); p_vpd_o


p_wue <- lt_v %>% 
  as_tibble() %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  mutate(dVPD_VPD = b1*38/b0) %>% 
  mutate(expectation = 0.5*(dCa_Ca - 0.5*dVPD_VPD)) %>% 
  ggplot(data=.,aes(x,y,fill=expectation*100))+
  geom_sf(data=oz_poly, inherit.aes = F)+
  geom_tile()+
  scale_x_continuous(breaks=seq(140,154,by=5))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scico::scale_fill_scico(expression(paste(Delta*NDVI[Pred.]("%"))),
                          palette = 'bamako', 
                          direction = -1,
                                               # limits=c(5,12),
                          #na.value = 'red',
                                               oob=scales::squish
  )+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=8),
        legend.position = c(1,1), 
        legend.justification = c(1,1)); p_wue

p_wue_sen <- lt_v_sen %>% 
  as_tibble() %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  mutate(dVPD_VPD = b1*38/b0) %>% 
  mutate(expectation = 0.5*(dCa_Ca - 0.5*dVPD_VPD)) %>% 
  ggplot(data=.,aes(x,y,fill=expectation*100))+
  geom_sf(data=oz_poly, inherit.aes = F)+
  geom_tile()+
  scale_x_continuous(breaks=seq(140,154,by=5))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scico::scale_fill_scico(expression(paste(Delta*NDVI[Pred.]("%"))),
                          palette = 'bamako', 
                          direction = -1,
                          # limits=c(0,12), #na.value = 'red',
                          oob=scales::squish
  )+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=8),
        legend.position = c(1,1), 
        legend.justification = c(1,1)); p_wue_sen

p_diff <- inner_join({lt_ndvi_year %>% as_tibble() %>% 
    filter(between(b0,0.1,1)) %>% 
    mutate(dNDVI = 100*(38*b1)/b0) %>% 
    select(x,y,dNDVI)},
    {lt_v %>% 
        as_tibble() %>% 
        filter(between(b1,-0.1,0.1)) %>% 
        mutate(dVPD_VPD = b1*38/b0) %>% 
        mutate(expectation = 0.5*(dCa_Ca - 0.5*dVPD_VPD)) %>% 
        mutate(expectation=expectation*100) %>% 
        select(x,y,expectation)
    }) %>% 
  filter(between(expectation,100*0.05,100*0.15)) %>% 
  filter(between(dNDVI, 100*-0.5,100*0.5)) %>% 
  ggplot(data=., aes(x,y,fill=dNDVI-expectation))+
  geom_sf(data=oz_poly, inherit.aes = F, fill='gray40',color='black')+
  geom_tile()+
  scale_x_continuous(breaks=seq(140,154,by=5))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scale_fill_gradient2(expression(paste(Delta,NDVI,"-",Delta,NDVI[Pred.],"(%)")), 
                       limits=c(-25,25))+
  # scale_fill_viridis_c(expression(paste("Expected",Delta*NDVI("%"))),
  #                      option='D', 
  #                      limits=c(0,20), 
  #                      na.value = 'red',
  #                      # oob=scales::squish
  # )+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=8),
        legend.position = c(1,1), 
        legend.justification = c(1,1)); p_diff

p_diff_sen <- inner_join({lt_ndvi_year %>% as_tibble() %>% 
    filter(between(b0,0.1,1)) %>% 
    mutate(dNDVI = 100*(38*b1)/b0) %>% 
    select(x,y,dNDVI)},
    {lt_v_sen %>% 
        as_tibble() %>% 
        filter(between(b1,-0.1,0.1)) %>% 
        mutate(dVPD_VPD = b1*38/b0) %>% 
        mutate(expectation = 0.5*(dCa_Ca - 0.5*dVPD_VPD)) %>% 
        mutate(expectation=expectation*100) %>% 
        select(x,y,expectation)
    }) %>% 
  filter(between(expectation,100*0.05,100*0.15)) %>% 
  filter(between(dNDVI, 100*-0.5,100*0.5)) %>% 
  ggplot(data=., aes(x,y,fill=dNDVI-expectation))+
  geom_sf(data=oz_poly, inherit.aes = F, fill='gray40',color='black')+
  geom_tile()+
  scale_x_continuous(breaks=seq(140,154,by=5))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scale_fill_gradient2(expression(paste(Delta,NDVI,"-",Delta,NDVI[Pred.],"(%)")), 
                       limits=c(-30,30)
                       )+
  # scale_fill_viridis_c(expression(paste("Expected",Delta*NDVI("%"))),
  #                      option='D', 
  #                      limits=c(0,20), 
  #                      na.value = 'red',
  #                      # oob=scales::squish
  # )+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=7),
        legend.position = c(1,1), 
        legend.title.align = 1,
        legend.key.width = unit(0.33,'cm'),
        legend.justification = c(1,1)); p_diff_sen

p_violin <- inner_join({lt_ndvi_year %>% as_tibble() %>% 
    filter(between(b0,0.1,1)) %>% 
    mutate(dNDVI = (38*b1)/b0) %>% 
    select(x,y,dNDVI)},{
      lt_v %>% 
        as_tibble() %>% 
        filter(between(b1,-0.1,0.1)) %>% 
        mutate(dVPD_VPD = b1*38/b0) %>% 
        mutate(expectation = 0.5*(dCa_Ca - 0.5*dVPD_VPD)) %>% 
        select(x,y,expectation)
    }) %>% 
  filter(between(expectation,0.05,0.15)) %>% 
  filter(between(dNDVI, -0.4,0.4)) %>% 
  inner_join(., kop, by=c("x","y")) %>% 
  inner_join(., {lt_p %>% 
      as_tibble() %>% 
      filter(between(b1,-0.1,0.1)) %>% 
      mutate(delta_precip = 100*(38*b1+b0)) %>% 
      select(x,y,delta_precip) %>% 
      inner_join(., kop, by=c("x","y")) %>% 
      group_by(zone) %>% 
      summarize(delta_precip=mean(delta_precip,na.rm=TRUE)) %>% 
      ungroup()
  }, by=c("zone")) %>% 
  mutate(zone = recode(zone, 'Desert'='Arid')) %>% 
  mutate(diff = dNDVI-expectation) %>% 
  ggplot(data=.,aes(zone, diff,fill=delta_precip))+
  geom_hline(aes(yintercept=0),color='grey50',lty=3)+
  geom_violin(draw_quantiles = c(0.25,0.5,0.75), 
              trim=TRUE)+
  # geom_boxplot()+
  # geom_point(aes(zone, expectation, color=zone, group=zone), 
  #            data=df_vpd, size=5, shape=15, alpha=0.5)+
  # geom_hline(aes(yintercept=0.5*(dCa_Ca - 0.5*dVPD_VPD)),color='red')+
  scale_color_viridis_d(option='B',end=0.85)+
  scale_fill_gradient2(expression(paste(Delta*P[12*mo],"(%)")),
    low = scico::scico(5,palette='roma')[1], 
                       mid=scico::scico(5,palette='roma')[3], 
                       high=scico::scico(5,palette='roma')[5], 
                       # limits=c(-12,12)
                       )+
  labs(x=NULL, y=expression(paste(Delta,"NDVI",-Delta*NDVI[Pred.]," (%)")))+
  theme_linedraw()+
  theme(legend.position = 'right', 
        panel.grid = element_blank()); p_violin

ggsave((p_vpd|p_wue|p_diff)/p_violin+plot_layout(heights = c(1,0.6)), 
       filename = 'figures/map_dvpd_dndvipred_ddifference_violin.png', 
       width = 26, height = 30, units='cm', dpi=350, type='cairo')


p_violin_sen <- inner_join({lt_ndvi_year %>% as_tibble() %>% 
    filter(between(b0,0.1,1)) %>% 
    mutate(dNDVI = (38*b1)/b0) %>% 
    select(x,y,dNDVI)},{
      lt_v_sen %>% 
        as_tibble() %>% 
        filter(between(b1,-0.1,0.1)) %>% 
        mutate(dVPD_VPD = b1*38/b0) %>% 
        mutate(expectation = 0.5*(dCa_Ca - 0.5*dVPD_VPD)) %>% 
        select(x,y,expectation)
    }) %>% 
  filter(between(expectation,0.05,0.15)) %>% 
  filter(between(dNDVI, -0.4,0.4)) %>% 
  inner_join(., kop, by=c("x","y")) %>% 
  inner_join(., {lt_p_sen %>% 
      as_tibble() %>% 
      # filter(between(b1,-0.1,0.1)) %>% 
      mutate(delta_precip = 100*(38*b1)/b0) %>% 
      select(x,y,delta_precip) %>% 
      inner_join(., kop, by=c("x","y")) %>% 
      group_by(zone) %>% 
      summarize(delta_precip=mean(delta_precip,na.rm=TRUE)) %>% 
      ungroup()
  }, by=c("zone")) %>% 
  mutate(zone = recode(zone, 'Desert'='Arid')) %>% 
  mutate(zone = recode(zone, 'Temperate Tas.'='Temp. Tasm.')) %>% 
  mutate(diff = 100*(dNDVI-expectation)) %>% 
  ggplot(data=.,aes(diff,zone,fill=delta_precip))+
  geom_vline(aes(xintercept=0),color='grey50',lty=3)+
  geom_violin(draw_quantiles = c(0.25,0.5,0.75), 
              trim=TRUE)+
  scale_color_viridis_d(option='B',end=0.85)+
  scale_fill_gradient2(expression(paste(Delta*P[12*mo],"(%)")),
                       low = scico::scico(7,palette='roma')[1], 
                       mid=scico::scico(7,palette='roma')[4], 
                       high=scico::scico(7,palette='roma')[7], 
                       limits=c(-40,40)
  )+
  labs(x=NULL, y=expression(paste(Delta,"NDVI",-Delta*NDVI[Pred.]," (%)")))+
  scale_y_discrete(limits=rev(structure(c(1L,2L,3L,4L,5L,6L,7L),# c(5L, 4L, 6L, 2L, 1L, 3L, 7L), 
                .Label = c("Equatorial",
                           "Tropical", "Subtropical", "Grassland", "Arid", "Temperate",
                           "Temp. Tasm."), class = c("ordered", "factor"))))+
  theme_linedraw()+
  theme(legend.position = 'top', 
        legend.key.height = unit(0.2,'cm'),
        panel.grid = element_blank()); p_violin_sen
p_violin_sen

library(patchwork)
ggsave((p_vpd_sen|p_wue_sen|p_diff_sen|p_violin_sen)+
         plot_layout()+
         plot_annotation(tag_levels = 'A'), 
       filename = 'figures/map_dvpd_dndvipred_ddifference_violin.png', 
       width = 30, height = 20, units='cm', dpi=350, type='cairo')



# lt_p %>% 
#   as_tibble() %>% 
#   filter(between(b1,-0.1,0.1)) %>% 
#   mutate(delta_precip = 100*(38*b1+b0)) %>% 
#   select(x,y,delta_precip)
# 
# 
# 
# 
# 
# lt_p %>% 
#   as_tibble() %>% 
#   filter(between(b1,-0.1,0.1)) %>% 
#   ggplot(data=.,aes(x,y,fill=100*(38*b1+b0)))+
#   geom_sf(data=oz_poly, inherit.aes = F,fill='gray50',color='black')+
#   geom_tile()+
#   scale_x_continuous(breaks=seq(140,154,by=5))+
#   coord_sf(xlim = c(140,154),
#            ylim = c(-45,-10), expand = FALSE)+
#   labs(x=NULL,y=NULL)+
#   scico::scale_fill_scico(palette = 'berlin',direction=-1, 
#                           limits=c(-33,33), 
#                           oob=scales::squish)+
#   # scale_fill_viridis_c("Precip\nincrease (%)",
#   #                      option='A', 
#   #                      # limits=c(0,20), 
#   #                      oob=scales::squish)+
#   theme(panel.background = element_rect(fill='lightblue'),
#         panel.grid = element_blank(), 
#         legend.position = 'bottom')
# 
# 
# 
# 
# lt_ndvi_year %>% as_tibble() %>% 
#   filter(between(b0,0.1,1)) %>% 
#   mutate(dNDVI = 100*(38*b1)/b0) %>% # pull(dNDVI) %>% quantile(., c(0.01,0.99))
#   ggplot(data=.,aes(x,y,fill=dNDVI))+
#   geom_sf(data=oz_poly, inherit.aes = F, fill='gray40',color='black')+
#   geom_tile()+
#   scale_x_continuous(breaks=seq(140,154,by=5))+
#   coord_sf(xlim = c(140,154),
#            ylim = c(-45,-10), expand = FALSE)+
#   labs(x=NULL,y=NULL)+
#   scale_fill_gradient2(limits=c(-15,30))+
#   # scale_fill_viridis_c(expression(paste("Expected",Delta*NDVI("%"))),
#   #                      option='D', 
#   #                      limits=c(0,20), 
#   #                      na.value = 'red',
#   #                      # oob=scales::squish
#   # )+
#   theme(panel.background = element_rect(fill='lightblue'),
#         panel.grid = element_blank())
# 
# 
# 
# 
# inner_join({lt_ndvi_year %>% as_tibble() %>% 
#     filter(between(b0,0.1,1)) %>% 
#     mutate(dNDVI = (38*b1)/b0) %>% 
#     select(x,y,dNDVI)},{
#       lt_v %>% 
#         as_tibble() %>% 
#         filter(between(b1,-0.1,0.1)) %>% 
#         mutate(dVPD_VPD = b1*38/b0) %>% 
#         mutate(expectation = 0.5*(dCa_Ca - 0.5*dVPD_VPD)) %>% 
#         select(x,y,expectation)
#     }) %>% 
#   filter(between(expectation,0.05,0.15)) %>% 
#   filter(between(dNDVI, -0.5,0.5)) %>% 
#   ggplot(data=., aes(expectation, dNDVI))+
#   geom_point(alpha=0.1)+
#   geom_smooth(method='lm')
# 
# 
# 
#   
# 
# 
# 
# 
# 
# 
# 
# 
# system.time(
#   lt_ndvi_wEpoch <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
#     .[date>= ymd("1982-01-01") & date<= ymd("2019-09-30")] %>% 
#     # .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
#     .[,`:=`(epoch=ifelse(hydro_year < 2001,0,1))] %>% 
#     .[is.na(ndvi_hyb)==F] %>% 
#     .[is.na(pe_anom_12mo)==F] %>% 
#     .[,.(beta = list(unname(fastLm(
#       X = cbind(1,cco2,precip_anom_12mo,epoch), 
#       y=ndvi_hyb, data=.SD)$coefficients))), 
#       by=.(x,y)] %>% 
#     .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2],b2=unlist(beta)[3],b3=unlist(beta)[4]), by=.(x,y)]
# )
# 
# system.time(
#   lt_v2 <- dat[date>=ymd("1982-01-01")][date<=ymd("2019-09-30")] %>% 
#     .[,`:=`(year_c = year-2000.5)] %>% 
#     .[,.(beta = list(unname(coefficients(lm(vpd15~hydro_year))))), 
#       by=.(x,y)] %>% 
#     .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
# )
# 
# 
# 
# system.time(
#   lt_ndvi_year <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
#     .[date>= ymd("1982-01-01") & date<= ymd("2019-09-30")] %>% 
#     # .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
#     .[,`:=`(epoch=ifelse(hydro_year < 2001,0,1))] %>% 
#     .[is.na(ndvi_hyb)==F] %>% 
#     .[is.na(pe_anom_12mo)==F] %>% 
#     .[,.(beta = list(unname(fastLm(
#       X = cbind(1,hydro_year,precip_anom_12mo,epoch), 
#       y=ndvi_hyb, data=.SD)$coefficients))), 
#       by=.(x,y)] %>% 
#     .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2],b2=unlist(beta)[3],b3=unlist(beta)[4]), by=.(x,y)]
# )
# 
# 
# mean(lt_ndvi_wEpoch$b1)
# lm(vpd15~hydro_year, data=dat)
# lm(vpd15~scale(hydro_year,center = T, scale=F), data=dat)
# lm(vpd15~scale(hydro_year,center = T, scale=F), data=dat[sample(.N,1e6)])
# lme4::lmer(vpd15~scale(hydro_year,center = T, scale=F)+(1|id), 
#            data=dat[sample(.N,1e6)])
# lt_v2$b1 %>% summary
# mean(dat$vpd15,na.rm=TRUE)
# 
# (2019*0.00639 - 1982*0.00639)/(-8.99827 + 1982*0.00639)
# (-8.99827 + 2019*0.00639)/(-8.99827 + 1982*0.00639)
# 
# 
# lm(vpd15~I(hydro_year-1982), data=dat)
# (3.666 + 0.00639*38)/(3.666)
# 
# dVPD_VPD <- mean(38*lt_v$b1,na.rm=TRUE)/mean(lt_v$b0 - 38*lt_v$b1,na.rm=TRUE) # centered covariate
# 
# # Percent increase of VPD
# dVPD_VPD <- mean(38*lt_v$b1,na.rm=TRUE)/mean(lt_v$b0,na.rm=TRUE) # centered covariate
# dVPD_VPD2 <- mean(2019*lt_v2$b1,na.rm=TRUE)/mean(lt_v$b0,na.rm=TRUE) # uncentered
# 
# 
# 
# 
# 
# 
# mean(dat[date>=ymd("2016-01-01")]$vpd15,na.rm=TRUE)/
# mean(dat[date>=ymd("1982-01-01")&date<=ymd("1985-01-01")]$vpd15,na.rm=TRUE)
# 
# x <- runif(100, 1,10)
# y <- 0.5*x + rnorm(100,0,0.1)
# plot(y~x)
# lm(y~x)
# (0.008029 + 10*0.501)/(0.5)
# #
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# mlo[date==ymd("1982-01-01")]
# mlo[date==ymd("2019-10-01")]
# 
# #388
# 
# as_tibble(lt_ndvi_year) %>% 
#   filter(between(b1,-0.01,0.01)) %>% 
#   pull(b1) %>% mean
# mean(lt_ndvi_year$b1,na.rm=TRUE)
# 
# 0.0009078149/mean(dat$ndvi_u)
# 
# (388-340)/(0.5*(388+340))
# 
# 
# 
# 
# (mlo[date==ymd("2019-10-01")]$co2_trend-mlo[date==ymd("1980-10-01")]$co2_trend)/
#   (0.5*(mlo[date==ymd("2019-10-01")]$co2_trend+mlo[date==ymd("1980-10-01")]$co2_trend))
# 
# 
# 
# 
# lt_v2$b0 %>% hist
# 
# 
# lt_v %>% 
#   as_tibble() %>% 
#   filter(between(b1,-0.1,0.1)) %>% 
#   ggplot(data=.,aes(x,y,fill=b0))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c(option='A')
# 
# lt_v %>% 
#   as_tibble() %>% 
#   filter(between(b1,-0.1,0.1)) %>% 
#   ggplot(data=.,aes(x,y,fill=b1))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_gradient2(high='#cf0000',low='navy',limits=c(-0.025,0.025))
# 
# 
# lt_v %>% 
#   as_tibble() %>% 
#   filter(between(b1,-0.1,0.1)) %>% 
#   ggplot(data=.,aes(x,y,fill=0.193 - 0.5*(b1*38)/b0))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c(option='B',limits=c(0,0.25))
# 
# 
# 
# lt_ndvi_wEpoch %>% 
#   as_tibble() %>% 
#   filter(between(b0,0.1,1)) %>% 
#   ggplot(data=.,aes(x,y,fill=b1))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_gradient2(limits=c(-0.004,0.004))
# 
# 
# mean(dat[date>= ymd("2015-01-01") & date<= ymd("2019-09-01")]$ndvi_hyb,na.rm=TRUE)/
# mean(dat[date>= ymd("1982-01-01") & date<= ymd("1986-12-31")]$ndvi_hyb,na.rm=TRUE)
# 
# mean(dat[date>= ymd("1982-01-01") & date<= ymd("2019-09-3")]$ndvi_hyb,na.rm=TRUE)
# 
# 
# lt_v %>% 
#   mutate(pct_vpd = b1/b0) %>% 
#   mutate(pct_vpd = mean(pct_vpd,na.rm=TRUE)) %>% 
#   pull(pct_vpd) %>% mean
# 
# 
# df_vpd <- lt_v %>% 
#   as_tibble() %>% 
#   inner_join(.,kop,by=c("x","y")) %>% 
#   mutate(zone = recode(zone,'Desert'='Arid')) %>% 
#   group_by(zone) %>% 
#   summarize(dVPD_VPD = mean(38*b1,na.rm=TRUE)/mean(b0,na.rm=TRUE)) %>% 
#   ungroup() %>% 
#   mutate(expectation = 0.5*(dCa_Ca - 0.5*dVPD_VPD))
# 
# lt_overall <- lt_ndvi_wEpoch %>% 
#   as_tibble() %>% 
#   filter(between(b0,0.15,1)) %>% 
#   filter(between(b1,-0.1,0.1)) %>% 
#   mutate(rel_ndvi = (38*b1)/(b0)) %>% 
#   filter(between(rel_ndvi, -0.333,0.333)) %>% 
#   mutate(zone = 'Overall')
#   
# lt_ndvi_wEpoch %>% 
#   inner_join(., kop,by=c("x","y")) %>% 
#   as_tibble() %>% 
#   filter(between(b0,0.15,1)) %>% 
#   filter(between(b1,-0.1,0.1)) %>% 
#   mutate(rel_ndvi = (38*b1)/(b0)) %>% 
#   filter(between(rel_ndvi, -0.333,0.333)) %>% 
#   mutate(zone = recode(zone,'Desert'='Arid')) %>% 
#   bind_rows(., lt_overall) %>% 
#   mutate(zone = fct_relevel(zone,'Overall',after=Inf)) %>% 
#   ggplot(data=.,aes(zone, rel_ndvi))+
#   geom_boxplot()+
#   geom_point(aes(zone, expectation, color=zone, group=zone), 
#              data=df_vpd, size=5, shape=15, alpha=0.5)+
#   geom_hline(aes(yintercept=0.5*(dCa_Ca - 0.5*dVPD_VPD)),color='red')+
#   scale_color_viridis_d(option='B',end=0.85)+
#   labs(x=NULL, y="% NDVI Change")+
#   theme_linedraw()+
#   theme(legend.position = 'none')
#   # geom_histogram(bins = 100, na.rm = TRUE)+
#   # stat_summary(fun='mean', aes(y=rel_ndvi))
#   # geom_vline(aes(xintercept=stat(rel_ndvi),color=zone))+
#   # geom_vline(aes(xintercept=0.5*(dCa_Ca - 0.5*dVPD_VPD)),col='blue')+
#   # facet_wrap(~zone)
# 
# 
# 
# lt_v %>% 
#   as_tibble() %>% 
#   filter(between(b1,-0.1,0.1)) %>% 
#   mutate(dww = 0.193 - 0.5*(b1*38)/b0) %>% 
#   inner_join(., 
#              lt_ndvi_wEpoch %>% rename(ndvi=b0,dndvi=b1) %>% 
#                select(x,y,ndvi,dndvi),
#              ,by=c("x","y")) %>% 
#   filter(between(dww,0.1,0.25)) %>% pull(dww) %>% summary
#   filter(between(ndvi,0.1,1)) %>% 
#   filter(between(dndvi,-0.01,0.01)) %>% 
#   mutate(val = (dndvi*80)/ndvi) %>% pull(val) %>% summary
#   ggplot(data=., aes(dww, val))+
#   geom_point()+
#   geom_smooth()
# 
# lt_v %>% 
#   as_tibble() %>% 
#   filter(between(b1,-0.1,0.1)) %>% 
#   mutate(dww = 0.193 - 0.5*(b1*38)/b0) %>% 
#   inner_join(., 
#              lt_ndvi_wEpoch %>% rename(ndvi=b0,dndvi=b1) %>% 
#                select(x,y,ndvi,dndvi),
#              ,by=c("x","y")) %>% 
#   inner_join(., kop,by=c("x","y")) %>% 
#   filter(between(dww,0.1,0.25)) %>% 
#   filter(between(ndvi,0.1,1)) %>% 
#   filter(between(dndvi,-0.01,0.01)) %>% 
#   mutate(val = (dndvi*80)/ndvi) %>%
#   ggplot(data=., aes(dww, val,color=zone))+
#   # geom_point()+
#   geom_smooth(method='lm')
# 
# 
# o <- dat %>% 
#   select(x,y,mape) %>% 
#   distinct()
# 
# 1-dim(o[mape>=1])[1]/(dim(o))[1]

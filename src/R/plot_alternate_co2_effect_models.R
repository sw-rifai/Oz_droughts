library(tidyverse)
library(data.table); setDTthreads(threads = 0)
library(lubridate); 
library(dtplyr);
library(RcppArmadillo)
library(sf); library(stars)
library(patchwork); 
library(zyp); 
library(mgcv)
set.seed(333)
# IMPORT ###################################################################
# load("data/gridCell_lm_ndvi_clim.Rdata") # grid cell linear regressions
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)

# vegetation index record
vi <- arrow::read_parquet("../data_general/MCD43/MCD43_AVHRR_NDVI_hybrid_2020-10-12.parquet" 
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

vi <- vi %>% select(-season)

dat <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
           col_select = c(
             "date", "hydro_year", "id",
            "precip", "precip_12mo","precip_anom", "precip_anom_12mo","map",
            "vpd15","vpd15_12mo","vpd15_anom", "vpd15_anom_12mo","mavpd15",
            "pet","pet_anom","pet_anom_12mo","pet_12mo", "mapet",
             "pe","pe_12mo","pe_anom_12mo", 
             'vc','veg_class',
             'month',
             "x", "y", "year")) %>% 
  as.data.table()

norms_mape <- dat %>% lazy_dt() %>% 
  filter(date>=ymd("1982-01-01") & date<=ymd("2011-12-31")) %>% 
  group_by(x,y,hydro_year) %>% 
  summarize(ppet_12mo = mean(precip_12mo/pet_12mo,na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(x,y) %>% 
  summarize(mappet = mean(ppet_12mo,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as.data.table()
dat <- merge(dat,norms_mape,by=c("x","y"))

dat <- dat[,`:=`(#pe_3mo = precip_3mo/pet_3mo, 
  pe_12mo = precip_12mo/pet_12mo 
  # pe_24mo = precip_24mo/pet_24mo, 
  # pe_36mo = precip_36mo/pet_36mo, 
  # pe_48mo = precip_48mo/pet_48mo
)]
dim(dat)
dat <- merge(dat, 
             vi,
             by=c("x","y","date","month","year"), 
             all=TRUE,allow.cartesian=TRUE)
dat <- dat[order(x,y,date)][, ndvi_3mo := frollmean(ndvi_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]

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
# dat <- dat[is.na(ndvi_3mo)==F & is.na(co2_int)==F]
center_co2 <- mean(dat$co2_int)
dat <- dat[,`:=`(cco2=co2_int-center_co2)]
gc()
dat <- dat[is.na(vc)==F]
dat <- dat[str_detect(vc,"Forests") |
             str_detect(vc, "Eucalypt") |
             str_detect(vc, "Rainforests")]
dat <- dat[x>= 140] # FILTER TO LON >= 140
# dat <- dat[ndvi_hyb>0][ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5]
dat[,`:=`(pe_anom_12mo = pe_12mo - mape)]
dat[,`:=`(epoch = ifelse(date<ymd("2000-12-31"),'avhrr','modis'))]
dat[,`:=`(year=year(date),month=month(date))] %>%
  .[,`:=`(season = case_when(month%in%c(3:5)~'MAM',
                             month%in%c(6:8)~'JJA',
                             month%in%c(9:11)~'SON',
                             month%in%c(12,1,2)~'DJF'))]

dat <- dat %>% mutate(epoch = as_factor(epoch), 
                      season = factor(season, levels=c("SON","DJF","MAM","JJA")))
kop <- arrow::read_parquet("../data_general/Koppen_climate/BOM_Koppen_simplified7.parquet")
kop <- setDT(kop)
kop <- kop[,.(x,y,zone)]
dat <- merge(dat, kop, by=c("x","y"),all = T)
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



# summarize to annual -----------------------------------------------------
dat_annual <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
  .[date>= ymd("1981-11-01") & date<= ymd("2019-08-30")] %>% 
  # .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
  .[,`:=`(epoch=ifelse(hydro_year < 2001,0,1))] %>% 
  .[is.na(ndvi_hyb)==F] %>% 
  .[is.na(pe_anom_12mo)==F] %>% 
  merge(., coords_keep,by=c('x','y'))
dat_annual <- dat_annual[,`:=`(hydro_year_c = hydro_year-1982, 
          frac_p_anom = precip_anom_12mo/map, 
          frac_ppet_anom = pe_anom_12mo/mappet, 
          frac_pet_anom = pet_anom_12mo/mapet, 
          frac_vpd_anom = vpd15_anom_12mo/mavpd15)]
dat_annual <- dat_annual %>% 
      select(x,y,date,ndvi_hyb,co2_trend,
       hydro_year,hydro_year_c,
       frac_p_anom,frac_ppet_anom,frac_pet_anom,frac_vpd_anom,
       map,mappet,mapet,mavpd15,
       epoch,nobs_annual)
dat_annual <- dat_annual[,.(ndvi_hyb = mean(ndvi_hyb,na.rm=TRUE),
       co2 = mean(co2_trend,na.rm=TRUE),
       frac_p_anom = mean(frac_p_anom,na.rm=TRUE), 
       frac_pet_anom = mean(frac_pet_anom,na.rm=TRUE),
       frac_ppet_anom = mean(frac_ppet_anom,na.rm=TRUE),
       frac_vpd_anom = mean(frac_vpd_anom,na.rm=TRUE),
       epoch = mean(epoch, na.rm=TRUE), 
       map = mean(map,na.rm=TRUE), 
       mapet = mean(mapet, na.rm=TRUE), 
       mappet = mean(mappet, na.rm=TRUE), 
       mavpd15 = mean(mavpd15,na.rm=TRUE)), 
    by=.(x,y,hydro_year_c)]
dat_annual <- dat_annual[is.infinite(frac_ppet_anom)==F]
dat_annual %>% dim

# dat_annual[frac_ppet_anom==max(frac_ppet_anom)]
# dat[near(x,147.6157,tol=0.01)&near(y,-42.55769,tol=0.01)] %>% 
#   ggplot(data=.,aes(date, pe_anom))+
#   geom_point()


# Percent increase in Ca -------------------------------------------------------
dCa_Ca <- 
  diff(range(mlo[date>=ymd("1981-12-01") & date <= ymd("2019-08-30")]$co2_trend))/
  mean(mlo[date>=ymd("1981-12-01") & date <= ymd("1982-11-01")]$co2_trend)
dCa_Ca_e1 <- 
  diff(range(mlo[date>=ymd("1981-12-01") & date <= ymd("2000-11-30")]$co2_trend))/
  mean(mlo[date>=ymd("1981-12-01") & date <= ymd("1982-11-01")]$co2_trend)
dCa_Ca_e2 <- 
  diff(range(mlo[date>=ymd("2000-12-01") & date <= ymd("2019-08-30")]$co2_trend))/
  mean(mlo[date>=ymd("2000-12-01") & date <= ymd("2001-11-01")]$co2_trend)


## Regression VPD Thiel Sen regression ---------------------------------------
system.time(
  lt_v_sen <- dat[date>=ymd("1981-11-01")][date<=ymd("2019-10-30")] %>% 
    .[,`:=`(hydro_year_c = hydro_year-1982)] %>% 
    .[is.na(vpd15)==F] %>% 
    .[,.(vpd15 = mean(vpd15,na.rm=TRUE)), by=.(x,y,hydro_year_c)] %>% 
    .[,.(beta = list(coef(zyp.sen(vpd15~hydro_year_c)))),by=.(x,y)] %>%
    .[,`:=`(b0=unlist(beta)[1], 
            b1=unlist(beta)[2]), by=.(x,y)]
)

# Plot VPD rel increase ----------------------------------------------------------------
p_vpd_sen <- lt_v_sen %>% 
  as_tibble() %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  filter(b0 > 0) %>% 
  ggplot(data=.,aes(x,y,fill=100*38*b1/b0))+
  geom_sf(data=oz_poly, inherit.aes = F, fill='gray50',color='black')+
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


# Plot GAM estimate CO2 effect on NDVI -----------------------------------------
o1 <- bam(ndvi_hyb~
            te(x,y,by=co2,fx = F)+
            epoch+
            s(mappet,k=5,bs='cs')+
            s(frac_p_anom,k=5,bs='cs')+
            s(frac_ppet_anom,k=5,bs='cs')+
            s(frac_pet_anom,k=5,bs='cs')+
            s(frac_vpd_anom,k=5,bs='cs')
          , 
          data=dat_annual, 
          select=TRUE, 
          discrete=TRUE)
summary(o1)
plot(o1,scheme=2)

summary(tmp_ndvi$co2)


norms_vpd <- lt_v_sen[,`:=`(vpd15_0 = b0,
                            vpd15_1 = b0+16*b1,
                            vpd15_2 = b0+37*b1)] %>% 
  .[,`:=`(frac_vpd_anom = (vpd15_1-vpd15_0)/vpd15_1 )]
ggplot(data=.,aes(x,y,fill=vpd15_1-vpd15_0))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2()


expand_grid(norms, tibble(co2=c(341.4,410.9),e=c('e1','e2'))) %>% 
  filter(mape<=2) %>% 
  mutate(frac_p_anom=0, 
         frac_ppet_anom=0,
         frac_pet_anom=0,
         frac_vpd_anom=0, 
         epoch=0) %>%
  filter(is.na(mape)==F) %>% 
  mutate(pred = predict(o1,newdata=.)) %>% 
  filter(is.na(pred)==F) %>% 
  pivot_wider(., names_from=c('e'), values_from=c('pred','co2')) %>% 
  mutate(dNDVI = 100*((pred_e2-pred_e1)/pred_e1)) %>%
  # filter(dNDVI > 0) %>% 
  ggplot(data=.,aes(x,y,fill=dNDVI))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(option='B',
                       limits=c(0,17),
                       oob=scales::squish)





# Plot WUE CO2 Fert expected relative increase in NDVI -------------------------
p_wue_sen <- lt_v_sen %>% 
  as_tibble() %>% 
  filter(b0 > 0) %>% 
  filter(between(b1,-0.05,0.05)) %>% 
  mutate(dVPD_VPD = b1*37/b0) %>% 
  mutate(expectation = 0.5*(dCa_Ca - 0.5*dVPD_VPD)) %>% 
  # pull(expectation) %>% quantile(., c(0.01,0.99))
  ggplot(data=.,aes(x,y,fill=expectation*100))+
  geom_sf(data=oz_poly, inherit.aes = F, fill='gray50',color='black')+
  geom_tile()+
  scale_x_continuous(breaks=seq(140,154,by=5))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scico::scale_fill_scico(expression(paste(Delta*NDVI[WUE~Pred.]("%"))),
                          palette = 'bamako', 
                          direction = -1,
                          # limits=c(5,15), #na.value = 'red',
                          oob=scales::squish
  )+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=8),
        legend.position = c(1,1), 
        legend.justification = c(1,1)); p_wue_sen






# Regression ---------------------------------------------------------
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
tmp_epoch_nobs %>% 
  inner_join(., tmp_ndvi %>% select(x,y,id) %>% distinct(), by='id') %>% 
  ggplot(data=.,aes(x,y,fill=obs_epoch))+geom_tile()+scale_fill_viridis_c()
vec_ids <- tmp_epoch_nobs %>% 
  filter(obs_epoch >= 1.9) %>% 
  pull(id)








dat[date==ymd("2001-01-01")] %>% mutate(mappet = map/mapet) %>% pull(mappet) %>% hist
dat[date==ymd("2001-01-01")] %>% mutate(mappet = map/mapet) %>% pull(mape) %>% hist
tmp_mappet <- dat %>% lazy_dt() %>% 
  filter(date>=ymd("1982-01-01") & date<=ymd("2011-12-31")) %>% 
  group_by(x,y,hydro_year) %>% 
  summarize(ppet_12mo = mean(precip_12mo/pet_12mo,na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(x,y) %>% 
  summarize(mappet = mean(ppet_12mo,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as.data.table()
tmp_mappet$mappet %>% hist
tmp_mappet %>% 
  ggplot(data=.,aes(x,y,fill=mappet))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()

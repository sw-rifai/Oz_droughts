library(tidyverse)
library(data.table); setDTthreads(threads = 8)
library(lubridate); 
library(mgcv); #library(mgcViz); 
library(gratia)
library(dtplyr); 
library(nls.multstart)
# IMPORT ###################################################################
vi <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet", 
                          col_select = c("x","y","date",
                                         "ndvi_c","ndvi_m","ndvi_hyb", 
                                         "evi2_hyb","evi2_m"))
tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id","season",
                             "precip",  "precip_anom",
                             # "precip_anom_3mo","precip_anom_36mo",
                             "precip_anom_12mo",
                             "map", 
                             "precip_12mo","precip_36mo",
                             # "tmax","tmax_anom","tmax_anom_sd", "matmax",
                             # "tmin",
                             "vpd15","vpd15_anom","vpd15_anom_sd","mavpd15",
                             # "vpd15_u",
                             "pet","mapet","pet_anom","pet_anom_3mo","pet_u","pet_sd",
                             "pet_anom_sd", 
                             "pet_12mo","pet_36mo",
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

# unique(tmp[,.(vc,veg_class)]) %>% View
# tmp <- tmp[order(x,y,date)][,tmean := (tmax+tmin)/2]
# tmp <- tmp[order(x,y,date)][, tmax_3mo := frollmean(tmax,n = 3,fill = NA,align='center'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, tmean_3mo := frollmean(tmean,n = 3,fill = NA,align='center'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, tmin_3mo := frollmean(tmin,n = 3,fill = NA,align='center'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, tmax_12mo := frollmean(tmax,n = 12,fill = NA,align='right'), by=.(x,y)]
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
tmp <- merge(tmp, 
      vi,
      by=c("x","y","date"), 
      all=TRUE,allow.cartesian=TRUE)
tmp <- tmp[order(x,y,date)][, ndvi_3mo := frollmean(ndvi_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]

rm(vi); gc(full=TRUE)

mlo <- read_table("../data_general/CO2_growth_rate/co2_mm_mlo_20200405.txt", 
                  skip = 72, col_names = F) %>% 
  set_names(
    c("year","month","ddate","co2_avg","co2_int","co2_trend","ndays")
  ) %>% 
  mutate(date = ymd(paste(year,month,1))) %>% 
  select(date,co2_int,co2_trend) %>% 
  as.data.table()
tmp <- merge(mlo,tmp,by="date")
gc()
#*******************************************************************************
# Determine optimal integration time of P:PET ---------------------------------
old_dat <- tmp[hydro_year %in% c(1984:1990)] %>% 
  .[season=="DJF"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
  .[ndvi_m>0] %>%
  .[is.na(precip_48mo)==F] %>% 
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  # .[veg_class %in% c(1,2)] %>% 
  .[is.na(veg_class)==F] %>% 
  # .[pe_3mo <= 2] %>% 
  .[sample(.N,10000)]
fit3 <-  old_dat %>% 
  nls_multstart(ndvi_3mo ~ SSlogis(pe_3mo, Asym, xmid, scal),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01),
                start_upper = c(Asym=1, xmid=1, scal=1),
                # supp_errors = 'Y',
                na.action = na.omit)
fit12 <- old_dat %>% 
  nls_multstart(ndvi_3mo ~ SSlogis(pe_12mo, Asym, xmid, scal),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01),
                start_upper = c(Asym=1, xmid=1, scal=1),
                # supp_errors = 'Y',
                na.action = na.omit)
fit24 <- old_dat %>% 
  nls_multstart(ndvi_3mo ~ SSlogis(pe_24mo, Asym, xmid, scal),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01),
                start_upper = c(Asym=1, xmid=1, scal=1),
                # supp_errors = 'Y',
                na.action = na.omit)
fit36 <- old_dat %>% 
  nls_multstart(ndvi_3mo ~ SSlogis(pe_36mo, Asym, xmid, scal),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01),
                start_upper = c(Asym=1, xmid=1, scal=1),
                # supp_errors = 'Y',
                na.action = na.omit)
fit48 <- old_dat %>% 
  nls_multstart(ndvi_3mo ~ SSlogis(pe_48mo, Asym, xmid, scal),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01),
                start_upper = c(Asym=1, xmid=1, scal=1),
                # supp_errors = 'Y',
                na.action = na.omit)

bbmle::AICtab(fit3, fit12,fit24, fit36, fit48)
bbmle::BICtab(fit3, fit12,fit24, fit36, fit48)
c(sqrt(mean(residuals(fit3)**2)),
sqrt(mean(residuals(fit12)**2)),
sqrt(mean(residuals(fit24)**2)),
sqrt(mean(residuals(fit36)**2)),
sqrt(mean(residuals(fit48)**2))
) -> vec; vec
plot(vec~c(3,12,24,36,48), ylim=c(0,0.15))
" Using the Precip_12mo:PET_12mo is a much better fit"
# END
# ******************************************************************************

#*******************************************************************************
# Fit Logistic model by season across  hydro_years ---------------------------------------
fn_logistic1 <- function(dat){
  fit <- nls_multstart(ndvi_3mo ~ SSlogis(pe_48mo,Asym,xmid,scal),
                       data = dat[sample(.N, 2000)],
                       iter = 100,
                       start_lower = c(Asym=0.5, xmid=0, scal=0),
                       start_upper = c(Asym=1, xmid=0.5, scal=1),
                       supp_errors = 'Y',
                       na.action = na.omit,
                       lower= c(0.5,  0,   0),
                       upper= c(1,    1, 1))
  bfit <- broom::tidy(fit)
  bfit$hydro_year <- unique(dat$hydro_year);
  bfit$season <- unique(dat$season)
  bfit$nobs <- dim(dat)[1]
  return(bfit)
}
# vec_id <- tmp[!veg_class %in% c(6,10,12,13,30,31,32)] %>% 
#   .[is.na(id)==F] %>% 
#   .[mape <= 2] %>% 
#   .[, sample(id, 10000)] 

tmp <- tmp[p_anom_12mo_frac > -0.1 & p_anom_12mo_frac < 0.1] 

tmp[,.(nobs = sum(is.na(ndvi_3mo)==F)), by=.(hydro_year,season)][['nobs']]


tmp <- tmp[hydro_year %in% c(1982:2019)] %>% 
  # .[id %in% vec_id] %>% 
  # .[season=="DJF"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_hyb>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F]
gc()


xdat <- tmp[hydro_year %in% c(1985:2019)] %>% 
  .[,fn_logistic1(.SD),by=.(hydro_year,season)]
gc()


xdat %>%
  inner_join(., {mlo %>% mutate(hydro_year=year(date)) %>% 
      group_by(hydro_year) %>% 
      summarize(co2 = mean(co2_trend,na.rm=TRUE))}) %>% 
  inner_join(., {tmp %>% group_by(hydro_year) %>% 
      summarize(pe_12mo = mean(pe_12mo,na.rm=TRUE)) %>% 
      ungroup()},by='hydro_year') %>% 
  as_tibble() %>%
  # filter(p.value < 0.1) %>%
  select(season,hydro_year,co2,pe_12mo,nobs, term,estimate) %>%
  spread(key = term, value=estimate) %>% 
  ggplot(data=., aes(co2, scal))+
  geom_smooth(se=F,method='lm')+
  geom_label(aes(label=hydro_year,fill=nobs),color='red')+
  scale_fill_viridis_c(begin = 0.1)+
  facet_wrap(~season)
#*******************************************************************************
# End Section ***
#*******************************************************************************




#*******************************************************************************
# Fit Logistic model + precip & vpd anom across hydro_years ---------------------------------------

fn_xmid <- function(dat){
  fit <- nls_multstart(ndvi_3mo ~ Asym/(1+exp((xmid-pe_12mo)/scal))+
                                  beta1*precip_anom_3mo+
                                  beta2*vpd15_anom_3mo,
                data = dat,
                iter = 100,
                start_lower = c(Asym=0.5, xmid=0, scal=0, beta1=-0.1 ,beta2=-0.1),
                start_upper = c(Asym=1, xmid=0.5, scal=1, beta1=0.1 ,beta2=0.1),
                supp_errors = 'Y',
                na.action = na.omit,
                lower= c(0.65,  0,   0, -0.1, -0.2),
                upper= c(1,    0.5, 2,  0.1,  0.2))
  bfit <- broom::tidy(fit)
  bfit$hydro_year <- unique(dat$hydro_year);
  bfit$season <- unique(dat$season)
  return(bfit)
}

vec_id <- tmp[!veg_class %in% c(6,10,12,13,30,31,32)] %>% 
  .[is.na(id)==F] %>% 
  .[mape <= 2] %>% 
  .[, sample(id, 5000)] 

fdat <- tmp[hydro_year %in% c(1982:2019)] %>% 
  .[id %in% vec_id] %>% 
  # .[season=="DJF"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_hyb>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  .[,`:=`(p_anom_12mo_frac = precip_anom_12mo/map)] %>% 
  .[p_anom_12mo_frac > -0.1 & p_anom_12mo_frac < 0.1] 
gc()
xdat <- fdat %>% 
  # .[sample(.N,50000)] %>%
  .[,fn_xmid(.SD),by=.(hydro_year,season)]
gc()

xdat %>% filter(hydro_year==2007)
xdat %>%
  inner_join(., {mlo %>% mutate(hydro_year=year(date)) %>% 
      group_by(hydro_year) %>% 
      summarize(co2 = mean(co2_trend,na.rm=TRUE))}) %>% 
  inner_join(., {fdat %>% group_by(hydro_year) %>% 
      summarize(pe_12mo = mean(pe_12mo,na.rm=TRUE)) %>% 
      ungroup()},by='hydro_year') %>% 
  as_tibble() %>%
  # filter(p.value < 0.1) %>%
  select(season,hydro_year,co2,pe_12mo, term,estimate) %>%
  spread(key = term, value=estimate) %>% 
  ggplot(data=., aes(co2, Asym))+
  geom_smooth(se=F,method='lm')+
  geom_label(aes(label=hydro_year,fill=pe_12mo),color='red')+
  scale_fill_viridis_c(begin = 0.1)+
  facet_wrap(~season)


d_anoms <- tmp[hydro_year %in% c(1982:2019)] %>% 
  .[id %in% vec_id] %>%
  .[season=="DJF"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_hyb>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  .[,.(precip_anom_3mo = mean(precip_anom_3mo, na.rm=TRUE), 
          vpd15_anom_3mo = mean(vpd15_anom_3mo, na.rm=TRUE)),
    by=hydro_year]

xdat %>%
  as_tibble() %>%
  # filter(p.value < 0.1) %>%
  select(season,hydro_year,term,estimate) %>%
  spread(key = term, value=estimate) %>% summary
  inner_join(., d_anoms, by='hydro_year') %>% 
  # [(log((A - R)/(2.0*A - 1.0)) + 0.693147180559945)*exp(-L)]
  # mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)) %>%
  mutate(p20 = scal*log((-5.0*beta1*precip_anom_3mo - 5.0*beta2*vpd15_anom_3mo + 1.0)*exp(xmid/scal)/(5.0*Asym + 5.0*beta1*precip_anom_3mo + 5.0*beta2*vpd15_anom_3mo - 1.0))
  ) %>% 
  mutate(p40 = scal*log((-5.0*beta1*precip_anom_3mo - 5.0*beta2*vpd15_anom_3mo + 2.0)*exp(xmid/scal)/(5.0*Asym + 5.0*beta1*precip_anom_3mo + 5.0*beta2*vpd15_anom_3mo - 2.0))
  ) %>%
  mutate(p60 = scal*log((-5.0*beta1*precip_anom_3mo - 5.0*beta2*vpd15_anom_3mo + 3.0)*exp(xmid/scal)/(5.0*Asym + 5.0*beta1*precip_anom_3mo + 5.0*beta2*vpd15_anom_3mo - 3.0))
  ) %>%
  # filter(!hydro_year %in% c(2012)) %>%
  # filter(hydro_year >= 2000) %>%
  ggplot(data=., aes(hydro_year, #Asym+Asym2+Asym3
                     scal))+
  geom_point()+
  geom_smooth(method='lm',color='black',se=F)
  # geom_label(aes(label=hydro_year))+
  scale_color_viridis_c(option='C',end=0.8)+
  scale_x_continuous(expand=c(0.01,0.01))+
  labs(x=NULL, y=expression(paste(NDVI~Asymptote)))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size=10), 
        legend.position = c(0.3,0.1), 
        legend.key.width = unit(1,'cm'),
        legend.direction = 'horizontal');p_asym

fn_logis <- function(x,Asym,xmid,scal) {
  # input <- seq(0.01,2,length.out = 100)
  Asym/(1+exp((xmid-x)/scal))}
wdat <- xdat %>%
  as_tibble() %>%
  # filter(p.value < 0.1) %>%
  select(season,hydro_year,term,estimate) %>%
  spread(key = term, value=estimate)


p_logistic <- expand_grid(wdat %>% select(-season), 
            x=seq(0.01,2,length.out = 100)) %>% 
  rowwise() %>% 
  mutate(p1 = fn_logis(x,Asym,xmid,scal)) %>% 
  mutate(pred = p1) %>% 
  # mutate(pred = fn_logis(x,Asym = max(c(Asym,Asym2,Asym3)),
  #                        xmid = (xmid+xmid2+xmid3)/3 ,
  #                        scal = (scal+scal2+scal3)/3 )) %>% 
  ggplot(data=., aes(x,pred,color=(hydro_year), group=hydro_year))+
  geom_line(alpha=0.5)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c("Year", option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2), expand=c(0,0))+
  scale_y_continuous(limits=c(0,1), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size=10), 
        legend.position = c(0.7,0.1), 
        legend.key.width = unit(1,'cm'),
        legend.direction = 'horizontal'); p_logistic

p_out <- p_logistic+p_asym+patchwork::plot_layout(ncol=2)

ggsave(p_out, filename = "figures/ndvi3mo_PE12mo_logistic_Asym_xmid.png", 
       dpi=300, type='cairo', 
       width = 30, height=20, units='cm')



xdat %>%
  as_tibble() %>%
  # filter(p.value < 0.1) %>%
  select(season,hydro_year,term,estimate) %>%
  spread(key = term, value=estimate) %>% #summary
  # mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)) %>%
  # filter(!hydro_year %in% c(2012)) %>%
  # filter(hydro_year >= 2000) %>%
  ggplot(data=., aes(hydro_year, #Asym+Asym2+Asym3
                     Asym, color=xmid))+
  geom_point()+
  geom_smooth(method='lm',color='black',se=F)+
  # geom_label(aes(label=hydro_year))+
  scale_color_viridis_c(option='C',end=0.8)+
  scale_x_continuous(expand=c(0.01,0.01))+
  labs(x=NULL, y=expression(paste(NDVI~Asymptote)))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size=10), 
        legend.position = c(0.3,0.1), 
        legend.key.width = unit(1,'cm'),
        legend.direction = 'horizontal');p_asym





# Multiple Time Lag P:PET w 3 Logistic Functions -------------------------------------------------
fn_logistic3 <- function(dat){
  fit <- nls_multstart(ndvi_3mo ~
                         Asym/(1+exp((xmid-pe_3mo)/scal))+
                         Asym2/(1+exp((xmid2-pe_12mo)/scal2))+
                         Asym3/(1+exp((xmid3-pe_36mo)/scal3)),
                       data = dat,
                       iter = 50,
                       start_lower = c(Asym=0,  xmid=0, scal=0.1,
                                       Asym2=0, xmid2=0, scal2=0.1,
                                       Asym3=0, xmid3=0, scal3=0.1),
                       start_upper = c(Asym=0.5,  xmid=0.5, scal=0.5,
                                       Asym2=0.5, xmid2=0.5, scal2=0.5,
                                       Asym3=0.5, xmid3=0.5, scal3=0.5),
                       supp_errors = 'Y',
                       na.action = na.omit,
                       #      Asym     xmid    Scal
                       lower= c(0,     0.01,   0.01, 
                                0,     0.05,   0.01,
                                0,     0.05,    0.01),
                       upper= c(1,    0.6,    0.1,
                                1,    1,    0.35,
                                1,    0.6,    0.4))
  bfit <- broom::tidy(fit)
  bfit$hydro_year <- unique(dat$hydro_year);
  bfit$season <- unique(dat$season)
  gof <- dat %>%
    lazy_dt() %>%
    mutate(pred = predict(fit,newdata=.)) %>%
    mutate(res = pred-ndvi_3mo) %>%
    select(pred,res,ndvi_3mo) %>%
    na.omit() %>%
    summarize(rmse = sqrt(mean((pred-ndvi_3mo)**2)),
              r2 = cor(pred,ndvi_3mo)**2) %>%
    as.data.table()
  bfit$rmse <- gof$rmse
  bfit$r2 <- gof$r2

  return(bfit)
}

vec_id <- tmp[!veg_class %in% c(6,10,12,13,30,31,32)] %>% 
  .[is.na(id)==F] %>% 
  .[mape <= 2] %>% 
  .[, sample(id, 2000)] 

xdat <- tmp[hydro_year %in% c(1984:2019)] %>% 
  .[id %in% vec_id] %>% 
  .[season=="DJF"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_hyb>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  .[,`:=`(p_anom_12mo_frac = precip_anom_12mo/map)] %>% 
  .[p_anom_12mo_frac > -0.15 & p_anom_12mo_frac < 0.15] %>% 
  # .[sample(.N,50000)] %>%
  .[,fn_logistic3(.SD),by=.(hydro_year)]
gc()

xdat %>%
  as_tibble() %>%
  # filter(p.value < 0.1) %>%
  select(season,hydro_year,term,estimate) %>%
  spread(key = term, value=estimate) %>% #summary
  # mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)) %>%
  # filter(!hydro_year %in% c(2012)) %>%
  # filter(hydro_year >= 2000) %>%
  ggplot(data=., aes(hydro_year, Asym+Asym2+Asym3))+
  geom_point()+
  geom_smooth(method='lm')+
  geom_label(aes(label=hydro_year))

# xdat %>% 
#   as_tibble() %>% 
#   # filter(p.value < 0.1) %>% 
#   select(season,hydro_year,term,estimate) %>% 
#   spread(key = term, value=estimate) %>% 
#   mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)) %>% 
#   # filter(!hydro_year %in% c(2012)) %>%
#   # filter(hydro_year >= 2000) %>%
#   ggplot(data=., aes(hydro_year, Asym))+
#   geom_point()+
#   geom_smooth(method='lm')+
#   geom_label(aes(label=hydro_year))

xdat %>% 
  filter(term=='Asym') %>% 
  ggplot(data=., aes(hydro_year, r2))+geom_point()+geom_label(aes(label=hydro_year))

# plot hydro year Asymp curves 
# fn_asym <- function(x,Asym,R0,lrc) {
#   # input <- seq(0.01,2,length.out = 100)
#   Asym+(R0-Asym)*exp(-exp(lrc)*x)}
# wdat <- xdat %>% 
#   as_tibble() %>% 
#   # filter(p.value < 0.1) %>% 
#   select(season,hydro_year,term,estimate) %>% 
#   spread(key = term, value=estimate)

# plot hydro year Logistic curves 
fn_logis <- function(x,Asym,xmid,scal) {
  # input <- seq(0.01,2,length.out = 100)
  Asym/(1+exp((xmid-x)/scal))}
wdat <- xdat %>%
  as_tibble() %>%
  # filter(p.value < 0.1) %>%
  select(season,hydro_year,term,estimate) %>%
  spread(key = term, value=estimate)


expand_grid(wdat %>% select(-season), 
            x=seq(0.01,2,length.out = 100)) %>% 
  rowwise() %>% 
  mutate(p1 = fn_logis(x,Asym,xmid,scal), 
         p2 = fn_logis(x,Asym2,xmid2,scal2), 
         p3 = fn_logis(x,Asym3,xmid3,scal3)) %>% 
  mutate(pred = p1+p2+p3) %>% 
  # mutate(pred = fn_logis(x,Asym = max(c(Asym,Asym2,Asym3)),
  #                        xmid = (xmid+xmid2+xmid3)/3 ,
  #                        scal = (scal+scal2+scal3)/3 )) %>% 
  ggplot(data=., aes(x,pred,color=(hydro_year), group=hydro_year))+
  geom_line(alpha=0.5)+
  geom_hline(aes(yintercept=0.7),color='black',lwd=0.5)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c("Year", option='B',end=0.9)+
  scale_x_continuous(limits=c(0,2), expand=c(0,0))+
  scale_y_continuous(limits=c(0.25,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size=10))+
  facet_wrap(~hydro_year)
ggsave("figures/ndvi3mo_PE12mo_multiYear_logistic.png", 
       dpi=500, type='cairo', width = 15, height=10, units='cm')

wdat %>% 
  # mutate(x=seq(-500,500,length.out = 100)) %>% 
  ggplot(data=., aes(beta1,beta2,color=hydro_year))+
  geom_point()+
  scale_color_viridis_c()
    geom_abline(aes(intercept=0,slope=beta1,color=hydro_year))+
  scale_y_continuous(limits=c(-0.01,0.01))
  
  
  
fn <- function(Asym, R0, lrc, hydro_year) stat_function(fun=function(x)Asym+(R0-Asym)*exp(-exp(lrc)*x), 
                                            aes(color=as_factor(hydro_year)))
preds <- mapply(fn, wdat$Asym, wdat$R0, wdat$lrc, wdat$hydro_year)
tmp[hydro_year>=1983][sample(.N, 10000)] %>% 
  ggplot(data=., 
    aes(precip_12mo/pet_12mo, ndvi_3mo, color=as_factor(hydro_year)))+
  preds+
  scale_color_viridis_d()



draw_fn <- function(d){
  stat_function(fun=function(x)Asym+(R0-Asym)*exp(-exp(lrc)*x), 
                color='black',
                # aes(group=as_factor(d$hydro_year)), 
                args=list(Asym=d$Asym, R0=d$R0, lrc=d$lrc))
}

draw_fn(wdat)


mapply(stat_function(fun=Asym+(R0-Asym)*exp(-exp(lrc)*x), 
                     aes(color=hydro_year), 
              args=list(Asym=Asym, R0=R0, lrc=lrc))
       wdat$As)

tibble(x = seq(0.1,2,length.out = 100)) %>% 
  ggplot(data=., ()) +
  draw_fn(wdat)
  
tibble(x = seq(0.1,2,length.out = 100)) %>% 
  ggplot(data=., aes(color=hydro_year))+
  geom_function(data=wdat, 
                fun=fn_asym, 
                aes(color=hydro_year), 
                args=list(Asym=wdat$Asym, R0=wdat$R0, lrc=wdat$lrc))






# Debug poor fits
nls_multstart(ndvi_3mo ~ SSasymp(input = precip_12mo/pet_12mo, Asym, R0,lrc),
              data = tmp[hydro_year %in% c(2011)] %>% 
                # .[season=="DJF"] %>%
                .[season=="DJF"] %>%
                .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
                .[ndvi_hyb>0] %>%
                .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
                .[is.na(veg_class)==F] %>% 
                .[sample(.N,20000)] ,
              iter = 10,
              start_lower = c(Asym=0.5, R0=0, lrc=0),
              start_upper = c(Asym=1, R0=0.25, lrc=1),
              # supp_errors = 'Y',
              na.action = na.omit, 
              lower= c(0.5, -0.1, -1)) %>% 
  summary()


summary(fit12)

tmp[hydro_year %in% c(1982:1989)] %>% 
  .[season %in% c("DJF")] %>%
  .[ndvi_hyb>0] %>%
  .[ndvi_anom_sd >= -3 & ndvi_anom_sd <= 3] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  .[pe_3mo <= 2] %>% 
  .[sample(.N,10000)] %>%
  .[,`:=`(Tk=tmax+273.15)] %>% 
  ggplot(data=., aes(precip_12mo/pet_12mo, ndvi_3mo,color=vpd15_anom))+
  geom_point(alpha=0.25)+
  geom_smooth(method='nls', se=F, color='black',          #
              formula = y~SSlogis(input = x,Asym,xmid,scal),
              method.args=list(
                start=list("Asym"=coef(fit12)["Asym"],
                           "xmid"=coef(fit12)["xmid"],
                           "scal"=coef(fit12)["scal"])))+
  scale_color_gradient2(expression(paste(Tmax~(degree*C))), 
                        mid='gray70')+
  labs(x=expression(paste(Precip["12 mo"]*":"*PET["12 mo"])), 
       y=expression(paste(NDVI[" 3 mo"])))+
  scale_x_continuous(limits=c(0,1.5), expand=c(0,0))+
  scale_y_continuous(limits=c(0,1), expand=c(0,0))+
  theme_linedraw()+
  theme(legend.position = 'bottom')+
  geom_smooth(method='nls', se=F, color='red',          #
              formula = y~SSlogis(input = x,Asym,xmid,scal),
              method.args=list(
                start=list("Asym"=coef(fit12)["Asym"],
                           "xmid"=coef(fit12)["xmid"],
                           "scal"=coef(fit12)["scal"])), 
              inherit.aes = F, 
              aes(precip_12mo/pet_12mo, ndvi_3mo,color=tmax_anom_3mo), 
              data=tmp[hydro_year %in% c(2014:2016)] %>% 
                .[season %in% c("DJF")] %>%
                .[ndvi_hyb>0] %>%
                .[ndvi_anom_sd >= -3 & ndvi_anom_sd <= 3] %>%
                .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
                .[is.na(veg_class)==F] %>% 
                .[pe_3mo <= 2] %>% 
                .[sample(.N,10000)])
  # geom_smooth(color='blue')+
  # geom_smooth(color='purple', 
  #             data=tmp[hydro_year %in% c(2014:2016)] %>% 
  #               .[season %in% c("DJF")] %>%
  #               .[ndvi_hyb>0] %>%
  #               .[ndvi_anom_sd >= -3 & ndvi_anom_sd <= 3] %>%
  #               .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  #               .[is.na(veg_class)==F] %>% 
  #               .[pe_3mo <= 2] %>% 
  #               .[sample(.N,10000)])
  


  
  # geom_smooth(formula=y~s(x,k=4), se=F,
  #             method='gam',
  #             method.args=list(select=TRUE 
  #                              # discrete=TRUE, 
  #                              # method='fREML'
  #             ))+
  # scale_x_continuous(limits=c(25,35))+
  # scale_y_continuous(limits=c(0.25,0.75))+
  # scale_color_viridis_d()
  # facet_wrap(~vc)


  
  
  
  
  
  
  

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

dat <- d3[hydro_year %in% c(2009)] %>% 
  # .[season=="DJF"] %>%
  .[season=="DJF"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_hyb>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F]


how_dat <- d3[hydro_year %in% c(1985:2019)] %>% 
  # .[season=="DJF"] %>%
  .[season=="DJF"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_hyb>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  # .[sample(.N,50000)] %>%
  .[,fn_bam(.SD),by=.(hydro_year)]
how_dat

how_dat %>% ggplot(data=., aes(hydro_year,rmse_out))+geom_point()+
  geom_point(aes(hydro_year,rmse_in),color='red')

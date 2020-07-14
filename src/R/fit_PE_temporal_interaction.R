library(brms)
library(tidyverse)
library(data.table); setDTthreads(threads = 8)
library(lubridate); 
library(dtplyr); 
options(mc.cores=parallel::detectCores()-3) 

# IMPORT ###################################################################
source("src/R/extract_awap_rad.R")
vi <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet", 
                          col_select = c("x","y","date",
                                         "ndvi_c","ndvi_m","ndvi_hyb", 
                                         "evi2_hyb","evi2_m"))
tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id","season",
                             "precip",  
                             # "precip_anom",
                             # "precip_anom_3mo","precip_anom_36mo",
                             "precip_anom_12mo",
                             "map", 
                             "precip_12mo",
                             # "precip_36mo",
                             "tmax",
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
tmp <- tmp[order(x,y,date)][, rad_3mo := frollmean(rad,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmax_anom_3mo := frollmean(tmax_anom,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, rad_anom_3mo := frollmean(rad_anom,n = 3,fill = NA,align='center'), by=.(x,y)]

# tmp <- tmp[order(x,y,date)][, tmean_3mo := frollmean(tmean,n = 3,fill = NA,align='center'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, tmin_3mo := frollmean(tmin,n = 3,fill = NA,align='center'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, tmax_12mo := frollmean(tmax,n = 12,fill = NA,align='right'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, pet_3mo := frollmean(pet,n = 3,fill = NA,align='right'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, precip_3mo := frollmean(precip,n = 3,fill = NA,align='right'), by=.(x,y)]
# tmp <- tmp[order(x,y,date)][, vpd15_3mo := frollmean(vpd15,n = 3,fill = NA,align='right'), by=.(x,y)]
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

#*******************************************************************************
# Residuals of simple Richards 1981-1985 fit -----------------------------------
#*******************************************************************************
source("src/R/functions_nonlinear.R")
train_son <- tmp[date %between% c(ymd("1982-01-01"),ymd("1986-12-31"))] %>% 
  .[season=='SON'] %>% 
  .[pe_12mo <= 2] %>% 
  .[sample(.N,100000)]
train_djf <- tmp[date %between% c(ymd("1982-01-01"),ymd("1986-12-31"))] %>% 
  .[season=='DJF'] %>% 
  .[pe_12mo <= 2] %>% 
  .[sample(.N,100000)]
train_mam <- tmp[date %between% c(ymd("1982-01-01"),ymd("1986-12-31"))] %>% 
  .[season=='MAM'] %>% 
  .[pe_12mo <= 2] %>% 
  .[sample(.N,100000)]
train_jja <- tmp[date %between% c(ymd("1982-01-01"),ymd("1986-12-31"))] %>% 
  .[season=='JJA'] %>% 
  .[pe_12mo <= 2] %>% 
  .[sample(.N,100000)]

ric_son <- nls.multstart::nls_multstart(ndvi_3mo ~ SSRichards(input = pe_12mo, 
                                                              Asym=Asym, 
                                                              xmid=xmid,
                                                              scal=scal,
                                                              lpow=lpow),
                                        data=train_son, 
                                        iter = 10,
                                        control=nls.control(maxiter=1000),
                                        start_lower = c(Asym=0.7561,
                                                        xmid=0.25,
                                                        scal=0.28,
                                                        lpow=-0.2), 
                                        start_upper = c(Asym=0.7561,
                                                        xmid=0.5,
                                                        scal=0.38,
                                                        lpow=0.9)) 
summary(ric_son)
yp <- predict(ric_son, newdata=tibble(pe_12mo=seq(0.1,2,length.out = 100)))[1:100]
xp <- seq(0.1,2,length.out=100)
tibble(xp,yp) %>% ggplot(data=., aes(xp,yp))+geom_line()+
  scale_x_continuous(limits=c(0,2))

ric_djf <- nls.multstart::nls_multstart(ndvi_3mo ~ SSRichards(input = pe_12mo, 
                                                              Asym=Asym, 
                                                              xmid=xmid,
                                                              scal=scal,
                                                              lpow=lpow),
                                        data=train_djf, 
                                        iter = 10,
                                        control=nls.control(maxiter=1000),
                                        start_lower = c(Asym=0.7561,
                                                        xmid=0.25,
                                                        scal=0.28,
                                                        lpow=-0.2), 
                                        start_upper = c(Asym=0.7561,
                                                        xmid=0.5,
                                                        scal=0.38,
                                                        lpow=0.9)) 
summary(ric_djf)
yp <- predict(ric_djf, newdata=tibble(pe_12mo=seq(0.1,2,length.out = 100)))[1:100]
xp <- seq(0.1,2,length.out=100)
tibble(xp,yp) %>% ggplot(data=., aes(xp,yp))+geom_line()+
  scale_x_continuous(limits=c(0,2))


ric_mam <- nls.multstart::nls_multstart(ndvi_3mo ~ SSRichards(input = pe_12mo, 
                                     Asym=Asym, 
                                     xmid=xmid,
                                     scal=scal,
                                     lpow=lpow),
               data=train_mam, 
               iter = 10,
               control=nls.control(maxiter=1000),
               start_lower = c(Asym=0.7561,
                         xmid=0.25,
                         scal=0.28,
                         lpow=-0.2), 
               start_upper = c(Asym=0.7561,
                               xmid=0.5,
                               scal=0.38,
                               lpow=0.9)) 
summary(ric_mam)

yp <- predict(ric_mam, newdata=tibble(pe_12mo=seq(0.1,2,length.out = 100)))[1:100]
xp <- seq(0.1,2,length.out=100)
tibble(xp,yp) %>% ggplot(data=., aes(xp,yp))+geom_line()+
  scale_x_continuous(limits=c(0,2))


ric_jja <- nls.multstart::nls_multstart(ndvi_3mo ~ SSRichards(input = pe_12mo, 
                                                              Asym=Asym, 
                                                              xmid=xmid,
                                                              scal=scal,
                                                              lpow=lpow),
                                        data=train_jja, 
                                        iter = 10,
                                        control=nls.control(maxiter=1000),
                                        start_lower = c(Asym=0.7561,
                                                        xmid=0.25,
                                                        scal=0.28,
                                                        lpow=-0.2), 
                                        start_upper = c(Asym=0.7561,
                                                        xmid=0.5,
                                                        scal=0.38,
                                                        lpow=0.9)) 
summary(ric_jja)

yp <- predict(ric_jja, newdata=tibble(pe_12mo=seq(0.1,2,length.out = 100)))[1:100]
xp <- seq(0.1,2,length.out=100)
tibble(xp,yp) %>% ggplot(data=., aes(xp,yp))+geom_line()+
  scale_x_continuous(limits=c(0,2))

gc()
o <- tmp[,.(x,y,date,season,hydro_year,ndvi_3mo, pe_12mo,cco2,co2_int)]
o1 <- o[season=="SON"]
o2 <- o[season=="DJF"]
o3 <- o[season=="MAM"]
o4 <- o[season=="JJA"]

v1 <- stats::predict(ric_son, newdata=o1,type='response')
v2 <- stats::predict(ric_djf, newdata=o2,type='response')
v3 <- stats::predict(ric_mam, newdata=o3,type='response')
v4 <- stats::predict(ric_jja, newdata=o4,type='response')

o1$pred_ndvi <- v1[1:length(v1)]
o2$pred_ndvi <- v2[1:length(v2)]
o3$pred_ndvi <- v3[1:length(v3)]
o4$pred_ndvi <- v4[1:length(v4)]
o <- rbindlist(list(o1,o2,o3,o4))
rm(o1,o2,o3,o4);gc()

o[sample(.N, 1000)] %>% 
  ggplot(data=., aes(pred_ndvi, ndvi_3mo))+
  geom_point()+
  geom_smooth(method='lm')+
  geom_abline(aes(intercept=0,slope=1),color="#990000",lwd=1)

o[is.na(pred_ndvi)==F][sample(.N, 10000)] %>% 
  mutate(res_ndvi = pred_ndvi - ndvi_3mo) %>% 
  ggplot(data=., aes(x=res_ndvi,hydro_year,
                     group=hydro_year,fill=stat(x)))+
  ggridges::geom_density_ridges(rel_min_height=0.01)+
  scale_fill_viridis_c()+
  facet_wrap(~season)

library(ggridges)
vec_cols <- RColorBrewer::brewer.pal(n=5, "BrBG")
p_out <- o[is.na(pred_ndvi)==F][is.na(ndvi_3mo)==F][sample(.N, 1000000)] %>% 
  mutate(res_ndvi = pred_ndvi - ndvi_3mo) %>% 
  mutate(season = factor(season, levels=c("SON","DJF","MAM","JJA"))) %>% 
 ggplot(data=., 
       aes(x = res_ndvi, y = as_factor(hydro_year), 
           fill = factor(stat(quantile)))) +
  stat_density_ridges( geom = "density_ridges_gradient", 
                       scale = 1.5, 
                      rel_min_height = 0.05, 
                      calc_edf=TRUE,
                      quantiles=10,
                      quantile_lines = TRUE) +
  geom_vline(aes(xintercept=0),color='red',lwd=1)+
  geom_hline(aes(yintercept=as_factor(1987)),color='black',lty=3,lwd=1)+
  scale_fill_brewer(name = "Deciles", 
                    palette = "BrBG", direction = -1) +
  labs(x=expression(paste(NDVI["simple P:PET pred"] - NDVI[obs])), 
       y=NULL)+
  scale_x_continuous(limits=c(-0.3,0.3), expand=c(0,0))+
  facet_wrap(~season,ncol = 4)+
  theme_linedraw()+
  theme(strip.text = element_text(face='bold'))
ggsave(filename = "figures/ndvi_residuals_simplePPETmod1982_1986_ggridges.png", 
       height=15, width=20, units='cm', dpi=350, type='cairo')
 
#*******************************************************************************
# NLS fits Richard func by Season  --------------------------------------------------
#*******************************************************************************
train_son <- tmp[season=='SON'][pe_12mo <= 2] %>% 
  # .[p_anom_12mo_frac > -0.75 & p_anom_12mo_frac < 0.75] %>%
  .[sample(.N,1000000)]
train_djf <- tmp[season=='DJF'][pe_12mo <= 2] %>% 
  # .[p_anom_12mo_frac > -0.75 & p_anom_12mo_frac < 0.75] %>%
  .[sample(.N,1000000)]
train_mam <- tmp[season=='MAM'][pe_12mo <= 2] %>% 
  # .[p_anom_12mo_frac > -0.75 & p_anom_12mo_frac < 0.75] %>%
  .[sample(.N,1000000)]
train_jja <- tmp[season=='JJA'][pe_12mo <= 2] %>% 
  # .[p_anom_12mo_frac > -0.75 & p_anom_12mo_frac < 0.75] %>%
  .[sample(.N,1000000)]


ric_x2_son <- nls(ndvi_3mo ~ 
                    mod_grad_ric_x2(x = pe_12mo,x2=cco2, 
                                    Asym=Asym,Asym2=Asym2, 
                                    xmid=xmid, xmid2=xmid2,
                                    scal=scal,scal2=scal2,
                                    lpow=lpow, lpow2=lpow2),
                  data=train_son, 
                  control=nls.control(maxiter=1000),
                  start = c(Asym=0.7561,Asym2=5e-4,
                            xmid=0.27,xmid2=3e-4,
                            scal=0.28,scal2=0,
                            lpow=-0.28,lpow2=0), 
                  lower=c(0.5, -0.01, 
                          -0.5, -0.01, 
                          0.15, -0.001, 
                          -0.9, -0.01), 
                  algorithm = 'port')
summary(ric_x2_son)

ric_x2_djf <- nls(ndvi_3mo ~ 
                    mod_grad_ric_x2(x = pe_12mo,x2=cco2, 
                                    Asym=Asym,Asym2=Asym2, 
                                    xmid=xmid, xmid2=xmid2,
                                    scal=scal,scal2=scal2,
                                    lpow=lpow, lpow2=lpow2),
                  data=train_djf, 
                  control=nls.control(maxiter=1000),
                  start = c(Asym=0.7561,Asym2=5e-4,
                            xmid=0.27,xmid2=3e-4,
                            scal=0.28,scal2=0,
                            lpow=-0.28,lpow2=0), 
                  lower=c(0.5, -0.01, 
                          -0.5, -0.01, 
                          0.15, -0.001, 
                          -0.9, -0.01), 
                  algorithm = 'port')
summary(ric_x2_djf)

ric_x2_mam <- nls(ndvi_3mo ~ 
                    mod_grad_ric_x2(x = pe_12mo,x2=cco2, 
                                    Asym=Asym,Asym2=Asym2, 
                                    xmid=xmid, xmid2=xmid2,
                                    scal=scal,scal2=scal2,
                                    lpow=lpow, lpow2=lpow2),
                  data=train_mam, 
                  control=nls.control(maxiter=1000),
                  start = c(Asym=0.7561,Asym2=5e-4,
                            xmid=0.27,xmid2=3e-4,
                            scal=0.28,scal2=0,
                            lpow=-0.28,lpow2=0), 
                  lower=c(0.5, -0.01, 
                          -0.5, -0.01, 
                          0.15, -0.001, 
                          -3, -0.01), 
                  algorithm = 'port')
summary(ric_x2_mam)

ric_x2_jja <- nls(ndvi_3mo ~ 
                    mod_grad_ric_x2(x = pe_12mo,x2=cco2, 
                                    Asym=Asym,Asym2=Asym2, 
                                    xmid=xmid, xmid2=xmid2,
                                    scal=scal,scal2=scal2,
                                    lpow=lpow, lpow2=lpow2),
                  data=train_jja, 
                  control=nls.control(maxiter=1000),
                  start = c(Asym=0.7561,Asym2=5e-4,
                            xmid=0.27,xmid2=3e-4,
                            scal=0.28,scal2=0,
                            lpow=-0.28,lpow2=0), 
                  lower=c(0.5, -0.01, 
                          -0.5, -0.01, 
                          0.15, -0.001, 
                          -0.9, -0.01), 
                  algorithm = 'port')
summary(ric_x2_jja)



pred_son <- expand_grid(#season=unique(tmp$season), 
  co2 = seq(min(tmp$co2_int),max(tmp$co2_int),length.out=50),
  pe_12mo = seq(0.01,2.05,length.out = 200)) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(ric_x2_son, newdata=.), 
         season=train_son$season[1])
pred_djf <- expand_grid(#season=unique(tmp$season), 
  co2 = seq(min(tmp$co2_int),max(tmp$co2_int),length.out=50),
  pe_12mo = seq(0.01,2.05,length.out = 200)) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(ric_x2_djf, newdata=.), 
         season = train_djf$season[1])
pred_mam <- expand_grid(#season=unique(tmp$season), 
  co2 = seq(min(tmp$co2_int),max(tmp$co2_int),length.out=50),
  pe_12mo = seq(0.01,2.05,length.out = 200)) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(ric_x2_mam, newdata=.), 
         season = train_mam$season[1])
pred_jja <- expand_grid(#season=unique(tmp$season), 
  co2 = seq(min(tmp$co2_int),max(tmp$co2_int),length.out=50),
  pe_12mo = seq(0.01,2.05,length.out = 200)) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(ric_x2_jja, newdata=.), 
         season=train_jja$season[1])

bind_rows(pred_son, pred_djf, pred_mam, pred_jja) %>% 
  ggplot(data=., aes(pe_12mo,pred,color=(co2), group=co2))+
  geom_line(alpha=1)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2.05),
                     breaks=c(0,0.5,1,1.5,2),
                     labels = c(0,0.5,1,1.5,2),
                     expand=c(0,0)
                     # guide = guide_axis(n.dodge=1, angle=0,check.overlap = TRUE)
                     )+
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  facet_wrap(~season,nrow=2)+
  theme_linedraw()+
  guides(color=guide_colorbar(title.position = 'top'))+
  theme(panel.grid = element_blank(),
        # panel.spacing.x = unit(6, "mm"),
        axis.text = element_text(size=10),
        # axis.text.x = element_text(angle=45, vjust=-0.5),
        legend.position = c(0.85,0.1), 
        legend.key.width = unit(0.65,'cm'),
        legend.direction = 'horizontal', 
        legend.background = element_rect(fill=NA))
ggsave("figures/ndvi3mo_PE12mo_richard_x2_nlsFit_bySeason.png", 
       width=15, height=12, units='cm', dpi=350, type='cairo')

# comparison with nonCO2 models
ric_son <- nls.multstart::nls_multstart(ndvi_3mo ~ SSRichards(input = pe_12mo, 
                                                              Asym=Asym, 
                                                              xmid=xmid,
                                                              scal=scal,
                                                              lpow=lpow),
                                        data=train_son, 
                                        iter = 10,
                                        control=nls.control(maxiter=1000),
                                        start_lower = c(Asym=0.7561,
                                                        xmid=0.25,
                                                        scal=0.28,
                                                        lpow=-0.2), 
                                        start_upper = c(Asym=0.7561,
                                                        xmid=0.5,
                                                        scal=0.38,
                                                        lpow=0.9)) 
summary(ric_son)
bbmle::AICtab(ric_son, ric_x2_son)

ric_djf <- nls.multstart::nls_multstart(ndvi_3mo ~ SSRichards(input = pe_12mo, 
                                                              Asym=Asym, 
                                                              xmid=xmid,
                                                              scal=scal,
                                                              lpow=lpow),
                                        data=train_djf, 
                                        iter = 10,
                                        control=nls.control(maxiter=1000),
                                        start_lower = c(Asym=0.7561,
                                                        xmid=0.25,
                                                        scal=0.28,
                                                        lpow=-0.2), 
                                        start_upper = c(Asym=0.7561,
                                                        xmid=0.5,
                                                        scal=0.38,
                                                        lpow=0.9)) 
summary(ric_djf)
bbmle::AICtab(ric_djf, ric_x2_djf)

ric_mam <- nls.multstart::nls_multstart(ndvi_3mo ~ SSRichards(input = pe_12mo, 
                                                              Asym=Asym, 
                                                              xmid=xmid,
                                                              scal=scal,
                                                              lpow=lpow),
                                        data=train_mam, 
                                        iter = 10,
                                        control=nls.control(maxiter=1000),
                                        start_lower = c(Asym=0.7561,
                                                        xmid=0.25,
                                                        scal=0.28,
                                                        lpow=-0.2), 
                                        start_upper = c(Asym=0.7561,
                                                        xmid=0.5,
                                                        scal=0.38,
                                                        lpow=0.9)) 
summary(ric_mam)
bbmle::AICtab(ric_mam, ric_x2_mam)


ric_jja <- nls.multstart::nls_multstart(ndvi_3mo ~ SSRichards(input = pe_12mo, 
                                                              Asym=Asym, 
                                                              xmid=xmid,
                                                              scal=scal,
                                                              lpow=lpow),
                                        data=train_jja, 
                                        iter = 10,
                                        control=nls.control(maxiter=1000),
                                        start_lower = c(Asym=0.7561,
                                                        xmid=0.25,
                                                        scal=0.28,
                                                        lpow=-0.2), 
                                        start_upper = c(Asym=0.7561,
                                                        xmid=0.5,
                                                        scal=0.38,
                                                        lpow=0.9)) 
bbmle::AICtab(ric_jja, ric_x2_jja)


fn_rmse <- function(pred, obs){
  sqrt(mean((pred-obs)**2))
}
tibble(season = c("SON","DJF","MAM","JJA"), 
       `rmse P:PET` =       c(fn_rmse(predict(ric_son), train_son$ndvi_3mo), 
                              fn_rmse(predict(ric_djf), train_djf$ndvi_3mo), 
                              fn_rmse(predict(ric_mam), train_mam$ndvi_3mo), 
                              fn_rmse(predict(ric_jja), train_jja$ndvi_3mo)),
       `rmse P:PET x CO2` = c(fn_rmse(predict(ric_x2_son), train_son$ndvi_3mo), 
                              fn_rmse(predict(ric_x2_djf), train_djf$ndvi_3mo), 
                              fn_rmse(predict(ric_x2_mam), train_mam$ndvi_3mo), 
                              fn_rmse(predict(ric_x2_jja), train_jja$ndvi_3mo)), 
       `AIC P:PET` = c(AIC(ric_son), 
                       AIC(ric_djf), 
                       AIC(ric_mam), 
                       AIC(ric_jja)), 
       `AIC P:PET x CO2` = c(AIC(ric_x2_son), 
                            AIC(ric_x2_djf), 
                            AIC(ric_x2_mam), 
                            AIC(ric_x2_jja))) %>% 
  knitr::kable()


fn_rmse(predict(ric_x2_jja), train_jja$ndvi_3mo)
fn_rmse(predict(ric_jja), train_jja$ndvi_3mo)

#*******************************************************************************
# END SECTION ***
#*******************************************************************************



logis_co2 <- function(pe_12mo, Asym, Asym2, xmid,xmid2, scal, scal2,cco2){
  (Asym+Asym2*cco2)/(1+exp(((xmid+xmid2*cco2)-pe_12mo)/(scal+scal2*cco2)))
}
model_grad_co2 <- deriv(
  body(logis_co2)[[2]], 
  namevec = c("Asym","Asym2","xmid","xmid2","scal","scal2"), 
  function.arg = logis_co2
)
center_co2 <- mean(tmp[season=='SON']$co2_int,na.rm=TRUE)
tmp <- tmp[,`:=`(cco2 = co2_int - center_co2)]
j1 <- nls(ndvi_3mo ~ model_grad_co2(pe_12mo, Asym, Asym2, 
                                    xmid,xmid2,scal,scal2,cco2=cco2),
          data=tmp[season=='SON'], 
          start = c(Asym = 0.6, Asym2 = 0.1, xmid=0.2,xmid2=0, scal=0.2,scal2=0))

pred_dat <- expand_grid(season=unique(tmp$season), 
                        co2 = mlo$co2_int,
                        pe_12mo = seq(0.01,2,length.out = 100)) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(j1, newdata=.))

pred_dat %>% 
  ggplot(data=., aes(pe_12mo,pred,color=(co2), group=co2))+
  geom_line(alpha=0.5)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2), expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size=10), 
        legend.position = c(0.7,0.1), 
        legend.key.width = unit(1,'cm'),
        legend.direction = 'horizontal')

weibull_type1 <- function(x,x2, B,B2,C,C2,D,D2){
  (C+C2*x2) + ((D+D2*x2) - (C+C2*x2)) * 
    exp(-exp(-(B+B2*x2)*(log(x)-log(2.718282))))
}
w1_grad <- deriv(
  body(weibull_type1)[[2]], 
  namevec = c("B","B2", "C","C2", "D","D2"), 
  function.arg = weibull_type1
)
w1_son <- nls(ndvi_3mo ~ w1_grad(x=pe_12mo,x2=cco2,B,B2,C,C2,D,D2),
          data=tmp[season=='SON'] %>% 
            .[p_anom_12mo_frac > -0.25 & p_anom_12mo_frac < 0.25], 
          start = c(B=0.4,B2=0,C=0.1,C2=0,D=2,D2=0))
yardstick::rmse_vec(test_dat$ndvi_3mo, predict(w1_son,newdata=test_dat))

bbmle::AICtab(j1,w1_son)
summary(w1_son)

pred_w1_son <- expand_grid(season=unique(tmp$season), 
                        co2 = mlo$co2_int,
                        pe_12mo = seq(0.01,2,length.out = 100)) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(w1_son, newdata=.))

pred_w1_son %>% 
  ggplot(data=., aes(pe_12mo,pred,color=(co2), group=co2))+
  geom_line(alpha=0.5)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2), expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size=10), 
        legend.position = c(0.7,0.1), 
        legend.key.width = unit(1,'cm'),
        legend.direction = 'horizontal')

holling3 <- function(x,A,B,C) ((A)*x^2)/((B)**2 + x**2) + C
mod_grad_holling3 <- deriv(
  body(holling3)[[2]], 
  namevec = c("A","B","C"), 
  function.arg = holling3
)
holling3_x2 <- function(x,x2,A,A2,B,B2) ((A+A2*x2)*x^2)/((B+B2*x2)**2 + x**2)
mod_grad_holling3_x2 <- deriv(
  body(holling3_x2)[[2]], 
  namevec = c("A","A2","B","B2"), 
  function.arg = holling3
)
h3_son <- nls(ndvi_3mo ~ mod_grad_holling3(x=pe_12mo,A,B,C),
              data=tmp[season=='SON'][pe_12mo <= 2] %>% 
                .[p_anom_12mo_frac > -0.25 & p_anom_12mo_frac < 0.25], 
              start = c(A=0.5,B=0.1,C=0))
h3_son <- nls.multstart::nls_multstart(ndvi_3mo ~ 
                                         (A*x^2)/(B**2 + x**2) + C,
                       data=tmp[season=='SON'][sample(.N,1000)] %>% 
                         .[p_anom_12mo_frac > -0.25 & p_anom_12mo_frac < 0.25],
                       iter=1000,
                       supp_errors = 'Y',
                       start_lower = c(A=0.5,B=0.1,C=0), 
                       start_upper = c(A=1,B=0.5,C=0.3))
h3_son
h3_son <- nls.multstart::nls_multstart(ndvi_3mo ~ 
                  mod_grad_holling3(x=pe_12mo,x2=cco2,A,A2,B,B2),
              data=tmp[season=='SON'][sample(.N,1000)] %>% 
                .[p_anom_12mo_frac > -0.25 & p_anom_12mo_frac < 0.25],
              iter=1000,
              supp_errors = 'Y',
              start_lower = c(A=0.5,A2=-0.01,B=0.1,B2=-0.1), 
              start_upper = c(A=1,A2=0.01,B=0.5,B2=0.1))
h3_son
curve(holling3(x,A=143,B=1237,C=-1.498),0,2)
points(ndvi_3mo~pe_12mo,data= tmp[season=='SON'][sample(.N,1000)])


source("src/R/functions_nonlinear.R")
ric_son <- nls.multstart::nls_multstart(
  ndvi_3mo ~ SSRichards(input = pe_12mo, Asym, xmid, scal,lpow),
               data=tmp[season=='SON'][pe_12mo <= 2] %>% 
                 .[p_anom_12mo_frac > -0.25 & p_anom_12mo_frac < 0.25] %>% 
                 .[sample(.N, 1000)], 
            iter=100,  supp_errors = 'Y',
               start_lower = c(Asym=0.5,xmid=0.15,scal=0.15,lpow=0.001), 
               start_upper = c(Asym=1,xmid=1,scal=0.5,lpow=0.1))
ric_son
ric_son <- nls(ndvi_3mo ~ SSRichards(input = pe_12mo, Asym, xmid, scal,lpow),
              data=tmp[season=='SON'][pe_12mo <= 2] %>% 
                .[p_anom_12mo_frac > -0.25 & p_anom_12mo_frac < 0.25], 
              start = c(Asym=0.75,xmid=0.25,scal=0.35,lpow=-0.3))
summary(ric_son)
predict(ric_son) %>% hist

ric_x2_son <- nls(ndvi_3mo ~ 
            mod_grad_ric_x2(x = pe_12mo,x2=cco2, 
                            Asym=Asym,Asym2=Asym2, 
                            xmid=xmid, xmid2=xmid2,
                            scal=scal,scal2=scal2,
                            lpow=lpow),
               data=tmp[season=='SON'][pe_12mo <= 2] %>% 
                 .[p_anom_12mo_frac > -0.25 & p_anom_12mo_frac < 0.25], 
            start = c(Asym=0.75,Asym2=5e-4,
                      xmid=0.27,xmid2=3e-4,
                      scal=0.28,scal2=0,
                      lpow=-0.28))
summary(ric_x2_son)


ric_x2 <- function(x,x2,Asym,Asym2,xmid,xmid2,scal,scal2,lpow,lpow2){
  (Asym+Asym*x2) * (1+exp(((xmid+xmid*x2) - x)/(scal+scal*x2)))^(-exp(-(lpow+lpow*x2)))
}
ric_x2_son <- nls(ndvi_3mo ~ ric_x2(x=pe_12mo,x2=cco2,
                                    Asym=Asym,Asym2=Asym2,
                                    xmid,xmid2,scal,scal2,
                                    lpow,lpow2),
               data=tmp[season=='SON'][pe_12mo <= 2] %>% 
                 .[p_anom_12mo_frac > -0.25 & p_anom_12mo_frac < 0.25], 
               start = c(Asym=0.75,Asym2=0,
                         xmid=0.27,xmid2=0,
                         scal=0.28,scal2=0,
                         lpow=-0.23,lpow2=0), 
               control=nls.control(maxiter=100, 
                                   tol = 1e-04), #!!!
               algorithm = 'plinear')

ric_x2_son <- nls.multstart::nls_multstart(
  ndvi_3mo ~ mod_grad_ric_x2(x = pe_12mo,x2=cco2, 
                             Asym=Asym,Asym2=Asym2, 
                             xmid=xmid, xmid2=xmid2,
                             scal=scal,scal2=scal2,
                             lpow=lpow),
                  data=tmp[season=='SON'][pe_12mo <= 2] %>% 
                .[p_anom_12mo_frac > -0.5 & p_anom_12mo_frac < 0.5], 
                iter=100, 
                supp_errors = 'Y',
          start_lower = c(Asym=0.75,Asym2=-0.001,
                            xmid=0.27,xmid2=0,
                            scal=0.28,scal2=0,
                            lpow=-0.23), 
          start_upper = c(Asym=0.8,Asym2=0.001,
                  xmid=0.25,xmid2=0.001,
                  scal=0.3,scal2=0.001,
                  lpow=-0.3))
ric_x2_son




# Stan fits the Richard  --------------------------------------------------
options(mc.cores=parallel::detectCores()-3) 
test_dat <- tmp[season=='SON'][pe_12mo <= 2] %>% 
  .[p_anom_12mo_frac > -0.15 & p_anom_12mo_frac < 0.15] %>%
  .[sample(.N,10000)]
bprior <- 
  prior(normal(0.77,0.1), nlpar = Asym, lb=0.60, ub=0.85) + #! need to specify lb=0 when using gamma prior
  prior(normal(0.0006,0.0001),nlpar=Asym2, lb=0,ub=0.01)+
  prior(normal(0.3216,0.05), nlpar = xmid, lb=0.15, ub=0.5)+    # VERY SENSITIVE 
  prior(normal(0.0004,0.0001),nlpar=xmid2, lb=-0.001,ub=0.001)+
  prior(normal(0.265,0.05), nlpar=scal, lb=0.15,ub=0.35)+
  prior(normal(0,0.001), nlpar=scal2, lb=-0.001,ub=0.001)+
  prior(normal(-0.13,0.1), nlpar = lpow, lb=-0.4,ub=0)+
  prior(normal(0,0.001),nlpar=lpow2, lb=-0.001,ub=0.001)
f <- bf(ndvi_3mo ~ 
        (Asym+Asym2*cco2) * (1+exp(((xmid+xmid2*cco2) - pe_12mo)/
                        (scal+scal2*cco2)))^(-exp(-(lpow+lpow2*cco2))),
        Asym + Asym2+ xmid + xmid2+ scal +scal2 + lpow + lpow2 ~ 1, 
        nl=TRUE)
make_stancode(f, prior=bprior, family=gaussian(),
              data=test_dat)
s_ric <- brm(f,
           data = test_dat, 
           prior = bprior, 
           # sample_prior = 'only'
           algorithm = 'meanfield'
           # control = list(adapt_delta=0.9995, 
           #                max_treedepth=13),
           # chains = 3,
           # iter = 250
)
s_ric
plot(s_ric)

s_ric <- update(s_ric, algorithm='meanfield',
                newdata=tmp[season=='SON'][pe_12mo <= 2] %>% 
                  .[p_anom_12mo_frac > -0.15 & p_anom_12mo_frac < 0.15] %>%
                  .[sample(.N,100000)])

vec_start <- fit3$fit %>% as.matrix %>% colMeans()

s_ric_mc <- update(s_ric, algorithm='sampling',
               newdata=tmp[season=='SON'][pe_12mo <= 2] %>%
                 .[p_anom_12mo_frac > -0.15 & p_anom_12mo_frac < 0.15] %>%
                 .[sample(.N,10000)],
               control=list(adapt_delta=0.995, 
                            max_treedepth=12), 
               chains=3, iter=1000)
print(s_ric_mc, digits=4)
plot(s_ric_mc)


fit3 <- update(s_ric, 
               newdata=tmp[season=='SON'][pe_12mo <= 2] %>%
                 .[p_anom_12mo_frac > -0.15 & p_anom_12mo_frac < 0.15] %>%
                 .[sample(.N,100000)],
              algorithm='meanfield', 
              control=list(tol_rel_obj=0.005, 
                           iter = 20000)
              )
bayes_R2(fit3)
summary(fit3)
plot(fit3)



pred_dat <- expand_grid(#season=unique(tmp$season), 
                        co2 = seq(min(tmp$co2_int),max(tmp$co2_int),length.out=50),
                        pe_12mo = seq(0.01,2,length.out = 200)) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(s_ric_mc, newdata=.)[,'Estimate'])

pred_dat %>% 
  ggplot(data=., aes(pe_12mo,pred,color=(co2), group=co2))+
  geom_line(alpha=1)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2), expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size=10), 
        legend.position = c(0.7,0.1), 
        legend.key.width = unit(1,'cm'),
        legend.direction = 'horizontal')


print(s_ric_mc,digits=4)
summary(s_ric_mc) %>% str
vec_coef <- (s_ric_mc$fit %>% as.matrix %>% colMeans())
curve(ric_x2(x = x, x2 = 0,
       Asym = vec_coef["b_Asym_Intercept"], 
       Asym2 = vec_coef["b_Asym2_Intercept"], 
       xmid = vec_coef["b_xmid_Intercept"], 
       xmid2 = vec_coef["b_xmid2_Intercept"], 
       scal = vec_coef["b_scal_Intercept"], 
       scal2 = vec_coef["b_scal2_Intercept"], 
       lpow = vec_coef["b_lpow_Intercept"], 
       lpow2 = vec_coef["b_lpow2_Intercept"]), 0,2, 
      ylab="NDVI",xlab="P:PET", ylim=c(0,1))
curve(ric_x2(x = x, x2 = 100,
             Asym = vec_coef["b_Asym_Intercept"], 
             Asym2 = vec_coef["b_Asym2_Intercept"], 
             xmid = vec_coef["b_xmid_Intercept"], 
             xmid2 = vec_coef["b_xmid2_Intercept"], 
             scal = vec_coef["b_scal_Intercept"], 
             scal2 = vec_coef["b_scal2_Intercept"], 
             lpow = vec_coef["b_lpow_Intercept"], 
             lpow2 = vec_coef["b_lpow2_Intercept"]*-10), 
      0,2, 
      add=T,col='blue')
curve(ric_x2(x = x, x2 = 100,
             Asym = vec_coef["b_Asym_Intercept"], 
             Asym2 = vec_coef["b_Asym2_Intercept"], 
             xmid = vec_coef["b_xmid_Intercept"], 
             xmid2 = vec_coef["b_xmid2_Intercept"], 
             scal = vec_coef["b_scal_Intercept"], 
             scal2 = vec_coef["b_scal2_Intercept"], 
             lpow = vec_coef["b_lpow_Intercept"], 
             lpow2 = vec_coef["b_lpow2_Intercept"]*-50), 0,2, 
      add=T,col='red',lty=3)
curve(ric_x2(x = x, x2 = -100,
             Asym = vec_coef["b_Asym_Intercept"], 
             Asym2 = vec_coef["b_Asym2_Intercept"], 
             xmid = vec_coef["b_xmid_Intercept"], 
             xmid2 = vec_coef["b_xmid2_Intercept"], 
             scal = vec_coef["b_scal_Intercept"], 
             scal2 = vec_coef["b_scal2_Intercept"], 
             lpow = vec_coef["b_lpow_Intercept"], 
             lpow2 = vec_coef["b_lpow2_Intercept"]), 0,2, 
      add=T,col='blue')
curve(ric_x2(x = x, x2 = -100,
             Asym = vec_coef["b_Asym_Intercept"], 
             Asym2 = vec_coef["b_Asym2_Intercept"], 
             xmid = vec_coef["b_xmid_Intercept"], 
             xmid2 = vec_coef["b_xmid2_Intercept"]*2, 
             scal = vec_coef["b_scal_Intercept"], 
             scal2 = vec_coef["b_scal2_Intercept"]*2, 
             lpow = vec_coef["b_lpow_Intercept"], 
             lpow2 = vec_coef["b_lpow2_Intercept"]), 0,2, 
      add=T,col='blue',lty=3)


# ************ ******************************************************************
# END SECTION ******************************************************************
# ************ ******************************************************************
test_dat <- tmp[season=='SON'][pe_12mo <= 2] %>% 
  .[p_anom_12mo_frac > -0.75 & p_anom_12mo_frac < 0.75] %>%
  .[sample(.N,1000000)]

print(s_ric_mc,digits=4)
bprior2 <- 
  prior(normal(0.77,0.1), nlpar = Asym, lb=0.60, ub=0.85) + #! need to specify lb=0 when using gamma prior
  prior(normal(0.0006,0.0001),nlpar=Asym2, lb=0,ub=0.01)+
  prior(normal(0.3216,0.05), nlpar = xmid, lb=0.15, ub=0.5)+    # VERY SENSITIVE 
  prior(normal(0.0004,0.0001),nlpar=xmid2, lb=-0.001,ub=0.001)+
  prior(normal(0.265,0.05), nlpar=scal, lb=0.15,ub=0.35)+
  prior(normal(0,0.001), nlpar=scal2, lb=-0.001,ub=0.001)+
  prior(normal(-0.13,0.1), nlpar = lpow, lb=-0.4,ub=0)+
  prior(normal(0,0.001),nlpar=lpow2, lb=-0.01,ub=0.01) # don't narrow further


ric_x2_son <- nls(ndvi_3mo ~ 
                    mod_grad_ric_x2(x = pe_12mo,x2=cco2, 
                                    Asym=Asym,Asym2=Asym2, 
                                    xmid=xmid, xmid2=xmid2,
                                    scal=scal,scal2=scal2,
                                    lpow=lpow, lpow2=lpow2),
                  data=test_dat, 
                  control=nls.control(maxiter=1000),
                  start = c(Asym=0.7561,Asym2=5e-4,
                            xmid=0.27,xmid2=3e-4,
                            scal=0.28,scal2=0,
                            lpow=-0.28,lpow2=0), 
                  lower=c(0.5, -0.01, 
                          0.2, -0.01, 
                          0.15, -0.001, 
                          -0.4, -0.01), 
                  algorithm = 'port')
summary(ric_x2_son)

ric_offset_son <- nls(ndvi_3mo ~ 
                    grad_ric_offset(x = pe_12mo,x2=cco2,
                                    offset=offset,offset2=offset2,
                                    Asym=Asym, 
                                    xmid=xmid,
                                    scal=scal,
                                    lpow=lpow),
                  data=test_dat, 
                  control=nls.control(maxiter=1000),
                  start = c(offset=0.001, offset2=0.001,
                            Asym=0.7561,
                            xmid=0.27,
                            scal=0.28,
                            lpow=-0.28), 
                  lower=c(0,-0.001,           #offset
                          0.5,         #Asym
                          0.2,         #xmid
                          0.15,        # scal
                          -0.5),       #lpow
                  algorithm = 'port')
ric_offset_son

bbmle::AICtab(ric_x2_son, ric_offset_son)

pred_dat <- expand_grid(#season=unique(tmp$season), 
  co2 = seq(min(tmp$co2_int),max(tmp$co2_int),length.out=50),
  pe_12mo = seq(0.01,2,length.out = 200)) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(ric_x2_son, newdata=.))

pred_dat %>% 
  ggplot(data=., aes(pe_12mo,pred,color=(co2), group=co2))+
  geom_line(alpha=1)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2), expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size=10), 
        legend.position = c(0.7,0.1), 
        legend.key.width = unit(1,'cm'),
        legend.direction = 'horizontal')
ggsave("figures/")


logis_model <- function(pe_12mo, Asym, Asym2, xmid,xmid2, scal, scal2,chydro_year){
  (Asym+Asym2*chydro_year)/(1+exp(((xmid+xmid2*chydro_year)-pe_12mo)/(scal+scal2*chydro_year)))
}
model_gradient <- deriv(
  body(logis_model)[[2]], 
  namevec = c("Asym","Asym2","xmid","xmid2","scal","scal2"), 
  function.arg = logis_model
)
j1 <- nls(ndvi_3mo ~ model_gradient(pe_12mo, Asym, Asym2, 
                                    xmid,xmid2,scal,scal2,chydro_year=hydro_year-2000.5),
          data=tmp[season=='SON'], 
          start = c(Asym = 0.6, Asym2 = 0.1, xmid=0.2,xmid2=0, scal=0.2,scal2=0))
j1
summary(j1)
cbind(predict(j1,newdata=tmp[season=='SON']), tmp[season=='SON']$ndvi_3mo) %>% 
  na.omit() %>% 
  cor()

pred_dat <- expand_grid(season=unique(tmp$season), 
                        hydro_year=1984:2019, 
                        pe_12mo = seq(0.01,2,length.out = 100)) %>% 
  mutate(chydro_year=hydro_year-2000.5) %>% 
  mutate(pred = predict(j1, newdata=.))

pred_dat %>% 
  ggplot(data=., aes(pe_12mo,pred,color=(hydro_year), group=hydro_year))+
  geom_line(alpha=0.5)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c("Year", option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2), expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size=10), 
        legend.position = c(0.7,0.1), 
        legend.key.width = unit(1,'cm'),
        legend.direction = 'horizontal')




  # mutate(pred = logis_model(pe_12mo,
  #                           Asym = coef(j1)["SON","Asym"], 
  #                           Asym2 = coef(j1)["SON","Asym2"], 
  #                           xmid = coef(j1)["SON","xmid"], 
  #                           xmid2 =coef(j1)["SON","xmid2"], 
  #                           scal = coef(j1)["SON","scal"], 
  #                           scal2 =coef(j1)["SON","scal2"], 
  #                           chydro_year = chydro_year))







j1 <- nlmer(ndvi_3mo ~ model_gradient(pe_12mo, Asym, Asym2, 
                                      xmid,xmid2,scal,scal2,chydro_year=hydro_year-2000.74)+
              (Asym | season) + (xmid | season) + (scal | season),
            data=tmp[sample(.N, 100000)], 
            start = c(Asym = 0.6, Asym2 = 0.1, xmid=0.2,xmid2=0, scal=0.2,scal2=0))
j1
summary(j1)

predict(j1, newdata=expand_grid(season=unique(tmp$season), 
                                hydro_year=1984:2019, 
                                pe_12mo = seq(0.01,2,length.out = 100)) %>% 
          mutate(chydro_year=hydro_year-2000.74))

pred_dat <- expand_grid(season=unique(tmp$season), 
                        hydro_year=1984:2019, 
                        pe_12mo = seq(0.01,2,length.out = 100)) %>% 
  mutate(chydro_year=hydro_year-2000.5) %>% 
  mutate(pred = logis_model(pe_12mo,
                            Asym = coef(j1)$season["SON","Asym"], 
                            Asym2 = coef(j1)$season["SON","Asym2"], 
                            xmid = coef(j1)$season["SON","xmid"], 
                            xmid2 =coef(j1)$season["SON","xmid2"], 
                            scal = coef(j1)$season["SON","scal"], 
                            scal2 =coef(j1)$season["SON","scal2"], 
                            chydro_year = chydro_year))

pred_dat %>% 
  ggplot(data=., aes(pe_48mo,pred,color=(hydro_year), group=hydro_year))+
  geom_line(alpha=0.5)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c("Year", option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2), expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size=10), 
        legend.position = c(0.7,0.1), 
        legend.key.width = unit(1,'cm'),
        legend.direction = 'horizontal')

# fit <- nls_multstart(ndvi_3mo ~ SSlogis(pe_48mo,Asym,xmid,scal),
#                      data = dat[sample(.N, 2000)],
#                      iter = 100,
#                      start_lower = c(Asym=0.5, xmid=0, scal=0),
#                      start_upper = c(Asym=1, xmid=0.5, scal=1),
#                      supp_errors = 'Y',
#                      na.action = na.omit,
#                      lower= c(0.5,  0,   0),
#                      upper= c(1,    1, 1))

xdat[term=='Asym']$estimate %>% hist
xdat[term=='xmid']$estimate %>% hist
xdat[term=='scal']$estimate %>% hist

bprior <- prior(normal(0.75,0.1), nlpar = Asym, lb=0.65, ub=1) + #! need to specify lb=0 when using gamma prior
  prior(normal(0.3,0.05), nlpar = xmid, lb=0.01)+    # VERY SENSITIVE 
  prior(normal(0.25,0.1), nlpar=scal, lb=0.01)

options(mc.cores=parallel::detectCores()-3) 
test_dat <- tmp[hydro_year==1993][is.na(ndvi_3mo)==F & is.na(pe_48mo)==F]
f <- bf(ndvi_3mo ~ Asym/(1+exp((xmid-pe_48mo)/scal)),
        Asym + xmid + scal ~ 1, 
        nl=TRUE)
make_stancode(f, prior=bprior, family=gaussian(),
              data=test_dat)

fit <- brm(f,
            data = test_dat[sample(.N, 1000)], 
            prior = bprior, 
            # sample_prior = 'only')
            algorithm = 'sampling', 
            control = list(adapt_delta=0.99, 
                           max_treedepth=15),
            chains = 3, 
            iter = 500
)
fit <- update(fit, newdata=tmp[hydro_year==1994 & season=='JJA'], 
              chains=4, iter=1000)
summary(fit)
xdat[hydro_year==1994 & season=="JJA"]

curve(SSlogis(x,0.65,0.15,0.11),0,2)
curve(SSlogis(x,0.65,0.15,0.2),0,2,add=T,col='red')


library(lme4)
?nlmer
nlmer(ndvi_3mo ~ SSlogis(pe_48mo, Asym, xmid, scal))





b_son <- mgcv::bam(ndvi_3mo ~ te(pe_12mo,cco2,k=4),
              data=tmp[season=='SON'][pe_12mo <= 2] %>%  
                .[p_anom_12mo_frac > -0.15 & p_anom_12mo_frac < 0.15],
              select=TRUE, discrete=TRUE, nthreads = 8)
b_son
b_son %>% summary 

pred_dat <- expand_grid(season=unique(tmp$season), 
                        co2 = seq(min(mlo$co2_int),max(mlo$co2_int),length.out=10),
                        pe_12mo = seq(0.01,2,length.out = 100)) %>% 
  mutate(cco2 = co2-center_co2) %>% 
  mutate(pred = predict(b_son, newdata=.))

pred_dat %>% 
  ggplot(data=., aes(pe_12mo,pred,color=(co2), group=co2))+
  geom_line(alpha=1)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2), expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size=10), 
        legend.position = c(0.7,0.1), 
        legend.key.width = unit(1,'cm'),
        legend.direction = 'horizontal')

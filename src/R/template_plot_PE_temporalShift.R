#*******************************************************************************
#* Description:
#* Plot NDVI~Precip:PET . This is meant to supplement to 
#* provide an visual confirmation of the choice of non-linear logistic
#* functional form.
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



# Plot pe_12mo w/GAMs -----------------------------------------------------
library(ggExtra)
vec_id <- tmp[mape<2] %>% 
          .[,sample(id,1000)]
  
p <- tmp[id %in% vec_id] %>% 
  .[is.na(season)==F] %>% 
  .[mape < 2] %>% 
  .[pe_12mo < 3] %>% 
  ggplot(data=., aes(pe_12mo, ndvi_3mo, 
                     color=hydro_year,
                     group=as_factor(hydro_year)))+
  geom_point(alpha=0)+
  geom_smooth(se=F,
              lwd=0.75,
              method='gam',
              formula=y~s(x,bs='cs'),
              method.args=list(select=TRUE,
                               method='REML'))+
  scale_x_continuous(limits=c(0,2), expand=c(0,0))+
  scale_color_viridis_c(option='B',end=0.9, 
                        breaks = c(1982,1990,2000,2010,2019))+
  labs(x=expression(paste(Precip["12 mo"]*":"*PET["12 mo"])), 
       y=expression(paste(NDVI[" 3 mo"])))+
  theme_linedraw()+
  guides(color=guide_colorbar(title = 'Hydro\nYear'))+
  theme(panel.grid = element_blank(), 
        legend.position = c(0.6,0.15), 
        legend.direction = 'horizontal', 
        legend.key.width = unit(1, 'cm'));
p <- ggMarginal(p,x='pe_12mo',y='ndvi_3mo', type='histogram')
ggsave(p, filename = "figures/gam_plot_pe12mo.png", 
       width = 17, height = 12, units='cm', dpi=350, type='cairo')  

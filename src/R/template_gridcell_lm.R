library(tidyverse); 
library(sf); 
library(arrow)
library(tidyverse); library(lubridate);
library(data.table)
setDTthreads(threads=8)
library(RcppArmadillo)

oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)
# tmp <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet")
tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet")
# data.table 
tmp <- setDT(tmp) # OR: tmp <- as.data.table(tmp)
gc(reset = T, full = T)

# #*******************************************************************************
# #* Add season and hydro year -----
# #*******************************************************************************
# vec_dates <- data.table(date=sort(unique(tmp$date))) %>% 
#   .[,quarter:=quarter(date)] %>% 
#   mutate(q = case_when(quarter==1~"DJF",
#                        quarter==2~"MAM",
#                        quarter==3~"JJA",
#                        quarter==4~"SON")) %>% 
#   mutate(season = factor(q,
#                          levels=c("DJF","MAM","JJA","SON"), 
#                          ordered=T)) %>% 
#   select(date,season) %>% 
#   mutate(hydro_year = year(date+months(1)))
# 
# tmp <- tmp[vec_dates,on=.(date)]
# gc(verbose = T, reset = T, full = T)
# #*******************************************************************************
# #* END SECTION
# #*******************************************************************************
# 
# #*******************************************************************************
# #* Calculate Climatology
# #*******************************************************************************
# tmp <- tmp[, `:=`(month = month(date))] # create month
# tmp <- tmp[, `:=`(year = year(date))]   # create year
# norms_ndvi <- tmp[is.na(ndvi_m)==F][date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
#                   .("ndvi_u" = mean(ndvi_m,na.rm=TRUE), 
#                     "ndvi_sd" = sd(ndvi_m,na.rm=TRUE), 
#                     "ndvi_p10" = quantile(ndvi_m, 0.1), 
#                     "ndvi_p90" = quantile(ndvi_m, 0.9)),
#                   by=.(x,y,month)] # joining on x,y,month
# tmp <- norms_ndvi[tmp,on=.(x,y,month)]
# tmp <- tmp[,`:=`(ndvi_anom = ndvi_m - ndvi_u)] %>% 
#           .[,`:=`(ndvi_anom_sd = ndvi_anom/ndvi_sd)]
# #*******************************************************************************
# #* END SECTION
# #*******************************************************************************



# NDVI trend (working!)
system.time(
  lt_ndvi_season <- tmp[ndvi_anom_sd >= -5 & ndvi_anom_sd <= 5] %>%
    .[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(ndvi_mcd, na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[,.(b1 = fastLm(X = cbind(1,year), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)

# long-term trend of mean monthly precip by season
system.time(
  lt_p_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(precip, na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year), y=val, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)]
)

# long-term trend of mean monthly PET by season
system.time(
  lt_pet_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(pet, na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year), y=val, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)]
)

# long-term trend of VPD15 by season
system.time(
  lt_vpd15_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(vpd15, na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year), y=val, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)]
)


# long-term trend of mean monthly tmax by season
system.time(
  lt_tmax_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(tmax, na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year), y=val, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)]
)

# long-term trend of mean monthly tmax by season
system.time(
  lt_t35_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = sum(t35, na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year), y=val, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)]
)

# ndvi sensitivyt to P & PET
system.time(
  sens_ndvi_p_pet_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(ndvi = mean(ndvi_mcd, na.rm=TRUE), 
         p = mean(precip_anom,na.rm=TRUE), 
         pet = mean(pet_anom,na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[is.na(ndvi)==F] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,p,pet), y=ndvi, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b_p=unlist(beta)[2], b_pet=unlist(beta)[3]), by=.(x,y,season)]
  
)

# ndvi anom sensitivity to P & PET
system.time(
  sens_ndviAnom_p_pet_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(ndvi = mean(ndvi_anom, na.rm=TRUE), 
         p = mean(precip_anom,na.rm=TRUE), 
         pet = mean(pet_anom,na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[is.na(ndvi)==F] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,p,pet), y=ndvi, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b_p=unlist(beta)[2], b_pet=unlist(beta)[3]), by=.(x,y,season)]
)
  
# ndvi anom sd sensitivity to P & PET
system.time(
  sens_ndviAnomSd_p_pet_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(ndvi = mean(ndvi_anom_sd, na.rm=TRUE), 
         p = mean(precip_anom,na.rm=TRUE), 
         pet = mean(pet_anom,na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[is.na(ndvi)==F] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,p,pet), y=ndvi, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b_p=unlist(beta)[2], b_pet=unlist(beta)[3]), by=.(x,y,season)]
)

# ndvi anom sd sensitivity to PE
system.time(
  sens_ndviAnomSd_pe_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(ndvi = mean(ndvi_anom_sd, na.rm=TRUE), 
         pe = mean(pe_anom,na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[is.na(ndvi)==F] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,pe), y=ndvi, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b_pe=unlist(beta)[2]), by=.(x,y,season)]
)


# NDVI anom sensitivity to P & VPD15
system.time(
  sens_ndviAnom_p_vpd15_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(ndvi = mean(ndvi_anom, na.rm=TRUE), 
         p = mean(precip_anom,na.rm=TRUE), 
         vpd15 = mean(vpd15_anom,na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[is.na(ndvi)==F] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,p,vpd15), y=ndvi, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b_p=unlist(beta)[2], b_vpd15=unlist(beta)[3]), by=.(x,y,season)]
)

# NDVI anom SD sensitivity to P & VPD15
system.time(
  sens_ndviAnomSD_p_vpd15_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(ndvi = mean(ndvi_anom_sd, na.rm=TRUE), 
         p = mean(precip_anom,na.rm=TRUE), 
         vpd15 = mean(vpd15_anom,na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[is.na(ndvi)==F] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,p,vpd15), y=ndvi, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b_p=unlist(beta)[2], b_vpd15=unlist(beta)[3]), by=.(x,y,season)]
)




save(sens_ndvi_p_pet_season,
          sens_ndviAnomSd_p_pet_season,
          sens_ndviAnomSd_pe_season,
          lt_ndvi_season,
          lt_p_season,
          lt_vpd15_season,
          lt_pet_season,
          lt_tmax_season, 
          lt_t35_season,
     sens_ndviAnom_p_vpd15_season, 
     sens_ndviAnomSD_p_vpd15_season,
     file="data/gridCell_lm_ndvi_clim.Rdata", 
          compress=TRUE)


# sens_ndviAnomSd_pe_season$b_pe %>% hist(100)
# sens_ndviAnomSd_pe_season %>% 
#   ggplot(data=.,aes(x,y,fill=b_pe))+geom_tile()+coord_equal()+
#   scale_fill_gradient2(
#     # limits=c(-1e-03,1e-03),oob=scales::squish
#     limits=c(-5,5),oob=scales::squish
#     )+facet_wrap(~season)+
#   theme_dark()
# 
# 
# system.time(
#   lt_season <- tmp[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
#     .[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
#     .[,.(val = mean(ndvi, na.rm=TRUE)), by=.(x,y,season,year)] %>% 
#     .[,.(b1 = fastLm(X = year, y = val, data=.SD)$coefficients), 
#       by=.(x,y,season)]
# )

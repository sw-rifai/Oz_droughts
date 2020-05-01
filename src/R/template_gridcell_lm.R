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
tmp <- arrow::read_parquet("/home/sami/srifai@gmail.com/work/research/data_general/Oz_misc_data/ARD_nirv_aclim_anoms.parquet")
# data.table 
tmp <- setDT(tmp) # OR: tmp <- as.data.table(tmp)

# NIRV trend (working!)
system.time(
  lt_nirv_season <- tmp[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>%
    .[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(nirv, na.rm=TRUE)), by=.(x,y,season,year)] %>% 
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

# long-term trend of mean monthly tmax by season
system.time(
  lt_tmax_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(tmax, na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year), y=val, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)]
)

# nirv sensitivity to P & PET
system.time(
  sens_nirvAnom_p_pet_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(nirv = mean(nirv_anom, na.rm=TRUE), 
         p = mean(precip_anom,na.rm=TRUE), 
         pet = mean(pet_anom,na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[is.na(nirv)==F] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,p,pet), y=nirv, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b_p=unlist(beta)[2], b_pet=unlist(beta)[3]), by=.(x,y,season)]
)
  
# nirv anom sd sensitivity to P & PET
system.time(
  sens_nirvAnomSd_p_pet_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(nirv = mean(nirv_anom_sd, na.rm=TRUE), 
         p = mean(precip_anom,na.rm=TRUE), 
         pet = mean(pet_anom,na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[is.na(nirv)==F] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,p,pet), y=nirv, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b_p=unlist(beta)[2], b_pet=unlist(beta)[3]), by=.(x,y,season)]
)

# nirv anom sd sensitivity to PE
system.time(
  sens_nirvAnomSd_pe_season <- tmp[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(nirv = mean(nirv_anom_sd, na.rm=TRUE), 
         pe = mean(pe_anom,na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[is.na(nirv)==F] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,pe), y=nirv, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b_pe=unlist(beta)[2]), by=.(x,y,season)]
)

save(sens_nirv_p_pet_season,
          sens_nirvAnomSd_p_pet_season,
          sens_nirvAnomSd_pe_season,
          lt_nirv_season,
          lt_p_season,
          lt_pet_season,
          lt_tmax_season, 
     file="data/gridCell_lm_nirv_clim.Rdata", 
          compress=TRUE)


# sens_nirvAnomSd_pe_season$b_pe %>% hist(100)
# sens_nirvAnomSd_pe_season %>% 
#   ggplot(data=.,aes(x,y,fill=b_pe))+geom_tile()+coord_equal()+
#   scale_fill_gradient2(
#     # limits=c(-1e-03,1e-03),oob=scales::squish
#     limits=c(-5,5),oob=scales::squish
#     )+facet_wrap(~season)+
#   theme_dark()
# 
# 
# system.time(
#   lt_season <- tmp[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>%
#     .[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
#     .[,.(val = mean(nirv, na.rm=TRUE)), by=.(x,y,season,year)] %>% 
#     .[,.(b1 = fastLm(X = year, y = val, data=.SD)$coefficients), 
#       by=.(x,y,season)]
# )

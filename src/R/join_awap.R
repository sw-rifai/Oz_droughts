library(tidyverse); library(arrow); 

#' Calculates saturation vapour pressure
#' @return saturation vapour pressure
calc_esat <- function(airtemp){
  #Tair in degrees C
  
  #From Jones (1992), Plants and microclimate: A quantitative approach 
  #to environmental plant physiology, p110
  esat <- 613.75 * exp(17.502 * airtemp / (240.97+airtemp))
  
  return(esat)
}

coords <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv")

vp9am <- read_parquet("../data_general/clim_grid/awap/parquet/awap_vp9am_monthly_1970_2019.parquet")
vp3pm <- read_parquet("../data_general/clim_grid/awap/parquet/awap_vp3pm_monthly_1970_2019.parquet")
tmin <- read_parquet("../data_general/clim_grid/awap/parquet/awap_tmin_monthly_1911_2019.parquet")
tmax <- read_parquet("../data_general/clim_grid/awap/parquet/awap_tmax_monthly_1911_2019.parquet")
precip <- read_parquet("../data_general/clim_grid/awap/parquet/awap_precip_monthly_1911_2019.parquet")


o <- inner_join(vp9am, vp3pm, by=c("id","date"))
o <- inner_join(o, tmin, by=c("id","date"))
o <- inner_join(o, tmax, by=c("id","date"))
o <- inner_join(o, precip, by=c('id','date'))
o <- inner_join(o, coords, by=c('id'))

o <- o %>% 
  mutate(vpd3pm = 0.01*(calc_esat(tmax)/10 - vp3pm)) %>% 
  mutate(vpd9am = 0.01*(calc_esat(tmin)/10 - vp9am))

o %>% 
  write_parquet(., sink="../data_general/clim_grid/awap/parquet/awap_joined_monthly_1970_2019.parquet", 
                compression = "snappy")  


# calc_esat(30)/1000
# o$vp3pm[1]
# 
# o$vp3pm %>% hist
# hist(calc_esat(o$tmax)/100)

# o %>% 
#   sample_n(10000) %>% 
#   ggplot(data=., aes(vp9am, vp3pm))+
#   geom_point()+
#   geom_abline(aes(intercept=0,slope=1),col='red')

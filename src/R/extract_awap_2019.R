library(tidyverse); 
library(sf); library(lubridate)
library(arrow); 

list.files("../data_general/clim_grid/awap/AWAP/tmp_2019/", 
                         full.names = T)


coords <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv")
coords <- sf::st_as_sf(coords, coords = c("longitude","latitude"), crs=4326)

raster::stack(fp, resolution=0.05)
stars::read_ncdf(fp, var='precip') %>% plot


# precip ------------------------------------------------
fp <- list.files("../data_general/clim_grid/awap/AWAP/tmp_2019/", 
                 full.names = T, pattern = 'precip')
vec_dates <- tibble(date=seq(ymd('2019-01-01'),ymd('2019-08-01'),by='1 month'))
junk <- velox::velox(raster::stack(fp, crs="EPSG:9122"))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$date
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="precip")
ex <- ex %>% 
  mutate(date=ymd(date)) %>% 
  mutate(days_in_month = days_in_month(date)) 
ex_precip <- ex

# tmax ------------------------------------------------
fp <- list.files("../data_general/clim_grid/awap/AWAP/tmp_2019/", 
                 full.names = T, pattern = 'tmax')
vec_dates <- tibble(date=seq(ymd('2019-01-01'),ymd('2019-08-01'),by='1 month'))
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$date
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="tmax")
ex <- ex %>% 
  mutate(date=ymd(date))
ex_tmax <- ex

# tmin ------------------------------------------------
fp <- list.files("../data_general/clim_grid/awap/AWAP/tmp_2019/", 
                 full.names = T, pattern = 'tmin')
vec_dates <- tibble(date=seq(ymd('2019-01-01'),ymd('2019-08-01'),by='1 month'))
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$date
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="tmin")
ex <- ex %>% 
  mutate(date=ymd(date))
ex_tmin <- ex

# vp9am ------------------------------------------------
fp <- list.files("../data_general/clim_grid/awap/AWAP/tmp_2019/", 
                 full.names = T, pattern = 'vapourpres_h09')
vec_dates <- tibble(date=seq(ymd('2019-01-01'),ymd('2019-08-01'),by='1 month'))
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$date
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="vp9am")
ex <- ex %>% 
  mutate(date=ymd(date))
ex_vp9am <- ex


# vp3pm ------------------------------------------------
fp <- list.files("../data_general/clim_grid/awap/AWAP/tmp_2019/", 
                 full.names = T, pattern = 'vapourpres_h15')
vec_dates <- tibble(date=seq(ymd('2019-01-01'),ymd('2019-08-01'),by='1 month'))
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$date
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="vp3pm")
ex <- ex %>% 
  mutate(date=ymd(date))
ex_vp3pm <- ex

tmp <- inner_join(ex_precip, ex_tmax, by=c("id","date"))
tmp <- inner_join(tmp, ex_tmin, by=c("id","date"))
tmp <- inner_join(tmp, ex_vp9am, by=c("id","date"))
tmp <- inner_join(tmp, ex_vp3pm, by=c("id","date"))


#' Calculates saturation vapour pressure
#' @return saturation vapour pressure
calc_esat <- function(airtemp){
  #Tair in degrees C
  
  #From Jones (1992), Plants and microclimate: A quantitative approach 
  #to environmental plant physiology, p110
  esat <- 613.75 * exp(17.502 * airtemp / (240.97+airtemp))
  
  return(esat)
}
tmp <- tmp %>% 
  mutate(vpd3pm = 0.01*(calc_esat(tmax)/10 - vp3pm)) %>% 
  mutate(vpd9am = 0.01*(calc_esat(tmin)/10 - vp9am))

tmp %>%
  write_parquet(., sink="../data_general/clim_grid/awap/AWAP/tmp_2019/awap_2019Jan_2019Aug.parquet",
                compression = "snappy")
# rm(ex,junk,vec_dates,fp)
# gc()
# 

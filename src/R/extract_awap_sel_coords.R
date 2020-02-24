library(tidyverse); 
library(sf); library(lubridate)
library(arrow); 

coords <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv")
coords <- sf::st_as_sf(coords, coords = c("longitude","latitude"), crs=4326)
# coords$id <- 1:nrow(coords)

# write_csv(coords %>% 
#             rename(longitude=lon, 
#                    latitude=lat), "data/Oz_avhrr_lai_gte1_renamed.csv")
# awap_tmean <- stars::read_ncdf("../data_general/clim_grid/awap/AWAP/Monthly_data_1911_2019/Monthly_mean_temperature_AWAP_masked_1911_2019.nc")
# awap_tmean %>% dim
# junk <- awap_tmean[,,,1:5]


# Testing methods for large point extraction in R -------------------------

# Tmean: faster with velox ------------------------------------------------
fp <- "../data_general/clim_grid/awap/AWAP/Monthly_data_1911_2019/Monthly_mean_temperature_AWAP_masked_1911_2019.nc"
vec_dates <- stars::read_ncdf(fp, var="time")
vec_dates <- vec_dates %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack("../data_general/clim_grid/awap/AWAP/Monthly_data_1911_2019/Monthly_mean_temperature_AWAP_masked_1911_2019.nc"))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="tmean")
ex %>% 
  mutate(date=ymd(date)) %>% 
  write_parquet(., sink="../data_general/clim_grid/awap/parquet/awap_tmean_monthly_1911_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)


# Tmin: faster with velox --------------------------------------------------
fp <- "../data_general/clim_grid/awap/AWAP/Monthly_data_1911_2019/Monthly_mean_min_temperature_AWAP_masked_1911_2019.nc"
vec_dates <- stars::read_ncdf(fp)
vec_dates <- vec_dates[,1,1,] %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="tmin")
ex %>% 
  mutate(date=ymd(date)) %>% 
  write_parquet(., sink="../data_general/clim_grid/awap/parquet/awap_tmin_monthly_1911_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)


# Tmax: faster with velox
fp <- "../data_general/clim_grid/awap/AWAP/Monthly_data_1911_2019/Monthly_mean_max_temperature_AWAP_masked_1911_2019.nc"
vec_dates <- stars::read_ncdf(fp)
vec_dates <- vec_dates[,1,1,] %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="tmax")
ex %>% 
  mutate(date=ymd(date)) %>% 
  write_parquet(., sink="../data_general/clim_grid/awap/parquet/awap_tmax_monthly_1911_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)
gc();


# Precip: faster with velox ---------------------------------------------------------
fp <- "../data_general/clim_grid/awap/AWAP/Monthly_data_1911_2019/Monthly_total_precipitation_AWAP_masked_1900_2019.nc"
vec_dates <- stars::read_ncdf(fp)
vec_dates <- vec_dates[,1,1,] %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="precip")
ex %>% 
  mutate(date=ymd(date)) %>% 
  write_parquet(., sink="../data_general/clim_grid/awap/parquet/awap_precip_monthly_1911_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)

# Vapor Pressure 9am ---------------------------------------------------------
fp <- "../data_general/clim_grid/awap/AWAP/AWAP/Monthly_mean_vprp9am_AWAP_masked_1970_2019.nc"
vec_dates <- stars::read_ncdf(fp)
vec_dates <- vec_dates[,1,1,] %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="vp9am")
ex %>% 
  mutate(date=ymd(date)) %>% 
  write_parquet(., sink="../data_general/clim_grid/awap/parquet/awap_vp9am_monthly_1970_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)
gc()

# Vapor Pressure 3pm ---------------------------------------------------------
fp <- "../data_general/clim_grid/awap/AWAP/AWAP/Monthly_mean_vprp3pm_AWAP_masked_1970_2019.nc"
vec_dates <- stars::read_ncdf(fp)
vec_dates <- vec_dates[,1,1,] %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="vp3pm")
ex %>% 
  mutate(date=ymd(date)) %>% 
  write_parquet(., sink="../data_general/clim_grid/awap/parquet/awap_vp3pm_monthly_1970_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)
gc()



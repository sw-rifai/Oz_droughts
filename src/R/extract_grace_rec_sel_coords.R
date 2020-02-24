library(tidyverse); 
library(sf); library(lubridate)
library(arrow); 

coords <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv")
coords <- sf::st_as_sf(coords, coords = c("longitude","latitude"), crs=4326)
#coords$id <- 1:nrow(coords)


# Testing methods for large point extraction in R -------------------------

# GRACE reconstruction GSFC_ERA5 1910-2019
fp <- "../data_general/GRACE_reconstruction/GRACE_REC_v03_GSFC_ERA5_monthly_ensemble_mean.nc"
vec_dates <- stars::read_ncdf(fp, var='rec_ensemble_mean')
vec_dates
vec_dates <- vec_dates[,1,1,] %>% as_tibble() %>% select(time)
gc()
junk <- velox::velox(raster::stack(fp), varname="rec_ensemble_mean")
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="grace_rec_era5")
ex %>% 
  mutate(date=ymd(date)) %>% 
  write_parquet(., sink="../data_general/GRACE_reconstruction/parquet/GRACE_ERA5_monthly_1979_2018.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)


# GRACE reconstruction GSFC_GSWP3 1910-2019
fp <- "../data_general/GRACE_reconstruction/GRACE_REC_v03_GSFC_GSWP3_monthly_ensemble_mean.nc"
vec_dates <- stars::read_ncdf(fp, var='rec_ensemble_mean')
vec_dates
vec_dates <- vec_dates[,1,1,] %>% as_tibble() %>% select(time)
gc()
junk <- velox::velox(raster::stack(fp,varname="rec_ensemble_mean"))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="grace_rec_gswp3")
ex %>% 
  mutate(date=ymd(date)) %>% 
  write_parquet(., sink="../data_general/GRACE_reconstruction/parquet/GRACE_GSWP3_monthly_1901_2014.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)

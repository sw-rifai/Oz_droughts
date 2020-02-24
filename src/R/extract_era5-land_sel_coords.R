library(tidyverse); 
library(sf); library(lubridate)
library(arrow); 

coords <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv")
coords <- sf::st_as_sf(coords, coords = c("longitude","latitude"), crs=4326)


# evaporation ------------------------------------------------
fp <- "../data_general/clim_grid/era5-land/Oz/Oz/Oz_era5-land_monmean_evaporation_1979_2019.nc"
vec_dates <- stars::read_ncdf(fp, var="time")
vec_dates <- vec_dates %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="evap")
ex <- ex %>% 
  mutate(date=ymd(date)) %>% 
  mutate(days_in_month = days_in_month(date)) 
ex <- ex %>% 
  mutate(evap = evap*days_in_month*-1000)

ex %>% 
  write_parquet(., sink="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_evaporation_1979_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)
gc()

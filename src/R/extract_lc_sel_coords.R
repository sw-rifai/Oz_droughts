library(stars); library(tidyverse); library(velox)
vec_coords <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv")
vec_coords <- sf::st_as_sf(vec_coords, coords = c("longitude","latitude"), crs=4326)

tree_cov <- raster::raster("../data_general/Oz_misc_data/Oz_CGLS-LC100_500m.tif", 
                     band=14)
junk <- velox::velox(tree_cov)
ex <- junk$extract_points(sp = vec_coords)
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

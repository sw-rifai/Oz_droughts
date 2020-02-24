library(stars); library(tidyverse); 

lc <- raster::raster("../data_general/Oz_misc_data/Oz_CGLS-LC100_500m.tif", 
                     band=1)
lc <- st_as_stars(lc)
lc %>% plot

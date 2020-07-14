library(tidyverse); library(data.table); 
library(stars); library(sf); library(lubridate)
library(viridisLite); 
library(terra)
# wget *.tif https://eo-data.csiro.au/remotesensing/v310/australia/monthly/cover/
# aust.006.tif

# wget -nc -A "*.aust.006.tif" -r -np -l1 --no-check-certificate -e robots=off https://eo-data.csiro.au/remotesensing/v310/australia/monthly/cover/


src_dir <- "/home/sami/Downloads/frac_cover/eo-data.csiro.au/remotesensing/v310/australia/monthly/cover"

vec_fp <- list.files(src_dir, pattern = '*.tif')

o <- stars::read_stars(file.path(src_dir, vec_fp[1]))
o <- terra::rast(file.path(src_dir,vec_fp[1]))
o %>% dim
names(o)
plot(o,col=inferno(10))

plot(o[,,1],col=inferno(10))

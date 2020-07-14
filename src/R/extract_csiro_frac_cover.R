library(tidyverse); library(data.table); 
library(stars); library(sf); library(lubridate)
library(viridisLite); 
# library(terra)
# wget *.tif https://eo-data.csiro.au/remotesensing/v310/australia/monthly/cover/
# aust.006.tif
# wget -nc -A "*.aust.006.tif" -r -np -l1 --no-check-certificate -e robots=off https://eo-data.csiro.au/remotesensing/v310/australia/monthly/cover/

src_dir <- "/home/sami/Downloads/frac_cover/eo-data.csiro.au/remotesensing/v310/australia/monthly/cover/eo-data.csiro.au/remotesensing/v310/australia/monthly/cover"
# src_dir <- "/home/sami/Downloads/frac_cover/eo-data.csiro.au/remotesensing/v310/australia/monthly/cover"
eoz <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_Tree_Cover_5000m_East_Oz_noMask_2000_2019.tif",
                         RasterIO = list(bands=1))
o <- stars::read_stars("../data_general/Oz_misc_data/FC.v310.MCD43A4_2001_2019.nc")
frac <- st_warp(src=o, dest=eoz, use_gdal = F)


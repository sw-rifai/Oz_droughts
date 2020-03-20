# DESCRIPTION: CAST A DATAFRAME OF VOD MONNTHLY MEAN VALUES TO A NETCDF
# Author: Sami Rifai
# Date: 2020-03-16
# To Do: turn this template into a function

library(tidyverse); library(ncdf4); library(lubridate); library(arrow)
# Change these as needed!!!
out_file_name <- "LPDRv2_VODnight_monmean_2002_2019.nc"
out_dir <- "../data_general/Oz_misc_data/"
short_name <- "vod_night"
long_name <- "VOD nighttime monthly mean"
units <- ""
title <- "LPDRv2 Vegetation Optical Depth Night time Monthly Mean"
compression_level <- 6


# Part 1: get the dataframe --------------------------------
# these are commented out because it's a time consuing process job
tmp <- read_parquet("../data_general/lpdr_test/VOD_LPDRv2_OZ.parquet")
tmp <- tmp %>% mutate(date=ymd(paste(year,month,1)))
tmp <- tmp %>% rename(vod_day = vod_desc, 
                      vod_night = vod_asc)
# Part 2: Cast explict NAs for missing values -------------------------------------

vec_dates <- seq(min(tmp$date), max(tmp$date), by='1 month')
df_fulldims <- expand_grid(lon=sort(unique(tmp$lon)), 
                           lat=sort(unique(tmp$lat)),
                           date=vec_dates)
df_fulldims


pre_tc <- left_join(df_fulldims, tmp, by=c("lon","lat","date"))
pre_tc <- pre_tc %>% select(lon,lat,date,sym(short_name))
pre_tc <- pre_tc %>% arrange(lon,lat,date) # This step seems important
pre_tc
unique(pre_tc$lon) %>% plot
unique(pre_tc$lat) %>% plot
unique(pre_tc$lon) %>% plot



# Part 3: Define tbl_cube ---------------------------------------------------------
tc <- pre_tc %>% 
  select(lon,lat,date,sym(short_name)) %>% 
  as.tbl_cube(
    dim_names=c("lon","lat","date"),
    met_name=c(sym(short_name)))

methods(class=class(tc))
tc$dims$lat %>% plot
tc$dims$lon %>% plot
tc$dims$date %>% plot



# Part 4: Define dimensions -----------------------------------------------
londim <- ncdim_def("lon", "degrees_east", as.double(tc$dims$lon))
latdim <- ncdim_def("lat", "degrees_north", as.double(tc$dims$lat))
timedim <- ncdim_def("time", units="days since 1970-01-01",
                     as.numeric(tc$dims$date), unlim = T)
fillvalue <- 1e20

origin
tc$dims$date[1]
tc$dims$date[1] %>% as.numeric()
origin+seconds(tc$dims$date[1] %>% as.numeric())

# Climate variables to put into ncdf file: 
def_var <- ncvar_def(name = short_name, 
                      units=units, 
                      list(londim, latdim,timedim), 
                      fillvalue, 
                      longname = long_name, 
                      prec = "single", 
                      compression=compression_level)

ncfname <- file.path(out_dir, out_file_name)  #"test_ndvi.nc"
ncout <- nc_create(ncfname, list(def_var), force_v4 = TRUE)

# put the array

ncvar_put(ncout, def_var, tc$mets$vod_night)

# put additional attributes into dimension and data variables
ncatt_put(ncout, "lon", "axis", "X")  
ncatt_put(ncout, "lat", "axis", "Y")

# add global attributes
ncatt_put(ncout, 0, "title", title)

# close the file, writing data to disk
nc_close(ncout)

#############################################################################

# stars::read_stars(ncfname)













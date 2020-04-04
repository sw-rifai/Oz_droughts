# DESCRIPTION: CAST A DATAFRAME OF SEASONAL MEAN VALUES TO A NETCDF
# Author: Sami Rifai
# Date: 2020-03-13
# Assumptions: 
# (1) data is in lon/lat
# (2) Only putting writing one variable to file
# (3) writing the variable as a single precision
# (4) There is a working version of CDO (important for compressing)
# To Do: turn this template into a function


library(tidyverse); library(ncdf4); library(lubridate)
# Change these as needed!!!
out_file_name <- "AVHRR_CDRv5_NDVI_yearSeason_mean_1982_2019.nc"
out_dir <- "../data_general/AVHRR_NDVI_CDR_V5/"
short_name <- "NDVI"
long_name <- "AVHRR CDR v5 NDVI yearly seasonal mean"
units <- ""
title <- "AVHRR CDR v5 NDVI Monthly Mean"
compression_level <- 6


# Part 1: get the seasonal value dataframe --------------------------------
# these are commented out because it's a time consuing process job
source("src/R/plot_map_avhrr_ndvi_anom_SEOZ.R")
tmp <- tmp %>% filter(is.na(hydro_year)==F)

# define seasonal date
vec_seas_date <- tmp %>% 
  select(hydro_year, season) %>% 
  distinct() %>% 
  mutate(date = case_when(season == "DJF" ~ ymd(paste(hydro_year,1,1)), 
                          season == "MAM" ~ ymd(paste(hydro_year,4,1)), 
                          season == "JJA" ~ ymd(paste(hydro_year,7,1)), 
                          season == "SON" ~ ymd(paste(hydro_year,10,1)))) %>% 
  arrange(date)

# to be converted into spatiotemporal array
pre_tc <- inner_join(tmp, vec_seas_date, by=c("hydro_year","season"))



# Part 2: Cast explict NAs for missing values -------------------------------------
tmp$lon %>% range
vec_lon <- seq(from=min(tmp$lon), to=max(tmp$lon), by=0.04491576)
tmp$lat %>% range
vec_lat <- seq(from=min(tmp$lat), to=max(tmp$lat), by=0.04491576)

df_fulldims <- expand_grid(lon=sort(unique(tmp$lon)), 
                           lat=sort(unique(tmp$lat)),
                           date=sort(vec_seas_date$date))

pre_tc %>% dim


pre_tc <- left_join(df_fulldims, pre_tc, by=c("lon","lat","date"))
pre_tc <- pre_tc %>% select(-hydro_year, -season)
pre_tc <- pre_tc %>% arrange(lon,lat,date) # This step seems important
pre_tc
unique(pre_tc$lon) %>% plot
unique(pre_tc$lat) %>% plot
unique(pre_tc$lon) %>% plot



# Part 3: Define tbl_cube ---------------------------------------------------------
tc <- pre_tc %>% 
  select(lon,lat,date,ndvi) %>% 
  as.tbl_cube(
    dim_names=c("lon","lat","date"),
    met_name=c('ndvi'))

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
def_ndvi <- ncvar_def(name = short_name, 
                              units=units, 
                              list(londim, latdim,timedim), 
                              fillvalue, 
                              longname = long_name, 
                              prec = "single", 
                              compression=compression_level)

ncfname <- file.path(out_dir, out_file_name)  #"test_ndvi.nc"
ncout <- nc_create(ncfname, list(def_ndvi), force_v4 = TRUE)

# put the array
ncvar_put(ncout, def_ndvi, tc$mets$ndvi)

# put additional attributes into dimension and data variables
ncatt_put(ncout, "lon", "axis", "X")  
ncatt_put(ncout, "lat", "axis", "Y")

# add global attributes
ncatt_put(ncout, 0, "title", title)

# close the file, writing data to disk
nc_close(ncout)

#############################################################################















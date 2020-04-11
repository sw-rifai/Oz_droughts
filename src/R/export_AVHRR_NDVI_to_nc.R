# DESCRIPTION: CAST A DATAFRAME OF SEASONAL MEAN VALUES TO A NETCDF
# Author: Sami Rifai
# Date: 2020-03-13
# Assumptions: 
# (1) data is in lon/lat
# (2) Only putting writing one variable to file
# (3) writing the variable as a single precision
# (4) There is a working version of CDO (important for compressing)
# To Do: turn this template into a function
library(raster); 
library(tidyverse); library(ncdf4); library(lubridate)
library(stars)
# Change these as needed!!!
src_path <- "../data_general/AVHRR_NDVI_CDR_V5/AVHRR_NDVI_monmean_1982_2019.tif"
out_file_name <- "AVHRR_CDRv5_NDVI_monMean_1982_2019.nc"
out_dir <- "../data_general/AVHRR_NDVI_CDR_V5/"
short_name <- "NDVI"
long_name <- "AVHRR CDR v5 NDVI monthly means"
units <- ""
title <- "AVHRR CDR v5 NDVI Monthly Mean"
compression_level <- 6

src <- stars::read_stars(src_path)
names(src) <- short_name
vec_dates <- seq(ymd("1982-01-01"),ymd("2019-12-01"),by='1 month')
src <- stars::st_set_dimensions(src, 3, values=vec_dates, names='time')
# stars::write_stars(src, 
#                    dsn=file.path(out_dir,"AVHRR_CDRv5_NDVI_monMean_1982_2019.nc4"), 
#                    # driver="netCDF",
#                    # type='Float32', 
#                    progress=T)
# system(paste0("cdo -f nc4c -z zip_9 -r settaxis,1982-01-01,12:00:00,1mon ", 
#        file.path(out_dir, out_file_name)," ",
#        file.path(out_dir, paste0("tmp_",out_file_name))))
# 
# src <- stars::read_stars(src_path)


# system("cdo -z zip_9 -r settaxis,1990-12-01,12:00:00,1mon outputs/ML_C_fluxes/tmp_199012_201612.nc outputs/ML_C_fluxes/stem_bayesGLMM_wetDry_terraClim_chirps_199012_201612.nc")

# src <- raster::setZ(src, vec_dates, name='time')
# writeRaster(src, filename=file.path(out_dir,out_file_name), 
#             format="CDF", 
#             options="COMPRESS=DEFLATE", 
#             overwrite=TRUE)

# Part 4: Define dimensions -----------------------------------------------
st_get_dimension_values(src,which = 'x')
londim <- ncdim_def("lon", "degrees_east", as.double(st_get_dimension_values(src,which = 'x')))
latdim <- ncdim_def("lat", "degrees_north", as.double(st_get_dimension_values(src,which = 'y')))
timedim <- ncdim_def("time", units="days since 1970-01-01",
                     as.numeric(st_get_dimension_values(src,which = 'time')), unlim = T)
fillvalue <- 1e20

origin

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
ncvar_put(ncout, def_ndvi, src$NDVI)

# put additional attributes into dimension and data variables
ncatt_put(ncout, "lon", "axis", "X")  
ncatt_put(ncout, "lat", "axis", "Y")

# add global attributes
ncatt_put(ncout, 0, "title", title)

# close the file, writing data to disk
nc_close(ncout)

#############################################################################















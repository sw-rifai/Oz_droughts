library(raster); 
library(stars)
library(tidyverse); library(sf); library(lubridate)
library(gdalUtils)

# figure out the date structure ---- 
fps <- list.files("/home/sami/scratch/LandsatFracCover/", recursive = T, 
                  full.names = T)
vec_years <- as.character(1988:2019)
vec_seasons <- c("03","06","09","12")
vec_dates <- seq(ymd("1988-03-01"),length.out=length(vec_years)*4, by='3 months')

# merge the separate states by seasonal date -----------
for(i in 1:length(vec_years)){
  for(j in 1:length(vec_seasons)){
    print(c("the year is:",vec_years[i]))
    pattern <- paste0("m",vec_years[i],vec_seasons[j])
    print(pattern)
    mlist <- (fps[str_detect(fps, pattern = pattern)])
    gdalbuildvrt(gdalfile = mlist, # uses all tiffs in the current folder
                 output.vrt = "v1.vrt")
    gdal_translate(src_dataset = "v1.vrt", 
                   dst_dataset = paste0('landsatCoverFrac_',pattern,'.tif'), 
                   output_Raster = TRUE, # returns the raster as Raster*Object
                   # if TRUE, you should consider to assign 
                   # the whole function to an object like dem <- gddal_tr..
                   options = c("BIGTIFF=YES", "COMPRESSION=LZW"))
  }
}

# Subset the big tif by band, then create spatiotemporal array of that cover fraction -----
lcf_list <- list.files(pattern = "landsatCoverFrac") %>% sort
s1 <- stars::read_stars(lcf_list, RasterIO = list(band=1), 
                        along='new_dimensions')
s1[,,,1] %>% plot

fps <- list.files("/home/sami/scratch/LandsatFracCover/", recursive = T, 
           pattern = '201506', 
           full.names = T)


gdalbuildvrt(gdalfile = fps[1:4], # uses all tiffs in the current folder
             output.vrt = "v1.vrt")

gdal_translate(src_dataset = "v1.vrt", 
               dst_dataset = "v1.tif", 
               output_Raster = TRUE, # returns the raster as Raster*Object
               # if TRUE, you should consider to assign 
               # the whole function to an object like dem <- gddal_tr..
               options = c("BIGTIFF=YES", "COMPRESSION=LZW"))

s1 <- read_stars(fps[1])
s2 <- read_stars(fps[2])
s3 <- read_stars(fps[3])
s4 <- read_stars(fps[4])


r1 <- raster::raster(fps[1])
r2 <- raster::raster(fps[2])
r3 <- raster::raster(fps[3])
r4 <- raster::raster(fps[4])


origin(r1)
origin(r2)
origin(r3)
origin(r4)


raster::plot(r)
plot(r2)
mosaic(r,r2) %>% plot


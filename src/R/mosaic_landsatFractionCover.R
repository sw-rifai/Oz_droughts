library(raster); 
library(stars); library(data.table); 
library(tidyverse); library(sf); library(lubridate); library(dtplyr)
library(gdalUtils)
library(foreach); library(doParallel)

# figure out the date structure ---- 
scratch_dir <- "/home/sami/scratch/"
fps <- list.files("/home/sami/scratch/LandsatFracCover/", recursive = T, 
                  full.names = T)
file.remove(fps[str_detect(fps, "xml")]) # damn qgis creates xml
vec_years <- as.character(1988:2019)
vec_seasons <- c("03","06","09","12")
vec_dates <- seq(ymd("1987-12-01"),length.out=length(fps)/4, by='3 months')
ref_grid <- stars::read_stars('../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif', 
                          RasterIO = list(bands=1))

# Foreach approach: extract the separate states by seasonal date -----------
dat <- list()
n_cores <- 20
cl <- makeCluster(n_cores)
registerDoParallel(cl)
tmp <- foreach(i = 1:length(vec_dates), 
               .packages = c("tidyverse","lubridate","stars","data.table")
               # .combine=rbind
               ) %dopar% {
                 pattern <- paste0("m",
                                   substr(vec_dates[i],1,4),
                                   substr(vec_dates[i],6,7))
                 print(pattern)
                 mlist <- (fps[str_detect(fps, pattern = pattern)])
                 
                 s1 <- stars::read_stars(mlist[1]) %>% st_warp(., ref_grid)
                 names(s1) <- "coverFraction"
                 s1 <- stars::st_set_dimensions(s1, 3, values = c("soil","gv","npv","alpha"),names='coverFraction')
                 s1 <- as.data.frame(s1) %>% 
                   filter(coverFraction.1>0) %>% 
                   mutate(coverFraction.1 = coverFraction.1-100)
                 
                 s2 <- stars::read_stars(mlist[2]) %>% st_warp(., ref_grid)
                 names(s2) <- "coverFraction"
                 s2 <- stars::st_set_dimensions(s2, 3, values = c("soil","gv","npv","alpha"),names='coverFraction')
                 s2 <- as.data.frame(s2) %>% 
                   filter(coverFraction.1>0) %>% 
                   mutate(coverFraction.1 = coverFraction.1-100)
                 
                 s3 <- stars::read_stars(mlist[3]) %>% st_warp(., ref_grid)
                 names(s3) <- "coverFraction"
                 s3 <- stars::st_set_dimensions(s3, 3, values = c("soil","gv","npv","alpha"),names='coverFraction')
                 s3 <- as.data.frame(s3) %>% 
                   filter(coverFraction.1>0) %>% 
                   mutate(coverFraction.1 = coverFraction.1-100)
                 
                 s4 <- stars::read_stars(mlist[4]) %>% st_warp(., ref_grid)
                 names(s4) <- "coverFraction"
                 s4 <- stars::st_set_dimensions(s4, 3, values = c("soil","gv","npv","alpha"),names='coverFraction')
                 s4 <- as.data.frame(s4) %>% 
                   filter(coverFraction.1>0) %>% 
                   mutate(coverFraction.1 = coverFraction.1-100)
                 
                 out <- bind_rows(s1,s2,s3,s4) %>% filter(is.na(coverFraction.1)==F)
                 tmp_date <- lubridate::ymd(paste(substr(pattern,2,5),substr(pattern,6,7),1))
                 out$date <- tmp_date
                 setDT(out)
                 out <- out %>% 
                   dcast(., x+y+date~coverFraction, 
                         value.var='coverFraction.1', 
                         fun.aggregate=mean, na.rm=TRUE) %>% 
                   select(-alpha)
                 out
               }
dat <- rbindlist(tmp)
dat[, npv:=npv/100]
dat[, gv:=gv/100]
dat[, soil:=soil/100]
arrow::write_parquet(dat, sink="../data_general/LandsatFracCover/parquet/SeasLandsatCoverFrac_0.05deg_1988_2019.parquet")



# For-loop approach ------------------------------------------------------------
# for(i in 1:length(vec_dates)){
#     print(paste("the date is:",vec_dates[i]))
#     pattern <- paste0("m",
#                       substr(vec_dates[i],1,4),
#                       substr(vec_dates[i],6,7))
#     print(pattern)
#     mlist <- (fps[str_detect(fps, pattern = pattern)])
#     
#     s1 <- stars::read_stars(mlist[1]) %>% st_warp(., ref_grid)
#     names(s1) <- "coverFraction"
#     s1 <- stars::st_set_dimensions(s1, 3, values = c("soil","gv","npv","alpha"),names='coverFraction')
#     s1 <- as.data.frame(s1)
#     
#     s2 <- stars::read_stars(mlist[2]) %>% st_warp(., ref_grid)
#     names(s2) <- "coverFraction"
#     s2 <- stars::st_set_dimensions(s2, 3, values = c("soil","gv","npv","alpha"),names='coverFraction')
#     s2 <- as.data.frame(s2)
#     
#     s3 <- stars::read_stars(mlist[3]) %>% st_warp(., ref_grid)
#     names(s3) <- "coverFraction"
#     s3 <- stars::st_set_dimensions(s3, 3, values = c("soil","gv","npv","alpha"),names='coverFraction')
#     s3 <- as.data.frame(s3)
#     
#     s4 <- stars::read_stars(mlist[4]) %>% st_warp(., ref_grid)
#     names(s4) <- "coverFraction"
#     s4 <- stars::st_set_dimensions(s4, 3, values = c("soil","gv","npv","alpha"),names='coverFraction')
#     s4 <- as.data.frame(s4)
#     
#     
#     out <- bind_rows(s1,s2,s3,s4) %>% filter(is.na(coverFraction.1)==F)
#     out$yearSeason <- pattern
#     setDT(out)
#     out <- out %>% 
#       dcast(., x+y+yearSeason~coverFraction, 
#             value.var='coverFraction.1', 
#             fun.aggregate=first)
# 
#     dat[[i]] <- out
#   }

# dat <- dat %>% 
#   lazy_dt() %>% 
#   select(-alpha) %>% 
#   mutate(date = lubridate::ymd(paste(substr(yearSeason,2,5),substr(yearSeason,6,7),1))) %>% 
#   select(-yearSeason) %>% 
#   as.data.table()
# 
# 
# dat %>% 
#   group_by(date) %>% 
#   summarize(val = mean(soil,na.rm=TRUE)) %>% 
#   ungroup() %>% 
#   ggplot(data=.,aes(date,val))+geom_line()
# 
# dat %>% 
#   filter(date==ymd("2020-03-01")) %>% 
#   ggplot(data=.,aes(x,y,fill=npv))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c()
# 
# 
# s1 %>% head
# s1 %>% filter(coverFraction=='npv') %>% 
#   filter(coverFraction.1>0) %>% 
#   ggplot(data=.,aes(x,y,fill=coverFraction.1))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c()



# ref_raster <- raster('../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif',band=1)
# ref_raster
# 
# r1 <- raster::stack(mlist[1])
# r1 <- raster::projectRaster(r1,to=ref_raster)
# 
# r2 <- raster::stack(mlist[2])
# r2 <- raster::projectRaster(r2,to=ref_raster)
# 
# r3 <- raster::stack(mlist[3])
# r3 <- raster::projectRaster(r3,to=ref_raster)
# 
# r4 <- raster::stack(mlist[4])
# r4 <- raster::projectRaster(r4,to=ref_raster)
# 
# r_mos <- mosaic(r1,r2,r3,r4,fun=mean)
# plot(r_mos)
# 
# s <- stars::read_stars(fps[1])
# s <- stars::st_warp(s,dest = ref_grid)
# sf::sf_project()
# 
# 
# # merge the separate states by seasonal date -----------
# for(i in 1:length(vec_years)){
#   for(j in 1:length(vec_seasons)){
#     print(c("the year is:",vec_years[i]))
#     pattern <- paste0("m",vec_years[i],vec_seasons[j])
#     print(pattern)
#     mlist <- (fps[str_detect(fps, pattern = pattern)])
#     gdalbuildvrt(gdalfile = mlist, # uses all tiffs in the current folder
#                  output.vrt = "/home/sami/scratch/landsat_scratch/v1.vrt")
#     gdal_translate(src_dataset = "/home/sami/scratch/landsat_scratch/v1.vrt", 
#                    dst_dataset = paste0('/home/sami/scratch/landsat_scratch/landsatCoverFrac_',pattern,'.tif'), 
#                    output_Raster = TRUE, # returns the raster as Raster*Object
#                    # if TRUE, you should consider to assign 
#                    # the whole function to an object like dem <- gddal_tr..
#                    options = c("BIGTIFF=YES", "COMPRESSION=LZW"))
#   }
# }
# 
# # Subset the big tif by band, then create spatiotemporal array of that cover fraction -----
# lcf_list <- list.files(pattern = "landsatCoverFrac") %>% sort
# s <- stars::read_stars(lcf_list[1])#, RasterIO = list(bands=1))
# names(s) <- "coverFraction"
# s <- stars::st_set_dimensions(s, 3, values = c("soil","gv","npv","alpha"),names='coverFraction')
# 
# i <- 3
# o <- stars::read_stars(lcf_list[i])#, RasterIO = list(bands=1))
# names(o) <- "coverFraction"
# o <- stars::st_set_dimensions(o, 3, values = c("soil","gv","npv","alpha"),names='coverFraction')
# s <- c(s,o, along='date')
# s
# 
# st_get_dimension_values(s,4)
# 
# 
# s1 <- stars::read_stars(lcf_list[1], RasterIO = list(bands=1))
# s2 <- stars::read_stars(lcf_list[2], RasterIO = list(bands=1))
# s <- c(s1,s2)
# names(s) <- 'soil'
# 
# 
# c(stars::read_stars(lcf_list[1],RasterIO=list(bands=1)),
#   stars::read_stars(lcf_list[2],RasterIO=list(bands=1)), 
#   along='band')
# 
# 
# 
# fps <- list.files("/home/sami/scratch/LandsatFracCover/", recursive = T, 
#            pattern = '201506', 
#            full.names = T)
# 
# 
# gdalbuildvrt(gdalfile = fps[1:4], # uses all tiffs in the current folder
#              output.vrt = "v1.vrt")
# 
# gdal_translate(src_dataset = "v1.vrt", 
#                dst_dataset = "v1.tif", 
#                output_Raster = TRUE, # returns the raster as Raster*Object
#                # if TRUE, you should consider to assign 
#                # the whole function to an object like dem <- gddal_tr..
#                options = c("BIGTIFF=YES", "COMPRESSION=LZW"))
# 
# s1 <- read_stars(fps[1])
# s2 <- read_stars(fps[2])
# s3 <- read_stars(fps[3])
# s4 <- read_stars(fps[4])
# 
# 
# r1 <- raster::raster(fps[1])
# r2 <- raster::raster(fps[2])
# r3 <- raster::raster(fps[3])
# r4 <- raster::raster(fps[4])
# 
# 
# origin(r1)
# origin(r2)
# origin(r3)
# origin(r4)
# 
# 
# raster::plot(r)
# plot(r2)
# mosaic(r,r2) %>% plot
# 

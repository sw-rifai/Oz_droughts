library(raster); library(rasterVis)
library(arrow)
library(sf); library(stars)
library(tidyverse); library(lubridate);
# grid <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_NIRV_monmean_EastOz_1982_2019.tif", 
#                           RasterIO = list(bands=1))
# NVIS and NIRV (forest & woodlands) ------------------------------------------------
dat <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_NIRV_monmean_EastOz_1982_2019.tif")
names(dat) <- "nirv"
vec_dates <- seq(ymd("1982-01-01"),ymd("2019-12-01"),by="1 month")
dat <- st_set_dimensions(dat, 3, values=vec_dates, names='date')
nvis <- stars::read_stars("../data_general/NVIS/nvis51_majorVegClass_0p05.tif")
nvis2 <- st_warp(src=nvis, dest=dat[,,,1], use_gdal = T)
names(nvis2) <- "veg_class"
nvis <- nvis2 %>% as_tibble()
codes <- readr::read_fwf("../data_general/NVIS/nvis51_majorVegClass_codes.txt", 
                         fwf_widths(c(2,100)), skip = 1) %>% 
  set_names(c("veg_class","veg_class_descrip")) %>% 
  mutate(vc = as.factor(veg_class_descrip))
nvis <- inner_join(nvis, codes, by='veg_class')
nvis <- nvis %>% filter(veg_class <= 15) # !!! only forests and woodlands !!!
nvis <- nvis %>% select(-veg_class_descrip)
dat <- dat %>% as_tibble()
dat <- inner_join(nvis, dat, by=c('x','y'))

########################################################################
# extract potential evaporation ------------------------------------------------
########################################################################
coords <- dat %>% select(x,y) %>% 
  distinct() %>% 
  st_as_sf(coords = c("x","y"), crs=4326)
fp <- "../data_general/clim_grid/era5-land/Oz/Oz/Oz_era5-land_pet_1981_2019.nc"
vec_dates <- stars::read_ncdf(fp, var="time")
vec_dates <- vec_dates %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$x <- st_coordinates(coords)[,"X"]
ex$y <- st_coordinates(coords)[,"Y"]
ex <- ex %>% 
  pivot_longer(c(-x,-y), 
               names_to="date", values_to="pet")
ex <- ex %>% 
  mutate(date=ymd(date)) %>% 
  mutate(days_in_month = days_in_month(date)) 
ex <- ex %>% 
  mutate(pet = pet*days_in_month*-1000)
# ex %>% 
#   write_parquet(., sink="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_PET_1979_2019.parquet", 
#                 compression = "snappy")  
dpet <- ex %>% 
  filter(is.na(pet)==F) %>% 
  select(-days_in_month)
rm(ex,junk,vec_dates,fp)
gc()
########################################################################
# *** END ***
########################################################################

########################################################################
# extract precip -------------------------------------------------------
########################################################################
coords <- dat %>% select(x,y) %>% 
  distinct() %>% 
  st_as_sf(coords = c("x","y"), crs=4326)
fp <- "../data_general/clim_grid/era5-land/Oz/Oz/Oz_era5-land_monmean_precip_1979_2019.nc"
vec_dates <- stars::read_ncdf(fp, var="time")
vec_dates <- vec_dates %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$x <- st_coordinates(coords)[,"X"]
ex$y <- st_coordinates(coords)[,"Y"]
ex <- ex %>% 
  pivot_longer(c(-x,-y), 
               names_to="date", values_to="precip")
ex <- ex %>% 
  mutate(date=ymd(date)) %>% 
  mutate(days_in_month = days_in_month(date)) 
ex <- ex %>% 
  mutate(precip = precip*days_in_month*1000)
# ex %>% 
#   write_parquet(., sink="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_PET_1979_2019.parquet", 
#                 compression = "snappy")  
dprecip <- ex %>% 
  filter(is.na(precip)==F) %>% 
  select(-days_in_month)
rm(ex,junk,vec_dates,fp)
gc()
################################################################
# *** END ***
################################################################


################################################################
# *** Join clim and VI
################################################################
clim <- inner_join(dpet, dprecip, by=c("x","y","date"))
rm(dpet, dprecip)
dat <- left_join(clim, dat, by=c("x","y","date"))

################################################################
# *** END ***
################################################################


################################################################
# *** CALC NORMS AND ANOMS ***
################################################################
library(data.table); library(dtplyr)
dat <- as.data.table(dat)
ldat <- dat %>% lazy_dt()
norms <- dat %>% 
  filter(date >= ymd('1982-01-01') & 
           date <= ymd('2011-12-31')) %>% 
  mutate(month=month(date)) %>% 
  group_by(x,y,month) %>% 
  summarize(pet_u = mean(pet, na.rm=T), 
            precip_u = mean(precip, na.rm=T), 
            nirv_u = mean(nirv, na.rm=T), 
            pet_sd = sd(pet, na.rm=T), 
            precip_sd = sd(precip, na.rm=T), 
            nirv_sd = sd(nirv, na.rm=T)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  filter(is.na(nirv_u)==F)
norms <- inner_join(norms, 
           norms %>% 
  group_by(x,y) %>% 
  summarize(map = sum(precip_u,na.rm=T), 
            mpet = sum(pet_u)) %>% 
  ungroup(), 
  by=c('x','y'))


dat <- left_join(dat, 
                 dat %>% 
  filter(date >= ymd('1982-01-01') & 
           date <= ymd('2011-12-31')) %>% 
  mutate(year=year(date)) %>% 
  group_by(x,y,year) %>% 
  summarize(ap = sum(precip,na.rm=T), 
            apet = sum(pet, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(x,y) %>% 
  summarize(ap_sd = sd(ap,na.rm = T), 
            apet_sd = sd(apet, na.rm = T)) %>% 
  ungroup(), 
  by=c("x","y"))
  


dat <- left_join(dat %>% mutate(month=month(date)), 
                 norms, 
                 by=c('x','y','month')) %>% 
  filter(veg_class <= 15) %>% 
  mutate(pet_anom = pet - pet_u, 
         precip_anom = precip - precip_u, 
         nirv_anom = nirv - nirv_u) %>% 
  mutate(pet_anom_sd = pet_anom/pet_sd, 
         precip_anom_sd = precip_anom/precip_sd, 
         nirv_anom_sd = nirv_anom/nirv_sd)

library(RcppRoll)
dat <- dat %>% 
  group_by(x,y) %>% 
  arrange(date) %>% 
  mutate(pet_12mo = roll_sumr(pet, n=12, fill=NA), 
         precip_12mo = roll_sumr(precip, n=12, fill=NA),
         pet_anom_min_3mo = roll_minr(pet_anom, n = 3, fill = NA), 
         pet_anom_min_6mo = roll_minr(pet_anom, n = 6, fill = NA), 
         pet_anom_min_9mo = roll_minr(pet_anom, n = 9, fill = NA), 
         pet_anom_min_12mo = roll_minr(pet_anom, n = 12, fill = NA), 
         pet_anom_max_3mo = roll_maxr(pet_anom, n = 3, fill = NA), 
         pet_anom_max_6mo = roll_maxr(pet_anom, n = 6, fill = NA), 
         pet_anom_max_9mo = roll_maxr(pet_anom, n = 9, fill = NA), 
         pet_anom_max_12mo = roll_maxr(pet_anom, n = 12, fill = NA),
         precip_anom_min_3mo = roll_minr(precip_anom, n = 3, fill = NA), 
         precip_anom_min_6mo = roll_minr(precip_anom, n = 6, fill = NA), 
         precip_anom_min_9mo = roll_minr(precip_anom, n = 9, fill = NA), 
         precip_anom_min_12mo = roll_minr(precip_anom, n = 12, fill = NA), 
         precip_anom_max_3mo = roll_maxr(precip_anom, n = 3, fill = NA), 
         precip_anom_max_6mo = roll_maxr(precip_anom, n = 6, fill = NA), 
         precip_anom_max_9mo = roll_maxr(precip_anom, n = 9, fill = NA), 
         precip_anom_max_12mo = roll_maxr(precip_anom, n = 12, fill = NA)) %>% 
  ungroup() %>% 
  rename(mapet=mpet) %>% 
  mutate(pet_anom_12mo = pet_12mo - mapet, 
         precip_anom_12mo = precip_12mo - map)


################################################################
# *** END ***
################################################################

################################################################
# *** WRITE TO DISK ***
################################################################
write_parquet(dat, 
              sink=paste0("../data_general/Oz_misc_data/nirv_nvis_clim_",Sys.Date(),".parquet"), 
              compression = 'snappy')
################################################################
# *** END ***
################################################################


################################################################
# Extract LAI for unique coordintes in dat ---
################################################################
vec_coords <- sf::st_as_sf(dat %>% select(x,y) %>% distinct, 
                           coords = c("x","y"), crs=4326)
junk <- stars::read_stars("../data_general/AVHRR_LAI_FAPAR_CDR_V5/AVHRR_LAI_monMedian__1982_2019.tif")
names(junk) <- "lai"
st_crs(junk) <- st_crs(4326)
junk <- st_crop(junk, st_bbox(c(xmin = 137,
                                ymin = -44,
                                xmax = 154,
                                ymax = -10), 
                              crs = st_crs(4326)))
junk <- st_set_dimensions(junk, 3, values=seq(ymd('1982-01-01'), ymd('2019-12-01'), by='1 month'), 
                  names='date')
vec_coords %>% 
  as_tibble() %>% 
  first()
o <- aggregate(junk, 
               vec_coords %>% 
                 as_tibble() %>% 
                 first(), 
               dplyr::first, as_points = T) %>%
  as_tibble()
ot <- o %>% as.data.frame() %>% as_tibble()
write_parquet(ot, sink="../data_general/AVHRR_LAI_FAPAR_CDR_V5/AVHRR_LAI_monMedian_NVIS_TreeVegClasses_1982_2019.parquet", 
              compression='snappy')
ot <- read_parquet("../data_general/AVHRR_LAI_FAPAR_CDR_V5/AVHRR_LAI_monMedian_NVIS_TreeVegClasses_1982_2019.parquet")
ot <- ot %>% 
  rowwise() %>% 
  mutate(x = unlist(geometry)[1],
         y = unlist(geometry)[2]) %>% 
  ungroup() %>% 
  select(-geometry)
norms_lai <- ot %>% 
  mutate(month=month(date), 
         year=year(date)) %>% 
  filter(date >= ymd('1982-09-01') & 
           date <= ymd('2011-12-31')) %>% 
  group_by(x,y,month) %>% 
  summarize(lai_u = mean(lai,na.rm=T), 
            lai_sd = sd(lai,na.rm=T)) %>% 
  ungroup()
norms_lai <- inner_join(norms_lai, 
                        norms_lai %>% 
                          group_by(x,y) %>% 
                          summarize(lai_min = min(lai_u), 
                                    lai_max = max(lai_u)) %>% 
                          ungroup() %>% 
                          mutate(lai_amp_u = lai_max - lai_min), 
                        by=c("x","y"))
ot <- left_join(ot %>% mutate(month=month(date)), 
                norms_lai, by=c("x","y","month"))
rm(norms_lai); 
gc()
write_parquet(ot, sink="../data_general/AVHRR_LAI_FAPAR_CDR_V5/AVHRR_LAI_monMedian_NVIS_TreeVegClasses_1982_2019.parquet", 
              compression='snappy')



# Import data -------------------------------------------------------------
dat <- read_parquet("../data_general/Oz_misc_data/nirv_nvis_clim_2020-04-04.parquet")
# !!! IMPORTANT !!! WHAT VEG CLASSES ARE SELECTED
dat <- dat %>% filter(date >= ymd('1981-09-01') & 
                        date <= ymd('2019-09-01')) # SS to before fire
dat <- dat %>% filter(veg_class <= 13) # SS to only veg classes w/Euc or rainforest trees
dim(dat); gc()

# Auxiliary covariates not yet in data ------------------------------------
# tree cover --- 
vec_coords <- sf::st_as_sf(dat %>% select(x,y) %>% distinct, 
                           coords = c("x","y"), crs=4326)
tree_cov <- raster::raster("../data_general/Oz_misc_data/Oz_CGLS-LC100_500m.tif", 
                           band=14)
junk <- velox::velox(tree_cov)
ex <- junk$extract_points(sp = vec_coords)
ex <- as_tibble(ex)
ex <- ex %>% 
  rename(tree_cov = V1) %>% 
  mutate(x=st_coordinates(vec_coords)[,"X"], 
         y=st_coordinates(vec_coords)[,"Y"])
tree_cov <- ex
rm(ex); gc()

# Import LAI ------
ot <- read_parquet("../data_general/AVHRR_LAI_FAPAR_CDR_V5/AVHRR_LAI_monMedian_NVIS_TreeVegClasses_1982_2019.parquet")
# end ---


dat <- left_join(dat, 
                 ot %>% select(-month), 
                 by=c('x','y','date'))
rm(ot); gc()
dat <- dat %>% 
  mutate(month=month(date), 
         ddate = decimal_date(date)) %>% 
  mutate(pet_anom_sd = pet_anom/pet_sd, 
         precip_anom_sd = precip_anom/precip_sd, 
         nirv_anom_sd = nirv_anom/nirv_sd, 
         precip_anom_sd_12mo = precip_anom_12mo/ap_sd, 
         pet_anom_sd_12mo = pet_anom_12mo/apet_sd)

# Last things to calc -----------------------------------------------------
dat <- dat %>% 
  mutate(pet_precip = pet/precip,
         pet_precip_anom_12mo = pet_anom_12mo/precip_anom_12mo,
         ma_pet_precip = mapet/map)
write_parquet(dat, 
              sink=paste0('../data_general/AVHRR_CDRv5_VI/ard_nirv_',Sys.Date(),".parquet"), 
              compression='gzip', 
              compression_level = 9)


# nearest neighbor approach to extract
# nn_coords <- RANN::nn2(
#   coords_lonlat_vod %>% st_coordinates(),
#   coords_vi %>% st_coordinates()
# ) 
# nn_coords$nn.idx[,1] %>% hist
# coords_lonlat_vod <- coords_lonlat_vod %>% 
#   mutate(idx_vod = row_number()) %>% 
#   mutate(lat = st_coordinates(.)[,2], 
#          lon = st_coordinates(.)[,1])
################################################################
# *** END ***
################################################################



################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################












# grid <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_NIRV_monmean_EastOz_1982_2019.tif")
# grid <- st_set_dimensions(grid, 3, values=vec_dates, names='time')
# dpet <- read_ncdf("../data_general/clim_grid/era5-land/Oz/Oz/Oz_era5-land_pet_1981_2019.nc")
# dpet <- dpet[,,,13:(456+11)]
# grid <- grid[,,,1:455]
# blank_grid <- st_warp(src=dpet, 
#                       dest=grid,
#                       use_gdal = T)
# 
# dpet[,,,1] %>% st_as_stars() %>% 
#   st_warp(grid) -> dpet2
# st_bbox(grid)
# st_crs(grid)
# stars::st_dimensions(grid)
# 
# blank_grid <- st_warp(src=dpet, 
#                 dest=grid,
#                 # crs = st_crs(grid), 
#                 # cellsize = attr(grid, 'dimensions')$x$delta,
#                 use_gdal = T)
# 
# attr(grid, 'dimensions')$x$delta
# 
# blank_grid <- st_as_stars(grid, data=NA)
# blank_grid <- st_set_dimensions(blank_grid, 3, values=vec_dates, names='date')
# 
# 
# rep(grid,2) %>% str
# grid
# 
# 
# plot(dpet[,,,1])
# 
# 
# dat %>% 
#   filter(date==min(date)) %>% 
#   ggplot(data=., aes(x,y,fill=nirv))+
#   geom_tile()+
#   scale_fill_viridis_b()+
#   coord_equal()
# 
# #
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# st_crs(dat)
# nvis2 %>% plot
# 
# 
# 
# 
# methods(class=class(dat))
# dat[,,,1] %>% plot
# plot(dat[,,,1], 
#      col=viridis::inferno(20,direction = -1, begin = 0), 
#      breaks=seq(0,0.3,length.out = 21))
# help(Math)
# 
# mean(dat[,,,1:10])
# st_get_dimension_values(dat, which = 4)
# 
# st_apply(dat[,,,1], c("x", "y"), mean)
# seq(12,by=12,length.out=30)
# 
# 
# 
# 
# 
# 
# 
# 
# # CHANGE THIS STUFF -------------------------------------------------------
# product_name_for_fig <- "AVHRR CDR v5 - NDVI"
# region <- "SEAUS"
# ref_years <- 1982:2010
# deg_coarse <- 0.125
# color_theme <- "BrBG"
# 
# # Part 1 - Import the TIFF ------------------------------------------------
# dat <- stars::read_stars("../data_general/AVHRR_NDVI_CDR_V5/AVHRR_NDVI_monmean_SEAUS_buffer_1982_2019.tif")
# vec_dates <- seq(ymd("1982-01-01"),ymd("2019-12-01"),by="1 month")
# vec_dates <- tibble(date=vec_dates, 
#                     band=1:dim(dat)[3])
# vec_dates <- vec_dates %>% distinct(date,band) %>% 
#   mutate(quarter = lubridate::quarter(date, fiscal_start = 11))
# vec_dates <- vec_dates %>% 
#   mutate(q = case_when(quarter==1~"DJF",
#                        quarter==2~"MAM",
#                        quarter==3~"JJA",
#                        quarter==4~"SON"))
# vec_dates <- vec_dates %>% mutate(season = factor(q,
#                                                   levels=c("DJF","MAM","JJA","SON"), 
#                                                   ordered=T))
# vec_dates <- vec_dates %>% 
#   mutate(hydro_year = year(date+months(1)))
# vec_dates <- vec_dates %>% select(date,season, hydro_year)
# vec_dates$band <- 1:dim(vec_dates)[1]
# dat <- dat %>% as_tibble()
# names(dat) <- c("lon","lat","band","ndvi")
# dat <- dat %>% filter(is.na(ndvi)==F); 
# gc();
# dat <- dat %>% filter(lon>=140 & lat <= -28 & lon<=154 & lat>= -40 )
# gc(reset = T);
# dat %>% dim
# dat <- inner_join(dat, vec_dates, by='band')
# gc()
# 
# 
# 
# 
# # Part 2 - Process the Anomaly --------------------------------------------
# library(data.table)
# library(dtplyr)
# library(dplyr, warn.conflicts = FALSE);
# source("src/R/helper_funs_Oz_droughts.R")
# 
# dat <- as.data.table(dat)
# gc()
# 
# # data.table stuff
# dt_dat <- lazy_dt(dat)
# 
# dat <- dat %>% 
#   mutate(lon = deg_coarse*round(lon/deg_coarse), 
#          lat = deg_coarse*round(lat/deg_coarse))
# 
# 
# dt_dat %>% 
#   group_by(season) %>% 
#   summarize(val = mean(ndvi)) %>% 
#   ungroup() %>% 
#   as_tibble()
# 
# tmp_norms <- dt_dat %>% 
#   filter(hydro_year %in% ref_years) %>% 
#   group_by(lon,lat,season) %>% 
#   summarize(ndvi_u = mean(ndvi),
#             ndvi_sd = sd(ndvi)) %>% 
#   ungroup() %>% 
#   as_tibble() # important to tell data.table to do the calculation
# tmp <- dt_dat %>% 
#   group_by(lon,lat,hydro_year,season) %>% 
#   summarize(ndvi = mean(ndvi)) %>% 
#   ungroup() %>% 
#   as_tibble() # important to tell data.table to do the calculation
# 
# tmp <- inner_join(tmp,tmp_norms,by=c('lon','lat','season'))
# # rm(dat); rm(dt_dat); gc(); 
# 
# 
# 
# 
# 
# 
# 
# # Part 3 - Map the Anomaly ------------------------------------------------
# vec_cols <- RColorBrewer::brewer.pal(n=7, name = color_theme)
# p <- tmp%>% 
#   mutate(ndvi_anom = ndvi - ndvi_u) %>% 
#   mutate(ndvi_anom_sd = ndvi_anom/ndvi_sd) %>% 
#   filter(hydro_year %in% c(2000:2019)) %>% 
#   ggplot(data=., aes(lon,lat,fill=ndvi_anom_sd))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_gradient2(expression(paste(NDVI~Anom.~(sigma))),
#                        high=vec_cols[7], low=vec_cols[1], 
#                        mid=vec_cols[4], 
#                        limits=c(-3,3))+
#   scale_x_continuous(expand=c(0,0))+
#   scale_y_continuous(expand=c(0,0))+
#   labs(title=product_name_for_fig)+
#   facet_grid(season~hydro_year)+
#   theme_linedraw()+
#   blah_theme+
#   theme(legend.position = 'bottom', 
#         legend.key.width = unit(2,'cm'))
# fig_filename <- paste0(
#   "figures/",
#   "map_", 
#   gsub(product_name_for_fig,pattern=' ',replacement = "_"), 
#   "_anom_seasonal_",region,"_",
#   Sys.Date(),".png"
# )
# 
# ggsave(p, filename = fig_filename, 
#        width=42,height=12,units='cm',dpi='retina')

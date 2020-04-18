library(raster); library(rasterVis)
library(arrow)
library(sf); library(stars)
library(tidyverse); library(lubridate);
library(data.table)
setDTthreads(threads=8)


#*******************************************************************************
# Get NVIS Coords ---------------------------------------------------------
#*******************************************************************************
base <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_NIRV_monmean_EastOz_1982_2019.tif",
                          RasterIO = list(bands=1))
nvis <- stars::read_stars("../data_general/NVIS/nvis51_majorVegClass_0p05.tif")
nvis2 <- st_warp(src=nvis, dest=base[,,], use_gdal = T)
names(nvis2) <- "veg_class"
nvis <- nvis2 %>% as_tibble() %>% as.data.table()
codes <- readr::read_fwf("../data_general/NVIS/nvis51_majorVegClass_codes.txt", 
                         fwf_widths(c(2,100)), skip = 1) %>% 
  set_names(c("veg_class","veg_class_descrip")) %>% 
  mutate(vc = as.factor(veg_class_descrip))
nvis <- inner_join(nvis, codes, by='veg_class')
nvis <- nvis %>% filter(veg_class <= 15) # !!! only forests and woodlands !!!
rm(base); #
#*******************************************************************************
#* END SECTION
#*******************************************************************************



#*******************************************************************************
# Extract AWAP PRECIP grid cells for east Oz ----------------------------------------------
#*******************************************************************************
aprecip <- stars::read_ncdf("../data_general/clim_grid/awap/AWAP/monthly/rain/AWAP_monthly_rain_1970_2019.nc")
names(aprecip) <- "precip"

eoz_box <- st_bbox(c(xmin = min(nvis$x),
          ymin = min(nvis$y),
          xmax = max(nvis$x),
          ymax = max(nvis$y)), 
        crs = st_crs(4326))
aprecip <- st_crop(aprecip, eoz_box)
aprecip <- aprecip %>% as_tibble() %>% as.data.table()
aprecip <- aprecip %>% units::drop_units()
coords_awap <- aprecip %>% select(longitude,latitude) %>% distinct()
coords_awap <- coords_awap %>% rename(x=longitude, y=latitude)
coords_awap_sf <- st_as_sf(coords_awap, coords = c('x','y'))

coords_vi <- nvis %>% select(x,y) %>% distinct()
coords_vi <- st_as_sf(coords_vi, coords = c("x","y"))
st_crs(coords_vi) <- st_crs(4326)
nn_coords <- RANN::nn2(
  coords_awap_sf %>% st_coordinates(),
  coords_vi %>% st_coordinates(), 
  k=1
)
# df of all coords in awap with idx
coords_keep_awap <- coords_awap %>% 
  mutate(idx_awap = row_number())

# subset df of awap coords to just those identified by nn_coords
coords_keep_awap <- coords_keep_awap[nn_coords$nn.idx[,1],] %>% as.data.table()

coords_dict <- tibble(x_vi=st_coordinates(coords_vi)[,"X"], 
                          y_vi=st_coordinates(coords_vi)[,"Y"], 
                          x_clim=coords_keep_awap$x, 
                          y_clim=coords_keep_awap$y)
coords_dict <- setDT(coords_dict)

# test if awap coords object has equal number of rows as coords_vi
assertthat::are_equal(dim(coords_keep_awap)[1], dim(coords_vi)[1])
# vis check that vi and clim coords are close
coords_dict %>% ggplot(data=., aes(x_vi,x_clim))+geom_point()
coords_dict %>% ggplot(data=., aes(y_vi,y_clim))+geom_point()
coords_dict %>% head
coords_dict <- coords_dict %>% rename(x=x_clim,y=y_clim) #!!!


# norms_p[norms_pet, on=.(x,y,month)]
aprecip <- aprecip %>% rename(x=longitude,y=latitude)
setDT(aprecip)
setkey(aprecip,"x","y","time")
setkey(coords_keep_awap,"x","y")

# left join keeping all coords in "coords_keep_awap"
aprecip <- aprecip[coords_keep_awap,on=.(x,y)]

# Count NA values by time 
# aprecip[, lapply(.SD,  function(x) sum(is.na(precip))), time]

coords_dict <- coords_dict %>% rename(x=x_clim,y=y_clim) #!!!
coords_dict2 %>% head
# aprecip <- coords_dict[aprecip,on=.(x_clim=x,y_clim=y),allow.cartestian=TRUE]
# # aprecip <- coords_dict[aprecip,on=.(x_clim=x,y_clim=y),allow.cartestian=TRUE] # !!! non working
# coords_dict[by=.EACHI][aprecip,on=.(x=x,y=y),by=.EACHI]
# aprecip[,,by=.EACHI][coords_dict,on=.(x=x,y=y),by=.EACHI]
# 
# merge(coords_dict, aprecip,by=c("x","y"),all=TRUE)

# complicated way of doing full join
aprecip <- merge(aprecip,coords_dict,by=c("x","y"),all=TRUE,allow.cartesian=TRUE)

# visual check
tmp[time%in%c(ymd("1990-01-01",tz='UTC'),
  ymd("2019-12-01",tz='UTC'))] %>%
  as_tibble() %>% 
  ggplot(data=., aes(x,y,fill=precip))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(direction = -1)+
  facet_grid(~as.factor(time))

aprecip$x %>% unique
st_coordinates(coords_vi) %>% unique
#*******************************************************************************
# END SECTION
#*******************************************************************************
 
 
 









#*******************************************************************************
# NVIS and NIRV (forest & woodlands) ------------------------------------------------
#*******************************************************************************
base <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_NIRV_monmean_EastOz_1982_2019.tif")
names(base) <- "nirv"
vec_dates <- seq(ymd("1982-01-01"),ymd("2019-12-01"),by="1 month")
base <- st_set_dimensions(base, 3, values=vec_dates, names='date')
nvis <- stars::read_stars("../data_general/NVIS/nvis51_majorVegClass_0p05.tif")
nvis2 <- st_warp(src=nvis, dest=base[,,,1], use_gdal = T)
names(nvis2) <- "veg_class"
nvis <- nvis2 %>% as_tibble() %>% as.data.table()
codes <- readr::read_fwf("../data_general/NVIS/nvis51_majorVegClass_codes.txt", 
                         fwf_widths(c(2,100)), skip = 1) %>% 
  set_names(c("veg_class","veg_class_descrip")) %>% 
  mutate(vc = as.factor(veg_class_descrip))
nvis <- inner_join(nvis, codes, by='veg_class')
nvis <- nvis %>% filter(veg_class <= 15) # !!! only forests and woodlands !!!
nvis <- nvis %>% select(-veg_class_descrip)
base <- base %>% as_tibble() %>% as.data.table()
base_vc <- base <- base[nvis,on=.(x,y)]
base <- base_vc; rm(base_vc)

# base[date==ymd("1982-01-01")] %>% 
#   ggplot(data=., aes(x,y,fill=nirv))+
#   geom_tile()
# base_vc[date==ymd("1982-01-01")] %>% 
#   ggplot(data=., aes(x,y,fill=nirv))+
#   geom_tile()

unique(apet$x) %in% unique(base$x)
unique(base$x) %in% unique(apet$x)
unique(base$x) %in% unique(e5pet$x)
#*******************************************************************************
# END SECTION
#*******************************************************************************





#*******************************************************************************
# JOIN ALL THE PIECES -----------------------------------------------------
#*******************************************************************************
tmp <- merge(aprecip %>% mutate(date=as.Date(time)),
             base %>% rename(x_vi=x,y_vi=y),
             by=c("x_vi","y_vi","date"))
# filter NA precip values [pretty sure these occur in consistent locations so 
# this will not create gaps in the time index]
tmp <- tmp[is.na(nirv)==F]
tmp <- tmp[is.na(precip)==F]
tmp %>% head
#*******************************************************************************
#*
#*******************************************************************************

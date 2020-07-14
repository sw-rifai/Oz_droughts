library(arrow)
library(sf); library(stars)
library(tidyverse); 
library(data.table); library(lubridate);
library(dtplyr)
setDTthreads(threads=round(0.75*parallel::detectCores()))

#*******************************************************************************
# Get NVIS Coords ---------------------------------------------------------
#*******************************************************************************
base <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet", 
                            col_select = c("x","y","vc","veg_class")) %>% 
  as.data.table() %>% lazy_dt()
nvis <- base %>% 
  group_by(x,y) %>% 
  summarize(veg_class = first(veg_class), 
            vc = first(vc)) %>% 
  ungroup() %>% 
  as.data.table()
rm(base); gc() 

coords_awap <- nvis %>% select(x,y)
coords_awap_sf <- st_as_sf(coords_awap, coords = c('x','y'))

#*******************************************************************************
#* END SECTION
#*******************************************************************************

#*******************************************************************************
# Extract AWAP radiation grid cells for east Oz ----------------------------------------------
#*******************************************************************************

src_path <- "/home/sami/srifai@gmail.com/work/research/data_general/clim_grid/awap/AWAP/monthly/rad/AWAP_monthly_rad_1990_2019.nc"

arad <- stars::read_ncdf(src_path)
names(arad) <- "rad"
st_crs(arad) <- st_crs(4326)

eoz_box <- st_bbox(c(xmin = min(nvis$x),
                     ymin = min(nvis$y),
                     xmax = max(nvis$x),
                     ymax = max(nvis$y)), 
                   crs = st_crs(4326))
arad <- st_crop(arad, eoz_box)
arad <- arad %>% as_tibble() %>% as.data.table()
arad <- arad %>% units::drop_units()
coords_awap <- arad %>% select(longitude,latitude) %>% distinct()
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
dim(coords_keep_awap)

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
coords_dict

arad <- arad %>% rename(x=longitude,y=latitude)

# complicated way of doing full join
arad <- merge(arad,coords_dict,by=c("x","y"),all=TRUE,allow.cartesian=TRUE)
arad <- arad[is.na(x_vi)==F]
arad %>% head

round(arad$x[1:5])==round(arad$x_vi[1:5])
round(arad$y[1:5])==round(arad$y_vi[1:5])

# visual check
arad[time%in%c(ymd("1990-01-01",tz='UTC'),
                ymd("2019-12-01",tz='UTC'))] %>%
  as_tibble() %>% 
  ggplot(data=., aes(x,y,fill=rad))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(direction = 1)+
  facet_grid(~as.factor(time))

# clean up 
rm(coords_awap, coords_awap_sf, coords_dict, coords_keep_awap, 
   coords_vi, eoz_box, nn_coords, nvis, src_path)
gc()
#*******************************************************************************
# END SECTION
#*******************************************************************************


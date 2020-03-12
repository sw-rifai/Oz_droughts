library(arrow); library(tidyverse); library(lubridate); 
library(sf); library(lubridate)
# ascending ~ 1:30am 
# descending ~ 1:30pm

# Import ------------------------------------------------------------------
fp <- list.files("../data_general/lpdr_test/LPDR_VOD/", full.names = T, pattern='parquet')
n_cores <- 10
  
library(foreach); library(doParallel)
cl <- makeCluster(n_cores)
registerDoParallel(cl)

out <- foreach(i = 1:length(fp), 
        .packages = c("tidyverse","lubridate","arrow"),
        .combine=rbind) %dopar% {
          o <- arrow::read_parquet(fp[i])
          test <- o %>% 
            mutate(month=month(date), 
                   year=year(date)) %>% 
            filter(band==5) %>% 
            group_by(x,y,month,year) %>% 
            summarize(vod_asc = mean(val_asc, na.rm=T), 
                      vod_desc = mean(val_desc, na.rm = T), 
                      vod_ddiff = mean(val_asc-val_desc, na.rm=T), 
                      vod_sigma = coef(lm(val_desc~val_asc))[2], 
                      vod_lambda = coef(lm(val_desc~val_asc))[1], 
                      vod_nobs = sum(is.na(val_asc)==F & is.na(val_desc)==F)) %>% 
            ungroup()
        test          
        }
arrow::write_parquet(out, 
                     sink=paste0("../data_general/lpdr_test/lpdr_vod_Oz_",Sys.Date(),".parquet"))



# Convert bizarre CRS -----------------------------------------------------
out <- read_parquet("../data_general/lpdr_test/lpdr_vod_Oz_2020-03-04.parquet")
orig <- stars::read_stars("../data_general/lpdr_test/LPDR_Oz/2002/AMSRU_Mland_2002170A.tif") %>% 
  sf::st_crs()
coords_orig <- out %>% select(x,y) %>% distinct()
tmp <- raster::raster("../data_general/lpdr_test/LPDR_Oz/2002/AMSRU_Mland_2002170A.tif")
srs_crs <- raster::crs(tmp)
coords <- sp::SpatialPoints(cbind(coords_orig$x, coords_orig$y), 
                            proj4string = srs_crs)
coords_sf <- st_as_sf(coords)
st_crs(4326)
coords_lonlat_vod <- sf::st_transform(coords_sf, st_crs(4326))
df_coords_lonlat_vod <- coords_lonlat_vod %>% st_coordinates() %>%
  as_tibble() %>% rename(lon=X,lat=Y) %>% 
  bind_cols(coords_orig, .)
out <- inner_join(out, df_coords_lonlat_vod, by=c('x','y'))

coords <- bind_cols(coords_orig, 
                    sf::st_coordinates(coords_lonlat_vod) %>% 
                      as_tibble() %>%
                      rename(lon=X, lat=Y))
coords_lonlat_vod

# Extract points for east Oz ----------------------------------------------
coords_vi <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv")
coords_vi <- st_as_sf(coords_vi, coords = c("longitude","latitude"))
st_crs(coords_vi) <- st_crs(4326)
nn_coords <- RANN::nn2(
  coords_lonlat_vod %>% st_coordinates(),
  coords_vi %>% st_coordinates()
) 
nn_coords$nn.idx[,1] %>% hist
coords_lonlat_vod <- coords_lonlat_vod %>% 
  mutate(idx_vod = row_number()) %>% 
  mutate(lat = st_coordinates(.)[,2], 
         lon = st_coordinates(.)[,1])
coords_lon_lat_vod_vi <- coords_lonlat_vod[nn_coords$nn.idx[,1],]
coords_lon_lat_vod_vi$id_vi <- coords_vi$id

# tmp <- coords_lonlat_vod %>% 
#   as_tibble() %>% 
#   select(-geometry)
# tmp$lon %>% min
# out$lon %>% min
out <- coords_lonlat_vod %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  inner_join(out, ., by=c('lon','lat'))
out_EOZ <- coords_lon_lat_vod_vi %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  inner_join(., out %>% select(-lon,-lat), by="idx_vod")

out %>% 
  arrow::write_parquet(., sink='../data_general/lpdr_test/VOD_LPDRv2_OZ.parquet', 
                       compression='snappy')

out_EOZ %>% 
  rename(id = id_vi) %>% 
  mutate(date=ymd(paste(year,month,1))) %>% 
  select(lon,lat,date,year,month,vod_asc, vod_desc, vod_sigma, 
         vod_lambda, vod_nobs) %>% 
  arrow::write_parquet(., sink="../data_general/Oz_misc_data/VOD_LPDRv2_EOZ.parquet", 
                       compression='snappy')

# SCRATCH -----------------------------------------------------------------


coords_vi <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv") %>% 
  rename(lon=longitude,lat=latitude) %>% 
  rename(id_vi = id)

tmp <- out_EOZ %>% 
  inner_join(coords_vi, by=c('id_vi')) 

tmp %>% 
  filter(lai_max < 3) %>% 
  sample_n(1000) %>% 
  select(lai_avg, lai_min, lai_max, lai_amp, 
         vod_asc, vod_desc, vod_ddiff) %>% 
  GGally::ggpairs()

tmp %>% 
  filter(lai_max < 3) %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(lai_avg, vod_desc))+
  geom_point()+
  geom_smooth()

out_EOZ %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(lat.x,lat.y))+geom_point()+
  geom_abline(aes(intercept=0,slope=1),col='red')

out %>% 
  mutate(date=ymd(paste(year,month,1))) %>% 
  # filter(date==min(date)) %>% 
  ggplot(data=., aes(x,y,fill=vod_lambda))+
  geom_tile()+
  scale_fill_viridis_c(option='B', limits=c(-1,2),oob=scales::squish)+
  facet_wrap(~month)





for(i in 1:length(fp)){
  arrow::read_parquet(fp[i]) %>% 
    filter(band==5) %>% 
    select(x,y,date,val_asc,val_desc) %>% 
    head() %>% print()
}

o <- arrow::read_parquet(list.files("../data_general/lpdr_test/LPDR_VOD/", full.names = T)[18])


# prototype ---------------------------------------------------------------
system.time(
  test <- o %>% 
    mutate(month=month(date)) %>% 
    filter(band==5) %>% 
    group_by(x,y,month) %>% 
    summarize(vod_asc = mean(val_asc, na.rm=T), 
              vod_desc = mean(val_desc, na.rm = T), 
              vod_ddiff = mean(val_asc-val_desc, na.rm=T), 
              vod_sigma = coef(lm(val_desc~val_asc))[2], 
              vod_lambda = coef(lm(val_desc~val_asc))[1], 
              vod_nobs = sum(is.na(val_asc)==F & is.na(val_desc)==F)) %>% 
    ungroup()
)

test %>% 
  filter(vod_sigma < 3 & 
           vod_sigma > -1 
           # vod_nobs >= 3
         ) %>% 
  ggplot(data=., aes(x,y,fill=vod_ddiff))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(limits=c(-0.5,0.5))+
  facet_wrap(~month)


# test <- o %>% 
#   mutate(month=month(date)) %>% 
#   filter(band==5) %>% 
#   filter(near(y,-3496920,tol=50000)) %>%
#   filter(near(x,13210586,tol = 50000)) %>% 
#   group_by(x,y,month) %>% 
#   summarize(vod_asc = mean(val_asc, na.rm=T), 
#             vod_desc = mean(val_desc, na.rm = T), 
#             vod_ddiff = mean(val_asc-val_desc, na.rm=T), 
#             vod_sigma = coef(lm(val_desc~val_asc))[2], 
#             vod_lambda = coef(lm(val_desc~val_asc))[1], 
#             vod_nobs = sum(is.na(val_asc)==F & is.na(val_desc)==F)) %>% 
#   ungroup()
dim(test)  
test %>% 
  ggplot(data=., aes(month, vod_sigma,color=x))+
  geom_point()+
  scale_color_viridis_c()










sf::st_crs(sf::read_sf("../data_general/lpdr_test/LPDR_Oz/2002/AMSRU_Mland_2002170A.tif"))
orig <- stars::read_stars("../data_general/lpdr_test/LPDR_Oz/2002/AMSRU_Mland_2002170A.tif") %>% 
  sf::st_crs()
coords_orig <- o %>% select(x,y) %>% distinct()
# j <- st_multipoint(x=cbind(coords_orig$x ,coords_orig$y))
# st_crs(j) <- orig
# sf::st_set_crs(j, orig)
# sf::st_set_crs(j, proj4text=as.character(orig)[2], valid=F)
# st_crs(orig) %>% st_transform(4326)
# rgdal::make_EPSG(orig)
# st_transform(j, orig)
# st_multipoint(cbind(1:5, rnorm(5))) %>%
#   st_set_crs(., st_crs(4326))
# st_as_sf(cbind(coords_orig$x, coords_orig$y))
# 
tmp <- raster::raster("../data_general/lpdr_test/LPDR_Oz/2002/AMSRU_Mland_2002170A.tif")
srs_crs <- raster::crs(tmp)
coords <- sp::SpatialPoints(cbind(coords_orig$x, coords_orig$y), 
                  proj4string = srs_crs)
coords_sf <- st_as_sf(coords)
st_crs(4326)
coords_lonlat_vod <- sf::st_transform(coords_sf, st_crs(4326))

coords <- bind_cols(coords_orig, 
          sf::st_coordinates(coords_lonlat) %>% 
  as_tibble() %>%
  rename(lon=X, lat=Y))

coords_lonlat
# st_nearest_points(coords_sf, coords)
coords_vi <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv")
coords_vi <- st_as_sf(coords_vi, coords = c("longitude","latitude"))
st_crs(coords_vi) <- st_crs(4326)

test_vi <- coords_vi[1:500,]
test_vod <- coords_lonlat[1:500, ]
tmp_j <- st_nearest_points(test_vod, test_vi)

nn_coords <- RANN::nn2(
 coords_lonlat_vod %>% st_coordinates(),
 coords_vi %>% st_coordinates()
 ) 
nn_coords$nn.idx[,1] %>% hist







o %>% 
  mutate(month=month(date)) %>% 
  group_by(x,y,month) %>% 
  summarize(asc = mean(val_asc,na.rm = T), 
            desc = mean(val_desc, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(sigma = asc-desc) %>% 
  ggplot(data=., aes(x,y,fill=sigma))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2()+
  facet_wrap(~month)

o %>% 
  filter(y <= -3e06) %>% 
  filter(x >= 1.4e07) %>%
  filter(y >= -4.5e06) %>% 
  mutate(month=month(date)) %>% 
  group_by(x,y,month) %>% 
  summarize(asc = mean(val_asc,na.rm = T), 
            desc = mean(val_desc, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(sigma = asc-desc) %>% 
  ggplot(data=., aes(x,y,fill=sigma))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(limits=c(0,6))+
  # scale_fill_gradient2()+
  facet_wrap(~month)


o %>% 
  filter(y <= -3e06) %>% 
  filter(x >= 1.3e07) %>% 
  filter(y >= -4.5e06) %>% 
  mutate(month=month(date)) %>% 
  # filter(month==12) %>% 
  ggplot(data=., aes(val_asc, val_desc))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~month)

options(digits = 3)

o %>% 
  filter(val_asc == max(val_asc)) %>% 
  select(x,y)

o %>% 
  filter(band==5) %>% 
  filter(near(y,-3496920,tol=1)) %>%
  filter(near(x,13210586,tol = 1)) %>% 
  mutate(month=month(date)) %>% 
  # filter(month==12) %>% 
  ggplot(data=., aes(val_asc, val_desc))+
  geom_point(alpha=0.5)+
  geom_smooth(method='lm',se=F)+
  facet_wrap(~month, scales = 'free')

o %>% 
  filter(band==5) %>% 
  filter(near(y,-3496920,tol=1)) %>%
  filter(near(x,13210586,tol = 1)) %>% 
  select(date, val_asc, val_desc) %>% 
  pivot_longer(cols = starts_with('val'), 
               names_to = "type",
               values_to = "vod") %>% 
  ggplot(data=., aes(date, vod, color=type))+
  geom_line()

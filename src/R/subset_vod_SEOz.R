library(arrow); library(tidyverse); library(lubridate); 
library(sf); library(lubridate)
# ascending ~ 1:30am 
# descending ~ 1:30pm

# Import ------------------------------------------------------------------
fp <- list.files("../data_general/lpdr_test/LPDR_VOD/", full.names = T, pattern='parquet')


library(foreach); library(doParallel)
n_cores <- 10
cl <- makeCluster(n_cores)
registerDoParallel(cl)


# Filter out daily values -------------------------------------------------------
out <- foreach(i = 1:length(fp), 
               .packages = c("tidyverse","lubridate","arrow"),
               .combine=rbind) %dopar% {
                 o <- arrow::read_parquet(fp[i])
                 test <- o %>% 
                   filter(band==5) %>% 
                   rename(vod_day = val_desc, 
                          vod_night = val_asc) 
                 test          
               }
out %>% dim


# Convert bizarre CRS -----------------------------------------------------
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

out %>% 
  write_parquet(., sink='../data_general/Oz_misc_data/LPDRv2_VOD_SEAUS_2002_2019.parquet', 
                compression='snappy')

out <- read_parquet("../data_general/Oz_misc_data/LPDRv2_VOD_SEAUS_2002_2019.parquet")
df_f <- out %>% select(x,y,date) %>% 
  expand(. ,nesting(x,y), date=sort(unique(out$date)))
df_f <- out %>% select(x,y,date) %>% distinct()
out <- full_join(df_f, out, by=c("x","y","date"))
out %>% 
  write_parquet(., sink='../data_general/Oz_misc_data/LPDRv2_VOD_SEAUS_2002_2019.parquet', 
                compression='snappy')

out %>% distinct(x,y) %>% dim
out %>% distinct(date) %>% dim

vec_dates <- out %>% distinct(date) %>% 
  mutate(quarter = quarter(date, fiscal_start = 11))
vec_dates <- vec_dates %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON"))
vec_dates <- vec_dates %>% mutate(season = factor(q,
                                                  levels=c("DJF","MAM","JJA","SON"), 
                                                  ordered=T))
vec_dates <- vec_dates %>% 
  mutate(hydro_year = year(date+months(1)))
vec_dates <- vec_dates %>% select(date, season, hydro_year)
out <- inner_join(out, vec_dates, by='date')
out_s <- out %>% 
  group_by(lon,lat,hydro_year,season) %>% 
  summarize(nobs_vod_day = sum(is.na(vod_day)==F), 
            nobs_vod_night = sum(is.na(vod_night)==F),
            vod_day = mean(vod_day, na.rm=T), 
            vod_night = mean(vod_night,na.rm=T), 
            vod_day_sd = sd(vod_day, na.rm=T), 
            vod_night_sd = sd(vod_night, na.rm=T)) %>% 
  ungroup()

out_s %>% 
  write_parquet(., sink='../data_general/Oz_misc_data/LPDRv2_VOD_YrSeason_SEAUS_2002_2019.parquet', 
                compression='snappy')

seoz <- out_s %>% 
  filter(hydro_year %in% 2003:2019) %>% 
  filter(lon >= 140 & lat <= -28) %>% 
  filter(lat >= -40)

df_f <- expand_grid(hydro_year=2002:2019, 
                    season=unique(seoz$season), 
                    lon=sort(unique(seoz$lon)), 
                    lat=sort(unique(seoz$lat)))
dim(df_f)
dim(seoz)
seoz <- full_join(seoz, df_f, by=c("lon","lat","hydro_year","season"))

seoz %>% 
  write_parquet(., sink='../data_general/Oz_misc_data/LPDRv2_VOD_YrSeason_SEAUS_2002_2019.parquet', 
                compression='snappy')

seoz %>% head

seoz %>% 
  group_by(hydro_year, season) %>% 
  summarize(val = mean(vod_day,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(hydro_year, val, color=season))+
  geom_line()


tmp <- seoz %>% 
  select(lon,lat,hydro_year,season,vod_day) %>% 
  as.tbl_cube(dim_names=c("lon","lat","hydro_year","season"), 
              met_name=c("vod_day","vod_night"))
tmp %>% dim
tmp$mets$vod_day %>% 
vec_seasons <- unique(seoz$season)
vec_lon <- sort(unique(seoz$lon))
vec_lat <- sort(unique(seoz$lat))
tmp2 <- st_as_stars(tmp$mets$vod_day, 
                    # curvilinear=c("lon","lat"),
            dimensions=st_dimensions(lon=vec_lon, 
                                     lat=vec_lat, 
                                     hydro_year=c(2002:2019), 
                                     season=vec_seasons 
                                     ))

ggplot()+geom_stars(data=tmp2[,,,15,1])

tmp2[,,,15,1]$X[,,1,1] %>% image


jj <- st_as_stars(seoz %>% 
                    select(lon,lat,hydro_year,season,vod_day), 
                  dimensions=st_dimensions(hydro_year=c(2002:2019), 
                                           season=vec_seasons, 
                                           lon=vec_lon, 
                                           lat=vec_lat), 
                  values=vod_day,
                  crs=st_crs(4326)
                  )
jj
jj %>% dim
jj %>% plot
jj <- st_redimension(jj, 
                     along=list("season"=c(unique(seoz$season) %>% as.character())))
jj %>% dim
jj %>% plot


library(stars)
tmp %>% dim
names(tmp)
tmp$met$vod_day[,,1,1] %>% t() %>%  image

st_as_stars(tmp, 
            curvilinear=c("lon","lat"),
            crs=st_crs(4326))
stars::write_stars(tmp, 
                   dsn="../data_general/Oz_misc_data/LPDRv2_VOD_YrSeason_SEAUS_2002_2019.nc", 
                   driver = "netCDF")


stars::write_stars(o[1], dsn="awap_precip_2019.nc",
                   layer='precip', 
                   driver="netCDF")


length(unique(seoz$hydro_year))*
length(unique(seoz$season))*
length(unique(seoz$lon))*
length(unique(seoz$lat))
dim(seoz)

out_s$lat %>% unique %>% sort %>% diff
seoz %>% 
  filter(hydro_year %in% c(2010:2015)) %>% 
  filter(lon >= 140 & lat <= -28) %>% 
  filter(lat >= -40) %>% 
  ggplot(data=., aes(lon,lat,fill=vod_day))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()+
  facet_grid(hydro_year ~ season)
  


# coords <- bind_cols(coords_orig, 
#                     sf::st_coordinates(coords_lonlat_vod) %>% 
#                       as_tibble() %>%
#                       rename(lon=X, lat=Y))
# coords_lonlat_vod

# out %>% 
#   filter(date==min(date)) %>% 
#   ggplot(data=., aes(x,y,fill=vod_day))+
#   geom_tile()+
#   scale_fill_viridis_c()



# # define dimensions
# londim <- ncdim_def("lon", "degrees_east", as.double(tmp_c$dims$lon))
# latdim <- ncdim_def("lat", "degrees_north", as.double(tmp_c$dims$lat))
# timedim <- ncdim_def("time", units="seconds since 1970-01-01",
#                      as.numeric(tmp_c$dims$date), unlim = T)
# fillvalue <- 1e20
# 
# origin
# tmp_c$dims$date[1]
# tmp_c$dims$date[1] %>% as.numeric()
# origin+seconds(tmp_c$dims$date[1] %>% as.numeric())
# 
# # Climate variables to put into ncdf file: 
# def_co2flux_land <- ncvar_def(name = 'co2flux_land_pgc_m2', units='mm', list(londim, latdim,timedim), 
#                               fillvalue, 'CO2 flux from land', prec = "single")
# 
# ncfname <- "../data_general/CarboScope/sEXTocNEET_v4.3_monthly.nc"
# ncout <- nc_create(ncfname, list(def_co2flux_land), force_v4 = TRUE)
# 
# # put the array
# ncvar_put(ncout, def_co2flux_land, tmp_c$mets$co2flux_land)
# 
# # put additional attributes into dimension and data variables
# ncatt_put(ncout, "lon", "axis", "X")  
# ncatt_put(ncout, "lat", "axis", "Y")
# 
# # add global attributes
# ncatt_put(ncout, 0, "title", 'CarboScope sEXTocNEET_v4.3_monthly')
# 
# # close the file, writing data to disk
# nc_close(ncout)
# 
# # cat(system(paste("ncdump -c", ncfname, intern = TRUE)), sep = '\n')
# # 
# system(paste("ncdump -c",ncfname))
# 
# ncfname
# system("cdo -f nc4c -z zip_6 sellonlatbox,-180,180,-23.5,23.5 -remapbic,/soge-home/staff/cenv0599/projects/data_general/GFC_forestArea/GFC_forestMask_0p5_2016.nc \
# ../data_general/CarboScope/sEXTocNEET_v4.3_monthly.nc/soge-home/staff/cenv0599/projects/data_general/era5/ERA5_CDS/e5_mvars.nc \
# ../data_general/CarboScope/sEXTocNEET_v4.3_monthly_0p5.nc")

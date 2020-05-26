library(arrow); library(tidyverse); 
library(sf); library(data.table); library(lubridate); 
# ascending ~ 1:30am 
# descending ~ 1:30pm

# Import ------------------------------------------------------------------
fp <- list.files("../data_general/lpdr_test/LPDR_VOD/", full.names = T, pattern='parquet')


library(foreach); library(doParallel)
n_cores <- 8
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Filter out daily values -------------------------------------------------------
out <- foreach(i = 1:length(fp), 
               .packages = c("tidyverse","arrow","data.table","lubridate"),
               .combine=rbind) %dopar% {
                 o <- arrow::read_parquet(fp[i])
                 o <- o %>% 
                   filter(band==5) %>% 
                   rename(vod_day = val_desc, 
                          vod_night = val_asc) %>% 
                   mutate(vod_ddiff = vod_night-vod_day) %>% 
                   filter(vod_ddiff >= 0) %>% 
                   as.data.table() %>% 
                   mutate(quarter = lubridate::quarter(date, fiscal_start = 11)) %>% 
                   mutate(q = case_when(quarter==1~"DJF",
                                        quarter==2~"MAM",
                                        quarter==3~"JJA",
                                        quarter==4~"SON")) %>% 
                   mutate(season = factor(q,
                                          levels=c("DJF","MAM","JJA","SON"), 
                                          ordered=T)) %>%   
                   mutate(hydro_year = lubridate::year(date+months(1)))
                 
                 o
               }

vec_years <- sort(unique(out$hydro_year))
out <- foreach(i = first(vec_years):last(vec_years), 
               .packages = c("tidyverse","arrow","data.table","lubridate"),
               .combine=rbind) %dopar% {
                 
          o <- out[band==5][hydro_year==i][,.(vod_day_avg = mean(vod_day, na.rm=TRUE), 
                   vod_night_avg = mean(vod_night, na.rm = TRUE), 
                   vod_ddiff = mean(vod_night-vod_day, na.rm=TRUE), 
                   vod_sigma = coef(lm(vod_day~vod_night))[2], 
                   vod_lambda = coef(lm(vod_day~vod_night))[1], 
                   vod_nobs = sum(is.na(vod_day)==F)),
                by=.(x,y,hydro_year,season)]
          o
}

library(sf)
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)
raster()

orig <- stars::read_stars("../data_general/lpdr_test/LPDR_Oz/2002/AMSRU_Mland_2002170A.tif") %>% 
  sf::st_crs()
oz_poly2 <- sf::st_transform(oz_poly, orig)
oz_poly2 %>% filter(NAME_1 == "Queensland") %>% st_bbox()
nvis_mask <- stars::read_stars("../data_general/NVIS/nvis51_majorVegClass_0p05.tif")
names(nvis_mask) <- "vc"
nvis_mask <- nvis_mask %>% mutate(blah=ifelse(vc<=15,1,0))
nvis_mask['blah'] %>% plot(col=sf.colors(2))
nvis_mask['vc']['vc'!=99] %>% plot(col=sf.colors(33,categorical = T))
# 
#   plot(col=sf.colors(32))

# Plot Sigma --------------------------------------------------------------
p_sigma <- out[vod_sigma>-0.5 & vod_sigma<2] %>% 
  as_tibble() %>% 
  ggplot(data=., aes(x,y,fill=vod_sigma))+
  geom_sf(data=oz_poly2,fill='grey30',color='grey10',inherit.aes = F)+
  geom_tile()+
  scale_fill_viridis_c(expression(paste(sigma)),
                       option='B', 
                       limits=c(-0.1,1.5), 
                       na.value = 'black', 
                       oob=scales::squish)+
  coord_sf(xlim = c(13578402, 14792697),
           ylim = c(-5209194,-3006637), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  facet_grid(season~hydro_year)+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(fill='#99A3C4'), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        strip.placement = 'inside', 
        strip.text = element_text(family='AvantGarde'), 
        legend.direction = 'horizontal', 
        legend.key.width = unit(1,units = 'cm'),
        legend.key.height = unit(0.25,units='cm'), 
        legend.margin = margin(0,0,0,0), 
        legend.spacing = unit(0,'cm')
  )
ggsave(p_sigma, filename = "figures/map_vod_sigma_seasonal_2002_2019.png", 
       width=20,height=15,units='cm',type='cairo',dpi='retina')



# 
# # Convert bizarre CRS -----------------------------------------------------
# orig <- stars::read_stars("../data_general/lpdr_test/LPDR_Oz/2002/AMSRU_Mland_2002170A.tif") %>% 
#   sf::st_crs()
# oz_poly2 <- sf::st_transform(oz_poly, orig)
# 
# coords_orig <- out %>% select(x,y) %>% distinct()
# tmp <- raster::raster("../data_general/lpdr_test/LPDR_Oz/2002/AMSRU_Mland_2002170A.tif")
# srs_crs <- raster::crs(tmp)
# coords <- sp::SpatialPoints(cbind(coords_orig$x, coords_orig$y), 
#                             proj4string = srs_crs)
# coords_sf <- st_as_sf(coords)
# st_crs(4326)
# coords_lonlat_vod <- sf::st_transform(coords_sf, st_crs(4326))
# df_coords_lonlat_vod <- coords_lonlat_vod %>% st_coordinates() %>%
#   as_tibble() %>% rename(lon=X,lat=Y) %>% 
#   bind_cols(coords_orig, .)
# out <- inner_join(out, df_coords_lonlat_vod, by=c('x','y'))
# 
# coords <- bind_cols(coords_orig, 
#                     sf::st_coordinates(coords_lonlat_vod) %>% 
#                       as_tibble() %>%
#                       rename(lon=X, lat=Y))
# coords_lonlat_vod


# 
# # Group by Hydro Year and Season ------------------------------------------
# out <- out %>% 
#   mutate(quarter = quarter(date, fiscal_start = 11))
# out <- out %>% 
#   mutate(q = case_when(quarter==1~"DJF",
#                        quarter==2~"MAM",
#                        quarter==3~"JJA",
#                        quarter==4~"SON"))
# out <- out %>% mutate(season = factor(q,
#                                       levels=c("DJF","MAM","JJA","SON"), 
#                                       ordered=T))
# out <- out %>% 
#   mutate(hydro_year = year(date+months(1)))
# ls()
# 
# 
# # Fit sigma by season and year --------------------------------------------
# vod_s <- out[band==5][,.(vod_day_avg = mean(vod_day, na.rm=TRUE), 
#                 vod_night_avg = mean(vod_night, na.rm = TRUE), 
#                 vod_ddiff = mean(vod_night-vod_day, na.rm=TRUE), 
#                 vod_sigma = coef(lm(vod_day~vod_night))[2], 
#                 vod_lambda = coef(lm(vod_day~vod_night))[1], 
#                 vod_nobs = sum(is.na(vod_day)==F)),
#              by=.(x,y,hydro_year,season)]
# 
# 
# vod_s <- out %>% 
#   filter(band==5) %>% 
#   group_by(x,y,hydro_year, season) %>% 
#   summarize(vod_day_avg = mean(vod_day, na.rm=T), 
#             vod_night_avg = mean(vod_night, na.rm = T), 
#             vod_ddiff = mean(vod_night-vod_day, na.rm=T), 
#             vod_sigma = coef(lm(vod_day~vod_night))[2], 
#             vod_lambda = coef(lm(vod_day~vod_night))[1], 
#             vod_nobs = sum(is.na(vod_day)==F)) %>% 
#   ungroup()

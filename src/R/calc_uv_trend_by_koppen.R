library(sf); library(stars); 
library(tidyverse); library(data.table); 
library(dtplyr)
library(lubridate); 

# load("data/gridCell_lm_ndvi_clim.Rdata") # grid cell linear regressions
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)

uv <- stars::read_stars("../data_general/clim_grid/era5/Oz_surface_uv.nc",proxy=F)
uv <- uv %>% as_tibble() %>% set_names(c("x","y","expver","date","uv")) %>% 
  units::drop_units()
uv_norms <- uv %>% 
  lazy_dt() %>% 
  filter(date>=ymd("1982-01-01")&date<=ymd("2010-12-31")) %>% 
  mutate(month = month(date)) %>% 
  group_by(x,y,month) %>% 
  summarize(uv_u = mean(uv,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble()

uv <- inner_join(uv_norms, uv %>% mutate(month=month(date)), by=c("x","y","month"))
uv <- uv %>% mutate(uv_anom = uv-uv_u)

# Load simplified BOM Koppen climate zones -------------------------------------
ref_grid <- stars::read_stars("../data_general/clim_grid/era5/Oz_surface_uv.nc",proxy=F)
ref_grid <- ref_grid[,,,1,1]
bom <- stars::read_stars("../data_general/Koppen_climate/BOM/kpngrp.txt")
bom <- st_warp(src=bom, dest=ref_grid[,,], use_gdal = F)
bom <- set_names(bom, 'koppen') %>% as_tibble()
bom <- left_join(ref_grid %>% as_tibble() %>% select(x,y), 
                 bom) %>% as.data.table()
bom %>% filter(x>=140)
bom <- sf::st_as_sf(bom, coords=c("x","y"))
st_crs(bom) <- st_crs("EPSG:4326")

dat <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet", 
                           col_select = c("x","y","map")) %>% 
  distinct()
coords <- dat %>% select(x,y,map) %>% distinct() %>% filter(x>=140)
coords <- sf::st_as_sf(coords, coords=c("x","y"))
st_crs(coords) <- st_crs("EPSG:4326")

bom$map <- coords[sf::st_nearest_feature(bom,coords),"map"] %>% pull(map)
bom  %>% plot

bom <- bind_cols(map=bom$map, koppen=bom$koppen, bom %>% st_coordinates())
bom <- bom %>% rename(x=X, y=Y)

kop <- bom %>% 
  as_tibble() %>% 
  filter(is.na(koppen)==F) %>% 
  mutate(zone = case_when(between(koppen,0,11) ~ 'Temperate', 
                          (y <= -40) ~ 'Tasmania',
                          between(koppen, 12,21)~'GD_temp', # Grassland
                          between(koppen, 22,30)~'GD_temp', # Desert
                          between(koppen, 31,34)~'Subtropical',
                          between(koppen, 35,40)~'Tropical', 
                          koppen >= 41 ~ 'Equatorial')) %>% 
  mutate(zone = ifelse(y < -40, 'Temperate Tas.', zone)) %>% 
  mutate(zone = ifelse(zone == "GD_temp" & map < 500, 'Desert',zone)) %>%
  mutate(zone = ifelse(zone == "GD_temp" & map >= 500, 'Grassland',zone)) %>%
  mutate(zone = factor(zone, levels = c("Equatorial","Tropical",
                                        "Subtropical","Grassland","Desert",
                                        "Temperate","Temperate Tas."), ordered = T))
kop <- kop %>% mutate(cz=zone)

uv <- uv %>% filter(x>=140)
uv <- left_join(uv, kop, by=c("x","y")) 
# end section ******************************************************************
uv <- uv %>% filter(is.na(koppen)==F) %>% filter(is.na(uv_anom)==F)

sf::gdal_rasterize(oz_poly["CC_1"])
oz_grid <- stars::st_rasterize(oz_poly["CC_1"])
slice(ref_grid, index = 'time',drop = T)

st_warp(oz_grid,ref_grid[,,1],use_gdal = F)
oz_grid %>% plot

st_crs(oz_poly)
st_crs(ref_grid) <- st_crs('EPSG:4326')
sf::st_intersects(oz_poly, ref_grid)


uv %>% filter(date==min(date)) %>% 
  ggplot(aes(x,y,fill=zone))+
  geom_tile()+
  geom_sf(inherit.aes = F, data=oz_poly,fill=NA,color='gray10')+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  scale_fill_viridis_d(option='B',direction = -1)


uv %>% 
  group_by(date) %>% 
  summarize(val = mean(uv_anom,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(date,val))+
  # geom_line()+
  geom_smooth(method='lm')

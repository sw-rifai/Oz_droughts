library(sf); library(tidyverse);
o <- dat %>%
  filter(date==ymd("2001-01-01"))
write_csv(o, path = "data/David_Fig_awap_data.csv")
o <- read_csv("data/David_Fig_awap_data.csv")

# load("data/gridCell_lm_ndvi_clim.Rdata") # grid cell linear regressions
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)


coords <- read_tsv("data/David_spa_coordinates.txt") %>% 
  rename(x=long,y=lat)
coords <- sf::st_as_sf(coords,coords=c("x","y"))["elevation"]
sf::st_crs(coords) <- sf::st_crs("EPSG:4326")

# Precip version 
o %>% 
  as_tibble() %>% 
  ggplot(data=.,aes(x,y,fill=map))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray60',color='black')+
  geom_tile()+
  geom_sf(inherit.aes = F, data=coords, aes(color=elevation))+
  # coord_sf(xlim = c(144,154),
  #          ylim = c(-20,-10), expand = FALSE)+
  coord_sf(xlim = c(140,154),
           ylim = c(-25,-10), expand = FALSE)+
  # coord_sf(xlim = c(140,154),
  #          ylim = c(-45,-10), expand = FALSE)+
  scale_color_viridis_c("Elevation (m)", option='D')+
  scale_fill_viridis_c(expression(paste("Precip"~"("*mm~yr**-1,")")), 
                       option='B')+
  labs(x=NULL,y=NULL)+
  guides(fill = 
           guide_colourbar(label = T), 
         color=guide_colorbar(label=T)) +
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(), 
        # legend.position = 'right',
        legend.position = c(1,1), 
        legend.justification = c(1,1),
        legend.key.width = unit(0.5,'cm'),
        legend.background = element_rect(fill=NA),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(), 
        axis.ticks = element_blank())


# VPD version 
o %>% 
  as_tibble() %>% 
  ggplot(data=.,aes(x,y,fill=mavpd15*10))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray60',color='black')+
  geom_tile()+
  geom_sf(inherit.aes = F, data=coords, aes(color=elevation))+
  # coord_sf(xlim = c(144,154),
  #          ylim = c(-20,-10), expand = FALSE)+
  coord_sf(xlim = c(140,154),
           ylim = c(-25,-10), expand = FALSE)+
  # coord_sf(xlim = c(140,154),
  #          ylim = c(-45,-10), expand = FALSE)+
  scale_color_viridis_c("Elevation (m)", option='D')+
  scale_fill_viridis_c(expression(paste(VPD["15:00"]~"(hPa)")), 
                       option='B')+
  labs(x=NULL,y=NULL)+
  guides(fill = 
           guide_colourbar(label = T), 
         color=guide_colorbar(label=T)) +
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(), 
        # legend.position = 'right',
        legend.position = c(1,1), 
        legend.justification = c(1,1),
        legend.key.width = unit(0.5,'cm'),
        legend.background = element_rect(fill=NA),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(), 
        axis.ticks = element_blank())

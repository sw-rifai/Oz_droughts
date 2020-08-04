#*******************************************************************************
# Plot MAP using AWAP precip grid cells for east Oz ----------------------------------------------
#*******************************************************************************
aprecip <- stars::read_ncdf("../data_general/clim_grid/awap/AWAP/monthly/rain/AWAP_monthly_rain_1970_2019.nc")
names(aprecip) <- "precip"
st_crs(aprecip) <- st_crs(4326)

eoz_box <- st_bbox(c(xmin = min(nvis$x),
                     ymin = min(nvis$y),
                     xmax = max(nvis$x),
                     ymax = max(nvis$y)), 
                   crs = st_crs(4326))
aprecip <- st_crop(aprecip, eoz_box)
aprecip <- aprecip %>% as_tibble() %>% as.data.table()
aprecip <- aprecip %>% units::drop_units()

m_p <- aprecip %>% 
  lazy_dt() %>% 
  rename(date=time, x=longitude, y=latitude) %>% 
  filter(date >= ymd("1981-01-01") & date <= ymd("2010-12-31")) %>% 
  group_by(x,y) %>% 
  summarize(ma_p = mean(precip,na.rm=TRUE)*12) %>% 
  ungroup() %>% 
  as_tibble()
rm(aprecip); gc()

p_map <- m_p %>% 
  ggplot(data=., aes(x,y,fill=ma_p))+
  geom_tile()+
  geom_sf(inherit.aes = F, data=oz_poly,fill=NA,color='gray10')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-44,-11), expand = FALSE)+
  scale_fill_viridis_b(expression(paste("MAP"~(mm))), 
                       direction=-1, option='A',
                       limits=c(0,2500), oob=scales::squish) +
  guides(fill = guide_colorbar(title.position='top'))+
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(),
        legend.position = 'right',
        # legend.text = element_text(size=5),
        legend.title = element_text(size=8),
        # legend.key.width = unit(0.25,'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        ); p_map
# END **************************************************************************



#*******************************************************************************
# Plot MATmax using AWAP grid cells for east Oz ----------------------------------------------
#*******************************************************************************
atmax <- stars::read_ncdf("../data_general/clim_grid/awap/AWAP/monthly/tmax/AWAP_monthly_tmax_1970_2019.nc")
names(atmax) <- "tmax"
st_crs(atmax) <- st_crs(4326)

eoz_box <- st_bbox(c(xmin = min(nvis$x),
                     ymin = min(nvis$y),
                     xmax = max(nvis$x),
                     ymax = max(nvis$y)), 
                   crs = st_crs(4326))
atmax <- st_crop(atmax, eoz_box)
atmax <- atmax %>% as_tibble() %>% as.data.table()
atmax <- atmax %>% units::drop_units()

m_tmax <- atmax %>% 
  lazy_dt() %>% 
  rename(date=time, x=longitude, y=latitude) %>% 
  filter(date >= ymd("1981-01-01") & date <= ymd("2010-12-31")) %>% 
  group_by(x,y) %>% 
  summarize(ma_tmax = mean(tmax,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble()
rm(atmax); gc()

p_matmax <- m_tmax %>% 
  ggplot(data=., aes(x,y,fill=ma_tmax))+
  geom_tile()+
  geom_sf(inherit.aes = F, data=oz_poly,fill=NA,color='gray10')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-44,-11), expand = FALSE)+
  scale_fill_viridis_b(expression(paste("MATmax"~(degree*C))), 
                       direction=1, option='B',
                       # limits=c(0,2500),
                       oob=scales::squish) +
  guides(fill = guide_colorbar(title.position='top'))+
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(),
        legend.position = 'right',
        # legend.text = element_text(size=5),
        legend.title = element_text(size=8),
        # legend.key.width = unit(0.25,'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  ); p_matmax
# END **************************************************************************


#*******************************************************************************
# Plot MA PET using AWAP pet grid cells for east Oz ----------------------------------------------
#*******************************************************************************
apet <- stars::read_ncdf("../data_general/clim_grid/awap/AWAP/monthly/pet/AWAP_monthly_PriestleyTaylor_PET_1990_2019.nc")
names(apet) <- "pet"
st_crs(apet) <- st_crs(4326)

eoz_box <- st_bbox(c(xmin = min(nvis$x),
                     ymin = min(nvis$y),
                     xmax = max(nvis$x),
                     ymax = max(nvis$y)), 
                   crs = st_crs(4326))
apet <- st_crop(apet, eoz_box)
apet <- apet %>% as_tibble() %>% as.data.table()
apet <- apet %>% units::drop_units()

m_pet <- apet %>% 
  lazy_dt() %>% 
  rename(date=time, x=longitude, y=latitude) %>% 
  filter(date >= ymd("1990-01-01") & date <= ymd("2019-12-31")) %>% 
  group_by(x,y) %>% 
  summarize(ma_pet = mean(pet,na.rm=TRUE)*12) %>% 
  ungroup() %>% 
  as_tibble()
rm(apet); gc()

p_mapet <- m_pet %>% 
  ggplot(data=., aes(x,y,fill=ma_pet))+
  geom_tile()+
  geom_sf(inherit.aes = F, data=oz_poly,fill=NA,color='gray10')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-44,-11), expand = FALSE)+
  scale_fill_viridis_b(expression(paste("MA PET"~(mm))), 
                       direction=1, option='D',
                       limits=c(0,2500), oob=scales::squish) +
  guides(fill = guide_colorbar(title.position='top'))+
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(),
        legend.position = 'right',
        # legend.text = element_text(size=5),
        legend.title = element_text(size=8),
        # legend.key.width = unit(0.25,'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  ); p_mapet
# END **************************************************************************

ggsave(p_matmax|p_map|p_mapet, 
              filename = "figures/map_of_MATmax_MAP_MAPET.png", 
              type='cairo', dpi=300,
              width=11*3, height=13, units='cm')
       

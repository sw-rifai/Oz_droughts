srtm <- stars::read_stars("../data_general/Oz_misc_data/SRTM_elevation_500m_EastOz_.tif")
base <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_Tree_Cover_5000m_East_Oz_noMask_2000_2019.tif", 
                          RasterIO = list(bands=1))
srtm <- st_warp(src=srtm, dest=base[,,], use_gdal = F)
srtm <- srtm %>% as_tibble() %>% set_names('x','y','elev') %>% na.omit()

bom <- stars::read_stars("../data_general/Koppen_climate/BOM/kpngrp.txt")
st_crs(srtm)
bom <- st_warp(src=bom, dest=base[,,], use_gdal = F)
plot(bom)
bom <- set_names(bom, 'koppen') %>% as_tibble()
bom <- left_join(base %>% as_tibble() %>% select(x,y), 
                 bom)

bom %>% ggplot(data=., aes(x,y,fill=as_factor(koppen)))+geom_tile()+coord_equal()+
  scale_fill_viridis_d()

# K-means Climate Zones & P:PET Trend & NDVI & VCF distributions --------------------------------------------------
jj <- ldat %>% 
  # filter(season %in% c("DJF","JJA")) %>% 
  filter(date >= ymd("1982-01-01") & date<=ymd("2011-12-31")) %>% 
  group_by(x,y) %>% 
  summarize(ppet_u = mean(pe, na.rm=TRUE),
            ppet_sd = sd(pe,na.rm=TRUE), 
            tmax_u = mean(tmax,na.rm=TRUE),
            tmax_sd = sd(tmax,na.rm=TRUE), 
            tmin_u = mean(tmin,na.rm=TRUE), 
            tmin_sd = sd(tmin,na.rm=TRUE),
            vpd15_u = mean(vpd15,na.rm=TRUE),
            vpd15_sd = sd(vpd15, na.rm=TRUE),
            precip_u = mean(precip,na.rm=TRUE), 
            precip_sd = sd(precip,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  na.omit()
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
jj2 <- jj %>% 
  # group_by(season) %>% 
  mutate_at(c("ppet_u","ppet_sd",
              'tmax_u','tmax_sd',
              'tmin_u','tmin_sd',
              'vpd15_u','vpd15_sd',
              'precip_u','precip_sd'), scale2) %>% 
  ungroup()
srtm <- srtm %>% 
  mutate(x_s = x, 
         y_s = y) %>% 
  mutate_at(c('elev','x_s','y_s'),scale2)
# jj2 <- jj2 %>% pivot_wider(names_from=season, 
#                    values_from=c(ppet_u,tmax_u,tmin_u,vpd15_u,precip_u, 
#                                  ppet_sd,tmax_sd,tmin_sd,vpd15_sd,precip_sd))
jj2
jj2  <- inner_join(jj2,srtm,by=c('x','y'))
jj2
set.seed(999)
jj3 <- kmeans(jj2 %>% select(x_s,y_s#,elev#,starts_with(c("ppet","tmin"))
                             ) %>% 
                as.matrix(),centers=6,
              iter.max = 100, 
              nstart = 100)
jj2$cluster <- jj3$cluster
jj2 <- inner_join(jj2, 
                  jj3$centers %>% as_tibble() %>% 
                    mutate(cluster = 1:6) %>% 
                    mutate(cz=cluster) %>% 
                    # mutate(cz = rank(tmax_u_DJF+tmax_u_JJA+tmax_u_MAM+tmax_u_SON)) %>% 
                    select(cluster, cz))

jj2 <- jj2 %>% select(-cluster)
# jj2 %>% select(x,y,cz) %>% arrow::write_parquet(.,sink="data/EOz_clim_kmeans6_v2.parquet")

p_left <- jj2 %>% 
  ggplot(data=., aes(x,y,fill=as_factor(cz)))+
  geom_sf(inherit.aes = F, data=oz_poly,
          fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,155),
           ylim = c(-45,-10), 
           expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  scale_fill_viridis_d(option='B',direction = -1,end=0.95)+
  # scico::scale_fill_scico_d(end=0.9,direction = 1)+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  guides(fill=guide_legend(title='Climate Zone', 
                           title.position = 'top'))+
  theme(legend.position = c(0.64,0.925), 
        legend.direction = 'horizontal',
        panel.grid = element_blank()); p_left


left_join(jj2 %>% select(x,y), bom) %>% pull(koppen) %>% hist
left_join(jj2 %>% select(x,y), bom) %>% 
 ggplot(data=.,aes(x,y,fill=as_factor(koppen)))+
 geom_sf(inherit.aes = F, data=oz_poly,
          fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,155),
           ylim = c(-45,-10), 
           expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  scico::scale_fill_scico_d()

left_join(jj2 %>% select(x,y), bom) %>% 
  mutate(zone = case_when(between(koppen,0,11) ~ 'Temperate', 
                          (y <= -40) ~ 'Tasmania',
                          between(koppen, 12,21)~'Grassland & Desert', # Grassland
                          between(koppen, 22,30)~'Grassland & Desert', # Desert
                          between(koppen, 31,34)~'Subtropical',
                          between(koppen, 35,40)~'Tropical', 
                          koppen >= 41 ~ 'Equatorial')) %>% 
  mutate(zone = ifelse(y < -40, 'Temperate Tas.', zone)) %>% #pull(zone) %>% table
  mutate(zone = factor(zone, levels = c("Equatorial","Tropical",
                                        "Subtropical","Grassland & Desert",
                                        "Temperate","Temperate Tas."), ordered = T)) %>% 
  ggplot(data=.,aes(x,y,fill=zone))+
  geom_sf(inherit.aes = F, data=oz_poly,
          fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,155),
           ylim = c(-45,-10), 
           expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  scale_fill_viridis_d(option='B')
  # scico::scale_fill_scico_d(direction = -1)


# 41 Equatorial
# 
# 35 Tropical # 35-40
# 
# 32 Subtropical # 32-34
# 
# 22 Desert   # 
# 
# 13 Grassland # 12-20
# 
# 3 Temperate # 1-11

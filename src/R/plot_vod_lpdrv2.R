library(tidyverse);library(lubridate); library(arrow)
# ascending ~ 1:30am 
# descending ~ 1:30pm

d <- read_parquet("../data_general/lpdr_test/VOD_LPDRv2_OZ.parquet")
d

d_grid <- d %>% select(lon,lat) %>% distinct() %>% 
 st_as_sf(d_grid, coords = c("lon","lat"))

d <- d %>% 
  inner_join(., 
             d %>% 
  group_by(lon,lat,month) %>% 
  summarize(vod_asc_u = mean(vod_asc,na.rm=T), 
            vod_desc_u = mean(vod_desc,na.rm=T), 
            vod_sigma_u = mean(vod_sigma,na.rm=T), 
            vod_lambda_u = mean(vod_lambda,na.rm=T)) %>% 
  ungroup(), 
  by=c("lon","lat","month")) %>% 
  mutate(vod_asc_anom = vod_asc - vod_asc_u, 
         vod_desc_anom = vod_desc - vod_desc_u, 
         vod_sigma_anom = vod_sigma - vod_sigma_u, 
         vod_lamda_anom = vod_lambda - vod_lambda_u)


vec_cols <- RColorBrewer::brewer.pal(n=7, name='BrBG')
p <- d %>% 
  # mutate(date = ymd(paste(year,month,1))) %>% 
  # mutate(hydro_year = year(date-months(6))) %>% 
  filter(year %in% 2003:2019) %>%
  # filter(month %in% c(12,1,2)) %>%
  mutate(lat=(1/4)*floor(lat*4)) %>%
  mutate(month = month(month, label = T)) %>% 
  ggplot(data=., aes(lon,lat,fill=vod_asc_anom))+
  geom_tile(col=NA)+
  coord_equal()+
  scale_fill_gradient2("VOD anomaly", 
                       low = vec_cols[1], 
                       high=vec_cols[7], 
                       mid='grey90', 
                       limits=c(-0.5,0.5), 
                       oob=scales::squish)+
  labs(x=NULL, y=NULL, title='Vegetation Optical Depth: Ascending Pass (1:30 am)')+
  facet_grid(row=vars(year), 
             cols = vars(month))+
  theme(panel.background = element_rect(fill='black'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_timeseries_fullOZ_vod_asc_anom.png', 
       dpi=400, width=35, height = 35, units='cm', scale=0.8)




# EOZ - VOD Asc Anom ----------------------------------------------------------
vec_cols <- RColorBrewer::brewer.pal(n=7, name='BrBG')
p <- d %>% 
  filter(lon>=140) %>% 
  filter(lat <= -20) %>% 
  filter(year %in% 2003:2019) %>%
  # filter(month %in% c(12,1,2)) %>%
  mutate(lat=(1/4)*floor(lat*4)) %>%
  mutate(month = month(month, label = T)) %>% 
  ggplot(data=., aes(lon,lat,fill=vod_asc_anom))+
  geom_tile(col=NA)+
  coord_equal()+
  scale_fill_gradient2("VOD anomaly", 
                       low = vec_cols[1], 
                       high=vec_cols[7], 
                       mid='grey90', 
                       limits=c(-0.5,0.5), 
                       oob=scales::squish)+
  labs(x=NULL, y=NULL, title='Vegetation Optical Depth Anomaly', 
       subtitle="Ascending Pass (1:30 am)")+
  facet_grid(row=vars(year), 
             cols = vars(month))+
  theme(panel.background = element_rect(fill='black'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_timeseries_EOZ_vod_asc_anom.png', 
       dpi='retina', 
       width=20, height = 35,
       units='cm', scale=0.8)

# EOZ - VOD Descending (daytime) Anom ----------------------------------------------------------
vec_cols <- RColorBrewer::brewer.pal(n=7, name='BrBG')
p <- d %>% 
  filter(lon>=140) %>% 
  filter(lat <= -20) %>% 
  filter(year %in% 2003:2019) %>%
  # filter(month %in% c(12,1,2)) %>%
  mutate(lat=(1/4)*floor(lat*4)) %>%
  mutate(month = month(month, label = T)) %>% 
  ggplot(data=., aes(lon,lat,fill=vod_desc_anom))+
  geom_tile(col=NA)+
  coord_equal()+
  scale_fill_gradient2("VOD anomaly", 
                       low = vec_cols[1], 
                       high=vec_cols[7], 
                       mid='grey90', 
                       limits=c(-0.5,0.5), 
                       oob=scales::squish)+
  labs(x=NULL, y=NULL, title='Vegetation Optical Depth Anomaly', 
       subtitle="Descending Pass (1:30 pm)")+
  facet_grid(row=vars(year), 
             cols = vars(month))+
  theme(panel.background = element_rect(fill='black'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_timeseries_EOZ_vod_desc_anom.png', 
       dpi='retina', 
       width=20, height = 35,
       units='cm', scale=0.8)


# EOZ - VOD Lambda Anom ----------------------------------------------------------
RColorBrewer::display.brewer.all(colorblindFriendly = T)
vec_cols <- RColorBrewer::brewer.pal(n=7, name='PuOr')
p <- d %>% 
  filter(lon>=140) %>% 
  filter(lat <= -20) %>% 
  filter(year %in% 2003:2019) %>%
  # filter(month %in% c(12,1,2)) %>%
  mutate(lat=(1/4)*floor(lat*4)) %>%
  mutate(month = month(month, label = T)) %>% 
  ggplot(data=., aes(lon,lat,fill=vod_lamda_anom))+
  geom_tile(col=NA)+
  coord_equal()+
  scale_fill_gradient2("VOD anomaly", 
                       low = vec_cols[7], 
                       high=vec_cols[1], 
                       mid='grey90', 
                       limits=c(-1.5,1.5),
                       oob=scales::squish)+
  labs(x=NULL, y=NULL, title='Lambda - Vegetation Optical Depth Anomaly' 
       # subtitle="Descending Pass (1:30 pm)"
       )+
  facet_grid(row=vars(year), 
             cols = vars(month))+
  theme(panel.background = element_rect(fill='black'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_timeseries_EOZ_vod_lambda_anom.png', 
       dpi='retina', 
       width=20, height = 35,
       units='cm', scale=0.8)

# EOZ - VOD Sigma Anom ----------------------------------------------------------
RColorBrewer::display.brewer.all(colorblindFriendly = T)
vec_cols <- RColorBrewer::brewer.pal(n=7, name='RdBu')
p <- d %>% 
  filter(lon>=140) %>% 
  filter(lat <= -20) %>% 
  filter(year %in% 2003:2019) %>%
  # filter(month %in% c(12,1,2)) %>%
  mutate(lat=(1/4)*floor(lat*4)) %>%
  mutate(month = month(month, label = T)) %>% 
  ggplot(data=., aes(lon,lat,fill=vod_sigma_anom))+
  geom_tile(col=NA)+
  coord_equal()+
  scale_fill_gradient2(expression(paste(sigma," ",anomaly)), 
                       low = vec_cols[7], 
                       high=vec_cols[1], 
                       mid='grey90', 
                       limits=c(-2,2),
                       oob=scales::squish)+
  labs(x=NULL, y=NULL, 
       title=expression(paste(sigma,' - Vegetation Optical Depth Anomaly')) 
       # subtitle="Descending Pass (1:30 pm)"
  )+
  facet_grid(row=vars(year), 
             cols = vars(month))+
  theme(panel.background = element_rect(fill='black'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_timeseries_EOZ_vod_sigma_anom.png', 
       dpi='retina', 
       width=20, height = 35,
       units='cm', scale=0.8)



# EOZ - VOD night - day  ----------------------------------------------------------
RColorBrewer::display.brewer.all(colorblindFriendly = T)
vec_cols <- RColorBrewer::brewer.pal(n=7, name='RdBu')
p <- d %>% 
  filter(lon>=140) %>% 
  filter(lat <= -20) %>% 
  filter(year %in% 2003:2019) %>%
  # filter(month %in% c(12,1,2)) %>%
  mutate(lat=(1/4)*floor(lat*4)) %>%
  mutate(month = month(month, label = T)) %>% 
  ggplot(data=., aes(lon,lat,fill=vod_asc-vod_desc))+
  geom_tile(col=NA)+
  coord_equal()+
  # scale_fill_viridis_c()+
  scale_fill_gradient2(expression(paste(Delta," diurnal ","VOD")),
                       low = vec_cols[7],
                       high=vec_cols[1],
                       mid='grey90',
                       limits=c(-0.2,0.2),
                       oob=scales::squish)+
  labs(x=NULL, y=NULL, 
       title=expression(paste(Delta,' Diurnal Vegetation Optical Depth')) 
       # subtitle="Descending Pass (1:30 pm)"
  )+
  facet_grid(row=vars(year), 
             cols = vars(month))+
  theme(panel.background = element_rect(fill='black'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_timeseries_EOZ_vod_ddiff.png', 
       dpi='retina', 
       width=17, height = 35,
       units='cm', scale=0.8)


# EOZ - VOD Sigma Anom ----------------------------------------------------------
RColorBrewer::display.brewer.all(colorblindFriendly = T)
vec_cols <- RColorBrewer::brewer.pal(n=7, name='RdBu')
p <- d %>% 
  filter(lon>=140) %>% 
  filter(lat <= -20) %>% 
  filter(year %in% 2003:2019) %>%
  # filter(month %in% c(12,1,2)) %>%
  mutate(lat=(1/4)*floor(lat*4)) %>%
  mutate(month = month(month, label = T)) %>% 
  ggplot(data=., aes(lon,lat,fill=vod_sigma))+
  geom_tile(col=NA)+
  coord_equal()+
  scale_fill_viridis_c(expression(paste(sigma)), 
                       option='B', 
                       limits=c(0,2), 
                       oob=scales::squish)+
  # scale_fill_gradient2(expression(paste(sigma)), 
  #                      low = vec_cols[7], 
  #                      high=vec_cols[1], 
  #                      mid='grey90', 
  #                      limits=c(-2,2),
  #                      oob=scales::squish)+
  labs(x=NULL, y=NULL, 
       title=expression(paste(sigma,' - Vegetation Optical Depth')) 
       # subtitle="Descending Pass (1:30 pm)"
  )+
  facet_grid(row=vars(year), 
             cols = vars(month))+
  theme(panel.background = element_rect(fill='black'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_timeseries_EOZ_vod_sigma.png', 
       dpi='retina', 
       width=17, height = 35,
       units='cm', scale=0.8)



# Plot VOD seasonally OZ -----------------------------------------------------
tmp <- read_parquet("../data_general/Oz_misc_data/VOD_Seasonaly_LPDRv2.parquet")
RColorBrewer::display.brewer.all(colorblindFriendly = T)
vec_cols <- RColorBrewer::brewer.pal(n=7, name='RdBu')
p <- tmp %>% 
  filter(hydro_year %in% 2003:2019) %>% 
  # filter(lon>=140) %>% 
  # filter(lat <= -20) %>% 
  # filter(year %in% 2003:2019) %>%
  # mutate(lat=(1/4)*floor(lat*4)) %>%
  ggplot(data=., aes(x,y,fill=vod_sigma))+
  geom_tile(col=NA)+
  coord_equal()+
  scale_fill_viridis_c(expression(paste(sigma)), 
                       option='B', 
                       limits=c(0,2), 
                       oob=scales::squish)+
  # scale_fill_gradient2(expression(paste(sigma)), 
  #                      low = vec_cols[7], 
  #                      high=vec_cols[1], 
  #                      mid='grey90', 
  #                      limits=c(-2,2),
  #                      oob=scales::squish)+
  labs(x=NULL, y=NULL, 
       title=expression(paste(sigma,' - Vegetation Optical Depth')) 
       # subtitle="Descending Pass (1:30 pm)"
  )+
  facet_grid(cols=vars(hydro_year), 
             row = vars(season))+
  theme(panel.background = element_rect(fill='black'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_seasonaltimeseries_OZ_vod_sigma.png', 
       dpi='retina', 
       width=35, height = 17,
       units='cm', scale=0.8)



# Plot VOD seasonally EOZ -----------------------------------------------------
tmp <- read_parquet("../data_general/Oz_misc_data/VOD_Seasonaly_LPDRv2.parquet")
RColorBrewer::display.brewer.all(colorblindFriendly = T)
vec_cols <- RColorBrewer::brewer.pal(n=7, name='RdBu')
p <- tmp %>% 
  filter(hydro_year %in% 2003:2019) %>% 
  filter(x > 13338903) %>%
  filter(y <= -2644624) %>%
  # filter(year %in% 2003:2019) %>%
  # mutate(lat=(1/4)*floor(lat*4)) %>%
  ggplot(data=., aes(x,y,fill=vod_sigma))+
  geom_tile(col=NA)+
  coord_equal()+
  scale_fill_viridis_c(expression(paste(sigma)), 
                       option='B', 
                       limits=c(0,2), 
                       oob=scales::squish)+
  # scale_fill_gradient2(expression(paste(sigma)), 
  #                      low = vec_cols[7], 
  #                      high=vec_cols[1], 
  #                      mid='grey90', 
  #                      limits=c(-2,2),
  #                      oob=scales::squish)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, y=NULL, 
       title=expression(paste(sigma,' - Vegetation Optical Depth')) 
       # subtitle="Descending Pass (1:30 pm)"
  )+
  facet_grid(cols=vars(hydro_year), 
             row = vars(season))+
  theme(panel.background = element_rect(fill='grey30'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_seasonaltimeseries_EOZ_vod_sigma.png', 
       dpi='retina', 
       width=35, height = 17,
       units='cm', scale=0.8)


# Plot VOD ddiff seasonally OZ -----------------------------------------------------
tmp <- read_parquet("../data_general/Oz_misc_data/VOD_Seasonaly_LPDRv2.parquet")
RColorBrewer::display.brewer.all(colorblindFriendly = T)
vec_cols <- RColorBrewer::brewer.pal(n=7, name='RdBu')
p <- tmp %>% 
  filter(hydro_year %in% 2003:2019) %>% 
  filter(x > 13338903) %>%
  filter(y <= -2644624) %>%
  inner_join(., {(.) %>% group_by(x,y,season) %>% 
      summarize(vod_ddiff_u = mean(vod_ddiff,na.rm=T)) %>% 
      ungroup()}) %>% 
  mutate(vod_ddiff_anom = vod_ddiff - vod_ddiff_u) %>% 
  ggplot(data=., aes(x,y,fill=vod_ddiff_anom))+
  geom_tile(col=NA)+
  coord_equal()+
  # scale_fill_viridis_c(expression(paste(sigma)), 
  #                      option='B', 
  #                      limits=c(), 
  #                      oob=scales::squish)+
  scale_fill_gradient2(expression(paste(Delta~Diurnal~VOD~Anom.)),
                       low = vec_cols[7],
                       high=vec_cols[1],
                       mid='grey90',
                       limits=c(-0.2,0.2),
                       oob=scales::squish)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, y=NULL, 
       title=expression(paste(Delta,' Diurnal VOD Anom.')) 
       # subtitle="Descending Pass (1:30 pm)"
  )+
  facet_grid(cols=vars(hydro_year), 
             row = vars(season))+
  theme(panel.background = element_rect(fill='grey30'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_seasonaltimeseries_EOZ_vod_ddiff_anom.png', 
       dpi='retina', 
       width=37, height = 20,
       units='cm', scale=0.8)


# Plot VOD ASC seasonally OZ -----------------------------------------------------
tmp <- read_parquet("../data_general/Oz_misc_data/VOD_Seasonaly_LPDRv2.parquet")
RColorBrewer::display.brewer.all(colorblindFriendly = T)
vec_cols <- RColorBrewer::brewer.pal(n=7, name='BrBG')
p <- tmp %>% 
  filter(hydro_year %in% 2003:2019) %>% 
  filter(x > 13338903) %>%
  filter(y <= -2644624) %>%
  inner_join(., {(.) %>% group_by(x,y,season) %>% 
      summarize(vod_asc_u = mean(vod_asc,na.rm=T)) %>% 
      ungroup()}) %>% 
  mutate(vod_asc_anom = vod_asc - vod_asc_u) %>% 
  ggplot(data=., aes(x,y,fill=vod_asc_anom))+
  geom_tile(col=NA)+
  coord_equal()+
  # scale_fill_viridis_c(expression(paste(sigma)), 
  #                      option='B', 
  #                      limits=c(), 
  #                      oob=scales::squish)+
  scale_fill_gradient2(expression(paste(VOD~Anom.)),
                       low = vec_cols[1],
                       high=vec_cols[7],
                       mid='grey90',
                       limits=c(-0.35,0.35),
                       oob=scales::squish)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, y=NULL, 
       title=expression(paste('Night (Ascending) Diurnal VOD Anom.')) 
       # subtitle="Descending Pass (1:30 pm)"
  )+
  facet_grid(cols=vars(hydro_year), 
             row = vars(season))+
  theme(panel.background = element_rect(fill='grey30'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_seasonaltimeseries_EOZ_vod_asc_anom.png', 
       dpi='retina', 
       width=37, height = 20,
       units='cm', scale=0.8)

# Plot VOD ASC seasonally OZ -----------------------------------------------------
tmp <- read_parquet("../data_general/Oz_misc_data/VOD_Seasonaly_LPDRv2.parquet")
RColorBrewer::display.brewer.all(colorblindFriendly = T)
vec_cols <- RColorBrewer::brewer.pal(n=7, name='BrBG')
p <- tmp %>% 
  filter(hydro_year %in% 2003:2019) %>% 
  filter(x > 13338903) %>%
  filter(y <= -2644624) %>%
  inner_join(., {(.) %>% group_by(x,y,season) %>% 
      summarize(vod_desc_u = mean(vod_desc,na.rm=T)) %>% 
      ungroup()}) %>% 
  mutate(vod_desc_anom = vod_desc - vod_desc_u) %>% 
  ggplot(data=., aes(x,y,fill=vod_desc_anom))+
  geom_tile(col=NA)+
  coord_equal()+
  # scale_fill_viridis_c(expression(paste(sigma)), 
  #                      option='B', 
  #                      limits=c(), 
  #                      oob=scales::squish)+
  scale_fill_gradient2(expression(paste(VOD~Anom.)),
                       low = vec_cols[1],
                       high=vec_cols[7],
                       mid='grey90',
                       limits=c(-0.35,0.35),
                       oob=scales::squish)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, y=NULL, 
       title=expression(paste('Daytime (Descending) Diurnal VOD Anom.')) 
       # subtitle="Descending Pass (1:30 pm)"
  )+
  facet_grid(cols=vars(hydro_year), 
             row = vars(season))+
  theme(panel.background = element_rect(fill='grey30'), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, units = 'cm'))
ggsave(p, filename = 'figures/map_seasonaltimeseries_EOZ_vod_desc_anom.png', 
       dpi='retina', 
       width=37, height = 20,
       units='cm', scale=0.8)

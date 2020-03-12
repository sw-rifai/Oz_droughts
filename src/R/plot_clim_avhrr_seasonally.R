library(tidyverse); library(lubridate); library(arrow)

#***************************************************************************
# Import EVI2 and clim (ERA5-Land) -----------------------------------------
#***************************************************************************
avhrr <- read_parquet(file="../data_general/AVHRR_EVI2_CDR_V5/EOz_AVHRR_EVI2_LAI_1981_2019.parquet")
ex_pet <- read_parquet(file='../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_PET_1979_2019.parquet')
ex_vpd <- read_parquet(file="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_vpd_1979_2019.parquet")
ex_precip <- read_parquet(file='../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_precip_1979_2019.parquet')
ex <- inner_join(ex_pet %>% select(-days_in_month), 
                 ex_vpd, 
                 by=c("id","date"))
ex <- inner_join(ex, ex_precip %>% select(-days_in_month), 
                 by=c("id","date"))
d <- inner_join(ex, 
                avhrr, 
                by=c("id","date"))
d <- d %>% 
  mutate(quarter = quarter(date, fiscal_start = 11))
d <- d %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"OSN"))
d <- d %>% mutate(season = factor(q,
                                  levels=c("DJF","MAM","JJA","OSN"), 
                                  ordered=T))
d <- d %>% 
  mutate(hydro_year = year(date+months(1)))
ds <- d %>% 
  filter(hydro_year >= 1981 & hydro_year <= 2019) %>% 
  group_by(hydro_year, season,lon,lat) %>% 
  summarize(evi2 = mean(evi2, na.rm=T), 
            evi2_anom = mean(evi2_anom, na.rm=T), 
            evi2_anom_sd = mean(evi2_anom_sd, na.rm=T), 
            precip = sum(precip), 
            t2m = mean(t2m, na.rm = T), 
            vpd = mean(vpd,na.rm = T), 
            pet = mean(pet,na.rm=T)) %>% 
  ungroup()
ds_clim <- d %>% 
  filter(hydro_year >= 1982 & hydro_year <= 2010) %>% 
  group_by(season,lon,lat) %>% 
  summarize(evi2_u = mean(evi2, na.rm=T), 
            evi2_sd = sd(evi2, na.rm=T), 
            precip_u = mean(precip)*3, 
            precip_sd = sd(precip)*3, 
            t2m_u = mean(t2m, na.rm = T), 
            t2m_sd = sd(t2m, na.rm = T), 
            vpd_u = mean(vpd,na.rm = T),
            vpd_sd = sd(vpd,na.rm=T),
            pet_u = mean(pet,na.rm=T), 
            pet_sd = sd(pet,na.rm=T)) %>% 
  ungroup()

ds <- inner_join(ds, ds_clim, by=c("lon","lat","season"))
rm(d); 
gc();
#********************************************************************************


#********************************************************************************
# Absolute EVI2 Seasonal Anomaly ------------------------------------------
#********************************************************************************
# RColorBrewer::display.brewer.all(colorblindFriendly = T)
vec_cols <- RColorBrewer::brewer.pal(n=7, name='BrBG')

p <- ds %>% 
  filter(hydro_year %in% c(1982:2019)) %>% 
  ggplot(data=., aes(lon,lat,fill=evi2_anom))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2("EVI2 Anom.", 
                       high=vec_cols[7], 
                       mid=vec_cols[4], 
                       low=vec_cols[1], 
                       limits=c(-0.15,0.15),
                       oob=scales::squish)+
  # scale_fill_viridis_c(limits=c(0,0.6), 
  #                      oob=scales::squish)+
  scale_x_continuous(limits=c(140,153.5), expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  facet_grid(season~hydro_year)+
  theme(panel.background = element_rect(fill='grey20'), 
        panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = 'bottom', 
        legend.key.width = unit(1, units='cm'))
ggsave(p, filename = "figures/evi2_anom_seasonal_1982_2019.png",
       width=38, height=10, units='cm', dpi='retina')
#********************************************************************************
# end ------------------------------------------
#********************************************************************************

#********************************************************************************
# Absolute EVI2 Seasonal Anomaly ------------------------------------------
#********************************************************************************
# RColorBrewer::display.brewer.all(colorblindFriendly = T)
vec_cols <- RColorBrewer::brewer.pal(n=7, name='BrBG')

p <- ds %>% 
  filter(hydro_year %in% c(1982:2019)) %>% 
  ggplot(data=., aes(lon,lat,fill=evi2_anom_sd))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(paste("EVI2 Anom. ",sigma)), 
                       high=vec_cols[7], 
                       mid=vec_cols[4], 
                       low=vec_cols[1], 
                       limits=c(-2,2),
                       oob=scales::squish)+
  # scale_fill_viridis_c(limits=c(0,0.6), 
  #                      oob=scales::squish)+
  scale_x_continuous(limits=c(140,153.5), expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  facet_grid(season~hydro_year)+
  theme(panel.background = element_rect(fill='grey20'), 
        panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = 'bottom', 
        legend.key.width = unit(1, units='cm'))
ggsave(p, filename = "figures/evi2_anom_sd_seasonal_1982_2019.png",
       width=38, height=10, units='cm', dpi='retina')
#********************************************************************************
# end ------------------------------------------
#********************************************************************************



#********************************************************************************
#  ------------------------------------------
#********************************************************************************
# library(ggpointdensity)
p <- ds %>% 
  mutate(pet_anom = pet-pet_u, 
         precip_anom = precip-precip_u) %>% 
  mutate(pet_anom_sd = pet_anom/pet_sd, 
         precip_anom_sd = precip_anom/precip_sd) %>% 
  filter(hydro_year %in% c(2010:2019)) %>% 
  ggplot(data=., aes(pet_anom_sd, precip_anom_sd, color=evi2_anom_sd))+
  geom_hline(aes(yintercept=0),col='grey90')+
  geom_vline(aes(xintercept=0),col='grey90')+
  # geom_pointdensity()+
  geom_point(alpha=0.25,size=0.5)+
  # scale_color_viridis_c(option='A')+
  scale_color_gradient2(expression(paste(EVI2~Anom.~(sigma))),
    limits=c(-3,3), oob=scales::squish)+
  scale_x_continuous(expand=c(0,0), limits=c(-4,4))+
  scale_y_continuous(expand=c(0,0), limits=c(-4,4))+
  labs(x=expression(paste(PET~Anom.~(sigma))), 
       y=expression(paste(Precip~Anom.~(sigma))))+
  theme_linedraw()+
  facet_grid(season~hydro_year)+
  theme(panel.background = element_rect(fill='grey20'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm'))
ggsave(p, filename = "figures/scatterplot_evi2_anom_sd_seasonal_selYears.png", 
       width=25, height=10, units='cm', dpi='retina')
#********************************************************************************
# end ------------------------------------------
#********************************************************************************




p_1980 <- ds %>% 
  mutate(pet_anom = pet-pet_u, 
         precip_anom = precip-precip_u) %>% 
  mutate(pet_anom_sd = pet_anom/pet_sd, 
         precip_anom_sd = precip_anom/precip_sd) %>% 
  filter(hydro_year %in% c(1980:1989)) %>% 
  ggplot(data=., aes(pet_anom_sd, precip_anom_sd, color=evi2_anom_sd))+
  geom_hline(aes(yintercept=0),col='grey90')+
  geom_vline(aes(xintercept=0),col='grey90')+
  # geom_pointdensity()+
  geom_point(alpha=0.25,size=0.5)+
  # scale_color_viridis_c(option='A')+
  scale_color_gradient2(expression(paste(EVI2~Anom.~(sigma))),
                        limits=c(-3,3), oob=scales::squish)+
  scale_x_continuous(expand=c(0,0), limits=c(-4,4))+
  scale_y_continuous(expand=c(0,0), limits=c(-4,4))+
  labs(x=expression(paste(PET~Anom.~(sigma))), 
       y=expression(paste(Precip~Anom.~(sigma))))+
  theme_linedraw()+
  facet_grid(season~hydro_year)+
  theme(panel.background = element_rect(fill='grey20'), 
        panel.grid = element_blank(), 
        legend.position = 'none', 
        legend.key.width = unit(1, 'cm'))

p_1990 <- ds %>% 
  mutate(pet_anom = pet-pet_u, 
         precip_anom = precip-precip_u) %>% 
  mutate(pet_anom_sd = pet_anom/pet_sd, 
         precip_anom_sd = precip_anom/precip_sd) %>% 
  filter(hydro_year %in% c(1990:1999)) %>% 
  ggplot(data=., aes(pet_anom_sd, precip_anom_sd, color=evi2_anom_sd))+
  geom_hline(aes(yintercept=0),col='grey90')+
  geom_vline(aes(xintercept=0),col='grey90')+
  # geom_pointdensity()+
  geom_point(alpha=0.25,size=0.5)+
  # scale_color_viridis_c(option='A')+
  scale_color_gradient2(expression(paste(EVI2~Anom.~(sigma))),
                        limits=c(-3,3), oob=scales::squish)+
  scale_x_continuous(expand=c(0,0), limits=c(-4,4))+
  scale_y_continuous(expand=c(0,0), limits=c(-4,4))+
  labs(x=expression(paste(PET~Anom.~(sigma))), 
       y=expression(paste(Precip~Anom.~(sigma))))+
  theme_linedraw()+
  facet_grid(season~hydro_year)+
  theme(panel.background = element_rect(fill='grey20'), 
        panel.grid = element_blank(), 
        legend.position = 'none', 
        legend.key.width = unit(1, 'cm'))

p_2000 <- ds %>% 
  mutate(pet_anom = pet-pet_u, 
         precip_anom = precip-precip_u) %>% 
  mutate(pet_anom_sd = pet_anom/pet_sd, 
         precip_anom_sd = precip_anom/precip_sd) %>% 
  filter(hydro_year %in% c(2000:2009)) %>% 
  ggplot(data=., aes(pet_anom_sd, precip_anom_sd, color=evi2_anom_sd))+
  geom_hline(aes(yintercept=0),col='grey90')+
  geom_vline(aes(xintercept=0),col='grey90')+
  # geom_pointdensity()+
  geom_point(alpha=0.25,size=0.5)+
  # scale_color_viridis_c(option='A')+
  scale_color_gradient2(expression(paste(EVI2~Anom.~(sigma))),
                        limits=c(-3,3), oob=scales::squish)+
  scale_x_continuous(expand=c(0,0), limits=c(-4,4))+
  scale_y_continuous(expand=c(0,0), limits=c(-4,4))+
  labs(x=expression(paste(PET~Anom.~(sigma))), 
       y=expression(paste(Precip~Anom.~(sigma))))+
  theme_linedraw()+
  facet_grid(season~hydro_year)+
  theme(panel.background = element_rect(fill='grey20'), 
        panel.grid = element_blank(), 
        legend.position = 'none', 
        legend.key.width = unit(1, 'cm'))

p_2010 <- ds %>% 
  mutate(pet_anom = pet-pet_u, 
         precip_anom = precip-precip_u) %>% 
  mutate(pet_anom_sd = pet_anom/pet_sd, 
         precip_anom_sd = precip_anom/precip_sd) %>% 
  filter(hydro_year %in% c(2010:2019)) %>% 
  ggplot(data=., aes(pet_anom_sd, precip_anom_sd, color=evi2_anom_sd))+
  geom_hline(aes(yintercept=0),col='grey90')+
  geom_vline(aes(xintercept=0),col='grey90')+
  # geom_pointdensity()+
  geom_point(alpha=0.25,size=0.5)+
  # scale_color_viridis_c(option='A')+
  scale_color_gradient2(expression(paste(EVI2~Anom.~(sigma))),
                        limits=c(-3,3), oob=scales::squish)+
  scale_x_continuous(expand=c(0,0), limits=c(-4,4))+
  scale_y_continuous(expand=c(0,0), limits=c(-4,4))+
  labs(x=expression(paste(PET~Anom.~(sigma))), 
       y=expression(paste(Precip~Anom.~(sigma))))+
  theme_linedraw()+
  facet_grid(season~hydro_year)+
  theme(panel.background = element_rect(fill='grey20'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm'))

library(patchwork)
big_p <- (p_1980/p_1990/p_2000/p_2010)
ggsave(big_p, 
       filename='figures/scatterplot_evi2_anom_sd_seasonal_1981_2019.png', 
       width=25, height=40, units = 'cm', dpi='retina')

big_p2 <- ((p_1980+p_1990)/(p_2000+p_2010))
ggsave(big_p2, 
       filename='figures/scatterplot_evi2_anom_sd_seasonal_hvAlign_1981_2019.png', 
       width=40, height=30, units = 'cm', dpi='retina')

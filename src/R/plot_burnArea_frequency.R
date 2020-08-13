library(tidyverse); library(stars); library(sf); library(data.table); 
library(dtplyr);
library(lubridate); 

# load("data/gridCell_lm_ndvi_clim.Rdata") # grid cell linear regressions
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)

kop <- arrow::read_parquet("../data_general/Koppen_climate/BOM_Koppen_simplified7.parquet")
pa <- stars::read_stars("../data_general/MCD64/MCD64_Oz/MCD64A1_AVHRRres_pixel_area_.tif", 
                        proxy = F) %>% 
  as_tibble() %>% 
  set_names(c("x","y","area_m2"))
ba <- stars::read_stars("../data_general/MCD64/MCD64_Oz/MCD64A1_BurnArea_reducedToAVHRRres_2001_2019_.tif", 
                        proxy = F) %>% 
  stars::st_set_dimensions(., 3, values=seq(ymd("2001-01-01"),ymd("2019-12-01"),by='1 month')) %>% 
  as_tibble() %>% 
  purrr::set_names(c('x','y','date','ba'))
ba <- ba %>% inner_join(., kop, by=c("x","y"))

ba$ba %>% max

ba %>% lazy_dt() %>% 
  group_by(x,y) %>% 
  summarize(burn = sum(ba>(0.25*30194220))) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  ggplot(data=.,aes(x,y,fill=burn))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  scale_fill_viridis_c('Burn Frequency', 
                       option='B', trans='log1p')+
  labs(x=NULL,y=NULL,
       subtitle='Event count where >25% of 25km2 grid cell burned')+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank())
  



# Burn Area trend map -----------------------------------------------------
library(RcppArmadillo)
ba <- ba %>% mutate(year=year(date))
ba$year %>% mean
system.time(
  lt_ba <- ba %>% as.data.table() %>% .[,`:=`(year_c = year-2010)] %>%
    .[,`:=`(ba_km2 = ba/(1000**2))] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=ba_km2, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)


p_mab <- lt_ba %>% 
  ggplot(data=.,aes(x,y,fill=b0))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray40',color='black')+
  geom_tile()+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), 
           expand = FALSE)+
  # scale_fill_gradient2()+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  scale_fill_viridis_b(expression(paste("Mean Annual Burn Area",(km**2))),
                       option='B', begin = 0.1)+
  labs(x=NULL,y=NULL,
       title='Mean Burn Area',
       subtitle='2001-2019',caption = 'Data source: MCD64')+
  guides(fill = guide_colorbar(title.position = 'top'))+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(),
        legend.title = element_text(size=8),
        legend.position = 'bottom'); p_mab

# Relative trend in BA
p_rt <- lt_ba %>% 
  ggplot(data=.,aes(x,y,fill=100*b1/b0))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray30',color='black')+
  geom_tile()+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  khroma::scale_fill_sunset("Relative Annual Change (%)")+
  # scale_fill_viridis_c('Burn Area Trend (km2 yr-1)', 
  #                      option='B')+
  labs(x=NULL,y=NULL,
       title='Relative Trend',subtitle = "2001-2019")+
  guides(fill = guide_colorbar(title.position = 'top'))+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=8),
        legend.position = 'bottom'); p_rt

# Absolute trend in BA
p_at <- lt_ba %>% 
  ggplot(data=.,aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray30',color='black')+
  geom_tile()+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  khroma::scale_fill_sunset(expression(paste("Burn Area (",km**2~yr**-1,")")), 
                            limits=c(-0.1,0.1),oob=scales::squish)+
  labs(x=NULL,y=NULL,
       title='Relative Trend',subtitle = '2001-2019')+
  guides(fill = guide_colorbar(title.position = 'top'))+
  theme(panel.background = element_rect(fill='lightblue'),
        panel.grid = element_blank(), 
        legend.title = element_text(size=8),
        legend.position = 'bottom'); p_at



# Burn area trend ---------------------------------------------------
d_tmp <- inner_join({ba %>% mutate(hydro_year=year(date+months(1))) %>% 
  group_by(hydro_year,zone) %>% 
  summarize(ba_km2 = sum(ba/1e6,na.rm=TRUE)) %>% 
  ungroup()}, 
  {ba %>% group_by(zone) %>% 
              summarize(mab=sum(ba/(1e6),na.rm=TRUE)/19) %>% 
              ungroup()}, 
          by=c("zone")) %>% 
  mutate(val = ba_km2/mab)
p_ts <- d_tmp %>% 
  ggplot(data=.,aes(hydro_year, 100*(val-1),color=zone))+
  # geom_line()+
  geom_smooth(method='lm',se=F)+
  scale_color_viridis_d("",
                        option='B',end=0.9, direction=-1,
                        limits=rev(c("Equatorial","Tropical", 
                                     "Subtropical", "Grassland","Desert", 
                                     "Temperate","Temperate Tas.")), 
                        labels=str_wrap(rev(c("Equat.","Trop.", 
                                              "Subtrop.", "Grassland", "Arid", 
                                              "Temperate","Temp. Tasm.")),width = 10))+
  scale_x_continuous(expand=c(0,0))+
  labs(y=expression(paste('Total Burn Area ',(km**2))), 
       x=NULL)+
  theme_linedraw()+
  theme(legend.position = c(0.1,0.01), 
        legend.justification = c(0.01,0.01), 
        legend.direction = 'horizontal', 
        legend.background = element_blank()); p_ts
  

p_out <- p_mab+p_rt+p_at+plot_layout(guides='keep',ncol = 3)
p_out/p_ts+plot_layout(heights=c(2,1))
ggsave(p_out/p_ts+plot_layout(heights=c(2,1.25)), 
       filename = 'figures/mcd64_burnArea_mean_trend.png', 
       width=16, height = 22, units='cm', dpi=350, type='cairo')

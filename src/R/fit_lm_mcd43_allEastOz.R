library(stars); library(tidyverse); library(data.table); library(lubridate)
setDTthreads(threads = 10)
library(dtplyr, warn.conflicts = FALSE);
library(RcppArmadillo)

#*******************************************************************************
# MCD43  ------------------------------------------------------------
#*******************************************************************************
mcd <- stars::read_stars("../data_general/MCD43/MCD43A4_NDVI_5000m_EastOz_mmean_maskFireDefor_2001_2019.tif") %>% 
  # slice('band', seq(1,by=2,to = 456)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2001-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>% 
  set_names(c("x","y","date","ndvi"))

base <- stars::read_stars("../data_general/MCD43/MCD43A4_NDVI_5000m_EastOz_mmean_maskFireDefor_2001_2019.tif", 
                          RasterIO = list(bands=1))
nvis <- stars::read_stars("../data_general/NVIS/nvis51_majorVegClass_0p05.tif")
nvis2 <- st_warp(src=nvis, dest=base[,,], use_gdal = T)
names(nvis2) <- "veg_class"
nvis <- nvis2 %>% as_tibble() %>% as.data.table()
codes <- readr::read_fwf("../data_general/NVIS/nvis51_majorVegClass_codes.txt", 
                         fwf_widths(c(2,100)), skip = 1) %>% 
  set_names(c("veg_class","veg_class_descrip")) %>% 
  mutate(vc = as.factor(veg_class_descrip)) 
vc <- left_join(nvis, codes, by='veg_class')
mcd <- vc[mcd, on=.(x,y)]
mcd[,`:=`(year=year(date))]


#*******************************************************************************
#* Add season and hydro year -----
#*******************************************************************************
vec_dates <- data.table(date=sort(unique(mcd$date))) %>% 
  .[,quarter:=quarter(date)] %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON")) %>% 
  mutate(season = factor(q,
                         levels=c("DJF","MAM","JJA","SON"), 
                         ordered=T)) %>% 
  select(date,season) %>% 
  mutate(hydro_year = year(date+months(1)))
mcd <- vec_dates[mcd,on=.(date)]


#*******************************************************************************
# Fit linear models by VC -------------------------------------------------
#*******************************************************************************
center_year <- mean(mcd$year)
system.time(
  lt_vc <- mcd[is.na(vc)==F][is.na(ndvi)==F][,`:=`(year_c = hydro_year- center_year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=ndvi, data=.SD)$coefficients))), 
      by=.(vc)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(vc)]  
)

system.time(
  lt_vc_season <- mcd[is.na(vc)==F][is.na(ndvi)==F][,`:=`(year_c = hydro_year - center_year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=ndvi, data=.SD)$coefficients))), 
      by=.(vc,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(vc,season)]  
)

#*******************************************************************************
# Fit linear models by x,y -------------------------------------------------
#*******************************************************************************
center_year <- mean(mcd$year)
system.time(
  lt_xy <- mcd[is.na(vc)==F][is.na(ndvi)==F][,`:=`(year_c = hydro_year- center_year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=ndvi, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)

system.time(
  lt_xy_season <- mcd[is.na(vc)==F][is.na(ndvi)==F][,`:=`(year_c = hydro_year- center_year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=ndvi, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)]  
)


# Delta Amplitude ---------------------------------------------------------------
lt_amp_xy <- mcd %>% 
  lazy_dt() %>% 
  filter(is.na(vc)==F) %>% 
  filter(is.na(ndvi)==F) %>% 
  group_by(x,y,hydro_year) %>% 
  summarize(ndvi_amp = max(ndvi,na.rm=TRUE) - min(ndvi,na.rm=TRUE), 
            nobs = sum(is.na(ndvi)==F)) %>% 
  ungroup() %>% 
  as.data.table()
center_year <- mean(mcd$year)
system.time(
  lt_amp_xy <- lt_amp_xy[,`:=`(year_c = hydro_year- center_year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=ndvi_amp, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)



# Plotting ----------------------------------------------------------------

library(ggridges)
vec_vc <- levels(vc$vc[1])
vc[lt_amp_xy,on=.(x,y)] %>% 
  .[!veg_class %in% c(99)] %>%
  ggplot(data=., aes(b1,y=reorder(vc, desc(vc))))+
  stat_density_ridges(
    rel_min_height=0.05,
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    # quantiles = c(0.025, 0.975), 
    quantile_lines = TRUE,
  ) +
  geom_vline(aes(xintercept=0),color='red',alpha=0.5)+
  scale_x_continuous(limits=c(-0.02,0.015))+
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )+
  # scale_y_discrete(limits = rev(levels(vec_vc)))+
  labs(x=expression(paste(Delta~"NDVI Annual Amplitude"~(yr**-1))), 
       y=NULL)+
  theme_linedraw()+
  theme(legend.position = 'none')

vc[lt_amp_xy,on=.(x,y)][is.na(b0)==F & is.na(b1)==F] %>% 
  .[!veg_class %in% c(99,28,24)] %>% 
  .[b0>0 & b0<0.3] %>% 
  .[b1>-0.01 & b1 < 0.01] %>% 
  ggplot(data=., aes(b0,b1))+
  stat_density_2d_filled(contour_var='ndensity')+
  geom_hline(aes(yintercept=0),color='red')+
  facet_wrap(~vc)

p <- inner_join(
 lt_amp_xy %>% as_tibble() %>% select(x,y,b1) %>% rename(delta_amp = b1),
 lt_xy %>% as_tibble() %>% select(x,y,b1) %>% rename(delta_ndvi=b1)) %>% 
  inner_join(vc, by=c('x','y')) %>% 
  filter(!veg_class %in% c(99,28,24,30,26,25,21,31,17)) %>% 
  filter(between(delta_ndvi,-0.01,0.01)) %>% 
  filter(between(delta_amp,-0.01,0.01)) %>% 
  ggplot(data=., aes(delta_ndvi, delta_amp))+
  stat_density_2d_filled(contour_var='ndensity')+
  scale_fill_viridis_d(option='B')+
  geom_hline(aes(yintercept=0),color='white',alpha=0.5)+
  geom_vline(aes(xintercept=0),color='white',alpha=0.5)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  facet_wrap(~str_wrap(vc,30),ncol = 4)+
  labs(x=expression(paste(Delta~NDVI~(yr**-1))), 
       y=expression(paste(Delta~"Annual NDVI Amplitude"~(yr**-1))))+
  guides(fill=guide_legend(title='Density Level'))+
  theme(strip.text = element_text(size=8), 
        legend.position = 'bottom', 
        legend.direction = 'horizontal')
ggsave(p, filename = "figures/Density2d_deltaNDVI_deltaNDVIAmp.png",
       width=23, height=30, units='cm', dpi=250, type='cairo')



p2 <- vc[lt_xy_season,on=.(x,y)] %>% 
  .[!veg_class %in% c(99)] %>% 
  ggplot(data=., aes(b1,y=reorder(vc, desc(vc))))+
  stat_density_ridges(
    rel_min_height=0.05,
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    # quantiles = c(0.025, 0.975), 
    quantile_lines = TRUE,
    fill="#0000FF1A"
  ) +
  geom_vline(aes(xintercept=0),color='red',alpha=1)+
  scale_x_continuous(limits=c(-0.01,0.01))+
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )+
  # scale_y_discrete(limits = rev(levels(vec_vc)))+
  labs(x=expression(paste(Delta~NDVI~(yr**-1))), 
       y=NULL)+
  facet_wrap(~season,nrow = 1)+
  theme_linedraw()+
  theme(legend.position = 'none')
ggsave(p2, filename = "figures/GGjoy_seasonalDeltaNDVI_byVC.png",
       width=25, height=20, units='cm', dpi=250, type='cairo')





# SCRATCH -----------------------------------------------------------------

lt_amp_xy %>% 
  filter(hydro_year==2015) %>% 
  ggplot(data=., aes(x,y,fill=ndvi_amp))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(limits=c(0,1))+
  # facet_wrap(~season)+
  theme_linedraw()+
  theme(panel.grid = element_blank())



lt_amp_xy %>% 
 ggplot(data=., aes(x,y,fill=b1))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(limits=c(-0.015,0.015), 
                       oob=scales::squish)+
  # facet_wrap(~season)+
  theme_linedraw()+
  theme(panel.grid = element_blank())


mcd[date==ymd('2015-01-01')] %>% 
  ggplot(data=., aes(x,y,fill=ndvi))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()


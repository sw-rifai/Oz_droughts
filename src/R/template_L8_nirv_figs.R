library(rasterVis); library(tidyverse); library(stars); library(lubridate)
############################################
list.files("../data_general/Oz_misc_data/",pattern='NIRV')

# wollemi
wollemi <- list.files("../data_general/Oz_misc_data/", pattern = "NIRV_wollemi",full.names = T)[1] %>% 
  read_stars() %>% 
  set_names("nirv_anom") %>% 
  st_set_dimensions(.,3,
   values=seq(ymd("2014-01-01"),by = "3 months",length.out = 24), names = 'date') %>% 
  slice(., 1:23, along='date') %>% 
  as_tibble() %>% 
  filter(is.na(nirv_anom)==F) %>%
  group_by(date
  ) %>% 
  summarize(val = median(nirv_anom,na.rm=TRUE), 
            val_05 = quantile(nirv_anom, 0.10), 
            val_95 = quantile(nirv_anom, 0.90)) %>% 
  ungroup() %>% 
  mutate(region='Wollemi')

torrington <- list.files("../data_general/Oz_misc_data/", pattern = "NIRV_torrington",full.names = T)[1] %>% 
  read_stars() %>% 
  set_names("nirv_anom") %>% 
  st_set_dimensions(.,3,
                    values=seq(ymd("2014-01-01"),by = "3 months",length.out = 24), names = 'date') %>% 
  slice(., 1:23, along='date') %>% 
  as_tibble() %>% 
  filter(is.na(nirv_anom)==F) %>%
  group_by(date
  ) %>% 
  summarize(val = median(nirv_anom,na.rm=TRUE), 
            val_05 = quantile(nirv_anom, 0.10), 
            val_95 = quantile(nirv_anom, 0.90)) %>% 
  ungroup() %>% 
  mutate(region='Torrington')

victoria_se <- list.files("../data_general/Oz_misc_data/", pattern = "NIRV_victoria_se",full.names = T)[1] %>% 
  read_stars() %>% 
  set_names("nirv_anom") %>% 
  st_set_dimensions(.,3,
                    values=seq(ymd("2014-01-01"),by = "3 months",length.out = 24), names = 'date') %>% 
  slice(., 1:23, along='date') %>% 
  as_tibble() %>% 
  filter(is.na(nirv_anom)==F) %>%
  group_by(date
  ) %>% 
  summarize(val = median(nirv_anom,na.rm=TRUE), 
            val_05 = quantile(nirv_anom, 0.10), 
            val_95 = quantile(nirv_anom, 0.90)) %>% 
  ungroup() %>% 
  mutate(region='SE Victoria')

# Yerranderie
yerr <- list.files("../data_general/Oz_misc_data/", pattern = "NIRV_yerr",full.names = T)[1] %>% 
  read_stars() %>% 
  set_names("nirv_anom") %>% 
  st_set_dimensions(.,3,
                    values=seq(ymd("2014-01-01"),by = "3 months",length.out = 24), names = 'date') %>% 
  slice(., 1:23, along='date') %>% 
  as_tibble() %>% 
  filter(is.na(nirv_anom)==F) %>%
  group_by(date
  ) %>% 
  summarize(val = median(nirv_anom,na.rm=TRUE), 
            val_05 = quantile(nirv_anom, 0.10), 
            val_95 = quantile(nirv_anom, 0.90)) %>% 
  ungroup() %>% 
  mutate(region='Yerranderie')

# Katoomba North
katoomba_n <- list.files("../data_general/Oz_misc_data/", pattern = "NIRV_katoomba_north",full.names = T)[1] %>% 
  read_stars() %>% 
  set_names("nirv_anom") %>% 
  st_set_dimensions(.,3,
                    values=seq(ymd("2014-01-01"),by = "3 months",length.out = 24), names = 'date') %>% 
  slice(., 1:23, along='date') %>% 
  as_tibble() %>% 
  filter(is.na(nirv_anom)==F) %>%
  group_by(date
  ) %>% 
  summarize(val = median(nirv_anom,na.rm=TRUE), 
            val_05 = quantile(nirv_anom, 0.10), 
            val_95 = quantile(nirv_anom, 0.90)) %>% 
  ungroup() %>% 
  mutate(region='Katoomba North')


# Plot the timeseries -----------------------------------------------------
bind_rows(katoomba_n, 
          torrington, 
          victoria_se, 
          wollemi, 
          # yerr
          ) %>% 
  ggplot(data=., aes(date, val, color=region, fill=region))+
  geom_hline(aes(yintercept=0),color='gray40')+
  geom_ribbon(aes(date, ymax=val_95,ymin=val_05),alpha=0.3,lty=0)+
  geom_line()+
  scale_x_date(expand=c(0,0))+
  labs(x=NULL,y=expression(paste(NIR[V]~anomaly)))+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_linedraw()+
  theme(panel.grid = element_blank());
  B <- 0.5
ggsave(p_ts, filename = "figures/timeSeries_L8_NIRV_anom_multiRegion.png", 
       width = 25*B, height=6*B, units='cm', dpi='retina')







# Carrai
vec_cols <- RColorBrewer::brewer.pal(n=11,"RdBu")
o <- list.files("../data_general/Oz_misc_data/", pattern = "NIRV_carrai",full.names = T)[1] %>% 
  read_stars()
names(o) <- 'nirv_anom'
o <- st_set_dimensions(o,3,
                       values=seq(ymd("2014-01-01"),by = "3 months",length.out = 24), names = 'date')
o <- o[,,,1:23]
carrai <- o %>% 
  as_tibble() %>% 
  filter(is.na(nirv_anom)==F) %>%
  group_by(date
  ) %>% 
  summarize(val = median(nirv_anom,na.rm=TRUE), 
            val_05 = quantile(nirv_anom, 0.10), 
            val_95 = quantile(nirv_anom, 0.90)) %>% 
  ungroup() %>% 
  mutate(region='Carrai')

# Nowra
vec_cols <- RColorBrewer::brewer.pal(n=11,"RdBu")
o <- list.files("../data_general/Oz_misc_data/", pattern = "NIRV_nowra",full.names = T)[1] %>% 
  read_stars()
names(o) <- 'nirv_anom'
o <- st_set_dimensions(o,3,
                       values=seq(ymd("2014-01-01"),by = "3 months",length.out = 24), names = 'date')
o <- o[,,,1:23]
nowra <- o %>%
  as_tibble() %>% 
  filter(is.na(nirv_anom)==F) %>%
  group_by(date
  ) %>% 
  summarize(val = median(nirv_anom,na.rm=TRUE), 
            val_05 = quantile(nirv_anom, 0.10), 
            val_95 = quantile(nirv_anom, 0.90)) %>% 
  ungroup() %>% 
  mutate(region='Nowra')

bind_rows(carrai, nowra, wollemi) %>% 
  ggplot(data=., aes(date, val, color=region,fill=region))+
  geom_hline(aes(yintercept=0),color='gray40')+
  geom_ribbon(aes(date, ymax=val_95,ymin=val_05),alpha=0.3,lty=0)+
  geom_line()+
  scale_x_date(expand=c(0,0))+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  labs(x=NULL,y=expression(paste(NIR[V]~anomaly)), 
       subtitle = "SE Australia")+
  theme_linedraw()+
  theme(panel.grid = element_blank())




# Wollemi south ----------------------------------------------------------------
vec_cols <- RColorBrewer::brewer.pal(n=11,"RdBu")
o <- list.files("../data_general/Oz_misc_data/", pattern = "wollemi",full.names = T)[1] %>% 
  read_stars()
names(o) <- 'nirv_anom'
o <- st_set_dimensions(o,3,
                       values=seq(ymd("2014-01-01"),by = "3 months",length.out = 24), names = 'date')
o <- o[,,,1:23]
p_ts <- ot %>% 
  filter(is.na(nirv_anom)==F) %>%
  group_by(date
  ) %>% 
  summarize(val = median(nirv_anom,na.rm=TRUE), 
            val_05 = quantile(nirv_anom, 0.10), 
            val_95 = quantile(nirv_anom, 0.90)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(date, val))+
  geom_hline(aes(yintercept=0),color='gray40')+
  geom_ribbon(aes(date, ymax=val_95,ymin=val_05),alpha=0.3,lty=0)+
  geom_line()+
  scale_x_date(expand=c(0,0))+
  labs(x=NULL,y=expression(paste(NIR[V]~anomaly)), 
       subtitle = "Wollemi region")+
  theme_linedraw()+
  theme(panel.grid = element_blank()); p_ts
B <- 0.5
ggsave(p_ts, filename = "figures/timeSeries_L8_NIRV_anom_wollemi_south.png", 
       width = 25*B, height=6*B, units='cm', dpi='retina')


os <- as(o[,,,],Class = 'Raster')  
os <- setZ(os,z = seq(ymd("2014-01-01"),by = "3 months",length.out = 23),name = 'date')

tibble(date=seq(ymd("2014-01-01"),by = "3 months",length.out = 23)) %>% 
  mutate(quarter = lubridate::quarter(date, fiscal_start = 11)) %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON")) %>% 
  mutate(season = factor(q,
                         levels=c("DJF","MAM","JJA","SON"), 
                         ordered=T)) %>% 
  mutate(label = paste0("",year(date)," ",season)) %>% 
  pull(label) -> vec_lab
names(os) <- vec_lab


myTheme <- rasterTheme(region = RColorBrewer::brewer.pal(11,"BrBG"))
myTheme$panel.background$col = 'gray30'


# breaks <- seq(-0.09,0.09,length.out = 21)
# cols <- colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG"))(length(breaks) - 1)
p <- levelplot(os, 
               par.settings=myTheme,
               margin=F, 
               at=seq(-0.1,0.1,length.out = 21),
               # at=breaks, 
               # col.regions=cols,
               names.attr=vec_lab)
png(filename = 'figures/map_landsat8_wollemiSouth_nirvAnom.png', 
    width = 25, 
    height = 25, 
    units='cm',res=350,type='cairo'); p; dev.off()

library(magick)
p1 <-   magick::image_read("figures/timeSeries_L8_NIRV_anom_wollemi_south.png")
p2 <- magick::image_read("figures/map_landsat8_wollemiSouth_nirvAnom.png")

magick::image_append(c(image_resize(p1,"3444x"),p2),stack=TRUE)




# Wollemi south ----------------------------------------------------------------
vec_cols <- RColorBrewer::brewer.pal(n=11,"RdBu")
o <- list.files("../data_general/Oz_misc_data/", pattern = "wollemi",full.names = T)[1] %>% 
  read_stars()
odem <- stars::read_stars("../data_general/Oz_misc_data/wollemi_south_dem-h.tif")
names(odem) <- "elev"
otpi <- stars::read_stars("../data_general/Oz_misc_data/wollemi_south_mtpi_srtm.tif")
names(otpi) <- "tpi"
oflow <- stars::read_stars("../data_general/Oz_misc_data/wollemi_south_flow_accum.tif")
names(oflow) <- "flow_accum"
oaspect <- stars::read_stars("../data_general/Oz_misc_data/wollemi_south_aspect.tif")
names(oaspect) <- "aspect"
olandforms <- stars::read_stars("../data_general/Oz_misc_data/wollemi_south_landforms.tif")
names(olandforms) <- "landforms"

names(o) <- 'nirv_anom'
o <- st_set_dimensions(o,3,
                       values=seq(ymd("2014-01-01"),by = "3 months",length.out = 24), names = 'date')
o <- o[,,,1:23]
ot <- o %>% as_tibble()
otdem <- odem %>% as_tibble()

junk <- ot %>% 
  inner_join(., otdem, by=c('x','y')) %>% 
  inner_join(., otpi %>% as_tibble(), by=c('x','y')) %>% 
  inner_join(., oflow %>% as_tibble(), by=c('x','y')) %>% 
  inner_join(., oaspect %>% as_tibble(), by=c('x','y')) %>% 
  inner_join(., olandforms %>% as_tibble(), by=c('x','y'))


dim(junk); library(mgcv)
junk <- junk %>% mutate(ddate=decimal_date(date), 
                        month=month(date))
fit <- bam(nirv_anom ~ #s(elev)+s(tpi)+s(flow_accum)+
                       # te(ddate,elev,tpi,flow_accum)+month, 
             # s(aspect,bs='cc'),
             s(aspect,tpi),
           data=junk %>% sample_n(10000) %>% 
             mutate(landforms=as.factor(landforms)), 
           select=TRUE, discrete = TRUE, method='fREML')
getViz(fit) %>% plot(fit)
summary(fit)
fn <- function(x){
  predict(fit, newdata=tibble(ddate=x,elev=800,flow_accum=1,tpi=0,month=7))
}
fn <- Vectorize(fn)
curve(fn(x), 2014,2020, n = 21)

ot %>% 
  filter(is.na(nirv_anom)==F) %>%
  group_by(date
           ) %>% 
  summarize(val = median(nirv_anom,na.rm=TRUE), 
            val_05 = quantile(nirv_anom, 0.25), 
            val_95 = quantile(nirv_anom, 0.75)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(date, val))+
  geom_hline(aes(yintercept=0),color='gray40')+
  geom_ribbon(aes(date, ymax=val_95,ymin=val_05),alpha=0.3,lty=0)+
  geom_line()+
  scale_color_viridis_d("aspect",end=0.95)+
  scale_fill_viridis_d("aspect", end=0.95)+
  scale_x_date(expand=c(0,0))+
  labs(x=NULL,y=expression(paste(NIR[V]~anomaly)))+
  theme_linedraw()+
  theme(panel.grid = element_blank())

ot %>% 
  # inner_join(., otdem, by=c('x','y')) %>% 
  inner_join(., otpi %>% as_tibble(), by=c('x','y')) %>%
  inner_join(., oaspect %>% as_tibble(), by=c('x','y')) %>% 
  # mutate(elev_class = case_when(between(elev,390,500)==T ~ "400-500 m",
  #                               between(elev,500,600)==T ~ "500-600 m",
  #                               between(elev,600,700)==T ~ "600-700 m",
  #                               between(elev,700,800)==T ~ "700-800 m",
  #                               between(elev,800,1000)==T ~ "800+ m")) %>% 
  mutate(tpi_class = case_when(between(tpi,-100,-50)==T ~ "< -50 m",
                                between(tpi,-50,-25)==T ~ "-50 - -25 m",
                                between(tpi,-25,0)==T ~ "-25 - 0 m",
                                between(tpi,0,25)==T ~ "0 - 25 m",
                               between(tpi,25,50)==T ~ "25 - 50 m",
                               between(tpi,50,100)==T ~ "50 - 100 m",
                                between(tpi,100,1000)==T ~ "100+ m")) %>%
  mutate(aspect = case_when((aspect >= 315 | aspect <= 45) ==T ~ "N",
                               between(aspect,45,135)==T ~ "E",
                               between(aspect,135,225)==T ~ "S",
                               between(aspect,225,315)==T ~ "W")) %>% 
  # mutate(flow_class = case_when(between(flow_accum,0,1)==T ~ "~ 1",
  #                               between(flow_accum,1.01,2)==T ~ "1-2 ",
  #                               between(flow_accum,2.01,10)==T ~ "2-10 ",
  #                               between(flow_accum,10,100)==T ~ "10-100 ",
  #                               between(flow_accum,100,1000)==T ~ "100+ ",)) %>% 
  filter(is.na(nirv_anom)==F) %>%
  group_by(date, aspect,tpi_class) %>% 
  summarize(val = median(nirv_anom,na.rm=TRUE), 
            val_05 = quantile(nirv_anom, 0.25), 
            val_95 = quantile(nirv_anom, 0.75)) %>% 
  ungroup() %>% 
  # filter(is.na(elev_class)==F) %>% 
  # filter(elev_class != "800+ m") %>% 
  ggplot(data=., aes(date, val,color=aspect,fill=aspect))+
  geom_hline(aes(yintercept=0),color='gray40')+
  geom_ribbon(aes(date, ymax=val_95,ymin=val_05),alpha=0.3,lty=0)+
  geom_line()+
  scale_color_viridis_d("aspect",end=0.95)+
  scale_fill_viridis_d("aspect", end=0.95)+
  scale_x_date(expand=c(0,0))+
  labs(x=NULL,y=expression(paste(NIR[V]~anomaly)))+
  facet_grid(aspect~tpi_class)+
  theme_linedraw()+
  theme(panel.grid = element_blank())

library(paletteer)
ggplot()+
  geom_stars(data=o[,,,23])+
  coord_equal()+
  # scale_fill_steps2(limits=c(-0.05,0.05), n.breaks=11, na.value = 'gray')+
  # scale_fill_paletteer_binned("grDevices::BrBG",n.breaks=11,limits=c(-0.05,0.05))+
  scale_fill_paletteer_c("grDevices::BrBG",
                         direction=1,
                         limits=c(-0.05,0.05),
                         oob=scales::squish)+
  # scale_fill_gradient2(expression(paste(NIR[V])),
  #   limits=c(-0.05,0.05), na.value = 'gray40',
  #   oob=scales::squish, high='blue',low='red')+
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))+
  facet_wrap(~date)+
  theme_void()


os <- as(o[,,,],Class = 'Raster')  
os <- setZ(os,z = seq(ymd("2014-01-01"),by = "3 months",length.out = 23),name = 'date')

tibble(date=seq(ymd("2014-01-01"),by = "3 months",length.out = 23)) %>% 
  mutate(quarter = lubridate::quarter(date, fiscal_start = 11)) %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON")) %>% 
  mutate(season = factor(q,
                         levels=c("DJF","MAM","JJA","SON"), 
                         ordered=T)) %>% 
  mutate(label = paste0("",year(date)," ",season)) %>% 
  pull(label) -> vec_lab
names(os) <- vec_lab


myTheme <- rasterTheme(region = RColorBrewer::brewer.pal(11,"BrBG"))
myTheme$panel.background$col = 'gray30'


# breaks <- seq(-0.09,0.09,length.out = 21)
# cols <- colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG"))(length(breaks) - 1)
p <- levelplot(os, 
               par.settings=myTheme,
               margin=F, 
               at=seq(-0.1,0.1,length.out = 21),
               # at=breaks, 
               # col.regions=cols,
               names.attr=vec_lab)
png(filename = 'figures/map_landsat8_wollemiSouth_nirvAnom.png', 
    width = 25, 
    height = 25, 
    units='cm',res=350,type='cairo'); p; dev.off()


# Katoomba ----------------------------------------------------------------

vec_cols <- RColorBrewer::brewer.pal(n=11,"RdBu")
o <- list.files("../data_general/Oz_misc_data//Landsat8_NIRV_katoomba_north_mean__2014_2019 (2).tif", pattern = "Landsat",full.names = T)[1] %>% 
  read_stars()
names(o) <- 'nirv_anom'
o <- st_set_dimensions(o,3,
 values=seq(ymd("2014-01-01"),by = "3 months",length.out = 24), names = 'date')

# ggplot()+
#   geom_stars(data=o[,,,1:4])+
#   coord_equal()+
#   scale_fill_gradient2(expression(paste(NIR[V])),
#     limits=c(-0.05,0.05), na.value = 'gray40', 
#     oob=scales::squish, high='blue',low='red')+
#   scale_x_discrete(expand=c(0,0))+
#   scale_y_discrete(expand=c(0,0))+
#   facet_wrap(~date)+
#   theme_void()


os <- as(o[,,,],Class = 'Raster')  
os <- setZ(os,z = seq(ymd("2014-01-01"),by = "3 months",length.out = 24),name = 'date')

tibble(date=seq(ymd("2014-01-01"),by = "3 months",length.out = 24)) %>% 
 mutate(quarter = lubridate::quarter(date, fiscal_start = 11)) %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON")) %>% 
  mutate(season = factor(q,
                         levels=c("DJF","MAM","JJA","SON"), 
                         ordered=T)) %>% 
  mutate(label = paste0("",year(date)," ",season)) %>% 
  pull(label) -> vec_lab
names(os) <- vec_lab


p <- levelplot(os, 
          par.settings=RdBuTheme(), margin=F, 
          at=seq(-0.05,0.05,length.out = 21), 
          names.attr=vec_lab)
png(filename = 'figures/map_landsat8_katoombaNorth_nirvAnom_v2.png', 
    width = 20, 
    height = 20, 
    units='cm',res=350,type='cairo'); p; dev.off()


# Nowra -------------------------------------------------------------------
os <- raster::stack("../data_general/Oz_misc_data//Landsat8_NIRV_nowra_mean__2014_2019.tif")  
os <- setZ(os,z = seq(ymd("2014-01-01"),by = "3 months",length.out = 24),name = 'date')

tibble(date=seq(ymd("2014-01-01"),by = "3 months",length.out = 24)) %>% 
  mutate(quarter = lubridate::quarter(date, fiscal_start = 11)) %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON")) %>% 
  mutate(season = factor(q,
                         levels=c("DJF","MAM","JJA","SON"), 
                         ordered=T)) %>% 
  mutate(label = paste0("",year(date)," ",season)) %>% 
  pull(label) -> vec_lab
names(os) <- vec_lab

myTheme <- RdBuTheme()
myTheme$panel.background$col = 'gray'

p <- levelplot(os, 
               par.settings=myTheme(), margin=F, 
               at=seq(-0.05,0.05,length.out = 21), 
               names.attr=vec_lab)
png(filename = 'figures/map_landsat8_Nowra_nirvAnom.png', 
    width = 20, 
    height = 20, 
    units='cm',res=350,type='cairo'); p; dev.off()




# Sydney 1 ------------------------------------------------------------------

# Sydney 2 ------------------------------------------------------------------

# Sydney 3 ------------------------------------------------------------------

# Sydney 4 ------------------------------------------------------------------

# Victoria SE  ------------------------------------------------------------------

# Wollongong ------------------------------------------------------------------

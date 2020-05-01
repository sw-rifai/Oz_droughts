library(raster); library(rasterVis); library(tidyverse); library(lubridate); 
library(stars); library(data.table);
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)


# Region 1 ----------------------------------------------------------------
mod <- stars::read_stars("../data_general/MOD13A2/MOD13A2_NIRV_1km_EastOz_NVIStreeClassMask_2003_2019-0000001792-0000000000.tif")
# levelplot(mod[[1]], margin=F)
mod <- stars::st_set_dimensions(mod,which = 3, 
                                values = seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"), 
                                names='date')
st_crs(mod) <- st_crs(4326)
names(mod) <- "nirv"
mod <- st_crop(mod, y = mod[,800:1792,400:1500,1] %>% st_bbox())

tmp <- mod %>% as.data.frame(.) %>% as.data.table(); rm(mod); gc()
names(tmp) <- c("x","y","date","nirv")
tmp <- tmp[y> -39 & y< -30 & x> 145]
tmp <- tmp[, `:=`(month = month(date))] # create month
tmp <- tmp[, `:=`(year = year(date))]   # create year
norms_nirv <- tmp[date>=ymd('2003-01-01')&date<=ymd("2018-12-31"), # filter to ref period
                  .("nirv_u" = mean(nirv,na.rm=TRUE), 
                    "nirv_sd" = sd(nirv,na.rm=TRUE)),
                  by=.(x,y,month)] # joining on x,y,month
tmp <- tmp[norms_nirv, on=.(x,y,month)]
tmp <- tmp[is.na(nirv_u)==F]
tmp <- tmp[,`:=`(nirv_anom = nirv-nirv_u)] %>% 
         .[,`:=`(nirv_anom_sd = nirv_anom/nirv_sd)]

myTheme <- RdBuTheme()
myTheme$panel.background$col = 'gray40' 
rasterFromXYZ(tmp[date>=ymd("2019-08-01")] %>% 
                .[,.(val=min(nirv_anom_sd,na.rm=TRUE)),by=.(x,y)] %>% 
                .[is.infinite(val)==F]) %>% 
  levelplot(., margin=F, at=seq(-3.5,3.5,length.out = 25), 
            par.settings=myTheme)

rasterFromXYZ(tmp[date==max(date)][,.(x,y,nirv_anom_sd)]) %>% 
  levelplot(., margin=F, at=seq(-5,5,length.out = 25), 
            par.settings=myTheme)

res_coarse <- 0.05
tmpc <- tmp %>% 
  mutate(x = res_coarse*round(x/res_coarse), 
         y = res_coarse*round(y/res_coarse))
tmpc <- tmpc[is.na(nirv_anom_sd)==F & nirv_anom_sd >= -5 & nirv_anom_sd <= 5]
tmpc <- tmpc[,.(val = min(nirv_anom_sd, na.rm=TRUE)), by=.(x,y,date)]

rasterFromXYZ(tmpc[date==max(date)][,.(x,y,val)]) %>% 
  levelplot(., margin=F, at=seq(-5,5,length.out = 25), 
            par.settings=myTheme)

# df_fulldims <- expand_grid(lon=sort(unique(tmp$lon)), 
#                            lat=sort(unique(tmp$lat)),
#                            date=sort(vec_seas_date$date))



source("src/R/helper_funs_Oz_droughts.R")
fn_tsr <- function(vi, d_threshold, r_threshold){
  # vi: vegetation index
  # d_threshold: level of vegetation index to signal a disturbance
  # r_threshold: level of vegetation index to recover from a disturbance
  # Assumptions: Continuous time record of vi (no gaps)
  tsr <- rep(0,length(vi)) # time since recovery array
  vec_d <- vi<d_threshold
  vec_r <- vi>r_threshold
  vec_d[is.na(vec_d)==T] <- FALSE
  vec_r[is.na(vec_r)==T] <- FALSE
  for(i in seq(2,length(vi))){
    tsr[i] <- tsr[i-1] + vec_d[i]
    tsr[i] <- ifelse(tsr[i] >=1 & vec_r[i]==F, tsr[i]+1, 0)
  }
  return(tsr)
}

empty <- expand_grid(x=unique(tmpc$x),y=unique(tmpc$y),date=unique(tmpc$date)) %>% 
  arrange(x,y,date) %>% as.data.table()
tmpc <- empty[tmpc,on=.(x,y,date)]
o <- tmpc %>% 
  as_tibble() %>% 
  mutate(year=year(date)) %>% 
  mutate(val = ifelse(val > 4.5 | val < -6, NA, val)) %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(val,na.rm=TRUE)) %>% 
  ungroup() %>% #pull(val) %>% summary
  # mutate(val = ifelse(year==2011,2.5,val)) %>%
  group_by(x,y) %>% 
  arrange(year) %>% 
  mutate(tsr = fn_tsr(val, d_threshold = -1.5, r_threshold = 1.5)) %>% 
  ungroup()




o %>% 
  # mutate(year=year(date)) %>% 
  # filter(year==2018) %>% pull(tsr) %>% summary
  ggplot(data=., aes(x,y,fill=tsr))+
  geom_sf(data=oz_poly, inherit.aes = F, fill='white')+
  geom_tile()+
  scale_fill_viridis_c(expression(paste("years since ",NIR[V]," recovery")),
                       option='B', end=0.95,
                       limits=c(0,5),
                       oob=scales::squish,
                       na.value = 'blue'
  )+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(151,153.5),
           ylim = c(-33.5,-30), expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 2)+
  guides(fill = guide_colorbar(title.position = "top"))+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(fill='#99A3C4'), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        strip.placement = 'inside', 
        strip.text = element_text(family='AvantGarde'), 
        legend.direction = 'horizontal', 
        legend.key.width = unit(1,units = 'cm'),
        legend.key.height = unit(0.5,units='cm')
        )
ggsave("figures/tsr_modis_-1.5_1.5_NSW.png", 
       width=12, height = 6, dpi = 'retina', type='cairo')

ggplot()+
  geom_sf(data=oz_poly #%>% filter(NAME_1 %in% c("New South Wales","Queensland"))
          )+
  # geom_tile(data=
  #             tmpc %>% group_by(x,y) %>% summarize(val=max(val,na.rm=T)) %>% ungroup(), 
  #           aes(x,y,fill=val))+
  geom_rect(aes(xmin=151,xmax=153.5,ymin=-33.5,ymax=-30),
            col='red',fill=NA)+
  coord_sf(xlim = c(151 - 12,153.5 + 1),
           ylim = c(-33.5 - 11,-30 + 5), expand = FALSE)+
  scale_fill_viridis_c(limits=c(1,3.5))+
  theme(panel.background = element_rect(fill='#99A3C4'), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(), 
        plot.margin = unit(c(rep(0,4)), "mm")
  )
B <- 1.7
ggsave("figures/tsr_modis_-1.5_1.5_NSW_INSET.png", 
       width=2.25*B, height = 3*B, units='cm', dpi = 'retina', type='cairo')

library(magick)
p1 <- magick::image_read("figures/tsr_modis_-1.5_1.5_NSW.png")
p2 <- magick::image_read("figures/tsr_modis_-1.5_1.5_NSW_INSET.png")


out <- image_composite(p1, image_scale(image_trim(p2), "x650"), offset = "+3405+870")
image_write(out, path="figures/tsr_mods_-1.5_1.5_NSW_with_inset.png")



# Region 2 ----------------------------------------------------------------
mod <- stars::read_stars("../data_general/MOD13A2/MOD13A2_NIRV_1km_EastOz_NVIStreeClassMask_2003_2019-0000001792-0000000000.tif")
# levelplot(mod[[1]], margin=F)
mod <- stars::st_set_dimensions(mod,which = 3, 
                                values = seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"), 
                                names='date')
st_crs(mod) <- st_crs(4326)
names(mod) <- "nirv"



# USEFUL FOR SUBSETTING --- 
# o <- st_apply(mod[,,,192:204], 1:2, mean, na.rm=TRUE)
# o[,1:1792,1000:1792] %>% st_bbox()
# rasterVis::levelplot(as(o[,800:1550,1000:1400], Class = 'Raster'), margin=F)
# plot(, 
#      col=viridis::inferno(50,end=0.9))
# 
# o <- mod[,,,100]
# as(o, Class = 'Raster') %>% rasterVis::levelplot(margin=F)

mod <- st_crop(mod, y = mod[,800:1550,1000:1400,] %>% st_bbox())
bbox <- st_bbox(mod)

tmp <- mod %>% as.data.frame(.) %>% as.data.table(); 
# rm(mod); gc()
names(tmp) <- c("x","y","date","nirv")
tmp <- tmp[, `:=`(month = month(date))] # create month
tmp <- tmp[, `:=`(year = year(date))]   # create year
norms_nirv <- tmp[date>=ymd('2003-01-01')&date<=ymd("2018-12-31"), # filter to ref period
                  .("nirv_u" = mean(nirv,na.rm=TRUE), 
                    "nirv_sd" = sd(nirv,na.rm=TRUE)),
                  by=.(x,y,month)] # joining on x,y,month
tmp <- tmp[norms_nirv, on=.(x,y,month)]
tmp <- tmp[is.na(nirv_u)==F]
tmp <- tmp[,`:=`(nirv_anom = nirv-nirv_u)] %>% 
  .[,`:=`(nirv_anom_sd = nirv_anom/nirv_sd)]

# myTheme <- RdBuTheme()
# myTheme$panel.background$col = 'gray40' 
# rasterFromXYZ(tmp[date>=ymd("2019-08-01")] %>% 
#                 .[,.(val=min(nirv_anom_sd,na.rm=TRUE)),by=.(x,y)] %>% 
#                 .[is.infinite(val)==F]) %>% 
#   levelplot(., margin=F, at=seq(-3.5,3.5,length.out = 25), 
#             par.settings=myTheme)
# 
# rasterFromXYZ(tmp[date==max(date)][,.(x,y,nirv_anom_sd)]) %>% 
#   levelplot(., margin=F, at=seq(-5,5,length.out = 25), 
#             par.settings=myTheme)

res_coarse <- 0.05
tmpc <- tmp %>% 
  mutate(x = res_coarse*round(x/res_coarse), 
         y = res_coarse*round(y/res_coarse))
tmpc <- tmpc[is.na(nirv_anom_sd)==F & nirv_anom_sd >= -5 & nirv_anom_sd <= 5]
tmpc <- tmpc[,.(val = min(nirv_anom_sd, na.rm=TRUE)), by=.(x,y,date)]

# rasterFromXYZ(tmpc[date==max(date)][,.(x,y,val)]) %>% 
#   levelplot(., margin=F, at=seq(-5,5,length.out = 25), 
#             par.settings=myTheme)

# df_fulldims <- expand_grid(lon=sort(unique(tmp$lon)), 
#                            lat=sort(unique(tmp$lat)),
#                            date=sort(vec_seas_date$date))



source("src/R/helper_funs_Oz_droughts.R")
fn_tsr <- function(vi, d_threshold, r_threshold){
  # vi: vegetation index
  # d_threshold: level of vegetation index to signal a disturbance
  # r_threshold: level of vegetation index to recover from a disturbance
  # Assumptions: Continuous time record of vi (no gaps)
  tsr <- rep(0,length(vi)) # time since recovery array
  vec_d <- vi<d_threshold
  vec_r <- vi>r_threshold
  vec_d[is.na(vec_d)==T] <- FALSE
  vec_r[is.na(vec_r)==T] <- FALSE
  for(i in seq(2,length(vi))){
    tsr[i] <- tsr[i-1] + vec_d[i]
    tsr[i] <- ifelse(tsr[i] >=1 & vec_r[i]==F, tsr[i]+1, 0)
  }
  return(tsr)
}

empty <- expand_grid(x=unique(tmpc$x),y=unique(tmpc$y),date=unique(tmpc$date)) %>% 
  arrange(x,y,date) %>% as.data.table()
tmpc <- empty[tmpc,on=.(x,y,date)]
o <- tmpc %>% 
  as_tibble() %>% 
  mutate(year=year(date)) %>% 
  mutate(val = ifelse(val > 4.5 | val < -6, NA, val)) %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(val,na.rm=TRUE)) %>% 
  ungroup() %>% #pull(val) %>% summary
  # mutate(val = ifelse(year==2011,2.5,val)) %>%
  group_by(x,y) %>% 
  arrange(year) %>% 
  mutate(tsr = fn_tsr(val, d_threshold = -1.75, r_threshold = 1.75)) %>% 
  ungroup()


o %>% 
  ggplot(data=., aes(x,y,fill=tsr))+
  geom_sf(data=oz_poly %>% st_crop(., bbox), 
          inherit.aes = F, fill='white')+
  geom_tile()+
  scale_fill_viridis_c(expression(paste("years since ",NIR[V]," recovery")),
                       option='B', end=0.95,
                       limits=c(0,15),
                       oob=scales::squish,
                       na.value = 'blue'
  )+
  labs(x=NULL,y=NULL)+
  coord_sf(#xlim = c(151,153.5),
           #ylim = c(-33.5,-30), 
           expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 3)+
  guides(fill = guide_colorbar(title.position = "top"))+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(fill='#99A3C4'), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        strip.placement = 'inside', 
        strip.text = element_text(family='AvantGarde'), 
        legend.direction = 'horizontal', 
        legend.key.width = unit(1,units = 'cm'),
        legend.key.height = unit(0.5,units='cm'))
ggsave("figures/tsr_modis_-1.5_1.5_VIC.png", 
       width=12, height = 6, dpi = 'retina', type='cairo')

ggplot()+
  geom_sf(data=oz_poly %>% st_crop(., c(-6,-8,5,3)+bbox), 
          inherit.aes = F, fill='white')+
  geom_rect(aes(xmin=bbox[1],xmax=bbox[3],ymin=bbox[2],ymax=bbox[4]),
            col='red',fill=NA)+
  coord_sf(#xlim = c(151,153.5),
    #ylim = c(-33.5,-30), 
    expand = FALSE)+
  scale_fill_viridis_c(limits=c(1,3.5))+
  theme(panel.background = element_rect(fill='#99A3C4'), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(), 
        plot.margin = unit(c(rep(0,4)), "mm")
  )
B <- 1.7
ggsave("figures/tsr_modis_-1.5_1.5_VIC_INSET.png", 
       width=2.25*B, height = 3*B, units='cm', dpi = 'retina', type='cairo')

library(magick)
p1 <- magick::image_read("figures/tsr_modis_-1.5_1.5_VIC.png")
p2 <- magick::image_read("figures/tsr_modis_-1.5_1.5_VIC_INSET.png")


out <- image_composite(p1, image_scale(image_trim(p2), "x600"), offset = "+3205+1100")
out
image_write(out, path="figures/tsr_mods_-1.5_1.5_VIC_with_inset.png")

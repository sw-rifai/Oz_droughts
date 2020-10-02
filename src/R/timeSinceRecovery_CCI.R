library(raster); library(rasterVis); 
library(tidyverse); library(lubridate); 
library(stars); library(data.table); setDTthreads(threads = 16)
library(dtplyr); library(patchwork); library(RcppRoll)
# Load data ---------------------------------------------------------------
# tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet") %>% 
#   as.data.table()
# lt <- lazy_dt(tmp)
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)

o <- stars::read_stars("../data_general/Oz_misc_data/MOD_CCI_1000m_SE_coastal_2001-06-01_to_2020-09-30.tif",
                       proxy = F)
o %>% st_get_dimension_values(3)
o <- o %>% stars::st_set_dimensions(., 3, 
                                    values=seq(ymd("2001-06-01"),ymd("2020-09-01"),by="1 month"),
                                    names = 'date')
# ggplot()+
#   geom_stars(data=o[,,,1])+
#   coord_equal()+
#   scale_fill_viridis_c(option='B',direction = -1)

o <- o %>% as_tibble() %>% as.data.table()
o <- o %>% set_names(c("x","y","date","cci"))
o <- o[is.na(cci)==F]
o <- o %>% mutate(month=month(date), 
                  year=year(date))
lt <- lazy_dt(o)
o_norms <- lt %>% 
  mutate(year=year(date), 
         month=month(date)) %>% 
  group_by(x,y,month) %>% 
  summarize(cci_u = mean(cci,na.rm=TRUE), 
            cci_sd = sd(cci,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as.data.table()
o <- merge.data.table(x=o, 
                      y=o_norms, 
                      by=c("x","y","month"), 
                      allow.cartesian = TRUE)
lt <- lazy_dt(o)
o <- lt %>% 
  mutate(cci_anom = cci-cci_u) %>% 
  mutate(cci_anom_sd = cci_anom/cci_sd) %>% 
  as.data.table()

o <- o[order(x,y,date)][, cci_anom_sd_3mo := frollmean(cci_anom_sd,n = 3,fill = NA,align='right',na.rm=TRUE), by=.(x,y)]
o <- o[order(x,y,date)][, cci_anom_sd_12mo := frollmean(cci_anom_sd,n = 12,fill = NA,align='right',na.rm=TRUE), by=.(x,y)]

# Functions ---------------------------------------------------------------
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


# Diagram Figure ----------------------------------------------------------
lt <- lazy_dt(o)
vec_xy <- lt %>% 
  # filter(cci > 0) %>% 
  filter(cci_anom_sd_12mo <= -2.5 & date <= ymd("2019-09-01")) %>% 
  as_tibble() %>% 
  select(x,y) %>% 
  distinct() %>% 
  first()
o %>% filter(x==vec_xy$x, y==vec_xy$y) %>% as_tibble() %>%
  ggplot(data=., aes(date, cci_anom_sd_3mo))+
  geom_line()
od <- o %>% filter(x==vec_xy$x, y==vec_xy$y) %>% as_tibble()
od <- od %>% 
  group_by(x,y) %>% 
  arrange(date) %>% 
  mutate(tsr = fn_tsr(cci_anom_sd_3mo, d_threshold = -1.5, r_threshold = 1.5)) %>% 
  ungroup() 
p_top <- od %>%
  # filter(date >= ymd('1983-01-01') & date <= ymd('2019-12-01')) %>% 
  ggplot(data=., aes(date, cci_anom_sd_3mo))+
  geom_hline(aes(yintercept=0))+
  geom_hline(aes(yintercept=-1.5),col='black',lty=3)+
  geom_hline(aes(yintercept=1.5),col='black',lty=3)+
  # geom_segment(aes(y=3,
  #                  yend=3,
  #                  x=ymd("1989-02-01"),
  #                  xend=ymd("1993-06-01")),col='#AA0000',lty=1,lwd=2)+
  geom_segment(aes(y=3,
                   yend=3,
                   x=ymd("2013-04-01"),
                   xend=ymd("2019-12-01")),col='#AA0000',lty=1,lwd=2)+
  geom_line(col='blue4')+
  scale_x_date(expand=c(0,0))+
  labs(x=NULL,y=expression(paste(cci~Anomaly~(sigma))))+
  theme_linedraw()+
  theme(panel.grid = element_blank()); p_top
p_bot <- od %>% 
  ggplot(data=., aes(date, tsr))+
  geom_line()+
  scale_x_date(expand=c(0,0))+
  labs(x=NULL,y=expression(paste("Time Since Recovery "*(Months))))+
  theme_linedraw()+
  theme(panel.grid = element_blank()); p_bot
p_join <- p_top + p_bot + plot_layout(ncol=1); p_join
ggsave(p_join, filename = "figures/diagram_timeSinceRecovery.png", 
       dpi='retina',type='cairo',width=18, height=12, units='cm')

lt %>% 
  group_by(date) %>% 
  summarize(n_neg = sum(cci < 0, na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  ggplot(data=., aes(date, n_neg))+
  geom_point()


# Time Since Recovery using 1-km cci hybrid ------------------------------
oc <- o %>% 
  filter(y< -35 & 
           y> -38 & 
           x> 145) %>% 
  as_tibble() 

inner_join(oc,  oc %>%
             select(x,y,date) %>% 
             expand_grid() %>% 
             arrange(x,y,date), 
           by=c('x','y','date')) %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(cci,na.rm=TRUE)) %>% 
  ungroup() %>% #pull(val) %>% is.na %>% table
  inner_join(., {.} %>% 
               filter(year >= 2001 & year <= 2011) %>% 
               group_by(x,y) %>% 
               summarize(val_sd = sd(val,na.rm=TRUE), 
                         val_u = mean(val,na.rm=TRUE)) %>% 
               ungroup(),by=c('x','y')) %>% 
  mutate(val_anom = val - val_u) %>% 
  mutate(val_anom_sd = val_anom/val_sd) %>% #pull(val_anom_sd) %>% hist
  group_by(x,y) %>% 
  arrange(year) %>% 
  mutate(tsr = fn_tsr(val_anom_sd, d_threshold = -1, r_threshold = 1)) %>% 
  ungroup() %>%  
  filter(year>= 2001) %>% # pull(tsr) %>% hist 
  ggplot(data=., aes(x,y,fill=tsr))+
  geom_sf(data=oz_poly, inherit.aes = F, fill='white')+
  geom_tile()+
  scale_fill_viridis_c(expression(paste("years since ",cci," recovery")),
                       option='B', end=0.9,
                       limits=c(0,10),
                       oob=scales::squish,
                       na.value = 'blue')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(145,153),
           ylim = c(-38,-35),
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
        legend.key.height = unit(0.5,units='cm')
  )















# Region 1 w/1km res ----------------------------------------------------------------
mod <- stars::read_stars("../data_general/MOD13A2/MOD13A2_NIRV_1km_EastOz_NVIStreeClassMask_2003_2019-0000001792-0000000000.tif")
# levelplot(mod[[1]], margin=F)
mod <- stars::st_set_dimensions(mod,which = 3, 
                                values = seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"), 
                                names='date')
st_crs(mod) <- st_crs(4326)
names(mod) <- "nirv"
mod <- st_crop(mod, y = mod[,800:1792,400:1500,1] %>% st_bbox())

o <- mod %>% as.data.frame(.) %>% as.data.table(); rm(mod); gc()
names(o) <- c("x","y","date","nirv")
o <- o[y> -39 & y< -30 & x> 145]
o <- o[, `:=`(month = month(date))] # create month
o <- o[, `:=`(year = year(date))]   # create year
norms_nirv <- o[date>=ymd('2003-01-01')&date<=ymd("2018-12-31"), # filter to ref period
                .("nirv_u" = mean(nirv,na.rm=TRUE), 
                  "nirv_sd" = sd(nirv,na.rm=TRUE)),
                by=.(x,y,month)] # joining on x,y,month
o <- o[norms_nirv, on=.(x,y,month)]
o <- o[is.na(nirv_u)==F]
o <- o[,`:=`(nirv_anom = nirv-nirv_u)] %>% 
  .[,`:=`(nirv_anom_sd = nirv_anom/nirv_sd)]

myTheme <- RdBuTheme()
myTheme$panel.background$col = 'gray40' 
rasterFromXYZ(o[date>=ymd("2019-08-01")] %>% 
                .[,.(val=min(nirv_anom_sd,na.rm=TRUE)),by=.(x,y)] %>% 
                .[is.infinite(val)==F]) %>% 
  levelplot(., margin=F, at=seq(-3.5,3.5,length.out = 25), 
            par.settings=myTheme)

rasterFromXYZ(o[date==max(date)][,.(x,y,nirv_anom_sd)]) %>% 
  levelplot(., margin=F, at=seq(-5,5,length.out = 25), 
            par.settings=myTheme)

res_coarse <- 0.05
oc <- o %>% 
  mutate(x = res_coarse*round(x/res_coarse), 
         y = res_coarse*round(y/res_coarse))
oc <- oc[is.na(nirv_anom_sd)==F & nirv_anom_sd >= -5 & nirv_anom_sd <= 5]
oc <- oc[,.(val = min(nirv_anom_sd, na.rm=TRUE)), by=.(x,y,date)]

rasterFromXYZ(oc[date==max(date)][,.(x,y,val)]) %>% 
  levelplot(., margin=F, at=seq(-5,5,length.out = 25), 
            par.settings=myTheme)

# df_fulldims <- expand_grid(lon=sort(unique(o$lon)), 
#                            lat=sort(unique(o$lat)),
#                            date=sort(vec_seas_date$date))




empty <- expand_grid(x=unique(oc$x),y=unique(oc$y),date=unique(oc$date)) %>% 
  arrange(x,y,date) %>% as.data.table()
oc <- empty[oc,on=.(x,y,date)]
o <- oc %>% 
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
  #             oc %>% group_by(x,y) %>% summarize(val=max(val,na.rm=T)) %>% ungroup(), 
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



# SE Victoria Region  ----------------------------------------------------------------
mod <- stars::read_stars("../data_general/MCD43/MCD43A4_cci_500m_SE_Vic_mmean_maskFireDefor_2001_2019.tif")
# levelplot(mod[[1]], margin=F)
mod <- stars::st_set_dimensions(mod,which = 3, 
                                values = seq(ymd("2001-01-01"),ymd("2019-12-01"),by="1 month"), 
                                names='date')
st_crs(mod) <- st_crs(4326)
names(mod) <- "cci"



# USEFUL FOR SUBSETTING --------------------------------------------- 
ggplot()+geom_stars(data=mod[,,,1])+scale_fill_viridis_c()+coord_equal()
# o <- st_apply(mod[,,,192:204], 1:2, mean, na.rm=TRUE)
# o[,1:1792,1000:1792] %>% st_bbox()
# rasterVis::levelplot(as(o[,800:1550,1000:1400], Class = 'Raster'), margin=F)
# plot(, 
#      col=viridis::inferno(50,end=0.9))
# 
# o <- mod[,,,100]
# as(o, Class = 'Raster') %>% rasterVis::levelplot(margin=F)

# mod <- st_crop(mod, y = mod[,800:1550,1000:1400,] %>% st_bbox())
bbox <- st_bbox(mod)

oo <- mod %>% as.data.frame(.) %>% as.data.table(); 
# rm(mod); gc()
names(oo) <- c("x","y","date","cci")
oo <- oo[, `:=`(month = month(date))] # create month
oo <- oo[, `:=`(year = year(date))]   # create year
norms_cci <- oo[date>=ymd('2001-01-01')&date<=ymd("2015-12-31"), # filter to ref period
                .("cci" = mean(cci,na.rm=TRUE)),
                by=.(x,y,year)] %>%  # joining on x,y,month
  .[,.(cci_u = mean(cci,na.rm=TRUE), 
       cci_sd = sd(cci,na.rm=TRUE)), by=.(x,y)]
oo <- norms_cci[oo, on=.(x,y)]
# oo <- oo[is.na(cci_u)==F]
oo <- oo[,`:=`(cci_anom = cci-cci_u)] %>% 
  .[,`:=`(cci_anom_sd = cci_anom/cci_sd)]

# myTheme <- RdBuTheme()
# myTheme$panel.background$col = 'gray40' 
# rasterFromXYZ(o[date>=ymd("2019-08-01")] %>% 
#                 .[,.(val=min(nirv_anom_sd,na.rm=TRUE)),by=.(x,y)] %>% 
#                 .[is.infinite(val)==F]) %>% 
#   levelplot(., margin=F, at=seq(-3.5,3.5,length.out = 25), 
#             par.settings=myTheme)
# 
# rasterFromXYZ(o[date==max(date)][,.(x,y,nirv_anom_sd)]) %>% 
#   levelplot(., margin=F, at=seq(-5,5,length.out = 25), 
#             par.settings=myTheme)

# res_coarse <- 0.05
# oc <- o %>% 
#   mutate(x = res_coarse*round(x/res_coarse), 
#          y = res_coarse*round(y/res_coarse))
# oc <- oc[is.na(nirv_anom_sd)==F & nirv_anom_sd >= -5 & nirv_anom_sd <= 5]
# oc <- oc[,.(val = min(nirv_anom_sd, na.rm=TRUE)), by=.(x,y,date)]

# rasterFromXYZ(oc[date==max(date)][,.(x,y,val)]) %>% 
#   levelplot(., margin=F, at=seq(-5,5,length.out = 25), 
#             par.settings=myTheme)

# df_fulldims <- expand_grid(lon=sort(unique(o$lon)), 
#                            lat=sort(unique(o$lat)),
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


# Specify thresholds ------------------------------------------------------
d_threshold <- -2.25
r_threshold <- 1.25
roi_name <- "SE_VIC"
#
empty <- expand_grid(x=unique(oo$x),y=unique(oo$y),date=unique(oo$date)) %>% 
  arrange(x,y,date) %>% as.data.table()
oc <- empty[oo,on=.(x,y,date)]
oc <- oc[is.na(cci_sd)==F]

oc <- oc %>% 
  as_tibble() %>% 
  filter(is.na(cci_sd)==F) %>% 
  filter(date <= ymd('2019-10-01')) %>% 
  # mutate(year=year(date)) %>% 
  mutate(cci_anom_sd = ifelse(cci_anom_sd > 6 | 
                                cci_anom_sd < -6, NA, cci_anom_sd)) %>%
  group_by(x,y,year) %>% 
  summarize(val = mean(cci_anom_sd,na.rm=TRUE)) %>%
  ungroup() %>% #pull(val) %>% summary
  # mutate(val = ifelse(year==2011,2.5,val)) %>%
  group_by(x,y) %>% 
  arrange(year) %>% 
  mutate(tsr = fn_tsr(val, d_threshold = d_threshold, r_threshold = r_threshold)) %>% 
  ungroup()

oc <- oc %>% 
  group_by(x,y) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup()


oc %>% 
  filter(id %in% sample.int(length(unique(oc$id)),1000)) %>% 
  ggplot(data=., aes(year, val, group=id))+
  geom_line(lwd=0.1)+
  geom_hline(aes(yintercept=0),col='blue')+
  geom_smooth(se=F, inherit.aes = F, aes(year,val),col='#BB0000')+
  scale_color_brewer(type='qual',palette=2)+
  scale_x_continuous(expand=c(0,0))+
  theme_linedraw()+
  theme(panel.grid=element_blank())

ggplot() +
  geom_smooth(se=F, inherit.aes = F, aes(year,val),col='#BB0000', 
              data=oc %>% 
                filter(id %in% sample.int(length(unique(oc$id)),100)))+
  geom_smooth(se=F, inherit.aes = F, aes(year,val),col='#BBB000', 
              data=oc %>% 
                filter(id %in% sample.int(length(unique(oc$id)),1000)))+
  geom_smooth(se=F, inherit.aes = F, aes(year,val),col='#BB00BB', 
              data=oc %>% 
                filter(id %in% sample.int(length(unique(oc$id)),10000)))+
  geom_smooth(se=F, inherit.aes = F, aes(year,val),col='#B0BFBB', 
              data=oc #%>% 
              # filter(id %in% sample.int(length(unique(oc$id)),100000))
  )+
  scale_x_continuous(expand=c(0,0))+
  theme_linedraw()+
  theme(panel.grid=element_blank())



library(rlang)
p_vic <- oc %>% 
  ggplot(data=., aes(x,y,fill=tsr))+
  geom_sf(data=oz_poly %>% st_crop(., bbox), 
          inherit.aes = F, fill='white')+
  geom_tile()+
  # scale_fill_viridis_c(expression(paste("years since ",cci," recovery")),
  #                      option='B', end=0.95,
  #                      limits=c(0,15),
  #                      oob=scales::squish,
  #                      na.value = 'blue'
  # )+
  scale_fill_gradientn(expr(paste("years since ",cci," recovery ",
                                  (!!d_threshold~-~!!r_threshold~sigma))),
                       colors=c("navy",viridis::inferno(10)), na.value = 'gray',
                       limits=c(0,10), oob=scales::squish)+
  labs(x=NULL,y=NULL)+
  coord_sf(#xlim = c(151,153.5),
    #ylim = c(-33.5,-30), 
    expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 4)+
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
fp1 <- file.path("figures",
                 paste0("tsr43_cci_sd_",d_threshold,"_",r_threshold,"_",roi_name,".png"))
ggsave(plot = p_vic, 
       filename = fp, 
       width=30, height = 18, units = 'cm', dpi = 500, type='cairo')


ggplot()+
  geom_sf(data=oz_poly %>% st_crop(., c(-3,-3,3,3)+bbox), 
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
fp2 <- file.path("figures",
                 paste0("tsr43","_",roi_name,"_Inset.png"))
ggsave(fp2, 
       width=2.25*B, height = 3*B, units='cm', dpi = 'retina', type='cairo')


library(magick)
p1 <- magick::image_read(fp1)
p2 <- magick::image_read(fp2)

out <- image_composite(p1, image_scale(image_trim(p2), "x1100"), offset = "+4655+2290")
out
fp3 <- file.path("figures",
                 paste0("tsr43_cci_sd_",d_threshold,"_",r_threshold,"_",roi_name,"_wInset.png"))
image_write(out, path=fp3)







# Variability exploration -------------------------------------------------

mod <- stars::read_stars("../data_general/MCD43/MCD43A4_cci_500m_SE_Vic_mmean_maskFireDefor_2001_2019.tif")
# levelplot(mod[[1]], margin=F)
mod <- stars::st_set_dimensions(mod,which = 3, 
                                values = seq(ymd("2001-01-01"),ymd("2019-12-01"),by="1 month"), 
                                names='date')
st_crs(mod) <- st_crs(4326)
names(mod) <- "cci"

junk <- st_apply(mod, 1:2, mean, na.rm=TRUE)
plot(junk,col=viridis::plasma(20), breaks='equal')
plot(st_apply(mod,1:2,sd,na.rm=TRUE),col=viridis::plasma(20), breaks='equal')

x <- rnorm(100)
percent_rank(x)
min_rank(x)
plot(cume_dist(x)~x)

RcppArmadillo::fastLm()
fn <- function(x){
  x <- x-mean(x,na.rm=TRUE)
  len <- (1:length(x)) - length(x)/2
  out <- unname(coef(RcppArmadillo::fastLm(x~len))[2])
  # tryCatch(coef(lm(x~len,na.rm=TRUE)[2]), error=function(err) 0)
  return(out)
}
plot(st_apply(mod,1:2,fn),col=viridis::plasma(20), breaks='equal')
unname(coef(RcppArmadillo::fastLm(x~x1))[2])
x <- rep(NA,100); x1 <- 1:100
tryCatch(lm(x~x1), )
tryCatch(coef(RcppArmadillo::fastLm(x~x1)), error=function(err) NA)
fn(rnorm(1000))

nvis <- stars::read_stars("../data_general/NVIS/nvis51_majorVegClass_0p05.tif")
oo <- mod %>% as.data.frame(.) %>% as.data.table(); 
# rm(mod); gc()
names(oo) <- c("x","y","date","cci")
oo <- oo[, `:=`(month = month(date))] # create month
oo <- oo[, `:=`(year = year(date))]   # create year
norms_cci <- oo[date>=ymd('2001-01-01')&date<=ymd("2015-12-31"), # filter to ref period
                .("cci_u" = mean(cci,na.rm=TRUE)),
                by=.(x,y,month)]
oo <- norms_cci[oo, on=.(x,y,month)]
oo <- oo[,`:=`(cci=ifelse(is.na(cci)==T, cci_u,cci))]
bb <- oo[, .(ma_cci=mean(cci,na.rm=FALSE)),by=.(x,y)][is.na(ma_cci)==F]
oo <- oo[bb,on=.(x,y)]
oo <- oo[,`:=`(cci_anom = cci-cci_u)] 

oo %>% 
  lazy_dt() %>% 
  filter(cci >= 0.1) %>% 
  group_by(month) %>% 
  summarize(rg = sd(cci,na.rm=TRUE)/mean(cci,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  ggplot(data=., aes(month,rg))+
  geom_point()



od <- oo %>% 
  # lazy_dt() %>% 
  group_by(x,y) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup() %>% 
  filter(id %in% sample.int(25000,1000)) %>% 
  as_tibble() %>% 
  mutate(val = ifelse(is.na(cci==T), cci_u, cci)) %>% 
  mutate(val_anom = val - cci_u) %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(val_d = signal::sgolayfilt(val_anom,n=5,m=1)) %>% 
  ungroup()


set.seed(3)
od %>% 
  filter(cci_anom != 0) %>% 
  filter(id %in% sample(unique(od$id), 100)) %>% 
  ggplot(data=., aes(date, val_d,group=as.factor(id)))+
  # geom_smooth(lwd=0.5,se=F,span=0.4)+
  ggpointdensity::geom_pointdensity(size=0.1)+
  geom_hline(aes(yintercept=0))+
  geom_vline(aes(xintercept=ymd('2017-01-01')))+
  scale_color_viridis_c()
# scale_color_brewer(palette = 'Set1')


vec <- sin(sort(rnorm(100, mean=1:100, sd=seq(1,5,length.out = 100))))
vec[sample.int(length(vec),5)] <- NA
signal::sgolayfilt(vec, n = 15) %>% 
  plot(type='l', ylim=c(-1,1)); lines(vec,col='red')





bbox <- st_bbox(mod)
dd <- oo %>% 
  lazy_dt() %>% 
  filter(cci >= 0.1) %>% 
  group_by(x,y,year) %>%
  summarize(cci = mean(cci,na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(x,y) %>% 
  mutate(rank = percent_rank(cci)) %>% 
  ungroup() %>% 
  # filter(date==max(date)) %>% 
  as_tibble() 
p <- dd %>%   
  ggplot(data=., aes(x,y,fill=rank*100))+
  geom_sf(data=oz_poly %>% st_crop(., bbox), 
          inherit.aes = F, fill='white')+
  geom_tile()+
  scale_fill_viridis_c(expression(paste("",cci," percentile")),
                       option='B', end=0.99, direction = -1,
                       limits=c(0,100),
                       oob=scales::squish,
                       na.value = 'blue'
  )+
  # scale_fill_gradientn(expr(paste("years since ",cci," recovery ",
  #                                 (!!d_threshold~-~!!r_threshold~sigma))),
  #                      colors=c("navy",viridis::inferno(10)), na.value = 'gray',
  #                      limits=c(0,10), oob=scales::squish)+
  labs(x=NULL,y=NULL)+
  coord_sf(#xlim = c(151,153.5),
    #ylim = c(-33.5,-30), 
    expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 4)+
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
fp_prank <- file.path("figures",
                      paste0("percentRank43_cci_","","se_vic",".png"))
ggsave(plot = p, 
       filename = fp_prank, 
       width=30, height = 18, units = 'cm', dpi = 500, type='cairo')

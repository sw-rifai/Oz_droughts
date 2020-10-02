library(raster); library(rasterVis); 
library(tidyverse); 
library(stars); library(data.table); setDTthreads(threads = 16)
library(lubridate);
library(dtplyr); library(patchwork); library(RcppRoll)
# Load data ---------------------------------------------------------------
# tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet") %>% 
#   as.data.table()
# lt <- lazy_dt(tmp)
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)


# Import CCI --------------------------------------------------------------
o <- stars::read_stars("../data_general/Oz_misc_data/MOD_CCI_1000m_SE_coastal2001-01-01_to_2020-09-30.tif",
                       proxy = F)
o %>% st_get_dimension_values(3)
o <- o %>% stars::st_set_dimensions(., 3, 
                                    values=seq(ymd("2001-01-01"),ymd("2020-09-01"),by="1 month"),
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
o_cci <- o; rm(o)
#*** END SECTION ***************************************************************

# Import NDMI --------------------------------------------------------------
o <- stars::read_stars("../data_general/Oz_misc_data/MOD_NDMI_1000m_SE_coastal_2001-01-01_to_2020-09-30.tif",
                       proxy = F)
o %>% st_get_dimension_values(3)
o <- o %>% stars::st_set_dimensions(., 3, 
                                    values=seq(ymd("2001-01-01"),ymd("2020-09-01"),by="1 month"),
                                    names = 'date')

o <- o %>% as_tibble() %>% as.data.table()
o <- o %>% set_names(c("x","y","date","ndmi"))
o <- o[is.na(ndmi)==F]
o <- o %>% mutate(month=month(date), 
                  year=year(date))
lt <- lazy_dt(o)
o_norms <- lt %>% 
  mutate(year=year(date), 
         month=month(date)) %>% 
  group_by(x,y,month) %>% 
  summarize(ndmi_u = mean(ndmi,na.rm=TRUE), 
            ndmi_sd = sd(ndmi,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as.data.table()
o <- merge.data.table(x=o, 
                      y=o_norms, 
                      by=c("x","y","month"), 
                      allow.cartesian = TRUE)
lt <- lazy_dt(o)
o <- lt %>% 
  mutate(ndmi_anom = ndmi-ndmi_u) %>% 
  mutate(ndmi_anom_sd = ndmi_anom/ndmi_sd) %>% 
  as.data.table()
o <- o[order(x,y,date)][, ndmi_anom_sd_3mo := frollmean(ndmi_anom_sd,n = 3,fill = NA,align='right',na.rm=TRUE), by=.(x,y)]
o <- o[order(x,y,date)][, ndmi_anom_sd_12mo := frollmean(ndmi_anom_sd,n = 12,fill = NA,align='right',na.rm=TRUE), by=.(x,y)]
o_ndmi <- o; rm(o)
#*** END SECTION ***************************************************************

# Import NDVI -------------------------------------------------------------
o <- stars::read_stars("../data_general/Oz_misc_data/MOD_NDVI_1000m_SE_coastal_2001-01-01_to_2020-09-30.tif",
                       proxy = F)
o %>% st_get_dimension_values(3)
o <- o %>% stars::st_set_dimensions(., 3, 
                                    values=seq(ymd("2001-01-01"),ymd("2020-09-01"),by="1 month"),
                                    names = 'date')
o <- o %>% as_tibble() %>% as.data.table()
o <- o %>% set_names(c("x","y","date","ndvi"))
o <- o[is.na(ndvi)==F]
o <- o %>% mutate(month=month(date), 
                  year=year(date))
lt <- lazy_dt(o)
o_norms <- lt %>% 
  mutate(year=year(date), 
         month=month(date)) %>% 
  group_by(x,y,month) %>% 
  summarize(ndvi_u = mean(ndvi,na.rm=TRUE), 
            ndvi_sd = sd(ndvi,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as.data.table()
o <- merge.data.table(x=o, 
                      y=o_norms, 
                      by=c("x","y","month"), 
                      allow.cartesian = TRUE)
lt <- lazy_dt(o)
o <- lt %>% 
  mutate(ndvi_anom = ndvi-ndvi_u) %>% 
  mutate(ndvi_anom_sd = ndvi_anom/ndvi_sd) %>% 
  as.data.table()
o <- o[order(x,y,date)][, ndvi_anom_sd_3mo := frollmean(ndvi_anom_sd,n = 3,fill = NA,align='right',na.rm=TRUE), by=.(x,y)]
o <- o[order(x,y,date)][, ndvi_anom_sd_12mo := frollmean(ndvi_anom_sd,n = 12,fill = NA,align='right',na.rm=TRUE), by=.(x,y)]
o_ndvi <- o; rm(o)
#*** END SECTION ***************************************************************

# Import LST -------------------------------------------------------------
o <- stars::read_stars("../data_general/MOD11A1_C6_LST/MOD11A1_C6_LST_1km_SE_coastal_2001-01-01_2020-09-30.tif",
                       proxy = F)
o %>% st_get_dimension_values(3)
o <- o %>% stars::st_set_dimensions(., 3, 
                                    values=seq(ymd("2001-01-01"),ymd("2020-09-01"),by="1 month"),
                                    names = 'date')
o <- o %>% as_tibble() %>% as.data.table()
o <- o %>% set_names(c("x","y","date","lst"))
o <- o[is.na(lst)==F]
o <- o %>% mutate(month=month(date), 
                  year=year(date))
lt <- lazy_dt(o)
o_norms <- lt %>% 
  mutate(year=year(date), 
         month=month(date)) %>% 
  group_by(x,y,month) %>% 
  summarize(lst_u = mean(lst,na.rm=TRUE), 
            lst_sd = sd(lst,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as.data.table()
o <- merge.data.table(x=o, 
                      y=o_norms, 
                      by=c("x","y","month"), 
                      allow.cartesian = TRUE)
lt <- lazy_dt(o)
o <- lt %>% 
  mutate(lst_anom = lst-lst_u) %>% 
  mutate(lst_anom_sd = lst_anom/lst_sd) %>% 
  as.data.table()
o <- o[order(x,y,date)][, lst_anom_sd_3mo := frollmean(lst_anom_sd,n = 3,fill = NA,align='right',na.rm=TRUE), by=.(x,y)]
o <- o[order(x,y,date)][, lst_anom_sd_12mo := frollmean(lst_anom_sd,n = 12,fill = NA,align='right',na.rm=TRUE), by=.(x,y)]
o_lst <- o; rm(o)
rm(o_norms)
#*** END SECTION ***************************************************************

# Import fire --------------------------------------------------------------
o <- stars::read_stars("../data_general/MCD64/MCD64_1km_SE_coastal_2001-01-01_2020-06-01.tif",
                       proxy = F)
o %>% st_get_dimension_values(3)
o <- o %>% stars::st_set_dimensions(., 3, 
                                    values=seq(ymd("2001-01-01"),ymd("2020-06-01"),by="1 month"),
                                    names = 'date')
o <- o %>% as_tibble() %>% as.data.table()
o <- o %>% set_names(c("x","y","date","fire"))
o <- o[is.na(fire)==F]
o_fire <- o; rm(o)

dat <- merge(o_cci, o_ndvi, by=c("x","y","year","month","date"))
dat <- merge(dat, o_ndmi, by=c("x","y","year","month","date"))
dat <- merge(dat, o_lst, by=c("x","y","year","month","date"))
dat <- merge(dat, o_fire, all.x=TRUE, all.y=FALSE, by=c("x","y","date"))

# dat <- dat[order(x,y,date)][, lst_max12mo := frollmean(lst_anom_sd,n = 12,fill = NA,align='right',na.rm=TRUE), by=.(x,y)]
fire_xy <- dat %>% 
  mutate(fire_bin = ifelse(fire > 0, 1,0)) %>% 
  group_by(x,y) %>% 
  summarize(fire_tot=sum(fire_bin,na.rm=TRUE)) %>% 
  ungroup()
vec_xy <- dat %>% 
  filter(
         cci_anom_sd_12mo <= -1.5 & 
         ndvi_anom_sd_12mo <= -1.5 & 
        date <= ymd("2019-09-01")) %>% 
  as_tibble() %>% 
  select(x,y) %>% 
  distinct() %>% 
  mutate(id = row_number())
vec_xy <- inner_join(vec_xy, fire_xy, by=c('x','y')) %>% 
  filter(fire_tot == 0)

od <- dat %>% filter(x%in%vec_xy$x, 
                     y%in%vec_xy$y) %>% 
  as_tibble() %>% arrange(date) 
od <- inner_join(vec_xy, od, by=c("x","y"))
sample_vec_xy <- sample(vec_xy$id, 50)
p_out <- od %>% 
  filter(id %in% sample_vec_xy) %>% 
  ggplot(data=.,aes(ndvi_anom_sd_12mo, cci_anom_sd_12mo,color=lst_anom_sd_12mo))+
  geom_hline(aes(yintercept=0))+
  geom_vline(aes(xintercept=0))+
  geom_text(data=od %>% 
              filter(id%in%sample_vec_xy) %>% 
              filter(month==1) %>% 
              filter(year%in%c(2003,2020)), 
            aes(label=year), 
            size=8, 
            color='black')+
  geom_point(alpha=0.5)+
  geom_path()+
  geom_point()+
  labs(x=expression(paste(NDVI~Anom.~(sigma))), 
       y=expression(paste(CCI~Anom.~(sigma))))+
  scale_color_gradient2(expression(paste(LST~Anom.~(sigma))),
    high='#cf0000',mid='gray',low='navy')+
  coord_equal()+
  theme_linedraw()+
  facet_wrap(~id, ncol = 10)+
  theme(legend.position = 'bottom'); p_out
ggsave(p_out, filename = "figures/codecline_ndvi_cci_lst.png", 
       width=15*4, height=9*4, units='cm', dpi=200)

p_out <- od %>% 
  filter(id %in% sample(sample_vec_xy,8)) %>% 
  select(date,id,ndvi_anom_sd_12mo,
                 ndmi_anom_sd_12mo, 
                 cci_anom_sd_12mo, 
                 lst_anom_sd_12mo) %>% 
  gather(-date,-id,key='key',value='value') %>% 
  ggplot(data=.,aes(date,value,color=key))+
  geom_hline(aes(yintercept=0))+
  # geom_vline(aes(xintercept=0))+
  # geom_point(alpha=0.5)+
  geom_path()+
  # geom_point()+
  labs(x=NULL, 
       y=expression(paste(Anomaly["12 mo"]~(sigma))))+
  scale_color_manual(values=c('ndvi_anom_sd_12mo'=colorspace::darken('purple',amount = 0.5),
                              'ndmi_anom_sd_12mo'=colorspace::darken('blue',amount = 0.5),
                              'cci_anom_sd_12mo'=colorspace::darken('orange',amount = 0.2), 
                              'lst_anom_sd_12mo'=colorspace::darken('red',amount = 0.5)))+
  # scale_color_viridis_d()+
  # coord_equal()+
  theme_linedraw()+
  facet_wrap(~id, ncol = 2)+
  theme(legend.position = 'bottom'); p_out
ggsave(p_out, filename = "figures/co-decline_timeseries_ndvi_ndmi_cci_lst.png", 
       width=15*2, height=9*2, units='cm', dpi=200)


p_out <- od %>% 
  filter(id %in% sample(sample_vec_xy,8)) %>% 
  select(date,id,ndvi_anom_sd_3mo, 
         cci_anom_sd_3mo, 
         lst_anom_sd_3mo) %>% 
  gather(-date,-id,key='key',value='value') %>% 
  ggplot(data=.,aes(date,value,color=key))+
  geom_hline(aes(yintercept=0))+
  # geom_vline(aes(xintercept=0))+
  # geom_point(alpha=0.5)+
  geom_path()+
  # geom_point()+
  labs(x=NULL, 
       y=expression(paste(Anomaly["3 mo"]~(sigma))))+
  scale_color_manual(values=c('ndvi_anom_sd_3mo'=colorspace::darken('green',amount = 0.5), 
                              'cci_anom_sd_3mo'=colorspace::darken('orange',amount = 0.2), 
                              'lst_anom_sd_3mo'=colorspace::darken('blue',amount = 0.5)))+
  # scale_color_viridis_d()+
  # coord_equal()+
  theme_linedraw()+
  facet_wrap(~id, ncol = 2)+
  theme(legend.position = 'bottom'); p_out
ggsave(p_out, filename = "figures/co-decline_timeseries_anom3mo_ndvi_cci_lst.png", 
       width=15*2, height=9*2, units='cm', dpi=200)


p_l <- dat[date==ymd("2003-09-01")] %>% 
  ggplot(data=.,aes(x,y,fill=cci_anom_sd_12mo))+
  geom_sf(data=oz_poly, inherit.aes = F, 
          fill='gray70', 
          color='black')+
  geom_tile()+
  coord_sf(xlim = c(145,153),
           ylim = c(-39,-31.5),
           expand = FALSE)+
  scico::scale_fill_scico(palette = 'roma', 
                          limits=c(-1.5,1.5), 
                          oob=scales::squish)+
  labs(x=NULL,y=NULL,title='2003 September')+
  guides(fill=guide_colorbar(title=
                               expression(paste('CCI anom'[12*mo],(sigma)))))+
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill='lightblue'), 
        legend.position = c(1,0), 
        legend.justification = c(1,0)); p_l
p_r <- dat[date==ymd("2019-09-01")] %>% 
  ggplot(data=.,aes(x,y,fill=cci_anom_sd_12mo))+
  geom_sf(data=oz_poly, inherit.aes = F, 
          fill='gray70', 
          color='black')+
  geom_tile()+
  coord_sf(xlim = c(145,153),
           ylim = c(-39,-31.5),
           expand = FALSE)+
  scico::scale_fill_scico(palette = 'roma', 
                          limits=c(-1.5,1.5), 
                          oob=scales::squish)+
  labs(x=NULL,y=NULL,title='2019 September')+
  guides(fill=guide_colorbar(title=
              expression(paste('CCI anom'[12*mo],(sigma)))))+
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill='lightblue'), 
        legend.position = c(1,0), 
        legend.justification = c(1,0)); p_r
ggsave(p_l+p_r+plot_layout(ncol=2), 
       filename='figures/map_cci_anom_compare_2003_2009Sep.png', 
       width = 16*1.5, height = 9*1.5, units='cm')

p_l <- dat[date==ymd("2003-09-01")] %>% 
  ggplot(data=.,aes(x,y,fill=ndvi_anom_sd_12mo))+
  geom_sf(data=oz_poly, inherit.aes = F, 
          fill='gray70', 
          color='black')+
  geom_tile()+
  coord_sf(xlim = c(145,153),
           ylim = c(-39,-31.5),
           expand = FALSE)+
  scico::scale_fill_scico(palette = 'roma', 
                          limits=c(-1.5,1.5), 
                          oob=scales::squish)+
  labs(x=NULL,y=NULL,title='2003 September')+
  guides(fill=guide_colorbar(title=
                               expression(paste('NDVI anom'[12*mo],(sigma)))))+
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill='lightblue'), 
        legend.position = c(1,0), 
        legend.justification = c(1,0))
p_r <- dat[date==ymd("2019-09-01")] %>% 
  ggplot(data=.,aes(x,y,fill=ndvi_anom_sd_12mo))+
  geom_sf(data=oz_poly, inherit.aes = F, 
          fill='gray70', 
          color='black')+
  geom_tile()+
  coord_sf(xlim = c(145,153),
           ylim = c(-39,-31.5),
           expand = FALSE)+
  scico::scale_fill_scico(palette = 'roma', 
                          limits=c(-1.5,1.5), 
                          oob=scales::squish)+
  labs(x=NULL,y=NULL,title='2019 September')+
  guides(fill=guide_colorbar(title=
                               expression(paste('NDVI anom'[12*mo],(sigma)))))+
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill='lightblue'), 
        legend.position = c(1,0), 
        legend.justification = c(1,0))
ggsave(p_l+p_r+plot_layout(ncol=2), 
       filename='figures/map_ndvi_anom_compare_2003_2009Sep.png', 
       width = 16*1.5, height = 9*1.5, units='cm')

p_l <- dat[date==ymd("2003-09-01")] %>% 
  ggplot(data=.,aes(x,y,fill=lst_anom_sd_12mo))+
  geom_sf(data=oz_poly, inherit.aes = F, 
          fill='gray70', 
          color='black')+
  geom_tile()+
  coord_sf(xlim = c(145,153),
           ylim = c(-39,-31.5),
           expand = FALSE)+
  scico::scale_fill_scico(palette = 'vik', 
                          limits=c(-1.5,1.5), 
                          oob=scales::squish)+
  labs(x=NULL,y=NULL,title='2003 September')+
  guides(fill=guide_colorbar(title=
                               expression(paste('LST anom'[12*mo],(sigma)))))+
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill='lightblue'), 
        legend.position = c(1,0), 
        legend.justification = c(1,0))
p_r <- dat[date==ymd("2019-09-01")] %>% 
  ggplot(data=.,aes(x,y,fill=lst_anom_sd_12mo))+
  geom_sf(data=oz_poly, inherit.aes = F, 
          fill='gray70', 
          color='black')+
  geom_tile()+
  coord_sf(xlim = c(145,153),
           ylim = c(-39,-31.5),
           expand = FALSE)+
  scico::scale_fill_scico(palette = 'vik', 
                          limits=c(-1.5,1.5), 
                          oob=scales::squish)+
  labs(x=NULL,y=NULL,title='2019 September')+
  guides(fill=guide_colorbar(title=
                               expression(paste('LST anom'[12*mo],(sigma)))))+
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill='lightblue'), 
        legend.position = c(1,0), 
        legend.justification = c(1,0))
ggsave(p_l+p_r+plot_layout(ncol=2), 
       filename='figures/map_lst_anom_compare_2003_2009Sep.png', 
       width = 16*1.5, height = 9*1.5, units='cm')

scico::scico_palette_show()
colorspace::qualitative_hcl(6) %>% colorspace::show()

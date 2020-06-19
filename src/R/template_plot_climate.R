#*******************************************************************************
#* Description:
#* Plot Climate stuff
#*
#*
#*
library(tidyverse); library(sf)
library(data.table); setDTthreads(threads = 10)
library(lubridate); 
library(mgcv); #library(mgcViz); 
library(dtplyr); 
library(RcppArmadillo); library(patchwork)
# IMPORT DATA ###################################################################

# load("data/gridCell_lm_ndvi_clim.Rdata") # grid cell linear regressions
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)

# vegetation index record
vi <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet", 
                          col_select = c("x","y","date",
                                         "ndvi_c","ndvi_m","ndvi_hyb", 
                                         "evi2_hyb","evi2_m"))
dat <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id","season",
                             "precip",  "precip_anom", 
                             "precip_anom_3mo","precip_anom_36mo",
                             "precip_anom_12mo",
                             "map",
                             "precip_12mo","precip_36mo",
                             "tmax","tmax_anom","tmax_anom_sd", "matmax",
                             "tmin","tmin_anom",
                             "vpd15","vpd15_anom","vpd15_anom_sd","mavpd15",
                             "vpd15_u",
                             "pet","mapet","pet_anom","pet_anom_3mo","pet_u","pet_sd",
                             "pet_anom_sd", "pet_12mo","pet_36mo",
                             "pe","mape",
                             # "ndvi_u",
                             # "ndvi_anom",
                             # "ndvi_anom_12mo",
                             "ndvi_anom_sd",
                             # "ndvi_mcd",
                             'vc','veg_class',
                             'month',
                             "x", "y", "year")) %>% 
  as.data.table() %>% 
  .[is.infinite(mape)==F]
dat <- dat[,`:=`(pe = precip/pet, 
                 pe_12mo = precip_12mo/pet_12mo)]
dat <- merge(dat, 
             vi,
             by=c("x","y","date"), 
             all=TRUE,allow.cartesian=TRUE)
dat <- dat[order(x,y,date)][, ndvi_3mo := frollmean(ndvi_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]
rm(vi); gc(full=TRUE)
# END Load dat *****************************************************************



# NDVI linear change -----------------------------------------------------------
system.time(
  lt_ndvi_season <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
    .[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)


vec_col <- RColorBrewer::brewer.pal(n=7, name='BrBG')
lt_ndvi_season %>% 
  ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_gradient2(expression(paste(Delta*NDVI~yr^-1)),
                       high=vec_col[7], mid=vec_col[4], low=vec_col[1],
                       limits=c(-0.0025,0.0025),
                       breaks=c(-0.0025,-0.001,0,0.001,0.0025),
                       labels=c("<-0.00025",-0.001,0,0.001,">0.0025"),
                       oob=scales::squish,
                       na.value='gray')+
  # scale_fill_viridis_c(expression(paste(Delta*NDVI~yr^-1)),
  #                      option='D',direction = 1,
  #                      limits=c(-0.0001,0.0015),
  #                      oob=scales::squish, na.value = 'red')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  facet_wrap(~season, nrow = 1)+
  guides(fill = 
          guide_colourbar(label = T)) +
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom',
        legend.key.width = unit(1.5,'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(), 
        axis.ticks = element_blank())
ggsave(filename = "figures/ndvi_seasonal_longtermTrend_1982_2019.png", 
       dpi=350, width=15,height=12,units='cm',type='cairo')


# NDVI linear change by epoch
system.time(
  lt_ndvi_season_p1 <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
    .[date>= ymd("1982-01-01") & date<= ymd("1999-12-31")] %>% 
    .[,.(val = mean(ndvi_c, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)
system.time(
  lt_ndvi_season_p2 <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
    .[date>= ymd("2000-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(ndvi_m, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)
lt_ndvi_season_p1$epoch <- "AVHRR NDVI 1982-1999"
lt_ndvi_season_p2$epoch <- "MODIS NDVI 2000-2019"


vec_col <- RColorBrewer::brewer.pal(n=7, name='BrBG')
bind_rows(lt_ndvi_season_p1, lt_ndvi_season_p2) %>% 
  ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_gradient2(expression(paste(Delta*NDVI~yr^-1)),
                       high=vec_col[7], mid=vec_col[4], low=vec_col[1],
                       limits=c(-0.005,0.005),
                       breaks=c(-0.005,-0.003,0,0.003,0.005),
                       labels=c("<-0.005",-0.003,0,0.003,">0.005"),
                       oob=scales::squish,
                       na.value='gray')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  facet_grid(epoch~season)+
  guides(fill = 
           guide_colourbar(label = T)) +
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom',
        legend.key.width = unit(1.5,'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(), 
        axis.ticks = element_blank())
ggsave(filename = "figures/ndvi_seasonal_longtermTrends_prePost2000epochs.png", 
       dpi=200, width=15,height=22,units='cm',type='cairo')
# END **************************************************************************




# Long-term change P, PET, PE ---------------------------------------------
system.time(
  lt_P_season <- dat[,.(val = mean(precip, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)
system.time(
  lt_PET_season <- dat[,.(val = mean(pet, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)
p_P <- lt_P_season %>% 
  ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_gradient2(expression(atop("Monthly P",(Delta~mm~yr**-1))),
                       limits=c(-2.5,2.5),
                       oob=scales::squish, 
                       na.value = 'gray')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  facet_wrap(~season, nrow = 1)+
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(), 
        legend.position = 'right',
        # legend.text = element_text(size=5),
        legend.title = element_text(size=8),
        # legend.key.width = unit(0.25,'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(), 
        axis.ticks = element_blank()); p_P
p_PET <- lt_PET_season %>% 
  ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_gradient2(expression(atop("Monthly PET",(Delta~mm~yr**-1))),
                       limits=c(-2,2),
                       high=RColorBrewer::brewer.pal(7,'PuOr')[1], 
                       mid=RColorBrewer::brewer.pal(7,'PuOr')[4], 
                       low=RColorBrewer::brewer.pal(7,'PuOr')[7],
                       oob=scales::squish, 
                       na.value = 'gray')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  facet_wrap(~season, nrow = 1)+
  # guides(fill=guide_colorbar(barwidth = 0.5))+
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(), 
        legend.position = 'right',
        # legend.text = element_text(size=5),
        legend.title = element_text(size=8),
        # legend.key.width = unit(0.25,'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(), 
        axis.ticks = element_blank()); p_PET

p_out <- cowplot::plot_grid(p_P,p_PET, ncol = 1)
ggsave(p_out, 
       filename = "figures/Map_EastOz_ltChange_seasonal_P_PET.png", 
       type='cairo', dpi=300,
       width=11, height=13, units='cm')



# system.time(
#   lt_PPET_season <- dat[,.(val = mean(pe, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
#     .[is.na(val)==F] %>% 
#     .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
#       by=.(x,y,season)]
# )
# 
# p_PPET <- lt_PPET_season %>% 
#   ggplot(data=., aes(x,y,fill=b1))+
#   geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
#   geom_tile()+
#   scale_fill_gradient2(expression(paste(atop(Monthly~frac(P,PET), (yr**-1)))),
#                        limits=c(-0.075,0.075),
#                        high=RColorBrewer::brewer.pal(7,'PRGn')[7], 
#                        mid=RColorBrewer::brewer.pal(7,'PRGn')[4], 
#                        low=RColorBrewer::brewer.pal(7,'PRGn')[1],
#                        oob=scales::squish, 
#                        na.value = 'gray')+
#   labs(x=NULL,y=NULL)+
#   coord_sf(xlim = c(140,154),
#            ylim = c(-45,-10), expand = FALSE)+
#   facet_wrap(~season, nrow = 1)+
#   theme(panel.background = element_rect(fill = '#99A3C4'), 
#         panel.grid = element_blank(), 
#         legend.position = 'right',
#         strip.text = element_text(face='bold'),
#         axis.text = element_blank(), 
#         axis.ticks = element_blank())


# long-term trend of annual P:PET ----------------------------------------------
system.time(
  lt_PPET_annual <- dat[,.(val = mean(pe_12mo, na.rm=TRUE)), by=.(x,y,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y)]
)
p_PPET <- lt_PPET_annual %>%
  ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_gradient2(expression(paste(atop(Delta~frac(P,PET), (yr**-1)))),
                       limits=c(-0.01,0.01),
                       high=scico::scico(11,palette = 'vik')[1],
                       mid=scico::scico(11,palette = 'vik')[6],
                       low=scico::scico(11,palette = 'vik')[11],
                       oob=scales::squish,
                       na.value = 'gray')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  theme(panel.background = element_rect(fill = '#99A3C4'),
        panel.grid = element_blank(),
        legend.position = 'right',
        legend.title = element_text(size=10),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(),
        axis.ticks = element_blank()); p_PPET
ggsave(p_PPET, 
       filename = "figures/Map_EastOz_ltChange_annual_PPET.png", 
       type='cairo', dpi=300,
       width=8, height=13, units='cm')




# long-term trend of mean monthly tmax by season -------------------------------
system.time(
  lt_tmax_season <- dat[is.na(tmax)==F][date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(tmax, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,hydro_year-2000.5), 
                                   y=val, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)]
)
system.time(
  lt_tmin_season <- dat[is.na(tmin)==F][date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(tmin, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)]
)

p_tmax <- lt_tmax_season %>% 
  ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_gradient2(expression(atop("Tmax",(Delta~degree*C~yr**-1))),
                       # limits=c(-2,2),
                       high=RColorBrewer::brewer.pal(7,'RdBu')[1], 
                       mid=RColorBrewer::brewer.pal(7,'RdBu')[4], 
                       low=RColorBrewer::brewer.pal(7,'RdBu')[7],
                       oob=scales::squish, 
                       na.value = 'gray')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  facet_wrap(~season, nrow = 1)+
  # guides(fill=guide_colorbar(barwidth = 0.5))+
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(), 
        legend.position = 'right',
        # legend.text = element_text(size=5),
        legend.title = element_text(size=8),
        # legend.key.width = unit(0.25,'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(), 
        axis.ticks = element_blank());p_tmax
p_tmin <- lt_tmin_season %>% 
  ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_gradient2(expression(atop("Tmin",(Delta~degree*C~yr**-1))),
                       # limits=c(-2,2),
                       high=RColorBrewer::brewer.pal(7,'RdBu')[1], 
                       mid=RColorBrewer::brewer.pal(7,'RdBu')[4], 
                       low=RColorBrewer::brewer.pal(7,'RdBu')[7],
                       oob=scales::squish, 
                       na.value = 'gray')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  facet_wrap(~season, nrow = 1)+
  # guides(fill=guide_colorbar(barwidth = 0.5))+
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(), 
        legend.position = 'right',
        # legend.text = element_text(size=5),
        legend.title = element_text(size=8),
        # legend.key.width = unit(0.25,'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(), 
        axis.ticks = element_blank()); p_tmin
p_out <- cowplot::plot_grid(p_tmax, p_tmin, ncol = 1)
ggsave(p_out, 
       filename = "figures/Map_EastOz_ltChange_seasonal_Tmax_Tmin.png", 
       type='cairo', dpi=300,
       width=11, height=13, units='cm')



p_vc <- dat[date==ymd("2000-01-01")] %>%
  .[veg_class %in% c(1:3,5:9,11,12,13,14)] %>%
  ggplot(data=., aes(x,y, fill=str_wrap(vc, 20)))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  # guides(fill=guide_colorbar(barwidth = 0.5))+
  # scale_fill_brewer(palette = 'Paired', direction = -1)+
  scico::scale_fill_scico_d("NVIS 5.1 Major Veg. Class",
    direction=-1, palette = 'batlow',end=0.95)+
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(), 
        legend.position = 'right',
        # legend.text = element_text(size=5),
        legend.title = element_text(size=8),
        # legend.key.width = unit(0.25,'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(), 
        axis.ticks = element_blank()); p_vc
ggsave(p_vc, 
       filename = 'figures/map_eastOz_NVIS.png', 
       width=10, height = 15, units='cm', dpi = 'retina', type='cairo')


scale_fill_brewer("Major Veg. Class", 
                  palette = "Paired")+
  labs(x=NULL,y=NULL,
       title=expression(paste("Tree Containing Major Veg. Classes of NVIS v5.1")))+
  theme_linedraw()+
  theme(legend.position = 'right', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank()); p_vc

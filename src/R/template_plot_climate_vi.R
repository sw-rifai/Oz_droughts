#*******************************************************************************
#* Description:
#* Plot Climate stuff
#*
#*
#*
library(tidyverse); library(sf)
library(data.table); setDTthreads(threads = 0)
library(lubridate); 
library(mgcv); #library(mgcViz); 
library(dtplyr); 
library(RcppArmadillo); library(patchwork)
library(stars)
library(foreach); library(doParallel)
# IMPORT DATA ###################################################################

# load("data/gridCell_lm_ndvi_clim.Rdata") # grid cell linear regressions
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)

frac <- stars::read_ncdf("../data_general/Oz_misc_data/csiro_FC.v310.MCD43A4_0p5_2001_2019.nc")
frac[,,,1] %>% as_tibble() %>% filter(is.na(soil)==F)
eoz <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_Tree_Cover_5000m_East_Oz_noMask_2000_2019.tif",
                         RasterIO = list(bands=1))
frac <- stars::st_warp(src=frac, dest=eoz, use_gdal = F)
frac <- frac %>% as.data.table() %>% lazy_dt() %>% 
  rename(date=time) %>%
  mutate(date=as.Date(date)) %>% 
  filter(is.na(npv)==F) %>% 
  as.data.table()

# vegetation index record
vi <- arrow::read_parquet("../data_general/MCD43/MCD43_AVHRR_NDVI_hybrid_2020-07-01.parquet", 
                          col_select = c("x","y","date",
                                         "ndvi_c","ndvi_mcd","ndvi_hyb", 
                                         "evi2_hyb","evi2_mcd","sz")) %>% 
  as.data.table()
vi <- frac[vi,on=.(x,y,date)]
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
                             "vpd15_12mo",
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
dat <- dat[order(x,y,date)][, vpd15_12mo := frollmean(vpd15,n = 12,fill = NA,align='right',na.rm=TRUE), by=.(x,y)]
dat <- dat[,`:=`(pe = precip/pet, 
                 pe_12mo = precip_12mo/pet_12mo)]
dat <- merge(dat, 
             vi,
             by=c("x","y","date"), 
             all=TRUE,allow.cartesian=TRUE)
dat <- dat[order(x,y,date)][, ndvi_3mo := frollmean(ndvi_hyb,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]
rm(vi); gc(full=TRUE)
ldat <- dat %>% lazy_dt()
# END Load awap clim dat *****************************************************************


# Load simplified BOM Koppen climate zones --------------------------------
ref_grid <- stars::read_stars('../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif', 
                              RasterIO = list(bands=1))
bom <- stars::read_stars("../data_general/Koppen_climate/BOM/kpngrp.txt")
bom <- st_warp(src=bom, dest=ref_grid[,,], use_gdal = F)
bom <- set_names(bom, 'koppen') %>% as_tibble()
bom <- left_join(ref_grid %>% as_tibble() %>% select(x,y), 
                 bom)
coords <- dat %>% select(x,y) %>% distinct()
kop <- left_join(coords, bom, by=c("x","y")) %>% 
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
                                        "Temperate","Temperate Tas."), ordered = T))
kop <- kop %>% mutate(cz=zone)
#*** End Kop zone load ********************************************************

# Vegetation continuous cover data ----------------------------------------
mod_tree <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_Tree_Cover_5000m_East_Oz_noMask_2000_2019.tif") %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2000-01-01"),ymd("2019-01-01"),by="1 year"), 
                    names = 'date') %>% 
  set_names(c("tree_cover")) %>% 
  as_tibble() %>% 
  as.data.table()

mod_nontree <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_NonTree_Vegetation_5000m_East_Oz_noMask_2000_2019.tif") %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2000-01-01"),ymd("2019-01-01"),by="1 year"), 
                    names = 'date') %>% 
  set_names(c("nontree_cover")) %>% 
  as_tibble() %>% 
  as.data.table()

mod_nonveg <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_NonVegetated_5000m_East_Oz_noMask_2000_2019.tif") %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2000-01-01"),ymd("2019-01-01"),by="1 year"), 
                    names = 'date') %>% 
  set_names(c("nonveg_cover")) %>% 
  as_tibble() %>% 
  as.data.table()

mod <- merge(mod_tree, mod_nontree, by = c("x","y","date"))
mod <- merge(mod, mod_nonveg, by = c("x","y","date"))
rm(mod_tree, mod_nontree, mod_nonveg); gc()

# add the NVIS vegetation classes
# base <- stars::read_stars('../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif', 
#                           RasterIO = list(bands=1))
base <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_Tree_Cover_5000m_East_Oz_noMask_2000_2019.tif", 
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
mod <- vc[mod, on=.(x,y)]
mod[,`:=`(year=year(date))]

# mod <- svi[mod, on=.(x,y,year)]

# Import Seasonal Landsat Cover Fraction ---------------------------------------
lcf <- arrow::read_parquet("../data_general/LandsatFracCover/parquet/SeasLandsatCoverFrac_0.05deg_1988_2019.parquet") %>% 
  as.data.table()
lcf[,`:=`(year=year(date), 
          month=month(date))]
lcf[,`:=`(season = case_when(month==3~'MAM',
                             month==6~'JJA',
                             month==9~'SON',
                             month==12~'DJF'))]
lcf[,`:=`(season = factor(season, levels = c('SON','DJF','MAM','JJA'),ordered = TRUE))]
lcf <- inner_join(vc,lcf,by=c('x','y'))
center_year_lcf <- mean(lcf$year,na.rm=TRUE)
lcf <- lcf[is.na(vc)==F]
# end section ******************************************************************
# END DATA IMPORT SECTION ******************************************************


# Linear change in Landsat Cover Fraction --------------------------------------
system.time(
  lt_lcf_npv <- lcf[is.na(veg_class)==F][date <= ymd('2019-09-01')][,`:=`(year_c = year-center_year_lcf)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=npv, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)] %>% 
    .[,.(x,y,season,b0,b1)]
)
system.time(
  lt_lcf_gv <- lcf[is.na(veg_class)==F][date <= ymd('2019-09-01')][,`:=`(year_c = year-center_year_lcf)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=gv, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)] %>% 
    .[,.(x,y,season,b0,b1)]
)
system.time(
  lt_lcf_bare <- lcf[is.na(veg_class)==F][date <= ymd('2019-09-01')][,`:=`(year_c = year-center_year_lcf)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=soil, data=.SD)$coefficients))), 
      by=.(x,y,season)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y,season)] %>% 
    .[,.(x,y,season,b0,b1)]
)

# END **************************************************************************

# NDVI linear change by season ***********************************************************
library(RcppArmadillo)
system.time(
  lt_ndvi_season <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
    .[date>= ymd("1982-01-01") & date<= ymd("2019-09-30")] %>% 
    .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)
# END **************************************************************************

# Linear change in VCF  ---------------------------------------------------
system.time(
  lt_tree <- mod[,`:=`(year_c = year-2009.5)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=tree_cover, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)
system.time(
  lt_nontree <- mod[,`:=`(year_c = year-2009.5)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=nontree_cover, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)
system.time(
  lt_nonveg <- mod[,`:=`(year_c = year-2009.5)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=nonveg_cover, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)


vcf <- inner_join(lt_nontree %>% rename(grass_u=b0, grass_b=b1) %>% select(-beta), 
                  lt_nonveg %>% rename(nonveg_u=b0, nonveg_b=b1) %>% select(-beta))
vcf <- lt_tree %>% lazy_dt() %>% 
  select(-beta) %>% 
  rename(tree_u=b0, 
         tree_b=b1) %>% 
  as.data.table() %>% 
  inner_join(., vcf)



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

# NDVI linear change by epoch --------------------------------------------------
system.time(
  lt_ndvi_season_p1 <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
    .[date>= ymd("1981-01-01") & date<= ymd("2000-12-31")] %>% 
    .[,.(val = mean(ndvi_c, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)
system.time(
  lt_ndvi_season_p2 <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
    .[date>= ymd("2001-01-01") & date<= ymd("2019-09-30")] %>% 
    .[,.(val = mean(ndvi_mcd, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)
lt_ndvi_season_p1$epoch <- "AVHRR NDVI 1981-2000"
lt_ndvi_season_p2$epoch <- "MODIS NDVI 2001-2019"


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

# Join NDVI linear change by epoch & P:PET change -------------------------------
p_left <- lt_PPET_annual %>%
  ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_gradient2(expression(paste(Delta~frac(P,PET)~(yr**-1))),
                       limits=c(-0.01,0.01),
                       high=scico::scico(11,palette = 'vik')[1],
                       mid="#ffffe3",
                       # mid=scico::scico(11,palette = 'vik')[6],
                       low=scico::scico(11,palette = 'vik')[11],
                       oob=scales::squish,
                       na.value = 'gray')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  guides(fill = guide_colorbar(title.position = 'top'))+
  theme(panel.background = element_rect(fill = '#99A3C4'),
        panel.grid = element_blank(),
        legend.position = c(0.825,0.88),
        # legend.position = 'bottom',
        legend.title = element_text(size=8),
        legend.key.width = unit(0.2, 'cm'),
        legend.key.height = unit(0.4, 'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(),
        axis.ticks = element_blank()); p_left

vec_col <- RColorBrewer::brewer.pal(n=7, name='BrBG')
p_right <- bind_rows(lt_ndvi_season_p1, lt_ndvi_season_p2) %>% 
  ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_gradient2(expression(paste(Delta*NDVI~yr^-1)),
                       high=vec_col[7], 
                       # mid=vec_col[4],
                       mid="#ffffe3",
                       low=vec_col[1],
                       limits=c(-0.005,0.005),
                       breaks=c(-0.005,0,0.005),
                       labels=c("<-0.005",0,">0.005"),
                       oob=scales::squish,
                       na.value='gray')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  facet_grid(epoch~season)+
  guides(fill = guide_colourbar(label = T)) +
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(), 
        # legend.position = c(0.475, 0.01),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.title = element_text(size=8),
        # legend.margin=margin(0,0,0,0),
        # legend.box.margin=margin(-10,-10,-10,-10),
        # legend.key.width = unit(1.5,'cm'),
        strip.text = element_text(face='bold'),
        axis.text = element_blank(), 
        axis.ticks = element_blank()); p_right
ggsave(p_left+p_right+plot_layout(widths = c(1.25,2), guides = 'keep'),
  filename = "figures/PPET_ndvi_seasonal_longtermTrends_prePost2000epochs.png", 
       dpi=350, width=15,height=15,units='cm',type='cairo')
# END **************************************************************************




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






# Koppen Climate Zones & P:PET Trend & NDVI & VCF distributions --------------------------------------------------
p_left <- kop %>% 
  ggplot(data=., aes(x,y,fill=as_factor(zone)))+
  geom_sf(inherit.aes = F, data=oz_poly,
          fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,155),
           ylim = c(-45,-10), 
           expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  scale_fill_viridis_d(option='B',direction = 1,end=0.95)+
  # scico::scale_fill_scico_d(end=0.9,direction = 1)+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  guides(fill=guide_legend(title='Climate Zone', 
                           title.position = 'top'))+
  theme(legend.position = c(0.75,0.85), 
        legend.direction = 'vertical',
        panel.grid = element_blank(), 
        panel.background = element_rect(fill='lightblue')); p_left

aa <- ldat %>% 
  group_by(x,y,hydro_year) %>% 
  summarize(
    ppet = mean(pe_12mo, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(is.na(ppet)==F) %>% 
  as_tibble()
aa <- aa %>% 
  inner_join(., kop %>% rename(cz=zone) %>% select(x,y,cz), by=c("x","y"))
p_right <- aa %>% 
  sample_frac(0.5) %>% 
  rename(`Climate Zone` = cz) %>% 
  ggplot(data=., aes(hydro_year, ppet))+
  # geom_point(alpha=0.05,color='gray')+
  geom_smooth(method='lm',color='black')+
  scale_x_continuous(expand=c(0,0), breaks = c(1983,1990,2000,2010,2019))+
  scale_y_continuous(expand=c(0,0), labels = scales::format_format(3))+
  facet_wrap(~`Climate Zone`,scales = 'free',labeller = label_value, 
             ncol = 2)+
  labs(x=NULL, y="P:PET")+
  theme_linedraw()+
  theme(strip.text = element_text(face='bold'), 
        panel.grid = element_blank(), 
        axis.text.x = element_text(size=7)); p_right

p_left+p_right

lut_kop <- c("Equatorial" = "Equat.",
             "Tropical" = "Trop.", 
             "Subtropical" = "Subtr.", 
             "Grassland & Desert" = "Grass.", 
             "Temperate" = "Temp.",
             "Temperate Tas." = "Tasm.")
p_bottom <- inner_join(kop,lt_ndvi_season,by=c("x","y")) %>% 
  rename(`Zone` = cz) %>% 
  filter(between(b1,-0.004,0.004)) %>% 
  mutate(season = factor(season, levels=c("SON","DJF","MAM","JJA"),ordered = T)) %>% 
  ggplot(data=., aes(b1,fill=as_factor(`Zone`)))+
  geom_histogram(bins = 30)+
  geom_vline(aes(xintercept=0),col='red')+
  # scico::scale_fill_scico_d()+
  scale_fill_viridis_d(option='B',direction = -1,end=0.9)+
  scale_x_continuous(expand=c(0,0), breaks=c(-0.003,0,0.003))+
  facet_grid(`Zone`~season, scales = 'free_y', 
             # labeller = label_wrap_gen(width=10, multi_line = TRUE)
             labeller = labeller(`Zone` = lut_kop)
             )+
  labs(x=expression(paste(Delta~NDVI~yr**-1)))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        strip.text = element_text(face='bold'), 
        legend.position = 'none', 
        axis.text.x = element_text(size=6)); p_bottom

library(ggridges)
p_vcf <- vcf %>% 
  as_tibble() %>% 
  filter(is.na(tree_u)==F) %>% 
  inner_join(., kop %>% select(x,y,cz), by=c("x","y")) %>% 
  select(cz, nonveg_b, grass_b, tree_b) %>%
  rename(`Non. Veg.`=nonveg_b, 
         `Non. Tree Veg.`=grass_b, 
         `Tree Veg.`=tree_b) %>% 
  gather(-cz, key = 'measure', value='estimate') %>% 
  mutate(measure = as_factor(measure)) %>% 
  filter(is.na(estimate)==F) %>% 
  ggplot(data=., aes(x=estimate,
                     y=cz,
                     fill=cz,
                     after_stat(scaled)))+
  ggridges::stat_density_ridges(
    quantile_lines = TRUE, 
    rel_min_height=0.01, 
    alpha=0.5, 
    scale=1, 
    color='black')+
  scale_fill_viridis_d(option='B',direction = 1,end=0.9)+
  geom_vline(aes(xintercept=0),color='red',lwd=0.75)+
  scale_x_continuous(limits=c(-1,1))+
  scale_y_discrete(expand=c(0,0),
     limits=rev(c("Equatorial","Tropical", 
              "Subtropical", "Grassland & Desert", 
              "Temperate","Temperate Tas.")), 
     labels=str_wrap(rev(c("Equatorial","Tropical", 
                  "Subtropical", "Grassland & Desert", 
                  "Temperate","Temperate Tasmania")),width = 10))+
  labs(x=expression(paste(Cover~Trend~('%'~yr**-1))), 
       y=NULL)+
  facet_wrap(~ measure, ncol = 6)+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        strip.text = element_text(face='bold'),
        legend.position = 'none'); p_vcf

(p_left|((p_right/p_bottom)))/(p_vcf)+    
  plot_layout(heights=c(20,5,3,2),
              nrow=3)

ggsave(plot = (p_left|(p_right/p_bottom/p_vcf)+
                 plot_annotation(tag_levels = 'A')),
       filename = "figures/map_KoppenZones_PPET_change_VCF.png", 
       width = 25, height=30, units='cm', dpi=350, type='cairo')

# END SECTION ******************************************************************

# K-means Climate Zones & P:PET Trend & NDVI & VCF distributions --------------------------------------------------
jj <- ldat %>% 
  group_by(x,y,season) %>% 
  summarize(ppet_u = mean(pe, na.rm=TRUE),
            ppet_sd = sd(pe,na.rm=TRUE), 
            tmax_u = mean(tmax,na.rm=TRUE),
            tmax_sd = sd(tmax,na.rm=TRUE), 
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
  # filter(season %in% c("DJF","JJA")) %>% 
  group_by(season) %>% 
  mutate_at(c("ppet_u","ppet_sd",
              'tmax_u','tmax_sd',
              'vpd15_u','vpd15_sd',
              'precip_u','precip_sd'), scale2) %>% 
  # mutate_at(c("tmax","precip"), scale2) %>% 
  ungroup()
jj2 <- jj2 %>% pivot_wider(names_from=season, 
                           values_from=c(ppet_u,tmax_u,vpd15_u,precip_u, 
                                         ppet_sd,tmax_sd,vpd15_sd,precip_sd))
set.seed(999)
jj3 <- kmeans(jj2 %>% select(-x,-y) %>% as.matrix(),centers=6,
              iter.max = 100, 
              nstart = 100)
jj2$cluster <- jj3$cluster
jj2 <- inner_join(jj2, 
                  jj3$centers %>% as_tibble() %>% 
                  mutate(cluster = 1:6) %>% 
  mutate(cz = rank(tmax_u_DJF+tmax_u_JJA+tmax_u_MAM+tmax_u_SON)) %>% 
    select(cluster, cz))

jj2 <- jj2 %>% select(-cluster)
jj2 %>% select(x,y,cz) %>% arrow::write_parquet(.,sink="data/EOz_clim_kmeans6.parquet")

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
#
aa <- ldat %>% 
  group_by(x,y,hydro_year) %>% 
  summarize(
    ppet = mean(pe_12mo, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(is.na(ppet)==F) %>% 
  as_tibble()
aa <- aa %>% 
  inner_join(., jj2 %>% select(x,y,cz), by=c("x","y"))
p_right <- aa %>% 
  sample_frac(0.5) %>% 
  rename(`Climate Zone` = cz) %>% 
  ggplot(data=., aes(hydro_year, ppet))+
  # geom_point(alpha=0.05,color='gray')+
  geom_smooth(method='lm',color='black')+
  scale_x_continuous(expand=c(0,0), breaks = c(1983,1990,2000,2010,2019))+
  scale_y_continuous(expand=c(0,0))+
  facet_wrap(~`Climate Zone`,scales = 'free',labeller = label_both, 
             ncol = 2)+
  labs(x=NULL, y="P:PET")+
  theme_linedraw()+
  theme(strip.text = element_text(face='bold'), 
        panel.grid = element_blank(), 
        axis.text.x = element_text(size=7)); p_right

p_left+p_right


# NDVI linear change ***
library(RcppArmadillo)
system.time(
  lt_ndvi_season <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
    .[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)

p_bottom <- inner_join(jj2,lt_ndvi_season,by=c("x","y")) %>% 
  rename(`Zone` = cz) %>% 
  filter(between(b1,-0.004,0.004)) %>% 
  ggplot(data=., aes(b1,fill=as_factor(`Zone`)))+
  geom_histogram(bins = 100)+
  geom_vline(aes(xintercept=0),col='red')+
  # scico::scale_fill_scico_d()+
  scale_fill_viridis_d(option='B',direction = -1,end=0.9)+
  scale_x_continuous(expand=c(0,0), breaks=c(-0.003,0,0.003))+
  facet_grid(`Zone`~season, scales = 'free_y')+
  labs(x=expression(paste(Delta~NDVI~yr**-1)))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        strip.text = element_text(face='bold'), 
        legend.position = 'none', 
        axis.text.x = element_text(size=6)); p_bottom


library(ggridges)
p_vcf <- vcf %>% 
  as_tibble() %>% 
  filter(is.na(tree_u)==F) %>% 
  inner_join(., jj2 %>% select(x,y,cz), by=c("x","y")) %>% 
  select(cz, nonveg_b, grass_b, tree_b) %>%
  rename(`Non. Veg.`=nonveg_b, 
         `Non. Tree Veg.`=grass_b, 
         `Tree Veg.`=tree_b) %>% 
  gather(-cz, key = 'measure', value='estimate') %>% 
  mutate(measure = as_factor(measure)) %>% 
  filter(is.na(estimate)==F) %>% 
  mutate(cz = as_factor(cz)) %>% 
  ggplot(data=., aes(x=estimate,
                     y=cz,
                     fill=cz,
                     after_stat(scaled)))+
  ggridges::stat_density_ridges(
                                quantile_lines = TRUE, 
                                rel_min_height=0.01, 
                                alpha=0.5, 
                                scale=1.5, 
                                color='black')+
  scale_fill_viridis_d(option='B',direction = -1,end=0.9)+
  geom_vline(aes(xintercept=0),color='red',lwd=0.75)+
  scale_x_continuous(limits=c(-1,1))+
  scale_y_discrete(expand=c(0,0), limits=rev(levels(as_factor(1:6))))+
  labs(x=expression(paste(Cover~Trend~('%'~yr**-1))), 
       y='Climate Zone')+
  facet_wrap(~ measure, ncol = 6)+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        strip.text = element_text(face='bold'),
        legend.position = 'none'); p_vcf

(p_left|((p_right/p_bottom)))/(p_vcf)+    
  plot_layout(heights=c(20,5,3,2),
              nrow=3)

ggsave(plot = (p_left|(p_right/p_bottom/p_vcf)+
                 plot_annotation(tag_levels = 'A')),
       filename = "figures/map_climateZones_PPET_change_VCF.png", 
       width = 25, height=30, units='cm', dpi=350, type='cairo')


# (p_left|((p_right/p_bottom)+plot_layout(heights = c(4,3))))/(p_vcf+
#           plot_layout(heights=c(1)))
# 
# ((p_left+(p_right/p_bottom))/p_vcf)+
#   plot_layout(guides='auto', 
#               heights=unit(c(10,5,3),c("cm",'cm','cm')))
# 


# NDVI GAM P:PET density plot ---------------------------------------------
library(viridisLite)
library(mgcv)
dat[pe_12mo <= 2][ndvi_m>0][sample(.N, 1e6)] %>% 
  ggplot(data=., aes(pe_12mo,ndvi_m))+
  # ggpointdensity::geom_pointdensity(size=0.5)+
  stat_density_2d(geom='raster',
                  aes(fill=after_stat(density)),
                  contour = F)+
  geom_smooth(se=F,color='red', method='gam',
              method.args=list(select=TRUE))+
  # scale_color_viridis_c()+
  scale_fill_gradientn(colors=c('black', viridis(99)), 
                       trans='identity')+
  # khroma::scale_fill_land()+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI)))+
  guides(fill=guide_colorbar(title.position = 'top', 
                             title = 'density'))+
  theme(legend.direction = 'horizontal',
        legend.position = c(0.75,0.15), 
        legend.background = element_rect(fill='black'), 
        legend.text = element_text(color='gray90'), 
        legend.title = element_text(color='gray90'), 
        legend.key.width = unit(1,'cm'))
ggsave(filename = 'figures/mcd43_ndvi_ppet_2d_density.png',
       width=16, height = 10, units='cm',type='cairo')

# NDVI GAM P:VPD density plot ---------------------------------------------
library(viridisLite)
library(mgcv)
dat[pe_12mo <= 2][ndvi_mcd>0][sample(.N, 1e6)] %>% 
  ggplot(data=., aes(precip_12mo/sqrt(vpd15_12mo),ndvi_mcd))+
  # ggpointdensity::geom_pointdensity(size=0.5)+
  stat_density_2d(geom='raster',
                  aes(fill=after_stat(density)),
                  contour = F)+
  geom_smooth(se=F,color='red', method='gam',
              method.args=list(select=TRUE))+
  # scale_color_viridis_c()+
  scale_fill_gradientn(colors=c('black', viridis(99)), 
                       trans='identity')+
  # khroma::scale_fill_land()+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x=expression(paste(paste(Precip.["12 month sum"])," / ", sqrt(VPD["12 month mean"]))), 
       y=expression(paste(NDVI)))+
  guides(fill=guide_colorbar(title.position = 'top', 
                             title = 'density'))+
  theme(legend.direction = 'horizontal',
        legend.position = c(0.75,0.15), 
        legend.background = element_rect(fill='black'), 
        legend.text = element_text(color='gray90'), 
        legend.title = element_text(color='gray90'), 
        legend.key.width = unit(1,'cm'))
ggsave(filename = 'figures/mcd43_ndvi_pvpd_2d_density.png',
       width=16, height = 10, units='cm',type='cairo')
# END ********

# P:PET Shift -------------------------------------------------------------
epoch1 <- ldat %>% 
  filter(hydro_year %in% c(1981:1985)) %>% 
  group_by(x,y) %>% 
  summarize(ppet = mean(pe_12mo,na.rm=TRUE), 
            ndvi_u = mean(ndvi_3mo,na.rm=TRUE)) %>% 
  as_tibble()
epoch2 <- ldat %>% 
  filter(hydro_year %in% c(2015:2019)) %>% 
  group_by(x,y) %>% 
  summarize(ppet = mean(pe_12mo,na.rm=TRUE), 
            ndvi_u = mean(ndvi_3mo,na.rm=TRUE)) %>% 
  as_tibble()
inner_join(epoch1,epoch2,by=c("x","y"),suffix=c("_1","_2")) %>% 
  mutate(delta_x = ppet_1 - ppet_2) %>% 
  filter(ppet_1 < 2) %>% 
  sample_n(100,weight = ppet_1) %>% 
  ggplot(data=., aes(ppet_1, ndvi_u_1))+
  geom_point(color='blue')+
  geom_segment(aes(xend=ppet_2,yend=ndvi_u_2), 
               arrow=arrow(length=unit(0.1,'cm')))+
  theme_linedraw()

o <- inner_join(epoch1,epoch2,by=c("x","y"),suffix=c("_1","_2")) %>% 
  filter(ppet_1 <= 2) %>% 
  filter(ndvi_u_1 > 0 & ndvi_u_2 > 0) %>% 
  mutate(delta_x = ppet_2 - ppet_1) %>% 
  mutate(id = cur_group_rows())
vec_ids <- sample(o$id, 1500)
p_vector <- o %>% 
  filter(id %in% vec_ids) %>% 
  # mutate(bobo = cut_interval(ppet_1,n=10)) %>% 
  # group_by(bobo) %>% 
  # sample_n(1000) %>% 
  ggplot(data=., aes(ppet_1, ndvi_u_2,color=delta_x))+
  # geom_point(color='navy')+
  # geom_point(color='red',aes(ppet_2,ndvi_u_2))+
  geom_point(data=epoch2 %>% filter(ndvi_u>0), 
             aes(ppet, ndvi_u), color=NA)+
  geom_segment(aes(xend=ppet_2,yend=ndvi_u_1),
               arrow=arrow(length=unit(0.1,'cm')))+
  scale_color_gradient2(
    mid='gray', limits=c(-0.25,0.25), 
    oob=scales::squish)+
  labs(x='P:PET',y='NDVI')+
  scale_x_continuous(limits=c(0,2.25),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_linedraw()+
  guides(color=guide_colorbar(title.position = 'top', 
        title = expression(paste(Delta~P*":"*PET))))+
  theme(legend.direction = 'horizontal',
        legend.position = c(0.75,0.15), 
        panel.grid = element_blank()); p_vector
p_out <- ggExtra::ggMarginal(p_vector, type='histogram')

ggsave(p_out, 
       filename = 'figures/ndvi_ppet_1981_1985_shift_2015_2019_wMarginDistribution.png',
       width=16, height = 10, units='cm',type='cairo')

library(magick)
p1 <- magick::image_read("figures/diagram_PPET_CO2_response.png")
p2 <- magick::image_read("figures/ndvi_ppet_1981_1985_shift_2015_2019_wMarginDistribution.png")
p_out <- magick::image_append(c(p1,p2),
                     stack=TRUE)
magick::image_write(p_out, path="figures/join_ndvi_ppet_vectorPlot_diagram.png")


#*******************************************************************************
# NDVI GAM time series by climate zone ------------------------------------
#*******************************************************************************
library(mgcv)

czones <- arrow::read_parquet("data/EOz_clim_kmeans6.parquet") %>% 
  as.data.table()
czones %>% 
  ggplot(data=., aes(x,y,fill=as_factor(cz)))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_d(option='B',direction = -1)

dat <- merge(czones, 
             dat,
             by=c("x","y"), 
             all=TRUE,allow.cartesian=TRUE)

vec_ids <- unique(dat[,.(id,cz)]) %>% .[is.na(id)==F & is.na(cz)==F]
vec_ids <- vec_ids[,.SD[sample(.N, min(1000,.N))],by=cz]
o <- dat[is.na(season)==F] %>%
  .[id %in% vec_ids$id] %>% 
  .[date >= ymd('1982-01-01')] %>% 
  .[date <= ymd('2019-10-01')] %>% 
  .[,.(x,y,date,season,cz,id,ndvi_hyb,ndvi_3mo,ndvi_mcd)]
vec_cols <- viridis::viridis(10, begin = 0.1,end=0.9)
factor(o$season[1], levels=c("SON","DJF","MAM","JJA"),ordered = T)
p_lm <- o %>%   
  mutate(season=factor(o$season, levels=c("SON","DJF","MAM","JJA"),ordered = T)) %>% 
  ggplot(data=., aes(date, ndvi_hyb))+
  geom_smooth(method='lm', color='black',se=F)+
  geom_smooth(method='lm',color=vec_cols[2],se=F, 
              data=o[date %between% c("1981-01-01","1991-01-01")])+
  geom_smooth(method='lm',color=vec_cols[3],se=F, 
              data=o[date %between% c("1986-01-01","1996-01-01")])+
  geom_smooth(method='lm',color=vec_cols[4],se=F, 
              data=o[date %between% c("1991-01-01","2001-01-01")])+
  geom_smooth(method='lm',color=vec_cols[5],se=F, 
              data=o[date %between% c("1996-01-01","2006-01-01")])+
  geom_smooth(method='lm',color=vec_cols[6],se=F, 
              data=o[date %between% c("2001-01-01","2011-01-01")])+
  geom_smooth(method='lm',color=vec_cols[7],se=F, 
              data=o[date %between% c("2006-01-01","2016-01-01")])+
  geom_smooth(method='lm',color=vec_cols[8],se=F, 
              data=o[date %between% c("2011-01-01","2019-09-01")])+
  geom_smooth(method='lm',color='red',se=F, 
              data=o[date %between% c("2001-01-01","2019-09-01")], 
              aes(date, ndvi_mcd))+
  geom_smooth(method='lm',color=scales::muted('red'),se=F, 
              data=o[date %between% c("1981-01-01","2000-12-01")], 
              aes(date, ndvi_hyb))+
  scale_x_date(expand=c(0,0))+
  labs(x=NULL, y="NDVI")+
  facet_grid(cz~season,scales='free')+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        strip.text = element_text(face='bold'))
ggsave(p_lm, filename = "figures/ndvi_lin_trend_10yr_segs.png", 
       width=20, height=15, units='cm', dpi=350, type='cairo')






# o %>%   
#   ggplot(data=., aes(date, ndvi_hyb))+
#   # geom_line(aes(date,ndvi_3mo,group=id),lwd=0.1)+
#   geom_smooth(method='lm', color='black',se=F)+
#   geom_smooth(method='gam',
#               formula=y~s(x,bs='tp',k=8),
#               method.args=list(select=F), 
#               color='blue', se=F)+
#   geom_smooth(method='gam',
#               formula=y~s(x,bs='cs',k=4),
#               method.args=list(select=T), 
#               color='red', se=F)+
#   scale_x_date(expand=c(0,0))+
#   facet_grid(cz~season,scales='free')+
#   theme_linedraw()
# 
# 
# tsblock <- expand_grid(hydro_year=1986:2015, 
#                        season=levels(dat$season[1]))
# tsblock$fit <- NA
# tsblock$beta0 <- NA
# tsblock$beta1 <- NA
# for(i in 1:dim(tsblock)[1]){
#   fy <- tsblock$hydro_year[i]
#   fs <- tsblock$season[i]
#   fit <- dat[season==fs][hydro_year>=(fy-2) & hydro_year<=(fy+2)] %>% 
#     .[sample(.N,1e5)] %>% 
#     lm(ndvi_hyb ~ date, data=.)
#   tsblock$beta0[i] <- coef(fit)[1]
#   tsblock$beta1[i] <- coef(fit)[2]
#   gc(verbose = F)
#   print(paste(fy,fs))
# }
# 
# tsblock %>% 
#   mutate(pred = beta0+beta1*hydro_year) %>% 
#   ggplot(data=., aes(hydro_year, pred))+
#   geom_point()+
#   facet_grid(season~cz)
# 
# 
# for(i in 1:dim(tsblock)[1]){
# }
# 
# 
# tsblock$fit[1] %>% unname %>% unlist
# 
# dat[is.na(ndvi_c)==F] %>% 
#   .[is.na(season)==F] %>% 
#   .[sample(.N, 100000)] %>% 
#   ggplot(data=., aes(date, ndvi_3mo))+
#   geom_smooth()+
#   geom_smooth(method='gam',
#               formula=y~s(x,bs='cs',k=5),
#               method.args=list(select=T), 
#               color='red', se=F)+
#   geom_smooth(method='lm',color='black')+
#   facet_grid(cz~season, scales='free')
# 
# ggplot()+
#   geom_smooth(data=dat[is.na(ndvi_c)==F] %>% 
#                 .[is.na(season)==F] %>% 
#                 .[date <= ymd("2001-01-01")] %>% 
#                 .[sz < 80] %>% 
#                 .[sample(.N, 100000)],
#               aes(date, ndvi_c),
#     method='lm',color='red')+
#   geom_smooth(data=dat[is.na(ndvi_c)==F] %>% 
#                 .[is.na(season)==F] %>% 
#                 .[date <= ymd("2001-01-01")] %>% 
#                 .[sample(.N, 100000)],
#               aes(date, ndvi_hyb),
#               method='lm',color='blue')+
#   geom_smooth(data=dat[is.na(ndvi_c)==F] %>% 
#                 .[is.na(season)==F] %>% 
#                 .[date >= ymd("2001-01-01")] %>% 
#                 .[date <= ymd("2017-12-01")] %>%
#                 .[sample(.N, 100000)],
#               aes(date, ndvi_c),
#               method='lm',color='#F50000')+
#   geom_smooth(data=dat[is.na(ndvi_mcd)==F] %>% 
#                 .[is.na(season)==F] %>% 
#                 .[date <= ymd("2017-12-01")] %>%
#                 .[sample(.N, 100000)],
#               aes(date, ndvi_mcd),
#               method='lm',color='blue')+
#   geom_smooth(data=dat[is.na(ndvi_hyb)==F] %>% 
#                 .[is.na(season)==F] %>% 
#                 .[date >= ymd("2001-01-01")] %>% 
#                 .[date <= ymd("2017-12-01")] %>%
#                 .[sample(.N, 100000)],
#               aes(date,ndvi_hyb),
#               method='gam',
#               formula=y~s(x,bs='cs',k=10),
#               method.args=list(select=T), 
#               color='black', se=F)+
#   facet_grid(cz~season, scales='free')
# 
# ggplot()+
#   geom_smooth(data=dat[is.na(ndvi_c)==F] %>% 
#                 .[is.na(season)==F] %>% 
#                 # .[date <= ymd("2001-01-01")] %>% 
#                 .[sample(.N, 100000)],
#               aes(date, ndvi_hyb),
#               method='lm',color='black')+
#   geom_smooth(data=dat[is.na(ndvi_c)==F] %>% 
#                 .[is.na(season)==F] %>% 
#                 .[date >= ymd("1981-01-01") & 
#                   date <= ymd("1987-12-31")] %>%
#                 .[sample(.N, 100000)],
#               aes(date, ndvi_hyb),
#               method='lm',color='blue')+
#   geom_smooth(data=dat[is.na(ndvi_c)==F] %>% 
#                 .[is.na(season)==F] %>% 
#                 .[date >= ymd("1987-01-01") & 
#                     date <= ymd("1992-12-31")] %>%
#                 .[sample(.N, 100000)],
#               aes(date, ndvi_hyb),
#               method='lm',color='blue')+
#   geom_smooth(data=dat[is.na(ndvi_c)==F] %>% 
#                 .[is.na(season)==F] %>% 
#                 .[date >= ymd("1992-01-01") & 
#                     date <= ymd("1997-12-31")] %>%
#                 .[sample(.N, 100000)],
#               aes(date, ndvi_hyb),
#               method='lm',color='blue')+
#   geom_smooth(data=dat[is.na(ndvi_c)==F] %>% 
#                 .[is.na(season)==F] %>% 
#                 .[date >= ymd("1997-01-01") & 
#                     date <= ymd("2003-12-31")] %>%
#                 .[sample(.N, 100000)],
#               aes(date, ndvi_hyb),
#               method='lm',color='blue')+
#   geom_smooth(data=dat[is.na(ndvi_c)==F] %>% 
#                 .[is.na(season)==F] %>% 
#                 .[date >= ymd("2003-01-01") & 
#                     date <= ymd("2008-12-31")] %>%
#                 .[sample(.N, 100000)],
#               aes(date, ndvi_hyb),
#               method='lm',color='blue')+
#   facet_grid(cz~season, scales='free')
# 
# dat[unique()]
# 
# 
# 
# dat[is.na(ndvi_m)==F]  
# dat[is.na(ndvi_hyb)==F]$date %>% min
# 
# (dat[season=="MAM" & cz==1] %>% 
#  .[date >= ymd("2001-12-01")] %>% 
#  .[date <= ymd("2017-12-01")])$ndvi_c %>% mean(., na.rm=TRUE)
# (dat[season=="MAM" & cz==1] %>% 
#     .[date >= ymd("2001-12-01")] %>% 
#     .[date <= ymd("2017-12-01")])$ndvi_mcd %>% mean(., na.rm=TRUE)
# (dat[season=="MAM" & cz==1] %>% 
#     .[date >= ymd("2001-12-01")] %>% 
#     .[date <= ymd("2017-12-01")])$ndvi_hyb %>% mean(., na.rm=TRUE)

  

#*******************************************************************************
# END
#*******************************************************************************

# # geofacet P:PET ----------------------------------------------------------
# library(geofacet)
# "row,col,code,name
# 1,3,qld_ne,QLD North Coast
# 1,1,qld_nw,QLD North Interior
# 1,2,qld_nc,QLD North Central
# 2,1,qld_sw,QLD South Interior
# 2,2,qld_sc,QLD South Central
# 2,3,qld_se,QLD South Coast
# 3,1,nsw_nw,NSW North Interior 
# 3,2,nsw_nc,NSW North Central
# 3,3,nsw_ne,NSW North Coastal
# 4,1,nsw_sw,NSW South Interior
# 4,2,nsw_sc,NSW South Central
# 4,3,nsw_se,NSW South Coast
# 5,3,vic_e,VIC Eastern
# 5,2,vic_w,VIC Western
# 5,1,sa_e,SouthAus Coastal
# 6,2,tas_w,Tasmania Western
# 6,3,tas_e,Tasmania Eastern"
# 
# mygrid <- data.frame(
#   row = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6),
#   col = c(3, 1, 2, 1, 2, 3, 1, 2, 3, 1, 2, 3, 3, 2, 1, 2, 3),
#   code = c("qld_ne", "qld_nw", "qld_nc", "qld_sw", "qld_sc", "qld_se", "nsw_nw", "nsw_nc", "nsw_ne", "nsw_sw", "nsw_sc", "nsw_se", "vic_e", "vic_w", "sa_e", "tas_w", "tas_e"),
#   name = c("QLD North Coast", "QLD North Interior", "QLD North Central", "QLD South Interior", "QLD South Central", "QLD South Coast", "NSW North Interior ", "NSW North Central", "NSW North Coastal", "NSW South Interior", "NSW South Central", "NSW South Coast", "VIC Eastern", "VIC Western", "SouthAus Coastal", "Tasmania Western", "Tasmania Eastern"),
#   stringsAsFactors = FALSE
# )
# geofacet::grid_preview(mygrid)
# # load("data/gridCell_lm_ndvi_clim.Rdata") # grid cell linear regressions
# oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
#                        layer="gadm36_AUS_1")
# oz_poly <- st_as_sf(oz_poly)
# oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)
# plot(oz_poly["NAME_1"])
# oz_poly %>% 
#   group_by(NAME_1) %>% 
#   summarize()
# 
# 
# 
# oo <- ldat %>% 
#   group_by(hydro_year,x,y) %>% 
#   summarize(ppet = mean(pe_12mo,na.rm=TRUE)) %>% 
#   ungroup() %>% 
#   filter(is.na(hydro_year)==F & is.na(ppet)==F) %>% 
#   as_tibble()
# 
# oo %>% 
#   mutate(zone = case_when( 
#     between(x, 141,144)&between(y,-20,-10)~'qld_nw',
#     between(x, 141,144)&between(y,-20,-10)~'qld_nc',
#     between(x, 141,144)&between(y,-20,-10)~'qld_ne',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     between(x, ,) & between(y, , )~'',
#     
#     TRUE ~ 'other')) %>% 
#   pull(zone) %>% table

# Plot NVIS ---------------------------------------------------------------
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


# scale_fill_brewer("Major Veg. Class", 
#                   palette = "Paired")+
#   labs(x=NULL,y=NULL,
#        title=expression(paste("Tree Containing Major Veg. Classes of NVIS v5.1")))+
#   theme_linedraw()+
#   theme(legend.position = 'right', 
#         axis.title = element_blank(), 
#         axis.text = element_blank(), 
#         panel.background = element_rect(fill='grey'), 
#         panel.grid = element_blank()); p_vc


# Mean Annual P:PET -------------------------------------------------------
mape <- ldat %>% 
  group_by(x,y) %>% 
  summarize(mape = mean(pe_12mo,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble()
p_mappet <- mape %>% 
  ggplot(data=., aes(x,y,fill=mape))+
  geom_sf(inherit.aes = F, data=oz_poly,
          fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,155),
           ylim = c(-45,-10), 
           expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  scale_fill_viridis_c(option='B',direction = -1,end=0.95, 
                       limits=c(0,2),
                       oob=scales::squish, 
                       breaks=c(0,0.5,1,1.5,2),
                       labels=c("0","0.5","1","1.5","2+"))+
  # scico::scale_fill_scico_d(end=0.9,direction = 1)+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  guides(fill=guide_colorbar(title='Mean Annual P:PET',
                           title.position = 'top'))+
  theme(legend.position = c(0.7,0.925), 
        legend.direction = 'horizontal',
        legend.background = element_rect(fill=NA),
        panel.grid = element_blank())
p_mappet+p_vc
ggsave(
       filename = 'figures/map_eastOz_MAPPET_NVIS.png', 
       width=20, height = 20, units='cm', dpi = 'retina', type='cairo')



# C3/C4 grass fraction ----------------------------------------------------
base <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_Tree_Cover_5000m_East_Oz_noMask_2000_2019.tif", 
                          RasterIO = list(bands=1))
og <- stars::read_ncdf("../data_general/Oz_misc_data/grass_fraction_cover/mstmip_driver_global_hd_grass_frac_presentveg_v1.nc4")
o3 <- stars::read_ncdf("../data_general/Oz_misc_data/grass_fraction_cover/mstmip_driver_global_hd_c3_rfrac_presentveg_v1.nc4")
o4 <- stars::read_ncdf("../data_general/Oz_misc_data/grass_fraction_cover/mstmip_driver_global_hd_c4_rfrac_presentveg_v1.nc4")
og <- st_warp(src=og, dest=base[,,], use_gdal = T)
o3 <- st_warp(src=o3, dest=base[,,], use_gdal = T)
o4 <- st_warp(src=o4, dest=base[,,], use_gdal = T)
grass <- inner_join(o3 %>% as_tibble() %>% set_names(c("x","y","c3")),
                   o4 %>% as_tibble() %>% set_names(c("x","y","c4")))
grass <- inner_join(grass, og %>% as_tibble() %>% set_names(c("x","y","grass_frac")))

grass %>% ggplot(data=., aes(c3*grass_frac,c4*grass_frac))+geom_point()


# NDVI linear change ***
library(RcppArmadillo)
system.time(
  lt_ndvi_season <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
    .[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)

czones <- arrow::read_parquet("data/EOz_clim_kmeans6.parquet") %>% 
  as.data.table()

inner_join(lt_ndvi_season, grass) %>% 
  inner_join(., czones) %>% 
  sample_n(10000) %>% 
  ggplot(data=., aes(c4/(c3+c4), b1))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_grid(season~cz, scales='free')

p_l <- grass %>% 
  filter(grass_frac > 0) %>% 
  ggplot(data=., aes(x,y, fill=c4/(c3+c4)))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c("fraction C4 of grass fraction")+
  labs(caption = stringr::str_wrap("data: NACP MsTMIP: Global and North American Driver Data for Multi-Model Intercomparison",width = 30), 
       x=NULL, y=NULL)+
  guides(fill=guide_colourbar(title.position = 'top'))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        legend.position = 'bottom')

p_r <- inner_join(lt_ndvi_season, grass) %>% 
  inner_join(., czones) %>% 
  mutate(c4=c4*grass_frac, 
         c3=c3*grass_frac) %>% 
  select(c3,c4,cz) %>% 
  gather(-cz, key='cover',value='estimate') %>% 
  sample_n(10000) %>%
  ggplot(data=., aes(estimate, fill=cover))+
    geom_histogram(alpha=0.8,bins=50)+
  # geom_histogram(aes(c4*grass_frac),fill='red',alpha=0.9, bins=50)+
  # geom_histogram(aes(c3*grass_frac),fill='blue',alpha=0.5, bins=50)+
  facet_wrap(~cz, scales='free', nrow = 6)

ggsave(plot = p_l|p_r+plot_layout(guides='keep'),
       filename = "figures/experimental_c4_czones.png", 
       width = 25, height=30, units='cm', dpi=350, type='cairo')



# NPV and changing non-tree veg fraction ---------------------------------------
czones <- arrow::read_parquet("data/EOz_clim_kmeans6.parquet") %>% 
  as.data.table()

o1 <- ldat %>% 
  filter(is.na(ndvi_mcd)==FALSE) %>% 
  group_by(x,y,date) %>% 
  summarize(ndvi = median(ndvi_mcd,na.rm=TRUE), 
            soil = median(soil,na.rm=TRUE), 
            gv = median(gv,na.rm=TRUE), 
            npv = median(npv,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as.data.table()
o1[,`:=`(tot= soil+gv+npv)]
o1[,`:=`(soil=soil/tot, 
         gv=gv/tot,
         npv=npv/tot)]
o1 <- merge(o1, czones, by=c("x","y"))
o1 <- merge(mod %>% select(-date), 
            o1 %>% mutate(year=year(date)), 
            by = c('x','y','year'))

o1 %>% 
  lazy_dt() %>% 
  group_by(year,x,y) %>% 
  summarize(ndvi = mean(ndvi, na.rm=TRUE), 
            npv = mean(npv,na.rm=TRUE), 
            gv = mean(gv,na.rm=TRUE), 
            soil = mean(soil, na.rm=TRUE),
            nontree=mean(nontree_cover,na.rm=TRUE), 
            tree=mean(tree_cover,na.rm=TRUE), 
            nonveg_cover=mean(nonveg_cover,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as.data.table() %>%
  na.omit() %>% 
  cor() %>% 
  corrplot::corrplot(method='number', type = 'full')

unique(o1[,.(veg_class,vc)])  
  
o1 %>% 
  lazy_dt() %>% 
  group_by(date,cz,vc,veg_class) %>% 
  summarize(npv = mean(npv,na.rm=TRUE), 
            gv = mean(gv,na.rm=TRUE), 
            soil = mean(soil, na.rm=TRUE),
            nontree=mean(nontree_cover,na.rm=TRUE), 
            tree=mean(tree_cover,na.rm=TRUE), 
            nonveg_cover=mean(nonveg_cover,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  filter(veg_class %in% c(2,3)) %>% 
  filter(cz==2) %>% #pull(date)
  ggplot(data=., aes(date, nonveg_cover,color=as_factor(cz),
                     group=as_factor(cz)))+
  geom_line()+
  geom_smooth(method='lm')+
  scale_color_viridis_d(option='B',end=0.9, direction = -1)+
  facet_wrap(~veg_class)
  

o1 %>% as_tibble() %>% 
  filter(cz==2) %>% 
  sample_n(10000) %>%
  ggplot(data=., aes(date, npv*100))+
  # geom_point()+
  geom_smooth(color='red')+
  geom_smooth(aes(date, nontree_cover), color='orange')
  geom_smooth(aes(hydro_year,gv),method='lm',color='green')+
  geom_smooth(aes(hydro_year,soil),method='lm',color='brown')


dat[is.na(npv) == F][, .SD[npv == max(npv, na.rm = TRUE)], 
                        keyby = .(x, y)] %>% 
  ggplot(data=., aes(x,y,fill=month))+
  geom_tile()+
  scico::scale_fill_scico(palette='romaO',direction = -1)+
  coord_equal()


o1 %>% ggplot(data=., aes(hydro_year,nontree_cover))+
  geom_smooth(method='lm')+
  facet_wrap(~cz, scales = 'free')

o1 %>% ggplot(data=., aes(hydro_year,soil,color=season))+
  geom_smooth()+
  facet_wrap(~cz, scales = 'free')

o1[cz==2] %>% sample_n(1000) %>% ggplot(data=., aes(npv,nontree_cover))+
  geom_point()+geom_smooth()

b <- o1[cz==2] %>% bam(nontree_cover~ti(npv,by=season)+ti(gv,by=season), data=., 
                  select=TRUE, discrete = TRUE)
summary(b); plot(b)
# End section ******************************************************************



# Exponential Function w/tmax ----------------------------------------------------
n_tmax <- nls_multstart(ndvi_3mo ~ 
        a1*(tmax_anom_3mo/tmax_u_3mo)+a2*(tmax_anom_3mo/tmax_u_3mo)**2 + 
        (b0+b1*cco2)*atan(b2*mape+b3*cco2+b4*pe_anom_12mo),
        data=train_dat, 
        iter = 10,
        supp_errors = 'Y',
        control=nls.control(maxiter=100),
        start_lower = c(a1=0,a2=0,b0=0,b1=0,b2=0,b3=0,b4=0), 
        start_upper = c(a1=1,a2=1,b0=1,b1=1,b2=0.01,b3=0.01,b4=0.01)) 
summary(n_tmax)

sqrt(mean((predict(n_tmax, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_tmax, newdata=test_dat),test_dat$ndvi_3mo)**2

expand_grid(matmax=25, 
            tmax_u_3mo=25, 
            tmax_anom_3mo=seq(0,40,length.out=100), 
            cco2=c(0),
            pe_anom_12mo=0,
            mape=c(0.25,0.5,0.75,1)) %>% 
  mutate(pred = predict(n_tmax, newdata=.)) %>% 
  ggplot(data=., aes(tmax_anom_3mo,pred,color=as_factor(mape)))+geom_line()

expand_grid(matmax=25, 
            tmax_u_3mo=25, 
            mape=seq(0,2,length.out=100), 
            pe_anom_12mo=0,
            cco2=c(-40,0,40),
            tmax_anom_3mo=c(0)) %>% 
  mutate(pred = predict(n_tmax, newdata=.)) %>% 
  ggplot(data=., aes(mape,pred,color=as_factor(cco2)))+geom_line()


#*******************************************************************************
library(mgcViz)
b <- bam(ndvi_3mo~s(mape,bs='cs',k=3)+
                  s(pe_anom_12mo,bs='cs',k=3)+
                  # te(co2_trend,pe_12mo,bs='cs',k=3)+
                  s(co2_trend,bs='cs',k=3)+
                  # s(pe_anom_12mo,bs='cs',k=3)+
                  # s(I(tmax_anom_12mo/matmax),bs='cs'), 
                 s(tmax_anom_12mo,bs='cs',k=3),
         data=train_dat, 
         select=TRUE, discrete = TRUE)
summary(b)
getViz(b) %>% plot(allTerms=TRUE) %>% print(pages=1)


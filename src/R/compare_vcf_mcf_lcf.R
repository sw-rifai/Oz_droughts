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
kop <- arrow::read_parquet("../data_general/Koppen_climate/BOM_Koppen_simplified7.parquet")

# MODIS Vegetation continuous cover product  ----------------------------------------
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
# end section ******************************************************************


# MODIS cover fraction from CSIRO -----------------------------------------
mcf <- stars::read_ncdf("../data_general/Oz_misc_data/csiro_FC.v310.MCD43A4_0p5_2001_2019.nc")
mcf[,,,1] %>% as_tibble() %>% filter(is.na(soil)==F)
eoz <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_Tree_Cover_5000m_East_Oz_noMask_2000_2019.tif",
                         RasterIO = list(bands=1))
mcf <- stars::st_warp(src=mcf, dest=eoz, use_gdal = F)
mcf <- mcf %>% as.data.table() %>% lazy_dt() %>% 
  rename(date=time) %>%
  mutate(date=as.Date(date)) %>% 
  filter(is.na(npv)==F) %>% 
  as.data.table()
mcf[,`:=`(year=year(date),month=month(date))] %>% 
  .[,`:=`(season = case_when(month%in%c(3:5)~'MAM',
                             month%in%c(6:8)~'JJA',
                             month%in%c(9:11)~'SON',
                             month%in%c(12,1,2)~'DJF'))]
mcf[,`:=`(season = factor(season, levels = c('SON','DJF','MAM','JJA'),ordered = TRUE))]
# end section ******************************************************************



# Landsat Cover Fraction from TERN Auscover -------------------------------------
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



# aggregate MODIS cover fraction to seasonal ------------------------------
system.time(
mcf_s <- mcf[,`:=`(hydro_year=year(date-months(1)))] %>% 
  .[,.(soil = mean(soil/(soil+gv+npv),na.rm=TRUE), 
       gv = mean(gv/(soil+gv+npv),na.rm=TRUE), 
       npv = mean(npv/(soil+gv+npv),na.rm=TRUE)),
    keyby=.(x,y,season,hydro_year)]
)
# end section ******************************************************************



# Compare Landsat and MODIS seasonal cover fractions ----------------------
lcf[,.(x,y)]
setnames(lcf,'year','hydro_year')
cf <- merge(lcf[,.(x,y,hydro_year,season,month,lcf,soil,gv,npv,vc)], 
      mcf_s, 
      by=c("x","y","season","hydro_year"), 
      suffixes = c("_l","_m"))
cf <- merge(cf, kop, by=c("x","y"))
cf <- cf %>% mutate(date=ymd(paste(hydro_year,month,1)))

lut_kop <- c("Equatorial" = "Equat.",
             "Tropical" = "Trop.", 
             "Subtropical" = "Subtr.", 
             "Grassland" = "Grass.", 
             "Desert" = "Des.",
             "Temperate" = "Temp.",
             "Temperate Tas." = "Tasm.")


p_ts_gv <- cf %>% 
  filter(date <= ymd("2019-08-01")) %>% 
  select(zone,hydro_year,season,gv_l,gv_m) %>% 
  gather(-zone, -hydro_year,-season,key='key',value='value') %>% 
  ggplot(data=.,aes(hydro_year, value,color=key))+
  geom_smooth()+
  scale_color_viridis_d('Sensor', 
            end=0.75, labels=c("gv_l"='Landsat',
                              "gv_m"="MODIS"))+
  scale_x_continuous(expand = c(0,0))+
  labs(x=NULL,y='Green Veg. Fraction')+
  facet_grid(zone~season,scales='free', 
             labeller = labeller(zone=lut_kop))+
  theme_linedraw()+
  theme(legend.position = 'bottom'); 
ggsave(p_ts_gv, filename = 'figures/compare_modis_landsat_cover_fraction_GV_timeseries.png',
       width=18, height=14, units='cm', dpi=350, type='cairo')

p_gv <- cf[sample(.N,100000)] %>% 
  ggplot(data=.,aes(gv_m, gv_l))+
  geom_density_2d_filled(contour_var = 'ndensity')+
  # geom_point(size=0.1,alpha=0.25)+
  # ggpointdensity::geom_pointdensity()+
  geom_smooth(method='lm',se=F,color='navy',lwd=2)+
  geom_smooth(method='lm',se=F,color='blue',lwd=0.5)+
  geom_abline(aes(intercept=0,slope=1),color='#cf0000',lwd=2)+
  geom_abline(aes(intercept=0,slope=1),color='#ff0000',lwd=0.5)+
  scico::scale_fill_scico_d(palette = 'tofino',direction = 1,begin=0.5)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x='MODIS Green Vegetation Fraction', 
       y='Landast Green Vegetation Fraction')+
  theme_linedraw()+
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)); p_gv
p_npv <- cf[sample(.N,100000)] %>% 
  ggplot(data=.,aes(npv_m, npv_l))+
  geom_density_2d_filled(contour_var = 'ndensity')+
  # geom_point(size=0.1,alpha=0.25)+
  # ggpointdensity::geom_pointdensity()+
  geom_smooth(method='lm',se=F,color='navy',lwd=2)+
  geom_smooth(method='lm',se=F,color='blue',lwd=0.5)+
  geom_abline(aes(intercept=0,slope=1),color='#cf0000',lwd=2)+
  geom_abline(aes(intercept=0,slope=1),color='#ff0000',lwd=0.5)+
  scico::scale_fill_scico_d(palette = 'lajolla',direction = -1)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x='MODIS Non Photosynthetic Vegetation Fraction', 
       y='Landast Non Photosynthetic Vegetation Fraction')+
  theme_linedraw()+
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)); p_npv

p_gv|p_npv
ggsave(p_gv|p_npv, 
       filename = 'figures/compare_modis_landsat_cover_fraction_1to1.png',
       width=25, height=14, units='cm', dpi=350, type='cairo')
# end section ******************************************************************





# Plot Landsat Time series ------------------------------------------------
tmp <- merge(lcf[is.na(vc)==F],kop,by=c("x","y"))
tmp <- tmp[is.na(zone)==F]
tmp_norms <- tmp[,.(gv_u = mean(gv,na.rm=TRUE), 
                    npv_u = mean(npv,na.rm=TRUE), 
                    soil_u = mean(soil,na.rm=TRUE)),
                 keyby=c("x","y","season")]
tmp <- merge(tmp,tmp_norms,by=c("x","y","season"))
tmp[,`:=`(gv_anom = gv-gv_u, 
           npv_anom = npv-npv_u, 
           soil_anom = soil-soil_u)]
tmp[,`:=`(gv_pct_anom = (100*(gv/gv_u) - 100), 
          npv_pct_anom = (100*(npv/npv_u) - 100), 
          soil_pct_anom = (100*(soil/soil_u) - 100))]

p_lcf_ts <- tmp %>% 
  filter(date <= ymd("2019-08-01")) %>% 
  select(zone,hydro_year,season,gv_pct_anom,npv_pct_anom,soil_pct_anom) %>% 
  gather(-zone, -hydro_year,-season,key='key',value='value') %>% 
  mutate(key=factor(key, levels = c("npv_pct_anom",
                                    "gv_pct_anom",
                                    "soil_pct_anom"),
                    ordered = T)) %>%
  ggplot(data=.,aes(hydro_year, value,color=key))+
  geom_smooth(method='lm')+
  scico::scale_color_scico_d('Fraction', 
                             palette ='batlow',
                        end=0.75, labels=c("gv_pct_anom"='GV',
                                           "npv_pct_anom"="NPV", 
                                           "soil_pct_anom"="Soil"))+
  # scale_color_viridis_d('Fraction', option='A',
  #                       end=0.75, labels=c("gv_pct_anom"='GV',
  #                                          "npv_pct_anom"="NPV", 
  #                                          "soil_pct_anom"="Soil"))+
  scale_x_continuous(expand = c(0,0))+
  labs(x=NULL,y='% Cover Fraction Anom.')+
  facet_grid(zone~season,scales='free', 
             labeller = labeller(zone=lut_kop))+
  theme_linedraw()+
  theme(legend.position = 'bottom'); 

ggsave(p_lcf_ts, 
       filename = 'figures/landsat_cover_fraction_timeseries.png',
       width=25, height=18, units='cm', dpi=350, type='cairo')

# end section ******************************************************************

# Plot MODIS Time series ------------------------------------------------
tmp <- merge(mcf,kop,by=c("x","y"))
tmp <- tmp[is.na(zone)==F]
tmp_norms <- tmp[,.(gv_u = mean(gv,na.rm=TRUE), 
                    npv_u = mean(npv,na.rm=TRUE), 
                    soil_u = mean(soil,na.rm=TRUE)),
                 keyby=c("x","y","month")]
tmp <- merge(tmp,tmp_norms,by=c("x","y","month"))
tmp[,`:=`(gv_anom = gv-gv_u, 
          npv_anom = npv-npv_u, 
          soil_anom = soil-soil_u)]
tmp[,`:=`(gv_pct_anom = (100*(gv/gv_u) - 100), 
          npv_pct_anom = (100*(npv/npv_u) - 100), 
          soil_pct_anom = (100*(soil/soil_u) - 100))]

p_mcf_ts <- tmp %>% 
  filter(date <= ymd("2019-08-01")) %>% 
  select(zone,hydro_year,season,gv_pct_anom,npv_pct_anom,soil_pct_anom) %>% 
  gather(-zone, -hydro_year,-season,key='key',value='value') %>% 
  mutate(key=factor(key, levels = c("npv_pct_anom",
                                    "gv_pct_anom",
                                    "soil_pct_anom"),
                    ordered = T)) %>%
  ggplot(data=.,aes(hydro_year, value,color=key))+
  geom_smooth(method='lm')+
  scico::scale_color_scico_d('Fraction', 
                             palette ='batlow',
                             end=0.75, labels=c("gv_pct_anom"='GV',
                                                "npv_pct_anom"="NPV", 
                                                "soil_pct_anom"="Soil"))+
  # scale_color_viridis_d('Fraction', option='A',
  #                       end=0.75, labels=c("gv_pct_anom"='GV',
  #                                          "npv_pct_anom"="NPV", 
  #                                          "soil_pct_anom"="Soil"))+
  scale_x_continuous(expand = c(0,0))+
  labs(x=NULL,y='% Cover Fraction Anom.')+
  facet_grid(zone~season,scales='free', 
             labeller = labeller(zone=lut_kop))+
  theme_linedraw()+
  theme(legend.position = 'bottom'); 

ggsave(p_mcf_ts, 
       filename = 'figures/MODIS_cover_fraction_timeseries.png',
       width=25, height=18, units='cm', dpi=350, type='cairo')

p_mcf_gam <- tmp %>% 
  filter(date <= ymd("2019-08-01")) %>% 
  select(zone,hydro_year,season,gv_pct_anom,npv_pct_anom,soil_pct_anom) %>% 
  gather(-zone, -hydro_year,-season,key='key',value='value') %>% 
  mutate(key=factor(key, levels = c("npv_pct_anom",
                                    "gv_pct_anom",
                                    "soil_pct_anom"),
                    ordered = T)) %>%
  ggplot(data=.,aes(hydro_year, value,color=key))+
  geom_smooth()+
  scico::scale_color_scico_d('Fraction', 
                             palette ='batlow',
                             end=0.75, labels=c("gv_pct_anom"='GV',
                                                "npv_pct_anom"="NPV", 
                                                "soil_pct_anom"="Soil"))+
  # scale_color_viridis_d('Fraction', option='A',
  #                       end=0.75, labels=c("gv_pct_anom"='GV',
  #                                          "npv_pct_anom"="NPV", 
  #                                          "soil_pct_anom"="Soil"))+
  scale_x_continuous(expand = c(0,0))+
  labs(x=NULL,y='% Cover Fraction Anom.')+
  facet_grid(zone~season,scales='free', 
             labeller = labeller(zone=lut_kop))+
  theme_linedraw()+
  theme(legend.position = 'bottom'); 

ggsave(p_mcf_gam, 
       filename = 'figures/MODIS_cover_fraction_timeseries_gam.png',
       width=25, height=18, units='cm', dpi=350, type='cairo')

# end section ******************************************************************


# Compare MODIS VCF with Landsat Seasonal Fraction ------------------------
diff(range(rnorm(10)))
tmp_l <- lcf[,.(gv_min=min(gv,na.rm=TRUE),
               gv_max=max(gv,na.rm=TRUE),
               npv_min=min(npv,na.rm=TRUE),
               npv_max=max(npv,na.rm=TRUE),
               soil_min=min(soil,na.rm=TRUE),
               soil_max=max(soil,na.rm=TRUE)),
             keyby=.(x,y,hydro_year)]
setnames(tmp_l, 'hydro_year','year')
tmp_l <- merge(tmp_l, 
      mod[,.(x,y,year,tree_cover,nontree_cover,nonveg_cover,vc)], 
      by=c('x','y','year'))
tmp_l <- merge(tmp_l, kop, by=c("x","y"))

tmp_l %>% as_tibble() %>% 
  sample_n(5e4) %>% 
  ggplot(data=.,aes(100*gv_min,tree_cover))+
  geom_point(alpha=0.1)+
  geom_smooth(method='lm')+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  facet_wrap(~zone,nrow = 2)

tmp_l %>% 
  ggplot(data=.,aes(year,100*gv_max))+
  # geom_point(alpha=0.1)+
  geom_smooth()+
  geom_smooth(data=tmp_l %>% filter(year<2019), 
              aes(year,100*gv_max),color='red')+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  facet_wrap(~zone,nrow = 2,scales = 'free')
# end section ******************************************************************




# Trend of MODIS VCF with and without 2019 -------------------------------------
# without 2019  --------------------------------------------------
library(RcppArmadillo)
system.time(
  lt_tree <- mod[year<2019][,`:=`(year_c = year-2009.5)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=tree_cover, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)
system.time(
  lt_nontree <- mod[year<2019][,`:=`(year_c = year-2009.5)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=nontree_cover, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)
system.time(
  lt_nonveg <- mod[year<2019][,`:=`(year_c = year-2009.5)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                   y=nonveg_cover, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)


vcf_no2019 <- inner_join(lt_nontree %>% rename(grass_u=b0, grass_b=b1) %>% select(-beta), 
                  lt_nonveg %>% rename(nonveg_u=b0, nonveg_b=b1) %>% select(-beta))
vcf_no2019 <- lt_tree %>% lazy_dt() %>% 
  select(-beta) %>% 
  rename(tree_u=b0, 
         tree_b=b1) %>% 
  as.data.table() %>% 
  inner_join(., vcf_no2019)
# end section ******************************************************************

# with 2019  --------------------------------------------------
library(RcppArmadillo)
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

vcf_w2019 <- inner_join(lt_nontree %>% rename(grass_u=b0, grass_b=b1) %>% select(-beta), 
                         lt_nonveg %>% rename(nonveg_u=b0, nonveg_b=b1) %>% select(-beta))
vcf_w2019 <- lt_tree %>% lazy_dt() %>% 
  select(-beta) %>% 
  rename(tree_u=b0, 
         tree_b=b1) %>% 
  as.data.table() %>% 
  inner_join(., vcf_w2019)
# end section ******************************************************************




# Plot comparison of MODIS VCF w/wo 2019 ----------------------------------
vcf_j <- bind_rows(vcf_w2019 %>% 
            mutate(y2019='yes'),
          vcf_no2019 %>% 
            mutate(y2019='no')) %>% as_tibble()



library(ggridges)
p2019 <- vcf_j %>% 
  as_tibble() %>% 
  filter(is.na(tree_u)==F) %>% 
  inner_join(., kop %>% select(x,y,cz), by=c("x","y")) %>% 
  select(cz,y2019, nonveg_b, grass_b, tree_b) %>%
  rename(`Non. Veg.`=nonveg_b, 
         `Non. Tree Veg.`=grass_b, 
         `Tree Veg.`=tree_b) %>% 
  gather(-cz,-y2019, key = 'measure', value='estimate') %>% 
  mutate(measure = as_factor(measure)) %>% 
  filter(is.na(estimate)==F) %>% 
  ggplot(data=., aes(x=estimate,
                     y=cz,
                     color=y2019,
                     fill=cz,
                     after_stat(scaled)))+
  ggridges::stat_density_ridges(
    quantile_lines = TRUE, 
    rel_min_height=0.01, 
    alpha=0.333,
    scale=1, 
    # color='black'
    )+
  scale_fill_viridis_d('Climate',
                       option='B',direction = 1,end=0.9)+
  scale_color_manual("Includes 2019",
                     values=c("yes"='red','no'='navy'))+
  geom_vline(aes(xintercept=0),color='black',lwd=0.75)+
  scale_x_continuous(limits=c(-1,1))+
  scale_y_discrete(expand=c(0,0),
                   limits=rev(c("Equatorial","Tropical", 
                                "Subtropical", "Grassland","Desert", 
                                "Temperate","Temperate Tas.")), 
                   labels=str_wrap(rev(c("Equatorial","Tropical", 
                                         "Subtropical", "Grassland", "Desert", 
                                         "Temperate","Temperate Tasmania")),width = 10), 
                   position = 'left')+
  labs(x=expression(paste(Cover~Trend~('%'~yr**-1))), 
       y=NULL)+
  facet_wrap(~ measure)+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        strip.text = element_text(face='bold'),
        # legend.position = 'none'
        ); p2019
ggsave(p2019, filename = 'figures/compare_modis_vcf_wwo2019.png', 
       width=22,height = 18,units='cm',dpi=350,type='cairo')




tmp %>% 
  filter(date <= ymd("2019-08-01")) %>% 
  # select(zone,hydro_year,season,gv_pct_anom,npv_pct_anom,soil_pct_anom) %>% 
  # gather(-zone, -hydro_year,-season,key='key',value='value') %>% 
  # mutate(key=factor(key, levels = c("npv_pct_anom",
  #                                   "gv_pct_anom",
  #                                   "soil_pct_anom"),
  #                   ordered = T)) %>%
  # filter(key=='soil_pct_anom') %>% 
  ggplot(data=.,aes(hydro_year, soil))+
  geom_smooth(method='lm')+
  scico::scale_color_scico_d('Fraction', 
                             palette ='batlow',
                             end=0.75, labels=c("gv_pct_anom"='GV',
                                                "npv_pct_anom"="NPV", 
                                                "soil_pct_anom"="Soil"))+
  # scale_color_viridis_d('Fraction', option='A',
  #                       end=0.75, labels=c("gv_pct_anom"='GV',
  #                                          "npv_pct_anom"="NPV", 
  #                                          "soil_pct_anom"="Soil"))+
  scale_x_continuous(expand = c(0,0))+
  labs(x=NULL,y='% Cover Fraction Anom.')+
  facet_grid(zone~season,scales='free', 
             labeller = labeller(zone=lut_kop))+
  theme_linedraw()+
  theme(legend.position = 'bottom'); 



###*****************************************************************************
# SCRATCH -----------------------------------------------------------------
###*****************************************************************************

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

ba %>% mutate(hydro_year=year(date+months(1))) %>% 
  group_by(hydro_year,zone) %>% 
  summarize(ba_km2 = sum(ba,na.rm=TRUE)/(1000**2)) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(hydro_year, ba_km2))+
  # geom_line()+
  geom_smooth(method='lm',color='black')+
  scale_x_continuous(expand=c(0,0))+
  labs(y=expression(paste('Total Burn Area ',(km**2))))+
  facet_wrap(~zone,scales='free')+
  theme_linedraw()


ba2 <- stars::read_stars("../data_general/MCD64/MCD64_Oz/MCD64A1_BurnArea_reducedResWsum_2011_2019_.tif", 
                         proxy = F) %>% 
  stars::st_set_dimensions(., 3, values=seq(ymd("2011-01-01"),ymd("2019-12-01"),by='1 month')) %>% 
  as_tibble() %>% 
  purrr::set_names(c('x','y','date','ba'))
ba <- bind_rows(ba1,ba2)

ba$ba %>% summary



ba <- ba %>% inner_join(.,kop,by=c("x","y")) %>% 
  filter(is.na(zone)==F) 
ba %>% 
  group_by(date,zone) %>% summarize(val =sum(ba,na.rm=TRUE)) %>% ungroup() %>% 
  ggplot(data=., aes(date,val,color=zone))+geom_smooth()



# vegetation index record
vi <- arrow::read_parquet("../data_general/MCD43/MCD43_AVHRR_NDVI_hybrid_2020-07-01.parquet", 
                          col_select = c("x","y","date",
                                         "ndvi_c","ndvi_mcd","ndvi_hyb", 
                                         "evi2_hyb","evi2_mcd","sz")) %>% 
  as.data.table()
vi <- frac[vi,on=.(x,y,date)]


library(stars); library(tidyverse); library(data.table); library(lubridate)
library(dtplyr, warn.conflicts = FALSE);
library(RcppArmadillo)

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


# var nvis_mask = nvis.lte(12).add(nvis.eq(22)).add(nvis.eq(24)).gte(1)
mod %>% lazy_dt() %>% 
  filter(veg_class %in% c(1:12,22,24)) %>% 
  group_by(date,vc) %>% 
  summarize(val=tree_cover+nontree_cover+nonveg_cover, 
            nobs=n()) %>% 
  as_tibble() %>%
  pull(val) %>% 
  hist

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

lt_veg <- inner_join(lt_tree %>% rename(tree_u=b0, 
                   delta_treecover=b1) %>% 
  select(-beta), 
 lt_nontree %>% rename(nontree_veg_u=b0, 
                   delta_nontree_veg=b1) %>% 
  select(-beta), by=c("x","y")) %>% 
  inner_join(.,
 lt_nonveg %>% rename(nonveg_u=b0, 
                   delta_nonveg=b1) %>% 
  select(-beta), 
 by=c("x","y")) %>% 
  as_tibble()


# Plotting Imports and Options --------------------------------------------------------------------
library(sf); library(patchwork)
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)

map_theme <- theme(panel.background = element_rect(fill = '#99A3C4'), 
      panel.grid = element_blank(), 
      legend.position = 'bottom', 
      axis.text = element_blank(), 
      axis.ticks = element_blank())

map_theme2 <- theme(panel.background = element_rect(fill = '#99A3C4'),
      panel.grid = element_blank(),
      legend.position = 'bottom',
      axis.text = element_blank(),
      axis.ticks = element_blank())
# ******************************************************************************


# EOZ_tree_grass_change ---------------------------------------------------
p_change <- lt_veg %>% 
  inner_join(., vc) %>% 
  mutate(change = 
           case_when(delta_treecover>=0.1 & delta_nontree_veg >= 0.1 ~ 'tree & grass incr.', 
                     delta_treecover<0.1 & delta_nontree_veg >= 0.1 ~ 'grass incr. & tree decr.',
                     delta_treecover>=0.1 & delta_nontree_veg < 0.1 ~ 'tree incr. & grass decr.', 
                     delta_nonveg > 0.1 ~ 'bare incr.', 
                     (between(delta_treecover,-0.1,0.1)==T) &
                       (between(delta_nontree_veg,-0.1,0.1)==T)~ "min change",
                     is.na(delta_treecover)==TRUE ~ NA_character_)) %>% #pull(change) %>% table
  filter(is.na(change)==F) %>% 
  ggplot(data=., aes(x,y,fill=change))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,159),
           ylim = c(-45,-10), expand = FALSE)+
  # scale_fill_viridis_d()+
  # scale_fill_brewer(type='qual')+
  scale_fill_manual("", 
                    values = c('tree & grass incr.' = 'blue', 
                               'tree incr. & grass decr.' = '#228833', 
                               'grass incr. & tree decr.' = '#F0E442', 
                               'bare incr.' = '#D55E00', 
                               'min change' = '#AAAAAA'))+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  guides(fill=guide_legend(ncol=2))+
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(), 
        # legend.position = c(0.7,0.88),
        legend.position = 'bottom',
        legend.direction = 'vertical',
        axis.text = element_blank(), 
        axis.ticks = element_blank()); p_change
ggsave(filename = "figures/EOZ_tree_grass_change.png", 
       type='cairo',
       width = 10, height = 18, units='cm',dpi=350)


# Delta treecover ---------------------------------------------------------
vec_col3 <- RColorBrewer::brewer.pal(n=7,'BrBG')
p_treecover <- lt_veg %>% 
  inner_join(., vc) %>% 
  filter(is.na(delta_treecover)==F) %>% 
  ggplot(data=., aes(x,y,fill=delta_treecover))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scale_fill_gradient2(expression(paste(Delta~"Tree Cover%"~(yr**-1))), 
                       limits=c(-1,1),oob=scales::squish, 
                       high=vec_col3[7], 
                       mid=vec_col3[4],
                       low=vec_col3[1])+
  theme_linedraw()+
  guides(fill=guide_colorbar(title.position = 'top'))+
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(),
        text = element_text(size=8),
        legend.position = 'bottom', 
        legend.key.width = unit(0.433,'cm'),
        axis.text = element_blank(), 
        axis.ticks = element_blank()); p_treecover

# Delta grasscover --------------------------------------------------------
vec_col3 <- RColorBrewer::brewer.pal(n=7,'BrBG')
p_nontree_veg_cover <- lt_veg %>% 
  inner_join(., vc) %>% 
  filter(is.na(delta_nontree_veg)==F) %>% 
  ggplot(data=., aes(x,y,fill=delta_nontree_veg))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  labs(x=NULL,y=NULL)+
  scale_fill_gradient2(expression(paste(Delta~"Non-tree Veg.%"~(yr**-1))), 
                       limits=c(-1,1),oob=scales::squish, 
                       high=vec_col3[7], 
                       mid=vec_col3[4],
                       low=vec_col3[1])+
  theme_linedraw()+
  guides(fill=guide_colorbar(title.position = 'top'))+
  theme(panel.background = element_rect(fill = '#99A3C4'), 
        panel.grid = element_blank(),
        text = element_text(size=8),
        legend.position = 'bottom',
        legend.title = element_text(),
        legend.key.width = unit(0.433,'cm'),
        axis.text = element_blank(), 
        axis.ticks = element_blank()); p_nontree_veg_cover
p_join <- p_treecover+p_nontree_veg_cover+p_change+plot_layout(ncol=3)
p_join
ggsave(p_join, 
       filename = "figures/EOz_map_delta_treecover_nontreeVegcover.png", 
       width=17, height = 15, units='cm', dpi=350, type='cairo')
#*******************************************************************************



# VCF Change by vegetation class ------------------------------------------
mod %>% select(veg_class,vc) %>% distinct() %>% arrange(veg_class)

p_vcf <- mod %>% 
  lazy_dt() %>% 
  filter(veg_class %in% c(1:3,5:9,11,12,13,14)) %>%
  # sample_frac(0.01) %>%
  mutate(year=year(date)) %>% 
  as.data.table() %>% 
  select(year, vc, tree_cover, nontree_cover) %>% 
  rename(Tree = tree_cover, `Non-tree veg.`=nontree_cover) %>% 
  gather(-year,-vc, key = 'vegetation', value='cover') %>% 
  ggplot(data=., aes(year, cover, color=vegetation))+
  # geom_point()+
  geom_smooth(
              method='bam',
              formula = y~s(x),
              method.args=list(select=TRUE,
                               discrete=TRUE))+
  scale_x_continuous(expand=c(0,0))+
  labs(x=NULL,y=expression(paste("Cover (%)")))+
  scale_color_viridis_d("",option='B', direction = -1, end=0.8)+
  facet_wrap(~vc, scales = 'free')+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        strip.text = element_text(face='bold'), 
        legend.position = 'bottom'); p_vcf
ggsave(p_vcf, filename = "figures/TimeSeries_VCF_by_VC.png", 
       width = 26, height = 18, units='cm', dpi = 300, 
       type='cairo')
#****************************************************************************


#****************************************************************************
# PART 2 ---------------
#****************************************************************************

library(stars); library(tidyverse); library(data.table); library(lubridate)
library(dtplyr, warn.conflicts = FALSE);
library(RcppArmadillo)


# Calculate climate zones -------------------------------------------------
dat <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id","season",
                             "precip",  
                             # "precip_anom", 
                             # "precip_anom_3mo","precip_anom_36mo",
                             # "precip_anom_12mo",
                             # "map",
                             "precip_12mo",
                             # "precip_36mo",
                             "tmax",
                             # "tmax_anom","tmax_anom_sd", "matmax",
                             # "tmin","tmin_anom",
                             "vpd15",
                             # "vpd15_anom","vpd15_anom_sd","mavpd15",
                             # "vpd15_u",
                             "pet",
                             # "mapet","pet_anom","pet_anom_3mo","pet_u","pet_sd",
                             # "pet_anom_sd", 
                             "pet_12mo", #"pet_36mo",
                             "pe",
                             "mape",
                             # "ndvi_u",
                             # "ndvi_anom",
                             # "ndvi_anom_12mo",
                             # "ndvi_anom_sd",
                             # "ndvi_mcd",
                             'vc','veg_class',
                             # 'month',
                             "x", "y", "year")) %>% 
  as.data.table() %>% 
  .[is.infinite(mape)==F]
dat <- dat[,`:=`(pe = precip/pet, 
                 pe_12mo = precip_12mo/pet_12mo)]
ldat <- dat %>% lazy_dt()
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




# vegetation index record
vi <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet", 
                          col_select = c("x","y","date",
                                         "ndvi_c","ndvi_m","ndvi_hyb", 
                                         "evi2_hyb","evi2_m")) %>% 
  as.data.table()
svi <- vi %>% lazy_dt() %>% 
  mutate(year=year(date)) %>% 
  group_by(x,y,year) %>% 
  summarize(ndvi_av = mean(ndvi_c, na.rm=TRUE),
            ndvi_av_sd = sd(ndvi_c, na.rm = TRUE),
            ndvi_mod = mean(ndvi_m, na.rm=TRUE),
            ndvi_mod_sd = sd(ndvi_m, na.rm = TRUE),
            ndvi_join = mean(ndvi_hyb, na.rm=TRUE),
            ndvi_join_sd = sd(ndvi_hyb, na.rm = TRUE),
            evi2_mod = mean(evi2_m, na.rm=TRUE),
            evi2_mod_sd = sd(evi2_m, na.rm = TRUE)) %>% 
  ungroup() %>% 
  as.data.table()


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

mod <- svi[mod, on=.(x,y,year)]


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


lt_ndvi <- mod[,`:=`(year_c = year-2009.5)] %>% 
  .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                                 y=ndvi_mod, data=.SD)$coefficients))), 
    by=.(x,y)] %>% 
  .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  

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

o <- inner_join(lt_ndvi %>% rename(ndvi_u=b0, ndvi_b=b1) %>% select(-beta), 
                lt_nontree %>% rename(grass_u=b0, grass_b=b1) %>% select(-beta))
o <- inner_join(o, lt_nonveg %>% rename(nonveg_u=b0, nonveg_b=b1) %>% select(-beta))
o <- lt_tree %>% lazy_dt() %>% 
  select(-beta) %>% 
  rename(tree_u=b0, 
         tree_b=b1) %>% 
  as.data.table() %>% 
  inner_join(., o)

clusters6 <- jj2 %>% select(x,y,cz)

khroma::scale_color_contrast()

library(ggridges)
o[is.na(tree_u)==F] %>% 
  as_tibble() %>% 
  inner_join(., clusters6, by=c("x","y")) %>% 
  select(cz, nonveg_b, grass_b, tree_b) %>%
  rename(`Non. Veg.`=nonveg_b, 
         `Non. Tree Veg.`=grass_b, 
         `Tree Veg.`=tree_b) %>% 
  gather(-cz, key = 'measure', value='estimate') %>% 
  rename(`Clim. Zone` = cz) %>% 
  mutate(measure = as_factor(measure)) %>% 
  filter(is.na(estimate)==F) %>% 
  ggplot(data=., aes(x=estimate,
                     y=measure,
                     fill=measure,
                     after_stat(scaled)))+
  ggridges::stat_density_ridges(alpha=1,
                                quantile_lines = TRUE, 
                                rel_min_height=0.01, 
                                alpha=0.9, 
                                scale=1, 
                                color='black')+
  khroma::scale_fill_contrast(reverse=T)+
  # geom_vline(aes(xintercept=0),color='white',lwd=0.6)+
  geom_vline(aes(xintercept=0),color='red',lwd=0.75)+
  scale_x_continuous(limits=c(-1.5,1.5))+
  scale_y_discrete(expand=c(0,0), position='left')+
  labs(x=expression(paste(Cover~Trend~('%'~yr**-1))), 
       y=NULL)+
  facet_wrap(~`Clim. Zone` , ncol = 6, labeller = label_both)+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        strip.text = element_text(face='bold'),
        legend.position = 'none')
  # geom_density(aes(grass_b), color='yellow')+
  # geom_density(aes(tree_b), color='green')

jj2 %>% ggplot(data=.,aes(x,y,fill=cz))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(option='B')



mod[year==2000][,.(x,y,vc,veg_class)][o,on=.(x,y)] %>% 
  .[veg_class %in% c(1:3,5:9,11,12,13,14)] %>% 
  .[is.na(ndvi_b)==F & is.na(grass_b)==F] %>% 
  # lazy_dt() %>% 
  # filter(is.na(veg_class)==F) %>% 
  # as_tibble() %>% 
  ggplot(data=., aes(ndvi_b, grass_b,color=tree_u))+
  # ggpointdensity::geom_pointdensity()+
  geom_point(alpha=0.05, size=0.5)+
  # geom_contour_filled()+
  scale_color_viridis_c()+
  geom_smooth(se=F, method = 'lm')+
  geom_hline(aes(yintercept=0),color='red')+
  geom_vline(aes(xintercept=0),color='red')+
  facet_wrap(~vc)+
  theme_linedraw()



mod[year==2000] %>% 
  .[veg_class %in% c(1:3,5:9,11,12,13,14)] %>% 
  ggplot(data=., aes(x,y,fill=tree_cover))+
  geom_tile()





# b_tree <- bam(tree_cover ~ s(year,by=vc), 
#               # family=betar(),
#               select=TRUE, method='fREML', discrete = T,
#               data=mod %>% 
#                 mutate( 
#                   year = year(date)))
# 
# gratia::evaluate_smooth(b_tree, "s(year)") %>% 
#   ggplot(data=., aes(year, est,color=vc))+
#   geom_line()+
#   scale_color_viridis_d()+
#   facet_wrap(~vc)
# 
# 
# 
# mod %>% 
#   lazy_dt() %>% 
#   filter(veg_class %in% c(1:12,22,24)) %>%
#   group_by(date,vc,veg_class) %>% 
#   summarize( 
#     tree_cover_u = mean(tree_cover,na.rm=TRUE), 
#     nontree_cover_u = mean(nontree_cover,na.rm=TRUE), 
#     nonveg_cover_u = mean(nonveg_cover,na.rm=TRUE),
#     tree_cover_sd = sd(tree_cover,na.rm=TRUE), 
#     nontree_cover_sd = sd(nontree_cover,na.rm=TRUE), 
#     nonveg_cover_sd = sd(nonveg_cover,na.rm=TRUE),
#     nobs=n()) %>% 
#   as_tibble()
# 
# 
# 
# 
# lt_veg %>% 
#   filter(is.na(delta_treecover)==F) %>% 
#   sample_frac(0.2) %>% 
#   ggplot(data=., aes(delta_treecover,delta_nontree_veg))+
#   stat_density2d_filled(contour_var = 'count', 
#                         contour=T, 
#                         na.rm = TRUE)+
#   geom_hline(aes(yintercept=0),col='red')+
#   geom_vline(aes(xintercept=0),col='red')+
#   scale_x_continuous(limits=c(-0.05,0.05),expand=c(0,0))+
#   scale_y_continuous(limits=c(-0.05,0.05),expand=c(0,0))
# 
# 
# lt_veg %>% 
#   filter(is.na(delta_treecover)==F) %>% 
#   sample_frac(0.2) %>% 
#   ggplot(data=., aes(delta_treecover,delta_nontree_veg))+
#   ggpointdensity::geom_pointdensity()+
#   geom_hline(aes(yintercept=0),col='red')+
#   geom_vline(aes(xintercept=0),col='red')+
#   geom_smooth()+
#   scale_color_viridis_c()
# 
# 
# 
# lt_veg %>% 
#   ggplot(data=., aes(x,y,
#                      fill=(tree_u-nontree_veg_u)/(tree_u+nontree_veg_u)))+
#   geom_tile()+
#   coord_equal()+
#   # scale_fill_viridis_c()
#   scale_fill_gradient2(limits=c(-1,1),oob=scales::squish)
# 
# lt_veg %>% 
#   ggplot(data=., aes(x,y,
#                      fill=delta_nontree_veg))+
#   geom_tile()+
#   coord_equal()+
#   # scale_fill_viridis_c()
#   scale_fill_gradient2(limits=c(-1,1),oob=scales::squish)
# 
# lt_veg %>% 
#   filter(is.na(delta_treecover)==F) %>% 
#   sample_frac(0.2) %>% 
#   ggplot(data=., aes(tree_u,delta_treecover))+
#   ggpointdensity::geom_pointdensity()+
#   geom_hline(aes(yintercept=0),col='red')+
#   geom_smooth()+
#   scale_color_viridis_c()
# 
# 
# 
# lt_veg %>% 
#   inner_join(., vc) %>% 
#   filter(veg_class < 10) %>% 
#   mutate(change = 
#            case_when(delta_treecover>0.1 & delta_nontree_veg > 0.1 ~ 'tree & grass increase', 
#                      delta_treecover<0.1 & delta_nontree_veg > 0.1 ~ 'grass increase',
#                      delta_treecover>0.1 & delta_nontree_veg < 0.1 ~ 'tree increase', 
#                      delta_nonveg > 0.1 ~ 'bare increase')) %>% 
#   group_by(vc) %>% 
#   summarize(both = sum(change=='tree & grass increase',na.rm=TRUE), 
#             grass = sum(change=='grass increase',na.rm=TRUE), 
#             tree = sum(change=='tree increase',na.rm=TRUE), 
#             bare = sum(change=='bare increase',na.rm=TRUE)) %>% 
#   gather(-vc, key='change', value='estimate') %>% 
#   ggplot(data=., aes(change, estimate))+
#   geom_col()+
#   facet_wrap(~vc, scales = 'free')
#   
# 
# 
# library(tricolore)
# tric_veg <- Tricolore(lt_veg,
#                        p1 = 'tree_u', 
#                       p2 = 'nontree_veg_u', 
#                       p3 = 'nonveg_u')
# 
# 
# 
# trend_xy <- bam(tree_cover~s(x,y,by=year), 
#                 data=mod[,`:=`(year=year(date))], 
#                 discrete = TRUE)
# getViz(trend_xy) %>% plot
# 
# # tree - grass
# (0.02 - -0.02)/0.04
# 
# 
# s_mod <- mod %>% 
#   lazy_dt() %>% 
#   filter(veg_class %in% c(1:12,22,24)) %>%
#   group_by(date,vc,veg_class) %>% 
#   summarize( 
#             tree_cover_u = mean(tree_cover,na.rm=TRUE), 
#             nontree_cover_u = mean(nontree_cover,na.rm=TRUE), 
#             nonveg_cover_u = mean(nonveg_cover,na.rm=TRUE),
#             tree_cover_sd = sd(tree_cover,na.rm=TRUE), 
#             nontree_cover_sd = sd(nontree_cover,na.rm=TRUE), 
#             nonveg_cover_sd = sd(nonveg_cover,na.rm=TRUE),
#             nobs=n()) %>% 
#   as_tibble()
# 
# b_tree <- bam(tree_cover_u ~ s(year,by=vc), 
#               # family=betar(),
#               select=TRUE, method='fREML', discrete = T,
#               data=s_mod %>% 
#                 mutate(tree_cover_u = tree_cover_u/100, 
#                        year = year(date)))
# plot(b_tree)
# 
# gratia::evaluate_smooth(b_tree, "s(year)") %>% 
#   ggplot(data=., aes(year, est,color=vc))+
#   geom_line()+
#   scale_color_viridis_d()+
#   facet_wrap(~vc)
# 
# 
# b_nontree <- gam(nontree_cover_u ~ year*vc, 
#               family=betar(),
#               # select=TRUE, method='REML',
#               data=s_mod %>% 
#                 mutate(nontree_cover_u = nontree_cover_u/100, 
#                        year = year(date)))
# summary(b_nontree)
# plot(b_nontree)
# 
# visreg::visreg(b_nontree, xvar='year', by = 'vc', scale='response', 
#                gg=TRUE)+
#   facet_wrap(~vc, nrow = 3,scales = 'free')
# 
# gratia::evaluate_parametric_term(b_nontree,'year') %>%
#   ggplot(data=., aes(year, est,color=vc))+
#   geom_line()+
#   scale_color_viridis_d()+
#   facet_wrap(~vc)+
#   theme(legend.position = 'none')
# 
# 
# gratia::evaluate_smooth(b_nontree, "s(year)") %>%
#   ggplot(data=., aes(year, est,color=vc))+
#   geom_line()+
#   scale_color_viridis_d()+
#   facet_wrap(~vc)+
#   theme(legend.position = 'none')
# 
# rlm <- MASS::rlm
# mod %>% lazy_dt() %>% 
#   filter(veg_class %in% c(1:12,22,24)) %>% 
#   group_by(date,vc) %>% 
#   summarize(val=mean(tree_cover,na.rm=TRUE), 
#             nobs=n()) %>% 
#   as_tibble() %>% 
#   ggplot(data=., aes(date, val))+
#   geom_point(aes(date,val,color=nobs))+
#   geom_smooth(method='rlm')+
#   scale_color_viridis_c()+
#   facet_wrap(~vc, scales="free")
# 

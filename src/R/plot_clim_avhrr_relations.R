library(arrow); library(tidyverse); library(lubridate); 
library(mgcv); library(mgcViz); library(RcppRoll)
library(sf)

#################################################################################################
#--- FUNCTIONS ---------------------------------------------------------------------------------#
#################################################################################################
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

f_cwd_v5 <- function(cwd_et,precip,et){
  # No reset during the wettest month of the year
  for(i in seq(2,length(precip))){
    
    cwd_et[i] <-  min(0, cwd_et[i-1] + (precip[i]) - max(et[i],1, na.rm=T), na.rm=T)
    cwd_et[i] <- ifelse(cwd_et[i] < -3000, -3000, cwd_et[i])
  }
  cwd_et
}
#################################################################################################
#--- END SECTION-- -----------------------------------------------------------------------------#
#################################################################################################


# Load data ---------------------------------------------------------------
d <-  
  read_arrow(stream = "../data_general/Oz_misc_data/ahvrr_clim_2020-02-24.parquet")

aus <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                   layer="gadm36_AUS_1")
# aus <- sf::st_transform_proj(aus, st_crs(4326))
aus <- st_as_sf(aus)


# data prep --------------------------------------------------------------------
d <- d %>% filter(c(lat> -15 & lon > 140)==F)
d <- d %>% 
  filter(is.na(precip)==F & is.na(evap)==F) %>% 
  mutate(cwd5 = NA) %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(cwd5 = f_cwd_v5(cwd_et = cwd5, precip = precip, et = evap)) %>% 
  ungroup()

d <- d %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(mcwd5_12mo = roll_minr(cwd5, n=12, fill=NA), 
         mcwd5_24mo = roll_minr(cwd5, n=24, fill=NA), 
         mcwd5_36mo = roll_minr(cwd5, n=36, fill=NA)) %>% 
  ungroup()

d <- d %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(precip_12mo = roll_sumr(precip, n=12, fill=NA), 
         et_12mo = roll_sumr(evap, n=12, fill=NA), 
         precip_24mo = roll_sumr(precip, n=24, fill=NA), 
         et_24mo = roll_sumr(evap, n=24, fill=NA), 
         precip_36mo = roll_sumr(precip, n=36, fill=NA), 
         et_36mo = roll_sumr(evap, n=36, fill=NA)) %>% 
  ungroup() 


norm_evi <- d %>% 
  filter(date >= ymd('1982-01-01') & date <= ymd("2010-12-31")) %>% 
  group_by(id, month) %>% 
  summarize(evi2_u = mean(evi2, na.rm=T), 
            evi2_sd = sd(evi2, na.rm=T)) %>% 
  ungroup()
d <- inner_join(d, norm_evi, by=c("id","month"))
d <- d %>% mutate(evi2_anom = evi2 - evi2_u) %>% 
  mutate(evi2_anom_sd = evi2_anom/evi2_sd)




# plot _ ------------------------------------------------------------------
d %>% 
  sample_frac(0.1) %>% 
  mutate(year=year(date)) %>% 
  group_by(year,month) %>% 
  summarize(val = mean(evi2_anom, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year, month, fill= val))+
  geom_tile(na.rm = F)+
  theme_linedraw()+
  scale_fill_gradient2(expression(paste(EVI2[anom])), 
                       na.value = 'black')+
  scale_x_continuous(expand=c(0,0), breaks = seq(1980,2020,by = 5))+
  scale_y_continuous(expand=c(0,0), breaks=seq(1,12,by=3))+
  labs(x=NULL, subtitle = "AVHRR EVI2 Monthly Anomaly")
ggsave(filename = "figures/ahvrr_evi2_anom_year_month_tile.png")
  

# plot _ ------------------------------------------------------------------
d %>% 
  sample_frac(0.1) %>% 
  mutate(year=year(date)) %>% 
  mutate(map = round(map, -2)) %>% 
  group_by(year,map) %>% 
  summarize(val = mean(evi2_anom, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year, map, fill= val))+
  geom_tile(na.rm = F)+
  theme_linedraw()+
  scale_fill_gradient2(expression(paste(EVI2[anom])), 
                       na.value = 'black')+
  scale_x_continuous(expand=c(0,0), breaks = seq(1980,2020,by = 5))+
  scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, subtitle = "AVHRR EVI2 Anomaly by Mean Annual Precip", 
       y=expression(paste("Mean Annual Precip"~(mm~yr**-1))))
ggsave(filename = "figures/ahvrr_evi2_anom_year_map_tile.png")

# plot evi2 anom sd by map ------------------------------------------------------------------
d %>% 
  sample_frac(0.1) %>% 
  mutate(year=year(date)) %>% 
  mutate(map = round(map, -2)) %>% 
  group_by(year,map) %>% 
  summarize(val = mean(evi2_anom_sd, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year, map, fill= val))+
  geom_tile(na.rm = F)+
  theme_linedraw()+
  scale_fill_gradient2(expression(paste(EVI2[anom])), 
                       na.value = 'black')+
  scale_x_continuous(expand=c(0,0), breaks = seq(1980,2020,by = 5))+
  scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, subtitle = "AVHRR EVI2 Anomaly SD by Mean Annual Precip", 
       y=expression(paste("Mean Annual Precip"~(mm~yr**-1))))
ggsave(filename = "figures/ahvrr_evi2_anom_sd_year_map_tile.png")


# plot evi2 sd anom by average min lai ------------------------------------
d %>% 
  mutate(lai_avg = round(lai_avg, digits = 1)) %>% 
  mutate(year=year(date)) %>% 
  group_by(year, lai_avg) %>% 
  summarize(val = mean(evi2_anom_sd, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year, lai_avg, fill= val))+
  geom_tile(na.rm = F)+
  theme_linedraw()+
  scale_fill_gradient2(expression(paste(EVI2[anom])), 
                       na.value = 'black', limits=c(-3,3), oob=scales::squish)+
  scale_x_continuous(expand=c(0,0), breaks = seq(1980,2020,by = 5))+
  scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, subtitle = "AVHRR EVI2 Anomaly SD by Mean Annual Min LAI", 
       y=expression(paste("Mean Annual Min LAI")))
ggsave(filename = "figures/ahvrr_evi2_anom_sd_year_LAImin_tile.png")



# plot evi2 sd anom by VPD3pm ------------------------------------
d %>% 
  sample_frac(0.1) %>% 
  mutate(year=year(date)) %>%
  mutate(lai_avg = round(lai_avg, digits = 0)) %>%
  group_by(year, lai_avg) %>% 
  summarize(val = mean(evi2_anom_sd, na.rm=T), 
            vpd3pm_anom = mean(vpd3pm_anom, na.rm=T)) %>%
  ungroup() %>%
  # mutate(vpd3pm_anom = round(vpd3pm_anom, digits = 1)) %>% 
  ggplot(data=., aes(year, vpd3pm_anom, color= val))+
  geom_point()+
  # geom_raster(na.rm = F, interpolate=T)+
  theme_linedraw()+
  scale_color_gradient2(expression(paste(EVI2[anom])), mid = 'grey',
                        high='blue',low='red',
                       na.value = 'black', limits=c(-2,2), oob=scales::squish)+
  scale_x_continuous(# expand=c(0,0), 
                     breaks = seq(1980,2020,by = 5))+
  # scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, 
       y=expression(paste("Mean Annual VPD anomaly")),
       subtitle = "AVHRR EVI2 Anomaly SD by VPD anom and LAI")+
  facet_wrap(~lai_avg, labeller = "label_both", ncol = 1)
ggsave(filename = "figures/ahvrr_evi2_anom_sd_VPD3PManom_LAImin.png", 
       height=8, width = 6)



# plot evi2 sd anom by precip anomaly ------------------------------------
d %>% 
  sample_frac(0.1) %>% 
  mutate(year=year(date)) %>%
  mutate(lai_avg = round(lai_avg, digits = 0)) %>%
  group_by(year, lai_avg) %>% 
  summarize(val = mean(evi2_anom_sd, na.rm=T), 
            precip_anom_sd = mean(precip_anom_sd, na.rm=T)) %>%
  ungroup() %>%
  # mutate(vpd3pm_anom = round(vpd3pm_anom, digits = 1)) %>% 
  ggplot(data=., aes(year, precip_anom_sd, color= val))+
  geom_point()+
  # geom_raster(na.rm = F, interpolate=T)+
  theme_linedraw()+
  scale_color_gradient2(expression(paste(EVI2[anom])), mid = 'grey',
                        high='blue',low='red',
                        na.value = 'black', limits=c(-2,2), oob=scales::squish)+
  scale_x_continuous(# expand=c(0,0), 
    breaks = seq(1980,2020,by = 5))+
  # scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, 
       y=expression(paste("Mean Annual Precip anomaly SD")),
       subtitle = "AVHRR EVI2 Anomaly SD by Precip Anom_SD and LAI")+
  facet_wrap(~lai_avg, labeller = "label_both", ncol = 1)
ggsave(filename = "figures/ahvrr_evi2_anom_sd_precipAnomSd_LAImin.png", 
       height=8, width = 6)



# plot evi2 sd anom by MCWD ------------------------------------
d %>% 
  sample_frac(0.1) %>% 
  mutate(year=year(date)) %>%
  mutate(lai_avg = round(lai_avg, digits = 0)) %>%
  group_by(year, lai_avg) %>% 
  summarize(val = mean(evi2_anom_sd, na.rm=T), 
            mcwd = mean(mcwd5_12mo, na.rm=T)) %>%
  ungroup() %>%
  # mutate(vpd3pm_anom = round(vpd3pm_anom, digits = 1)) %>% 
  ggplot(data=., aes(year, mcwd, color= val))+
  geom_point()+
  # geom_raster(na.rm = F, interpolate=T)+
  theme_linedraw()+
  scale_color_gradient2(expression(paste(EVI2[anom])), mid = 'grey',
                        high='blue',low='red',
                        na.value = 'black', limits=c(-2,2), oob=scales::squish)+
  scale_x_continuous(# expand=c(0,0), 
    breaks = seq(1980,2020,by = 5))+
  # scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, 
       y=expression(paste("MCWD (mm)")),
       subtitle = "AVHRR EVI2 Anomaly SD by MCWD and LAI")+
  facet_wrap(~lai_avg, labeller = "label_both", ncol = 1)
ggsave(filename = "figures/ahvrr_evi2_anom_sd_mcwd_LAImin.png", 
       height=8, width = 6)



# Date of min EVI2 --------------------------------------------------------
tmp <- d %>% 
  group_by(lon,lat) %>% 
  filter(evi2 == min(evi2)) %>% 
  ungroup()
p_l <- tmp %>% 
  ggplot(data=., aes(lon,lat,fill=evi2_anom_sd))+
  geom_sf(data=aus, inherit.aes = F)+
  geom_tile()+
  coord_sf(xlim = c(140,154), 
           ylim = c(-10,-45), expand = FALSE)+
  scale_x_continuous(breaks=c(140,145,150))+
  scale_fill_viridis_c(expression(paste(EVI[anom[sigma]])), 
                       limits=c(-5,0), oob=scales::squish,
                       option='B', direction = -1, na.value = "black")+
  labs(x=NULL, y=NULL, title = "Min. EVI2 Anom.")+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='#bdaf97'), 
        axis.text = element_text(size=8)); 
p_l
p_r <- tmp %>% 
  ggplot(data=., aes(lon,lat,fill=decimal_date(date)))+
  geom_sf(data=aus, inherit.aes = F)+
  geom_tile()+
  coord_sf(xlim = c(140,154), 
           ylim = c(-10,-45), expand = FALSE,ndiscr = 3)+
  scale_x_continuous(breaks=c(140,145,150))+
  scale_fill_viridis_c(NULL, 
                       limits=c(1982,2020), oob=scales::squish,
                       option='B', direction = 1, 
                       end=0.9)+
  labs(x=NULL, y=NULL, title = "Date of Min. EVI2")+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='#bdaf97'), 
        axis.text = element_text(size=8))
library(patchwork)
ggsave(p_l+p_r, filename = "figures/Map_EastOz_MinEVI_DateOfMin.png", 
       height = 12, width = 15, units='cm', dpi = "retina")


# Date of most negative EVI SD anomaly ------------------------------------
avhrr <- read_parquet(file="../data_general/AVHRR_EVI2_CDR_V5/EOz_AVHRR_EVI2_LAI_1981_2019.parquet")
coords <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv") %>% 
  rename(lon=longitude, lat=latitude)
aus <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                   layer="gadm36_AUS_1")
# aus <- sf::st_transform_proj(aus, st_crs(4326))
aus <- st_as_sf(aus)

min_avhrr <- avhrr %>% 
  group_by(lon,lat) %>% 
  filter(evi2_anom_sd == min(evi2_anom_sd)) %>% 
  ungroup()

min_avhrr %>% 
  mutate(dec_date = decimal_date(date)) %>% 
  ggplot(data=., aes(lon,lat, color=dec_date))+
  geom_sf(data=aus, inherit.aes = F)+
  geom_point(size=0.1)+
  scale_color_viridis_c("Year", option='B')+
  # scale_color_gradient2(limits=c(-5,5), oob=scales::squish)+
  coord_sf(xlim = c(140,154), 
           ylim = c(-15,-45), expand = FALSE,ndiscr = 3)+
  theme(panel.background = element_rect(fill='#bdaf97'), 
        axis.text = element_text(size=8))


avhrr %>% 
  ggplot(data=., aes(lon,lat, color=evi2_anom_sd))+
  geom_sf(data=aus, inherit.aes = F)+
  geom_point(size=0.1)+
  scale_color_gradient2(limits=c(-5,5), oob=scales::squish)+
  coord_sf(xlim = c(140,154), 
           ylim = c(-15,-45), expand = FALSE,ndiscr = 3)+
  theme(panel.background = element_rect(fill='#bdaf97'), 
        axis.text = element_text(size=8))



avhrr %>%
  filter(id %in% sample(unique(avhrr$id), 10)) %>% 
  mutate(hydro_year = year(date-months(6))) %>% 
  group_by(id, hydro_year) %>% 
  summarize(val = mean(evi2_anom_sd, na.rm=T), 
            nobs= n()) %>% 
  ungroup() %>% 
  filter(nobs>=10) %>% 
  ggplot(data=., aes(hydro_year, val,col=as.factor(id)))+
  geom_hline(aes(yintercept=0),col='black')+
  geom_line()+
  theme_linedraw()




# EVI2sd by month ---------------------------------------------------------
p <- d %>% 
  filter(hydro_year==2000) %>% 
  ggplot(data=., aes(lon,lat, color=evi2_sd))+
  geom_sf(data=aus, inherit.aes = F)+
  geom_point(size=0.05)+
  scale_color_viridis_c("SD", option='B')+
  # scale_color_gradient2(limits=c(-5,5), oob=scales::squish)+
  coord_sf(xlim = c(140,154), 
           ylim = c(-15,-45), expand = FALSE,ndiscr = 3)+
  scale_x_continuous(breaks=c(140,150))+
  labs(x=NULL, y=NULL, title = expression(paste(SD~of~EVI2)))+
  theme(panel.background = element_rect(fill='#bdaf97'), 
        axis.text = element_text(size=8))+
  facet_wrap(~month, nrow = 2, labeller = "label_both")
ggsave(p, filename = "figures/Map_EastOz_SD_of_EVI_byMonth.png", 
       height = 12, width = 15, units='cm', dpi = "retina")


# LAI amplitude ---------------------------------------------------------
p2 <- d %>% 
  filter(date==min(date)) %>% 
  ggplot(data=., aes(lon,lat, color=norm_lai_amp))+
  geom_sf(data=aus, inherit.aes = F)+
  geom_point(size=0.05)+
  scale_color_viridis_c("Amplitude", option='B')+
  # scale_color_gradient2(limits=c(-5,5), oob=scales::squish)+
  coord_sf(xlim = c(140,154), 
           ylim = c(-15,-45), expand = FALSE,ndiscr = 3)+
  scale_x_continuous(breaks=c(140,150))+
  labs(x=NULL, y=NULL, title = expression(paste(Amplitude~of~LAI)))+
  theme(panel.background = element_rect(fill='#bdaf97'), 
        axis.text = element_text(size=8))
ggsave(p2, filename = "figures/Map_EastOz_Amplitude_of_LAI.png", 
       height = 12, width = 15, units='cm', dpi = "retina")



# Plot min evi2 from 2019 AVHRR -------------------------------------------
avhrr <- read_parquet(file="../data_general/AVHRR_EVI2_CDR_V5/EOz_AVHRR_EVI2_LAI_1981_2019.parquet")
p2 <- avhrr %>% 
  mutate(year=year(date)) %>% 
  filter(year==2019) %>% 
  group_by(lon,lat) %>% 
  filter(evi2_anom_sd == min(evi2_anom_sd, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(lon,lat, color=evi2_anom_sd))+
  geom_sf(data=aus, inherit.aes = F)+
  geom_point(size=0.05)+
  scale_color_viridis_c("Min. EVI2", option='B', direction = -1,
                        limits=c(-5,0), oob=scales::squish)+
  # scale_color_gradient2(limits=c(-5,5), oob=scales::squish)+
  coord_sf(xlim = c(140,154), 
           ylim = c(-15,-45), expand = FALSE,ndiscr = 3)+
  scale_x_continuous(breaks=c(140,150))+
  labs(x=NULL, y=NULL, title = expression(paste(Min~EVI~SD~of~2019)))+
  theme(panel.background = element_rect(fill='#bdaf97'), 
        axis.text = element_text(size=8))
ggsave(p2, filename = "figures/Map_EastOz_Min2019EVI2anomSD.png", 
       height = 12, width = 15, units='cm', dpi = "retina")




avhrr %>% 
  mutate(year=year(date)) %>% 
  filter(year==2019) %>%
  filter(date<=ymd('2019-09-30')) %>% 
  group_by(lon,lat) %>% 
  filter(evi2_anom == min(evi2_anom, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(lon,lat, color=evi2_anom))+
  geom_sf(data=aus, inherit.aes = F)+
  geom_point(size=0.05)+
  scale_color_viridis_c("", option='B', direction = -1,
                        limits=c(-0.35,0),
                        oob=scales::squish)+
  # scale_color_gradient2(limits=c(-0.5,0.5), oob=scales::squish)+
  coord_sf(xlim = c(140,154), 
           ylim = c(-15,-45), expand = FALSE,ndiscr = 3)+
  scale_x_continuous(breaks=c(140,150))+
  labs(x=NULL, y=NULL, title = expression(paste(EVI~Anom.~2019)))+
  theme(panel.background = element_rect(fill='#bdaf97'), 
        axis.text = element_text(size=8))
ggsave(filename = "figures/Map_EastOz_Min2019_preFire_EVI2anom.png", 
       height = 12, width = 15, units='cm', dpi = "retina")



avhrr <- avhrr %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(pre_min_evi2_12mo = roll_minr(evi2, n=12, fill=NA), 
         pre_min_evi2_24mo = roll_minr(evi2, n=24, fill=NA), 
         pre_min_evi2_36mo = roll_minr(evi2, n=36, fill=NA), 
         pre_max_evi2_12mo = roll_maxr(evi2, n=12, fill=NA), 
         pre_max_evi2_24mo = roll_maxr(evi2, n=24, fill=NA), 
         pre_max_evi2_36mo = roll_maxr(evi2, n=36, fill=NA)) %>% 
  ungroup()

d %>% 
  filter(id %in% 3333) %>% 
  mutate(year=year(date)) %>% 
  filter(year %in% c(2009,2010,2011,2012)) %>% 
  mutate(month=month(date)) %>% 
  ggplot(data=., aes(precip_deriv, evi2,color=decimal_date(date)))+
  geom_point()+
  geom_path()+
  scale_color_viridis_c()+
  theme_linedraw()

d %>% 
  # filter(id %in% 3333) %>% 
  mutate(year=year(date)) %>% 
  filter(year %in% c(2009,2010,2011,2012)) %>% 
  # mutate(month=month(date)) %>% 
  group_by(date) %>% 
  summarize(precip_deriv = mean(precip_deriv), 
            evi2=mean(evi2)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(precip_deriv, evi2,color=decimal_date(date)))+
  geom_point()+
  geom_path()+
  scale_color_viridis_c()+
  theme_linedraw()




# K-means East Oz Climate -------------------------------------------------
# cluster climate with kmeans
d <- read_parquet(file =  "../data_general/clim_grid/awap/parquet/awap_EOZ_wDroughtMets_2020-02-27.parquet")
avhrr <- read_parquet(file="../data_general/AVHRR_EVI2_CDR_V5/EOz_AVHRR_EVI2_LAI_1981_2019.parquet")
d <- inner_join(d, 
                avhrr %>% select(-id, -month), 
                by=c("lon","lat","date"))
tree_cover <- read_csv(file="../data_general/Oz_misc_data/CGLS_EOz_tree_cover.csv", guess_max = 10000) %>% 
  rename(tree_cover=mean) %>% 
  select(id,tree_cover)
d <- inner_join(d, tree_cover, by='id')
rm(avhrr); gc();
d <- d %>% filter(lon>=140)
sort(names(d))

tmp_d <- d %>% 
  filter(date==ymd("2000-01-01")) %>%
  select(lon,lat,map,matmax,matmin) %>% 
  filter(is.na(map)==F) %>% 
  distinct()
seas_d <- d %>% 
  filter(date >= ymd('1982-01-01') & 
           date<=ymd('2010-12-31')) %>% 
  filter(month %in% c(12,1,2)) %>% 
  group_by(lon,lat) %>% 
  summarize(summer_vpd3pm = mean(vpd3pm), 
            summer_precip = mean(precip)) %>% 
  ungroup()
tmp_d <- inner_join(tmp_d, seas_d, by=c('lon','lat'))  

scale_this <- function(x) as.vector(scale(x))

tmp_d %>% select(-lon,-lat) %>% apply(., 2, scale_this) %>% dim

tmp_k3 <- tmp_d %>% select(-lon,-lat) %>% apply(., 2, scale_this) %>% kmeans(., centers = 3, nstart = 10)
tmp_k4 <- tmp_d %>% select(-lon,-lat) %>% apply(., 2, scale_this)%>% kmeans(., centers = 4, nstart = 10)
tmp_k5 <- tmp_d %>% select(-lon,-lat) %>% apply(., 2, scale_this) %>% kmeans(., centers = 5, nstart = 10)
tmp_k6 <- tmp_d %>% select(-lon,-lat) %>% apply(., 2, scale_this) %>% kmeans(., centers = 6, nstart = 10)
tmp_k7 <- tmp_d %>% select(-lon,-lat) %>% apply(., 2, scale_this) %>% kmeans(., centers = 7, nstart = 10)

tmp_d %>% 
  mutate(k3=tmp_k3$cluster, 
         k4=tmp_k4$cluster, 
         k5 = tmp_k5$cluster, 
         k6=tmp_k6$cluster, 
         k7=tmp_k7$cluster) %>% 
  ggplot(data=., aes(lon,lat,fill=as.factor(k5)))+
  geom_tile()+
  coord_equal()+
  scale_fill_brewer(type = 'qual')+
  theme_dark()

rnorm(100, mean=100, sd=30) %>% round(digits = -1)
tmp_d %>% select(lon,lat) %>% mutate(k5=tmp_k5$cluster)

d %>% 
  mutate(hydro_year=year(date+months(9))) %>% 
  filter(hydro_year %in% c(2014:2019)) %>% 
  inner_join(., 
             tmp_d %>% select(lon,lat) %>% mutate(k5=tmp_k5$cluster), 
             by=c('lon','lat')) %>% 
  # filter(k5==5) %>% 
  group_by(date,k5,hydro_year) %>% 
  summarize(precip_deriv = mean(precip_deriv), 
            evi2=mean(evi2)) %>% 
  ungroup() %>% 
  inner_join(.,  
             {.} %>% group_by(k5,hydro_year) %>% 
                 summarize(evi2_u=mean(evi2)) %>% ungroup(), 
             by=c('k5','hydro_year')) %>% 
  # arrange(date) %>% 
  mutate(order = month(date+months(9))) %>%
  ggplot(data=., aes(precip_deriv, evi2,color=order))+
  geom_hline(aes(yintercept=evi2_u),col='red')+
  geom_point()+
  geom_path()+
  # geom_label(mapping = aes(label=order))+
  scale_color_viridis_c(end = 0.9)+
  theme_linedraw()+
  facet_grid(rows=vars(k5),cols = vars(hydro_year), scales='free')
ggsave(filename = "figures/evi2_cycle_hydroYears_k5_2014_2019.png", 
       width = 25, height=15, units='cm', dpi = 'retina')




# # data split --------------------------------------------------------------
# d_train <- d %>% 
#   sample_n(50000)
# d_test <- d %>%
#   sample_n(100000) %>% 
#   anti_join(., d_train, by=c("lon","lat","date")) %>% 
#   sample_n(50000)
# 
# 
# # Visualize Multicollinearity ---------------------------------------------
# d_train %>% 
#   select(evi2, map, matmax, matmin) %>% 
#   drop_na() %>% 
#   cor %>% 
#   corrplot::corrplot(., method='number')
# 
# 
# 
# # Base evi2 mod on constants ---------------------------------------------------
# m_base <- bam(evi2 ~ s(lon,lat)+s(map)+s(matmax), 
#               data=d_train, 
#               method='fREML', 
#               discrete=T, 
#               select=T ,
#               family=gaussian(link='identity'))
# summary(m_base)
# 
# d_test %>% 
#   mutate(evi2_pred = predict(m_base, newdata=., type='response')) %>% 
#   filter(is.na(evi2_pred)==F) %>% 
#   summarize(r2 = cor(evi2, evi2_pred)**2, 
#             rmse = sqrt(mean(evi2-evi2_pred)**2)) %>% 
#   ungroup()
# 
# getViz(m_base) %>% plot
# 
# # Mod on constants + seasons -------------------------------------------------
# m_1 <- bam(evi2 ~ te(lat,lon,month)+s(map)+s(matmax), 
#            data=d_train, 
#            method='fREML', 
#            discrete=T, 
#            select=T ,
#            family=gaussian(link='identity'))
# summary(m_1)
# 
# d_test %>% 
#   mutate(evi2_pred = predict(m_1, newdata=., type='response')) %>% 
#   filter(is.na(evi2_pred)==F) %>% 
#   summarize(r2 = cor(evi2, evi2_pred)**2, 
#             rmse = sqrt(mean(evi2-evi2_pred)**2)) %>% 
#   ungroup()
# 
# m_1 %>% plot
# 
# 
# # Mod on constants + seasons + met anom -------------------------------------------------
# m_2 <- bam(evi2 ~ te(lat,lon,month)+s(map)+s(matmax)+
#              s(precip_anom)+
#              s(vpd3pm_anom), 
#            data=d_train, 
#            method='fREML', 
#            discrete=T, 
#            select=T ,
#            family=gaussian(link='identity'))
# summary(m_2)
# 
# d_test %>% 
#   mutate(evi2_pred = predict(m_2, newdata=., type='response')) %>% 
#   filter(is.na(evi2_pred)==F) %>% 
#   summarize(r2 = cor(evi2, evi2_pred)**2, 
#             rmse = sqrt(mean(evi2-evi2_pred)**2)) %>% 
#   ungroup()
# 
# m_2 %>% plot
# 
# 
# 
# #****************************************************************************
# # EVI2 ANOMALY MODELS --------------------------------------------------
# #****************************************************************************
# # Base evi2 mod on constants ---------------------------------------------------
# # ma: model anomaly 
# ma_base <- bam(evi2_anom ~ s(lon,lat)+s(map)+s(matmax), 
#                data=d_train, 
#                method='fREML', 
#                discrete=T, 
#                select=T ,
#                family=gaussian(link='identity'))
# summary(ma_base)
# 
# d_test %>% 
#   mutate(evi2_anom_pred = predict(ma_base, newdata=., type='response')) %>% 
#   filter(is.na(evi2_anom_pred)==F) %>% 
#   # filter(is.na(evi2_anom_sd)==F) %>%
#   # select(evi2_anom_sd, evi2_anom_sd_pred) %>% cor
#   # pull(evi2_anom_sd_pred) %>% is.na %>% table
#   summarize(r2 = cor(evi2_anom, evi2_anom_pred)**2, 
#             rmse = sqrt(mean(evi2_anom - evi2_anom_pred)**2)
#   ) %>% 
#   ungroup()
# 
# getViz(ma_base) %>% plot
# 
# 
# # Base evi2 mod on constants + met ---------------------------------------------------
# # ma: model anomaly 
# ma_2 <- bam(evi2_anom ~ s(lon,lat)+s(map)+s(matmax)+
#               s(vpd3pm_anom)+
#               s(mcwd5_12mo), 
#             data=d_train, 
#             method='fREML', 
#             discrete=T, 
#             select=T ,
#             family=gaussian(link='identity'))
# summary(ma_2)
# 
# d_test %>% 
#   mutate(evi2_anom_pred = predict(ma_2, newdata=., type='response')) %>% 
#   filter(is.na(evi2_anom_pred)==F) %>% 
#   # filter(is.na(evi2_anom_sd)==F) %>%
#   # select(evi2_anom_sd, evi2_anom_sd_pred) %>% cor
#   # pull(evi2_anom_sd_pred) %>% is.na %>% table
#   summarize(r2 = cor(evi2_anom, evi2_anom_pred)**2, 
#             rmse = sqrt(mean(evi2_anom - evi2_anom_pred)**2)
#   ) %>% 
#   ungroup()
# 
# getViz(ma_2) %>% plot
# 
# 
# 
# 
# 
# 
# #****************************************************************************
# # EVI2 ANOMALY SD MODELS --------------------------------------------------
# #****************************************************************************
# # Base evi2 mod on constants ---------------------------------------------------
# # mas: model anomaly sd
# mas_base <- bam(evi2_anom_sd ~ s(lon,lat)+s(map)+s(matmax), 
#                 data=d_train, 
#                 method='fREML', 
#                 discrete=T, 
#                 select=T ,
#                 family=gaussian(link='identity'))
# summary(mas_base)
# 
# d_test %>% 
#   mutate(evi2_anom_sd_pred = predict(mas_base, newdata=., type='response')) %>% 
#   filter(is.na(evi2_anom_sd_pred)==F) %>% 
#   filter(is.na(evi2_anom_sd)==F) %>%
#   # select(evi2_anom_sd, evi2_anom_sd_pred) %>% cor
#   # pull(evi2_anom_sd_pred) %>% is.na %>% table
#   summarize(r2 = cor(evi2_anom_sd, evi2_anom_sd_pred)**2, 
#             rmse = sqrt(mean(evi2_anom_sd - evi2_anom_sd_pred)**2)
#   ) %>% 
#   ungroup()
# 
# getViz(mas_base) %>% plot
# 
# 
# 
# 
# 
# 
# # Mod on constants + seasons + met anom -------------------------------------------------
# mas_2 <- bam(evi2_anom_sd ~ te(lat,lon,month)+s(map)+s(matmax)+
#                s(precip_anom)+
#                s(vpd3pm_anom), 
#              data=d_train, 
#              method='fREML', 
#              discrete=T, 
#              select=T ,
#              family=gaussian(link='identity'))
# summary(mas_2)
# 
# d_test %>% 
#   mutate(evi2_anom_sd_pred = predict(mas_2, newdata=., type='response')) %>% 
#   filter(is.na(evi2_anom_sd_pred)==F) %>% 
#   filter(is.na(evi2_anom_sd)==F) %>%
#   # select(evi2_anom_sd, evi2_anom_sd_pred) %>% cor
#   # pull(evi2_anom_sd_pred) %>% is.na %>% table
#   summarize(r2 = cor(evi2_anom_sd, evi2_anom_sd_pred)**2, 
#             rmse = sqrt(mean(evi2_anom_sd - evi2_anom_sd_pred)**2)
#   ) %>% 
#   ungroup()
# 
# getViz(mas_2) %>% plot
# 
# 
# 
# 
# # scratch -----------------------------------------------------------------
# # 
# # d %>% 
# #   filter(c(lat> -15 & lon > 145)==F) %>%
# #   sample_n(10000) %>% 
# #   ggplot(data=., aes(lon,lat,color=evap))+
# #   geom_point(size=0.1)+
# #   coord_equal()+
# #   scale_color_viridis_c()
# 
# d_train %>% 
#   # sample_n(10000) %>% 
#   ggplot(data=., aes(lon,lat, fill=lai_amp))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c()
# 

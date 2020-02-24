library(arrow); library(tidyverse); library(lubridate); 
library(mgcv); library(mgcViz); library(RcppRoll)
library(sf)

#################################################################################################
#--- FUNCTIONS ---------------------------------------------------------------------------------#
#################################################################################################
f.cwd_et_v3 <- function(cwd_et,precip,et,month, wmy){
  for(i in seq(2,length(precip))){
    
    cwd_et[i] <- ifelse((0.9*precip[i]) < (et[i]*2), 
                        min(0, cwd_et[i-1] + (0.9*precip[i]) - max(et[i],40, na.rm=T), na.rm=T), 
                        0)
    cwd_et[i] <- ifelse(month[i]==wmy[i], 0, cwd_et[i])
    cwd_et[i] <- ifelse(cwd_et[i] < -1000, -1000, cwd_et[i])
  }
  cwd_et
}
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
d <- d %>% filter(c(lat> -15 & lon > 145)==F)
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

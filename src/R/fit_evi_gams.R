library(arrow); library(tidyverse); library(lubridate); 
library(mgcv); library(mgcViz); library(RcppRoll)

# Load data ---------------------------------------------------------------
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

coords <- read_csv('data/coords_set_EA_lai_amp0p5_min0p5.csv') %>% 
  select(id, longitude,latitude) %>% 
  rename(lon=longitude, lat=latitude)
d2 <- read_parquet(file="../data_general/clim_grid/awap/AWAP/tmp_2019/awap_2019Jan_2019Aug.parquet")
d2 <- inner_join(d2, coords, by=c("id"))


# data prep --------------------------------------------------------------------
d <- d %>% filter(c(lat> -15 & lon >= 140)==F)
d <- d %>% filter(is.nan(precip_u)==F) %>% 
           filter(is.nan(evap_u)==F)
d <- d %>% mutate(precip_anom_12mo = precip_12mo-map)
d <- d %>% mutate(precip_anom_sd_12mo = precip_anom_12mo/)

# data split --------------------------------------------------------------
d_train <- d %>% 
  filter(date < ymd('2000-01-01')) %>% 
  sample_n(100000)
d_test <- d %>%
  filter(date < ymd('2000-01-01')) %>% 
  sample_n(200000) %>% 
  anti_join(., d_train, by=c("lon","lat","date")) %>% 
  sample_n(100000)

po_train <- d %>% 
  filter(date >= ymd('2000-01-01')) %>% 
  sample_n(50000)
po_test <- d %>%
  filter(date >= ymd('2000-01-01')) %>% 
  sample_n(100000) %>% 
  anti_join(., d_train, by=c("lon","lat","date")) %>% 
  sample_n(50000)

# Visualize Multicollinearity ---------------------------------------------
d_train %>% 
  select(evi2, map, matmax, matmin, tmax, vpd3pm) %>% 
  drop_na() %>% 
  cor %>% 
  corrplot::corrplot(., method='number')



# Base evi2 mod on constants ---------------------------------------------------
m_base <- bam(evi2 ~ s(lon,lat)+s(map)+s(matmax), 
              data=d_train, 
              method='fREML', 
              discrete=T, 
              select=T ,
              family=gaussian(link='identity'))
summary(m_base)

d_test %>% 
  mutate(evi2_pred = predict(m_base, newdata=., type='response')) %>% 
  filter(is.na(evi2_pred)==F) %>% 
  summarize(r2 = cor(evi2, evi2_pred)**2, 
            rmse = sqrt(mean(evi2-evi2_pred)**2)) %>% 
  ungroup()

getViz(m_base) %>% plot

# Mod on constants + seasons -------------------------------------------------
m_1 <- bam(evi2 ~ te(lat,lon,month)+s(map)+s(matmax), 
              data=d_train, 
              method='fREML', 
              discrete=T, 
              select=T ,
              family=gaussian(link='identity'))
summary(m_1)

d_test %>% 
  mutate(evi2_pred = predict(m_1, newdata=., type='response')) %>% 
  filter(is.na(evi2_pred)==F) %>% 
  summarize(r2 = cor(evi2, evi2_pred)**2, 
            rmse = sqrt(mean(evi2-evi2_pred)**2)) %>% 
  ungroup()

m_1 %>% plot


# Mod on constants + seasons + met anom -------------------------------------------------
m_2 <- bam(evi2 ~ te(lat,lon,month)+
             s(map, precip_12mo)+
             s(matmax)+
             s(precip_anom)+
             s(vpd3pm_u,
              vpd3pm_anom), 
           data=d_train, 
           method='fREML', 
           discrete=T, 
           select=T ,
           family=gaussian(link='identity'))
summary(m_2)
getViz(m_2) %>% plot
d_test %>% 
  mutate(evi2_pred = predict(m_2, newdata=., type='response')) %>% 
  filter(is.na(evi2_pred)==F) %>% 
  summarize(r2 = cor(evi2, evi2_pred)**2, 
            rmse = sqrt(mean(evi2-evi2_pred)**2)) %>% 
  ungroup()

m_2 %>% plot

bind_rows(d_test, po_test) %>% 
  mutate(hydro_year = year(date+months(6))) %>% 
  mutate(evi2_pred = predict(m_2, newdata=., type='response')) %>% 
  filter(is.na(evi2_pred)==F) %>% 
  filter(hydro_year >= 1982) %>% 
  group_by(hydro_year) %>% 
  summarize(r2 = cor(evi2, evi2_pred)**2, 
            rmse = sqrt(mean(evi2-evi2_pred)**2)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(hydro_year, r2))+
  geom_point()+
  geom_smooth(span=0.3)


#****************************************************************************
# EVI2 ANOMALY MODELS --------------------------------------------------
#****************************************************************************
# Base evi2 mod on constants ---------------------------------------------------
# ma: model anomaly 
ma_base <- bam(evi2_anom ~ s(lon,lat)+s(map)+s(matmax), 
                data=d_train, 
                method='fREML', 
                discrete=T, 
                select=T ,
                family=gaussian(link='identity'))
summary(ma_base)

d_test %>% 
  mutate(evi2_anom_pred = predict(ma_base, newdata=., type='response')) %>% 
  filter(is.na(evi2_anom_pred)==F) %>% 
  # filter(is.na(evi2_anom_sd)==F) %>%
  # select(evi2_anom_sd, evi2_anom_sd_pred) %>% cor
  # pull(evi2_anom_sd_pred) %>% is.na %>% table
  summarize(r2 = cor(evi2_anom, evi2_anom_pred)**2, 
            rmse = sqrt(mean(evi2_anom - evi2_anom_pred)**2)
  ) %>% 
  ungroup()

getViz(ma_base) %>% plot


# Base evi2 mod on constants + seasons ---------------------------------------------------
# ma: model anomaly 
ma_2 <- bam(evi2_anom ~ s(lon,lat)+s(map)+s(matmax)+
                        s(vpd3pm_u)+
                        s(cwd5_u), 
               data=d_train, 
               method='fREML', 
               discrete=T, 
               select=T ,
               family=gaussian(link='identity'))
summary(ma_2)
plot(ma_2)

d_test %>% 
  mutate(evi2_anom_pred = predict(ma_2, newdata=., type='response')) %>% 
  filter(is.na(evi2_anom_pred)==F) %>% 
  # filter(is.na(evi2_anom_sd)==F) %>%
  # select(evi2_anom_sd, evi2_anom_sd_pred) %>% cor
  # pull(evi2_anom_sd_pred) %>% is.na %>% table
  summarize(r2 = cor(evi2_anom, evi2_anom_pred)**2, 
            rmse = sqrt(mean(evi2_anom - evi2_anom_pred)**2)
  ) %>% 
  ungroup()

getViz(ma_2) %>% plot




# EVI2 anomaly mod 3 ---------------------------------------------------
# ma: model anomaly 
ma_3 <- bam(evi2_anom ~ 
              s(evi2_u)+               # set the expectation
              te(lat,lon,vpd3pm_anom)+ #
              te(map,precip_12mo)
              , 
            data=d_train, 
            method='fREML', 
            discrete=T, 
            select=T ,
            family=gaussian(link='identity'))
summary(ma_3)
plot(ma_3)

d_test %>% 
  mutate(evi2_anom_pred = predict(ma_2, newdata=., type='response')) %>% 
  filter(is.na(evi2_anom_pred)==F) %>% 
  # filter(is.na(evi2_anom_sd)==F) %>%
  # select(evi2_anom_sd, evi2_anom_sd_pred) %>% cor
  # pull(evi2_anom_sd_pred) %>% is.na %>% table
  summarize(r2 = cor(evi2_anom, evi2_anom_pred)**2, 
            rmse = sqrt(mean(evi2_anom - evi2_anom_pred)**2)
  ) %>% 
  ungroup()

getViz(ma_2) %>% plot





#****************************************************************************
# EVI2 ANOMALY SD MODELS --------------------------------------------------
#****************************************************************************
# Base evi2 mod on constants ---------------------------------------------------
# mas: model anomaly sd
mas_base <- bam(evi2_anom_sd ~ s(lon,lat)+s(map)+s(matmax), 
              data=d_train, 
              method='fREML', 
              discrete=T, 
              select=T ,
              family=gaussian(link='identity'))
summary(mas_base)

d_test %>% 
  mutate(evi2_anom_sd_pred = predict(ma_base, newdata=., type='response')) %>% 
  filter(is.na(evi2_anom_sd_pred)==F) %>% 
  filter(is.na(evi2_anom_sd)==F) %>%
  # select(evi2_anom_sd, evi2_anom_sd_pred) %>% cor
  # pull(evi2_anom_sd_pred) %>% is.na %>% table
  summarize(r2 = cor(evi2_anom_sd, evi2_anom_sd_pred)**2, 
            rmse = sqrt(mean(evi2_anom_sd - evi2_anom_sd_pred)**2)
            ) %>% 
  ungroup()

getViz(ma_base) %>% plot


# Base evi2 mod on ... ---------------------------------------------------
# mas: model anomaly sd
mas_2 <- bam(evi2_anom_sd ~ s(lon,lat)+
               s(precip_deriv)+
               s(vpd3pm_anom)+
               s(precip_anom_12mo), 
                data=d_train, 
                method='fREML', 
                discrete=T, 
                select=T ,
                family=gaussian(link='identity'))
summary(mas_2)

d_test %>% 
  bind_rows(., po_test) %>% 
  mutate(evi2_anom_sd_pred = predict(mas_2, newdata=., type='response')) %>% 
  filter(is.na(evi2_anom_sd_pred)==F) %>% 
  filter(is.na(evi2_anom_sd)==F) %>%
  # select(evi2_anom_sd, evi2_anom_sd_pred) %>% cor
  # pull(evi2_anom_sd_pred) %>% is.na %>% table
  group_by(hydro_year) %>% 
  summarize(r2 = cor(evi2_anom_sd, evi2_anom_sd_pred)**2, 
            rmse = sqrt(mean(evi2_anom_sd - evi2_anom_sd_pred)**2)
  ) %>% 
  ungroup() %>% 
  filter(hydro_year>=1981 & hydro_year<=2019) %>% 
  ggplot(data=., aes(hydro_year, rmse))+
  geom_line()+
  geom_point()+
  geom_smooth(method='lm')

getViz(ma_base) %>% plot


# Base evi2 mod on ... ---------------------------------------------------
# mas: model anomaly sd
mas_3 <- bam(evi2_anom_sd ~ 
               te(lon,lat)+          # somewhat important
               te(precip_deriv, vpd3pm_u)+
               s(evi2_u, evi2_sd)+             # important
               tree_cover+
               # s(precip_deriv)+
               te(map, matmax)+ 
               s(precip_anom_12mo),
               # te(precip_deriv,
               #    vpd3pm_anom,
               #    precip_anom_12mo), 
             data=d_train, 
             method='fREML', 
             discrete=T, 
             select=T ,
             family=gaussian(link='identity'))
summary(mas_3)
plot(mas_3)
getViz(mas_3) %>% plot(allTerms=T)
plotSlice(sm(getViz(mas_3),1), 
          fix=list("precip_anom_12mo"=c(-500,0,500)), 
          a.facet=list(nrow=2))+
  l_fitRaster()+
  coord_equal()+
  scale_fill_gradient2()

plot(sm(getViz(mas_3),3))+
  l_fitRaster()+
  scale_fill_gradient2()

plot(sm(getViz(mas_3), 3))+
  l_fitLine()+
  l_ciLine()+
  l_rug()

plotSlice(sm(getViz(mas_3),4), 
          fix=list("precip_anom_12mo"=c(-1000,-500,0,500)))+
  l_fitRaster()+
  scale_fill_gradient2()+
  l_rug()




# model anomaly sd 4 ---------------------------------------------------
mas_4 <- bam(evi2_anom_sd ~ 
               te(lon,lat)+                    # somewhat important
               s(evi2_u, evi2_sd)+             # important
               # s(norm_lai_amp)+
               # te(map, precip_sd) +
               tree_cover +
               s(vpd3pm_u, vpd3pm_anom)+
               # te(vpd3pm, vpd9am)+
               s(precip_deriv, cwd5)+
               s(map, precip_anom_12mo)
             ,
             data=d_train, 
             method='fREML', 
             discrete=T, 
             select=T ,
             family=gaussian(link='identity'))
summary(mas_4)
plot(mas_4)
getViz(mas_4) %>% plot(allTerms=T)
plotSlice(sm(getViz(mas_4),1), 
          fix=list("precip_anom_12mo"=c(-500,0,500)), 
          a.facet=list(nrow=2))+
  l_fitRaster()+
  coord_equal()+
  scale_fill_gradient2()

plot(sm(getViz(mas_4),3))+
  l_fitRaster()+
  scale_fill_gradient2()

plot(sm(getViz(mas_4), 3))+
  l_fitLine()+
  l_ciLine()+
  l_rug()

plotSlice(sm(getViz(mas_4),4), 
          fix=list("precip_anom_12mo"=c(-1000,-500,0,500)))+
  l_fitRaster()+
  scale_fill_gradient2()+
  l_rug()



d_test %>% 
  bind_rows(., po_test) %>% 
  mutate(evi2_anom_sd_pred = predict(mas_4, newdata=., type='response')) %>% 
  filter(is.na(evi2_anom_sd_pred)==F) %>% 
  filter(is.na(evi2_anom_sd)==F) %>%
  mutate(norm_lai_max = round(norm_lai_max, digits = -0.1)) %>% 
  group_by(hydro_year,norm_lai_max) %>% 
  summarize(r2 = cor(evi2_anom_sd, evi2_anom_sd_pred)**2, 
            rmse = sqrt(mean(evi2_anom_sd - evi2_anom_sd_pred)**2), 
            precip_anom = mean(precip_anom_12mo), 
            val_pred = mean(predict(evi2_anom_sd), na.rm=T), 
            val_obs = mean(evi2_anom_sd)
  ) %>% 
  ungroup() %>% 
  filter(hydro_year>=1981 & hydro_year<2019) %>% 
  ggplot(data=., aes(hydro_year, rmse))+
  geom_rect(aes(xmin=hydro_year-0.5,xmax=hydro_year+0.5, 
                ymin=0,ymax=1.25,fill=precip_anom))+
  geom_line()+
  geom_point()+
  # geom_smooth(method='lm')+
  scale_fill_gradient2()+
  theme_linedraw()+
  facet_wrap(~norm_lai_max)


d_test %>% 
  bind_rows(., po_test) %>% 
  mutate(evi2_anom_sd_pred = predict(mas_4, newdata=., type='response')) %>% 
  filter(is.na(evi2_anom_sd_pred)==F) %>% 
  filter(is.na(evi2_anom_sd)==F) %>%
  mutate(tree_cover = round(tree_cover, digits = -1)) %>% 
  group_by(hydro_year,tree_cover) %>% 
  summarize(r2 = cor(evi2_anom_sd, evi2_anom_sd_pred)**2, 
            rmse = sqrt(mean(evi2_anom_sd - evi2_anom_sd_pred)**2), 
            precip_anom = mean(precip_anom_12mo), 
            val_pred = mean(evi2_anom_sd_pred, na.rm=T), 
            val_obs = mean(evi2_anom_sd)
  ) %>% 
  ungroup() %>% 
  filter(hydro_year>=1981 & hydro_year<=2019) %>% 
  ggplot(data=., aes(hydro_year, val_obs))+
  geom_rect(aes(xmin=hydro_year-0.5,xmax=hydro_year+0.5, 
                ymin=-1.25,ymax=1.25,fill=precip_anom))+
  geom_line()+
  geom_line(aes(hydro_year, val_pred), 
            lty=1, col=scales::muted('orange'),lwd=1)+
  # geom_point()+
  # geom_smooth(method='lm')+
  scale_fill_gradient2()+
  theme_linedraw()+
  facet_wrap(~tree_cover)



# Mod on constants + seasons + met anom -------------------------------------------------
mas_base <- bam(evi2_anom_sd ~ te(lat,lon,month)+s(map)+s(matmax)+
             s(cwd5_u)+
             s(vpd3pm_u), 
           data=d_train, 
           method='fREML', 
           discrete=T, 
           select=T ,
           family=gaussian(link='identity'))
summary(mas_2)

d_test %>% 
  mutate(evi2_anom_sd_pred = predict(mas_2, newdata=., type='response')) %>% 
  filter(is.na(evi2_anom_sd_pred)==F) %>% 
  filter(is.na(evi2_anom_sd)==F) %>%
  # select(evi2_anom_sd, evi2_anom_sd_pred) %>% cor
  # pull(evi2_anom_sd_pred) %>% is.na %>% table
  summarize(r2 = cor(evi2_anom_sd, evi2_anom_sd_pred)**2, 
            rmse = sqrt(mean(evi2_anom_sd - evi2_anom_sd_pred)**2)
  ) %>% 
  ungroup()

getViz(mas_2) %>% plot




# scratch -----------------------------------------------------------------
# 
# d %>% 
#   filter(c(lat> -15 & lon > 145)==F) %>%
#   sample_n(10000) %>% 
#   ggplot(data=., aes(lon,lat,color=evap))+
#   geom_point(size=0.1)+
#   coord_equal()+
#   scale_color_viridis_c()

d_train %>% 
  # sample_n(10000) %>% 
  ggplot(data=., aes(lon,lat, fill=lai_amp))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()


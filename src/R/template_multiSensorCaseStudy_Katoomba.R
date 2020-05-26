library(tidyverse); library(lubridate); library(patchwork)
list.files("../data_general/Oz_misc_data/",pattern = 'Katoomba')

#WORKING

# AVHRR -------------------------------------------------------------------
d1 <- read_csv("../data_general/Oz_misc_data/AVHRR_NIRV__mean_Katoomba (3).csv") %>% 
  mutate(sensor='AVHRR') %>% 
  select(-`system:index`,-time,-.geo) %>% 
  mutate(date=date(date)) %>% 
  mutate(year=year(date),month=month(date)) %>% 
  rowwise() %>% 
  mutate(cloud = as.numeric(intToBits(QA)[1+1]), # 1 is yes
         shadow = as.numeric(intToBits(QA)[2+1]), # 1 is yes
         ddv = as.numeric(intToBits(QA)[5+1]), # 1 is yes 
         night = as.numeric(intToBits(QA)[6+1]), # 1 is night
         ch1_5 = as.numeric(intToBits(QA)[7+1]), # 1 is good
         brdf_invalid = as.numeric(intToBits(QA)[14+1]), # 1 is bad
         NDVI = (SREFL_CH2 - SREFL_CH1)/(SREFL_CH2 + SREFL_CH1), 
         time = 0.01*TIMEOFDAY+10) %>% 
  ungroup()
  # mutate(NIRV = NDVI*(SREFL_CH1*0.001))

(SREFL_CH3 - SREFL_CH2)/(SREFL_CH3 + SREFL_CH2)

# as.numeric(bitwAnd(2**1, 16514)) # cloud
# as.numeric(bitwAnd(16514, 2**2) # shadow
# bitwAnd(16514, 2**7) # ch 1 & 5
# bitwAnd(2**14,16514) # brdf
# 
# as.numeric(intToBits(16514)[1+1])

p_avhrr <- d1 %>% 
  filter(cloud == 0) %>% 
  filter(ch1_5 == 1) %>% 
  filter(shadow == 0) %>% 
  # filter(time <= 18) %>% 
  # filter(RELAZ > 0) %>% 
  mutate(date=date(date)) %>% 
  mutate(year=year(date),month=month(date)) %>% 
  inner_join(.,{.} %>%
      filter(between(date,ymd("2001-01-01"),ymd("2015-12-31"))) %>%
      group_by(month) %>%
      summarize(NDVI_u = mean(NDVI,na.rm=T)) %>%
      ungroup(),by='month') %>%
  mutate(NDVI_anom = NDVI-NDVI_u) %>%
  filter(date >= ymd("2000-01-01") & date < ymd("2019-12-17")) %>%
  ggplot(data=., aes(date, NDVI_anom,color=SZEN*0.01))+
  # geom_hline(aes(yintercept=0),color='black')+
  # geom_hline(aes(yintercept=18),color='black')+
  geom_point(alpha=0.33)+
  # geom_line()+
  geom_smooth(formula=y~s(x,bs='gp',k=19),
              method='gam', method.args=list(method='REML'),
              se=F,
              color="#d15e0d")+
  scale_color_viridis_c("solar zen. angle", end=0.9)+
  scale_x_date(expand=c(0.01,0.01))+
  geom_hline(aes(yintercept=0))+
  # scale_y_continuous(limits=c(12,19))+
  labs(x=NULL,y=expression(paste(NDVI~anom.)), 
       title='AVHRR CDR v5 (uncorrected)')+
  theme_linedraw()+
  theme(panel.grid = element_blank()); p_avhrr


d1 %>% 
  filter(cloud == 0) %>% 
  filter(ch1_5 == 1) %>% 
  filter(shadow == 0) %>% 
  ggplot(data=., aes(SZEN*0.01, NDVI))+
  ggpointdensity::geom_pointdensity()+
  geom_smooth()+
  scale_color_viridis_c()
  # facet_wrap(~year)

d1 %>% 
  filter(cloud == 0) %>% 
  filter(ch1_5 == 1) %>% 
  filter(shadow == 0) %>% 
  group_by(year) %>% 
  summarize(sza = median(SZEN*0.01,na.rm=TRUE), 
            ndvi = median(NDVI,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(data=., aes( sza, ndvi))+
  geom_point()+
  geom_smooth(method='lm', se=F, color='black')+
  geom_label(aes(label=year),size=3)+
  theme_linedraw()+
  labs(y="Median NDVI",x='Solar Zenith Angle', 
       subtitle='AVHRR CDR v5 - Katoomba region')
  # geom_line()+geom_point()

d1 %>% 
  filter(cloud == 0) %>% 
  filter(ch1_5 == 1) %>% 
  filter(shadow == 0) %>% 
  group_by(year,month) %>% 
  summarize(sza=mean(SZEN*0.01,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(date = ymd(paste(year,month,1))) %>% 
  ggplot(data=., aes(date, sza))+
  geom_point()+
  geom_smooth(span=0.2)+
  theme_linedraw()+
  labs(y='Solar Zenith Angle', title='AVHRR CDR v5')


  


# MOD13A1 VIs -------------------------------------------------------------
d2 <- read_csv("../data_general/Oz_misc_data/MOD13A1_Katoomba.csv") %>% 
  mutate(NIRV = NDVI*NIR) %>% 
  mutate(sensor='MOD') %>% 
  select(-`system:index`,-time,-.geo)

p_mod13 <- d2 %>% 
  mutate(date=date(date)) %>% 
  mutate(year=year(date),month=month(date)) %>% 
  inner_join(.,{.} %>% 
               filter(between(date,ymd("2001-01-01"),ymd("2015-12-31"))) %>% 
               group_by(month) %>% 
               summarize(NIRV_u = mean(NIRV,na.rm=T), 
                         NDVI_u = mean(NDVI,na.rm=T), 
                         EVI_u = mean(EVI,na.rm=T)) %>% 
               ungroup(),by='month') %>% 
  mutate(NIRV_anom = NIRV-NIRV_u, 
         NDVI_anom = NDVI - NDVI_u, 
         EVI_anom = EVI - EVI_u) %>% 
  filter(date >= ymd("2001-01-01") & date < ymd("2019-12-17")) %>% 
  ggplot(data=., aes(date, NIRV_anom))+
  geom_hline(aes(yintercept=0),color='black')+
  geom_point(alpha=0.33)+
  geom_smooth(formula=y~s(x,bs='gp',k=19), 
              method='gam', method.args=list(method='REML'), 
              se=F,
              color="#d15e0d")+
  scale_x_date(expand=c(0.01,0.01))+
  labs(x=NULL,y=expression(paste(NIR[V]~anom.)),
       title="MODIS TERRA")+
  theme_linedraw()+
  theme(panel.grid = element_blank()); p_mod13

p_avhrr+p_mod13+plot_layout(ncol=1)





# MYD11A1 LST -------------------------------------------------------------
d3 <- read_csv("../data_general/Oz_misc_data/MYD11A1_mean_Katoomba.csv") %>% 
  mutate(sensor='MYD') %>% 
  select(-`system:index`,-time,-.geo)

# Landsat 5 SMA -----------------------------------------------------------
d4 <- read_csv("../data_general/Oz_misc_data/Landsat5_SMA__mean_Katoomba.csv") %>% 
  mutate(sensor='TM') %>% 
  select(-`system:index`,-time,-.geo)


# Landsat 7 SMA -----------------------------------------------------------
d5 <- read_csv("../data_general/Oz_misc_data/Landsat7_SMA__mean_Katoomba.csv") %>% 
  mutate(sensor='TM+') %>% 
  select(-`system:index`,-time,-.geo)


# Landsat 8 SMA -----------------------------------------------------------
d6 <- read_csv("../data_general/Oz_misc_data/Landsat8_SMA__mean_Katoomba.csv") %>% 
  mutate(sensor='OLI') %>% 
  select(-`system:index`,-time,-.geo)


# MCD64 -----------------------------------------------------------
d7 <- read_csv("../data_general/Oz_misc_data/MCD64_sum_Katoomba.csv") %>% 
  mutate(sensor='MCD64') %>% 
  select(-`system:index`,-time,-.geo)


# FIRMS -----------------------------------------------------------
d8 <- read_csv("../data_general/Oz_misc_data/FIRMS_sum_Katoomba.csv") %>% 
  mutate(sensor='FIRMS') %>% 
  select(-`system:index`,-time,-.geo)

# MOD09GQ VIs -------------------------------------------------------------
d9 <- read_csv("../data_general/Oz_misc_data/MOD09GQ_Katoomba (2).csv") %>% 
  mutate(sensor='MOD') %>% 
  select(-`system:index`,-time,-.geo)
  # filter(VI_mean == 0)

p_mod09 <- d9 %>% 
  filter(VI_mean == 0) %>% 
  filter(NDVI_count >= 134) %>% # pull(NIRV_mean)
  # filter(orbit_pnt < 0.25) %>% 
  # filter(obscov > 20) %>% 
  mutate(date=date(date)) %>% 
  mutate(year=year(date),month=month(date)) %>% 
  # inner_join(.,{.} %>% 
  #              filter(between(date,ymd("2001-01-01"),ymd("2015-12-31"))) %>% 
  #              group_by(month) %>% 
  #              summarize(NIRV_u = mean(NIRV,na.rm=T), 
  #                        NDVI_u = mean(NDVI,na.rm=T), 
  #                        # EVI_u = mean(EVI,na.rm=T)
  #                        ) %>% 
  #              ungroup(),by='month') %>% 
  # mutate(NIRV_anom = NIRV-NIRV_u, 
  #        NDVI_anom = NDVI - NDVI_u, 
  #        # EVI_anom = EVI - EVI_u
  #        ) %>% 
  filter(date >= ymd("2001-01-01") & date < ymd("2019-12-17")) %>% 
  ggplot(data=., aes(date, NIRV_mean,color=obscov_mean))+
  geom_hline(aes(yintercept=0),color='black')+
  geom_point(alpha=0.33)+
  scale_color_viridis_c()+
  # geom_smooth(formula=y~s(x,bs='gp',k=19), 
  #             method='gam', method.args=list(method='REML'), 
  #             se=F,
  #             color="#d15e0d")+
  # scale_x_date(expand=c(0.01,0.01))+
  # labs(x=NULL,y=expression(paste(NIR[V]~anom.)))+
  theme_linedraw()+
  theme(panel.grid = element_blank()); p_mod09

# myd13a1 VIs -------------------------------------------------------------
d10 <- read_csv("../data_general/Oz_misc_data/MYD13A1_Katoomba.csv") %>% 
  mutate(sensor='MYD', NIRV = NDVI*NIR) %>% 
  select(-`system:index`,-time,-.geo)
# filter(VI_mean == 0)

p_myd13a1 <- d10 %>% 
  # filter(VI_mean == 0) %>% 
  # filter(NDVI_count >= 134) %>% # pull(NIRV_mean)
  # filter(orbit_pnt < 0.25) %>% 
  # filter(obscov > 20) %>% 
  mutate(date=date(date)) %>% 
  mutate(year=year(date),month=month(date)) %>% 
  inner_join(.,{.} %>%
             filter(between(date,ymd("2001-01-01"),ymd("2015-12-31"))) %>%
             group_by(month) %>%
             summarize(NIRV_u = mean(NIRV,na.rm=T),
                       NDVI_u = mean(NDVI,na.rm=T),
                       # EVI_u = mean(EVI,na.rm=T)
                       ) %>%
             ungroup(),by='month') %>%
  mutate(NIRV_anom = NIRV-NIRV_u,
       NDVI_anom = NDVI - NDVI_u,
       # EVI_anom = EVI - EVI_u
       ) %>%
  filter(date >= ymd("2001-01-01") & date < ymd("2019-12-17")) %>% 
  ggplot(data=., aes(date, NIRV_anom))+
  geom_hline(aes(yintercept=0),color='black')+
  geom_point(alpha=0.33)+
  scale_color_viridis_c()+
  # geom_smooth(formula=y~s(x,bs='gp',k=19), 
  #             method='gam', method.args=list(method='REML'), 
  #             se=F,
  #             color="#d15e0d")+
  # scale_x_date(expand=c(0.01,0.01))+
  # labs(x=NULL,y=expression(paste(NIR[V]~anom.)))+
  theme_linedraw()+
  theme(panel.grid = element_blank()); p_myd13a1


# VNP13A1 VIs -------------------------------------------------------------
d11 <- read_csv("../data_general/Oz_misc_data/VNP13A1_Katoomba.csv") %>% 
  mutate(EVI=EVI/10000, EVI2 = EVI2/10000, NDVI=NDVI/10000, 
         NIR_reflectance = NIR_reflectance/10000, 
         SWIR1_reflectance = SWIR1_reflectance/10000, 
         SWIR2_reflectance = SWIR2_reflectance/10000, 
         SWIR3_reflectance = SWIR3_reflectance/10000
         ) %>% 
  mutate(NIRV = NDVI*NIR_reflectance) %>% 
  mutate(sensor='VIIRS') %>% 
  select(-`system:index`,-time,-.geo)

p_vnp13a1 <- d11 %>% 
  # filter(VI_mean == 0) %>% 
  # filter(NDVI_count >= 134) %>% # pull(NIRV_mean)
  # filter(orbit_pnt < 0.25) %>% 
  # filter(obscov > 20) %>% 
  mutate(date=date(date)) %>% 
  mutate(year=year(date),month=month(date)) %>% 
  inner_join(.,{.} %>%
               filter(between(date,ymd("2001-01-01"),ymd("2017-12-31"))) %>%
               group_by(month) %>%
               summarize(NIRV_u = mean(NIRV,na.rm=T),
                         NDVI_u = mean(NDVI,na.rm=T),
                         # EVI_u = mean(EVI,na.rm=T)
               ) %>%
               ungroup(),by='month') %>%
  mutate(NIRV_anom = NIRV-NIRV_u,
         NDVI_anom = NDVI - NDVI_u,
         # EVI_anom = EVI - EVI_u
  ) %>%
  filter(date >= ymd("2001-01-01") & date < ymd("2019-12-17")) %>% 
  ggplot(data=., aes(date, NIRV_anom))+
  geom_hline(aes(yintercept=0),color='black')+
  geom_point(alpha=0.33)+
  scale_color_viridis_c()+
  geom_smooth(formula=y~s(x,bs='gp',k=7),
              method='gam', method.args=list(method='REML'),
              se=F,
              color="#d15e0d")+
  # scale_x_date(expand=c(0.01,0.01))+
  # labs(x=NULL,y=expression(paste(NIR[V]~anom.)))+
  theme_linedraw()+
  theme(panel.grid = element_blank()); p_vnp13a1


###############################################################################

d1 %>% select(NDVI,NIRV) %>% drop_na() %>% cor

d_ba <- d7 %>% 
  arrange(date) %>% 
  mutate(ba = RcppRoll::roll_sumr(burnAreaM2,n=12,fill=NA)) %>% 
  mutate(ba_ha = ba/(100**2)) %>% 
  filter(ba_ha > 0) %>% 
  select(date, ba_ha) %>% 
  mutate(date=date(date))

bind_rows(d1 %>% select(-NIRV),
          d2 %>% select(-NIR),d3) %>% 
  gather(-region, -date,-sensor, key='measure',value='estimate') %>% 
  mutate(year=year(date), month=month(date)) %>% 
  group_by(region,sensor,measure,year,month) %>% 
  summarize(estimate = mean(estimate,na.rm = T)) %>% 
  ungroup() %>% 
  group_by(region,sensor,measure,month) %>%
  mutate(estimate = scale(estimate)) %>%
  ungroup() %>%
  mutate(measure=paste(measure,sensor)) %>% 
  mutate(date=ymd(paste(year,month,1))) %>% 
  # filter(is.na(estimate)==F) %>% 
  ggplot(data=., aes(date, estimate,color=measure))+
  geom_rect(data=d_ba, inherit.aes = F,
            aes(xmin=date, xmax=date+months(1), 
                ymin=-3, ymax=3, fill=ba_ha))+
  scale_fill_viridis_c(option='B',end=0.9)+
  geom_point(alpha=0.5)+
  geom_line()+
  scale_x_date(limits=c(ymd("2015-01-01","2019-12-31"))) 
  # facet_wrap(~measure,scales='free_y',ncol = 2)
  # geom_smooth(se=F)

bind_rows(d4,d5,d6) %>% 
  select(date) %>% distinct()

bind_rows(d4,d5,d6) %>% 
  gather(-region, -date,-sensor, key='measure',value='estimate') %>% 
  mutate(year=year(date), month=month(date)) %>% 
  group_by(region,sensor,measure,year,month) %>% 
  summarize(estimate = mean(estimate,na.rm = T)) %>% 
  ungroup() %>% 
  # group_by(region,sensor,measure,month) %>% 
  # mutate(estimate = scale(estimate)) %>% 
  # ungroup() %>% 
  mutate(date=ymd(paste(year,month,1))) %>% 
  filter(sensor=='OLI') %>% 
  ggplot(data=., aes(date, estimate,color=sensor))+
  geom_rect(data=d_ba, inherit.aes = F,
            aes(xmin=date, xmax=date+months(1), 
                           ymin=0, ymax=1, fill=ba_ha))+
  scale_fill_viridis_c(option='B')+
  geom_point(alpha=0.5)+
  scale_x_date()+
  facet_wrap(~measure,scales='free_y',ncol = 1)
  
#  















# ! NON WORKING ! --------------------------------------------------------
list.files("../data_general/Oz_misc_data/",pattern = 'Katoomba',full.names = T)[3] %>% 
  read_csv() %>% names






mlo[date==ymd("1982-01-01")]
mlo[date==ymd("2019-10-01")]

#388

(388-340)/(0.5*(388+340))




(mlo[date==ymd("2019-10-01")]$co2_trend-mlo[date==ymd("1980-10-01")]$co2_trend)/
  (0.5*(mlo[date==ymd("2019-10-01")]$co2_trend+mlo[date==ymd("1980-10-01")]$co2_trend))


v_u <- ldat %>% 
  group_by(x,y) %>% 
  summarize(vpd_u = mean(vpd15,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble()

dat[is.na(ndvi_hyb)==F][date<=ymd("2019-09-30")]$hydro_year %>% mean
system.time(
  lt_v <- dat[date>=ymd("1982-01-01")][date<=ymd("2019-09-30")] %>% 
    .[,`:=`(year_c = year-2000.5)] %>% 
    .[,.(beta = list(unname(fastLm(X = cbind(1,year_c), 
                    y=vpd15, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)

system.time(
  lt_v <- dat[date>=ymd("1982-01-01")][date<=ymd("2019-09-30")] %>% 
    .[,`:=`(year_c = year-2000.5)] %>% 
    .[,.(beta = list(unname(coefficients(lm(vpd15~year_c))))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2]), by=.(x,y)]  
)


lt_v

lt_v %>% 
  as_tibble() %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  ggplot(data=.,aes(x,y,fill=b0))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(option='B')

lt_v %>% 
  as_tibble() %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  ggplot(data=.,aes(x,y,fill=b1))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(high='#cF0000',low='navy',limits=c(-0.025,0.025))

lt_v %>% 
  as_tibble() %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  ggplot(data=.,aes(x,y,fill=0.193 - 0.5*(b1*38)/b0))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(option='B',limits=c(0,0.25))


system.time(
  lt_ndvi_wEpoch <- dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
    .[date>= ymd("1982-01-01") & date<= ymd("2019-09-30")] %>% 
    # .[,.(val = mean(ndvi_3mo, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[,`:=`(epoch=ifelse(hydro_year < 2001,0,1))] %>% 
    .[is.na(ndvi_hyb)==F] %>% 
    .[is.na(pe_anom_12mo)==F] %>% 
    .[,.(beta = list(unname(fastLm(
      X = cbind(1,cco2,pe_anom_12mo,epoch), 
      y=ndvi_hyb, data=.SD)$coefficients))), 
      by=.(x,y)] %>% 
    .[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2],b2=unlist(beta)[3],b3=unlist(beta)[4]), by=.(x,y)]
)

lt_ndvi_wEpoch %>% 
  as_tibble() %>% 
  filter(between(b0,0.1,1)) %>% 
  ggplot(data=.,aes(x,y,fill=b1))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(limits=c(-0.004,0.004))


lt_v %>% 
  mutate(pct_vpd = b1/b0) %>% 
  mutate(pct_vpd = mean(pct_vpd,na.rm=TRUE)) %>% 
  pull(pct_vpd) %>% mean

# Percent increase of VPD
dVPD_VPD <- mean(38*lt_v$b1,na.rm=TRUE)/mean(lt_v$b0,na.rm=TRUE)
dNDVI_NDVI <- mean(38*lt_ndvi_wEpoch$b1,na.rm=TRUE)/mean(lt_ndvi_wEpoch$b0,na.rm=TRUE)

dCa_Ca <- 
diff(range(mlo[date>=ymd("1982-01-01") & date <= ymd("2019-09-30")]$co2_trend))/
mean(mlo[date>=ymd("1982-01-01") & date <= ymd("2019-09-30")]$co2_trend)

# Expected WUE related increase (Donohue 2013)
0.5*(dCa_Ca - 0.5*dVPD_VPD)

# Actual percent relative increase in NDVI
dNDVI_NDVI

df_vpd <- lt_v %>% 
  as_tibble() %>% 
  inner_join(.,kop,by=c("x","y")) %>% 
  mutate(zone = recode(zone,'Desert'='Arid')) %>% 
  group_by(zone) %>% 
  summarize(dVPD_VPD = mean(38*b1,na.rm=TRUE)/mean(b0,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(expectation = 0.5*(dCa_Ca - 0.5*dVPD_VPD))

lt_overall <- lt_ndvi_wEpoch %>% 
  as_tibble() %>% 
  filter(between(b0,0.15,1)) %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  mutate(rel_ndvi = (38*b1)/(b0)) %>% 
  filter(between(rel_ndvi, -0.333,0.333)) %>% 
  mutate(zone = 'Overall')
  
lt_ndvi_wEpoch %>% 
  inner_join(., kop,by=c("x","y")) %>% 
  as_tibble() %>% 
  filter(between(b0,0.15,1)) %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  mutate(rel_ndvi = (38*b1)/(b0)) %>% 
  filter(between(rel_ndvi, -0.333,0.333)) %>% 
  mutate(zone = recode(zone,'Desert'='Arid')) %>% 
  bind_rows(., lt_overall) %>% 
  mutate(zone = fct_relevel(zone,'Overall',after=Inf)) %>% 
  ggplot(data=.,aes(zone, rel_ndvi))+
  geom_boxplot()+
  geom_point(aes(zone, expectation, color=zone, group=zone), 
             data=df_vpd, size=3, shape=15)+
  geom_hline(aes(yintercept=0.5*(dCa_Ca - 0.5*dVPD_VPD)),color='red')+
  scale_color_viridis_d(option='B',end=0.9)+
  labs(x=NULL, y="% NDVI Change")+
  theme_linedraw()
  # geom_histogram(bins = 100, na.rm = TRUE)+
  # stat_summary(fun='mean', aes(y=rel_ndvi))
  # geom_vline(aes(xintercept=stat(rel_ndvi),color=zone))+
  # geom_vline(aes(xintercept=0.5*(dCa_Ca - 0.5*dVPD_VPD)),col='blue')+
  # facet_wrap(~zone)



lt_v %>% 
  as_tibble() %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  mutate(dww = 0.193 - 0.5*(b1*38)/b0) %>% 
  inner_join(., 
             lt_ndvi_wEpoch %>% rename(ndvi=b0,dndvi=b1) %>% 
               select(x,y,ndvi,dndvi),
             ,by=c("x","y")) %>% 
  filter(between(dww,0.1,0.25)) %>% pull(dww) %>% summary
  filter(between(ndvi,0.1,1)) %>% 
  filter(between(dndvi,-0.01,0.01)) %>% 
  mutate(val = (dndvi*80)/ndvi) %>% pull(val) %>% summary
  ggplot(data=., aes(dww, val))+
  geom_point()+
  geom_smooth()

lt_v %>% 
  as_tibble() %>% 
  filter(between(b1,-0.1,0.1)) %>% 
  mutate(dww = 0.193 - 0.5*(b1*38)/b0) %>% 
  inner_join(., 
             lt_ndvi_wEpoch %>% rename(ndvi=b0,dndvi=b1) %>% 
               select(x,y,ndvi,dndvi),
             ,by=c("x","y")) %>% 
  inner_join(., kop,by=c("x","y")) %>% 
  filter(between(dww,0.1,0.25)) %>% 
  filter(between(ndvi,0.1,1)) %>% 
  filter(between(dndvi,-0.01,0.01)) %>% 
  mutate(val = (dndvi*80)/ndvi) %>%
  ggplot(data=., aes(dww, val,color=zone))+
  # geom_point()+
  geom_smooth(method='lm')

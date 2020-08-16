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
  
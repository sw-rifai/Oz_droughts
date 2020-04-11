library(tidyverse); library(lubridate); library(arrow); 
library(sf); library(mgcv); library(mgcViz)

dat <- read_parquet("../data_general/AVHRR_CDRv5_VI/ard_nirv_2020-04-04.parquet")
# split data --------------------------------------------------------------
nobs_mod <- 1e5
d_train <- dat %>% sample_n(nobs_mod)
d_test <- dat %>% sample_n(nobs_mod)

d_test %>% 
  ggplot(data=.,aes(ddate,precip_anom_12mo))+
  geom_smooth()

d_train %>% 
  mutate(year=year(date)) %>% 
  select(year, 
         nirv_anom_sd, 
         precip_anom_sd_12mo, 
         pet_anom_sd_12mo) %>% 
  na.omit() %>% 
  group_by(year) %>% 
  summarize(rho = cor(precip_anom_sd_12mo, pet_anom_sd_12mo)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year, rho))+
  geom_line()+
  geom_point()


# Long-term VI sensitivity to PET & Precip. -------------------------------
mlt <- bam(nirv_anom_sd ~
              te(x,y)+
              te(lai_amp_u, 
                 pet_anom_sd_12mo,
                 precip_anom_sd_12mo),
            data=d_train, 
            select=T, discrete=T)

plotSlice(sm(getViz(mlt),2), 
          fix=list(#"lai_u"=c(1,2,3), 
            "lai_amp_u"=c(0.5,1.5,2.5)
            # "ddate"=c(1988,
            #           1995, 
            #           2001, 
            #           2018
            # )   # 1988.5, 1995.5, 2001.5; 2018.5
          ))+
  l_fitRaster(noiseup = T, mul = 2)+
  # l_fitContour()+
  l_rug()+
  scale_fill_gradient2(expression(paste(eta)), 
                       limits=c(-3,3),
                       oob=scales::squish)+
  geom_hline(aes(yintercept=0),col='grey')


# m4 ------------------------------------------------------------
m4.s <- bam(nirv_anom_sd ~
              te(x,y)+
              te(ddate, 
                 lai_amp_u, 
                 pet_anom_sd_12mo)+
              te(ddate, 
                 lai_amp_u, 
                 precip_anom_sd_12mo),
            data=d_train, 
            select=T, discrete=T)
m4.s %>% summary
m4.s %>% plot
gv4.s <- getViz(m4.s)
plot(gv4.s, allTerms = T)
plotSlice(sm(gv4.s,2), 
          fix=list(#"lai_u"=c(1,2,3), 
            "lai_amp_u"=c(0.5,1.5,2.5)
            # "ddate"=c(1988,
            #           1995, 
            #           2001, 
            #           2018
            # )   # 1988.5, 1995.5, 2001.5; 2018.5
          ))+
  l_fitRaster(noiseup = T, mul = 2)+
  # l_fitContour()+
  l_rug()+
  scale_fill_gradient2(expression(paste(eta)), 
                       limits=c(-3,3),
                       oob=scales::squish)+
  geom_hline(aes(yintercept=0),col='grey')
plotSlice(sm(gv4.s,3), 
                  fix=list(#"lai_u"=c(1,2,3), 
                    "lai_amp_u"=c(0.5,1.5,2.5)
                    # "ddate"=c(1988,
                    #           1995, 
                    #           2001, 
                    #           2018
                              # )   # 1988.5, 1995.5, 2001.5; 2018.5
                  ))+
  l_fitRaster(noiseup = T, mul = 2)+
  # l_fitContour()+
  l_rug()+
  scale_fill_gradient2(expression(paste(eta)), 
                       limits=c(-3,3),
                       oob=scales::squish)+
  geom_hline(aes(yintercept=0),col='grey')
  # geom_vline(aes(xintercept=0),col='grey')
  # scale_x_continuous(limits=c(-4,4),expand=c(0,0))+
  # scale_y_continuous(limits=c(-4,4),expand=c(0,0))+
  # labs(x=expression(paste(12~month~precip~anom~(sigma))), 
  #      y=expression(paste(12~month~PET~anom~(sigma))), 
  #      title=expression(paste('Time Varying P & PET effects upon NIR-V Anomalies',(sigma))))
p4.s
ggsave(filename = 'figures/P_PET_effects_nirv_anom_sd_m4.png', 
       width=18, height = 15, units='cm', dpi = 'retina', type='cairo')


# m3 ------------------------------------------------------------
m3.s <- bam(nirv_anom_sd ~
              s(vc, bs='re')+
              te(ddate, 
                 lai_amp_u, 
                 precip_anom_sd_12mo,
                 pet_anom_sd_12mo),
            data=d_train, 
            select=T, discrete=T)
m3.s %>% summary
m3.s %>% plot
gv3.s <- getViz(m3.s)
plot(gv3.s)
p3.s <- plotSlice(sm(gv3.s,2), 
                  fix=list(#"lai_u"=c(1,2,3), 
                    "lai_amp_u"=c(0.5,1.5,2.5),
                    "ddate"=c(1988,
                              1995, 
                              2001, 
                              2018)   # 1988.5, 1995.5, 2001.5; 2018.5
                  ))+
  l_fitRaster(noiseup = T, mul = 2)+
  l_fitContour()+
  l_rug()+
  scale_fill_gradient2(expression(paste(eta)), 
                       limits=c(-5,5), 
                       oob=scales::squish)+
  geom_hline(aes(yintercept=0),col='grey')+
  geom_vline(aes(xintercept=0),col='grey')+
  scale_x_continuous(limits=c(-4,4),expand=c(0,0))+
  scale_y_continuous(limits=c(-4,4),expand=c(0,0))+
  labs(x=expression(paste(12~month~precip~anom~(sigma))), 
       y=expression(paste(12~month~PET~anom~(sigma))), 
       title=expression(paste('Time Varying P & PET effects upon NIR-V Anomalies',(sigma))))
ggsave(filename = 'figures/P_PET_effects_nirv_anom_sd.png', 
       width=18, height = 15, units='cm', dpi = 'retina', type='cairo')



# m3 ------------------------------------------------------------
m3.s <- bam(nirv_anom_sd ~
              s(vc, bs='re')+
              te(ddate, 
                 lai_amp_u, 
                 precip_anom_sd_12mo,
                 pet_anom_sd_12mo),
            data=d_train, 
            select=T, discrete=T)
m3.s %>% summary
m3.s %>% plot
gv3.s <- getViz(m3.s)
plot(gv3.s)
p3.s <- plotSlice(sm(gv3.s,2), 
          fix=list(#"lai_u"=c(1,2,3), 
                  "lai_amp_u"=c(0.5,1.5,2.5),
                   "ddate"=c(1988,
                             1995, 
                             2001, 
                             2018)   # 1988.5, 1995.5, 2001.5; 2018.5
                   ))+
  l_fitRaster(noiseup = T, mul = 2)+
  l_fitContour()+
  l_rug()+
  scale_fill_gradient2(expression(paste(eta)), 
                       limits=c(-5,5), 
                       oob=scales::squish)+
  geom_hline(aes(yintercept=0),col='grey')+
  geom_vline(aes(xintercept=0),col='grey')+
  scale_x_continuous(limits=c(-4,4),expand=c(0,0))+
  scale_y_continuous(limits=c(-4,4),expand=c(0,0))+
  labs(x=expression(paste(12~month~precip~anom~(sigma))), 
       y=expression(paste(12~month~PET~anom~(sigma))), 
       title=expression(paste('Time Varying P & PET effects upon NIR-V Anomalies',(sigma))))
ggsave(filename = 'figures/P_PET_effects_nirv_anom_sd.png', 
       width=18, height = 15, units='cm', dpi = 'retina', type='cairo')

m3 <- bam(nirv_anom_sd ~
              s(vc, bs='re')+
              te(x,y)+
              te(ddate, 
                 lai_amp_u, 
                 precip_anom_12mo,
                 pet_anom_12mo),
            data=d_train, 
            select=T, discrete=T)
summary(m3)
gv3 <- getViz(m3)
plot(gv3)
plotSlice(sm(gv3,2), 
          fix=list(#"lai_u"=c(1,2,3), 
            "lai_amp_u"=c(0.5,1.5,2.5),
            "ddate"=c(1988,
                      1995, 
                      2001, 
                      2018)   # 1988.5, 1995.5, 2001.5; 2018.5
          ))+
  l_fitRaster(noiseup = T, mul = 2)+
  l_fitContour()+
  l_rug()+
  scale_fill_gradient2(#limits=c(-5,5), 
                       oob=scales::squish)+
  geom_hline(aes(yintercept=0),col='grey')+
  geom_vline(aes(xintercept=0),col='grey')
  # scale_x_continuous(limits=c(-3,3),expand=c(0,0))+
  # scale_y_continuous(limits=c(-3,3),expand=c(0,0))




# Base model with non-dynamic components ----------------------------------
m0 <- bam(nirv ~ 
            vc+
            ddate+
            s(month,vc, bs='fs')+
            s(ddate,by=vc), 
          data=d_train, 
          select=T, discrete=T)
m0 %>% summary
m0 %>% plot
getViz(m0) %>% plot(., allTerms=T)



m0.s <- bam(nirv_anom_sd ~ 
            vc+
            ddate+
            s(month,vc, bs='fs')+
            s(ddate,by=vc), 
          data=d_train, 
          select=T, discrete=T)
m0.s %>% summary
m0.s %>% plot
getViz(m0.s) %>% plot(., allTerms=T)


m1.s <- bam(nirv_anom_sd ~ 
              vc+
              ddate
              # s(ma_pet_precip, k=5)+
              # s(month,vc, bs='fs')+
              # te(lai_amp_u,
              #   precip_anom_sd_12mo,
              #   pet_anom_sd_12mo)
              # te(ddate, pet_anom_sd_12mo,by=vc)
              # s(ddate,by=vc)
            , 
            data=d_train,
            method='fREML',
            select=T, 
            discrete=T)
m1.s %>% summary
m1.s %>% plot
gv1.s <- getViz(m1.s,post = F) 
gv1.s %>% plot(., allTerms=T)
plot(m1.s, scheme=2)


plotSlice(sm(gv1.s,2), 
          fix=list("ma_pet_precip"=c(2.5,5,10,15)))+
  l_fitRaster()+
  l_fitContour()+
  l_rug()+
  scale_fill_gradient2()



# m1 <- bam(nirv_anom_sd ~ 
#             vc+
#             ddate+
#             s(month,vc, bs='fs')+
#             s(ddate,by=vc), 
#           data=d_train, 
#           select=T, discrete=T)
# m0 %>% summary
# m0 %>% plot
# getViz(m0) %>% plot(., allTerms=T)

m1.s <- bam(nirv_anom_sd ~ 
              vc+
              ddate+
              te(x,y)+
              s(pet_anom_max_3mo, k=3)+
              s(precip_anom_min_3mo, k=3)+
              s(month,vc, bs='fs')+
              s(ddate,by=vc, k=5), 
            data=d_train, 
            select=T, discrete=T)
m1.s %>% summary
getViz(m1.s) %>% plot(., allTerms=T)


m2.s <- bam(nirv_anom_sd ~ 
              vc+
              ddate+
              te(x,y)+
              s(pet_anom_max_3mo,
                precip_anom_min_3mo, 
                lai_amp_u, k=3)+
              s(month,vc, bs='fs')+
              s(ddate,by=vc, k=5)
            , 
            data=d_train, 
            select=T, discrete=T)
m2.s %>% summary
plot(m2.s, scheme=2, rug=T)
getViz(m2.s) %>% plot(., allTerms=T)



d_train %>% 
  select(nirv_anom, 
         pet_anom, 
         pet_anom_sd, 
         pet_anom_max_3mo,
         pet_anom_max_6mo,
         pet_anom_max_9mo,
         pet_anom_max_12mo,
         pet_anom_min_3mo,
         pet_anom_min_6mo,
         pet_anom_min_9mo,
         pet_anom_min_12mo,
         
         precip_anom, 
         precip_anom_sd, 
         precip_anom_max_3mo,
         precip_anom_max_6mo,
         precip_anom_max_9mo,
         precip_anom_max_12mo,
         precip_anom_min_3mo,
         precip_anom_min_6mo,
         precip_anom_min_9mo,
         precip_anom_min_12mo) %>% 
  na.omit() %>% 
  cor() %>% 
  corrplot::corrplot(method='number', 
                     type='upper', 
                     tl.pos = 'd', 
                     cl.pos = 'n')

m2.s <- bam(nirv_anom_sd ~ 
              vc*ddate+
              s(x,y)+
              s(pet_anom_sd_max_3mo,lai_amp_u,k=5)+
              s(precip_anom_sd_max_3mo,lai_amp_u,k=5)+
              s(month,vc, bs='fs'),
            data=d_train %>% 
              mutate(pet_anom_sd_max_3mo = pet_anom_max_3mo/pet_sd, 
                     precip_anom_sd_min_12mo = precip_anom_min_12mo/precip_sd), 
            select=T, discrete=T)
m2.s %>% summary
plot(m2.s, scheme=2, rug=T)
getViz(m2.s) %>% plot(., allTerms=T)



m1 <- bam(evi2 ~ 
            vc+
            ddate+ # proxy for CO2
            s(vpd3pm_anom_sd, vc, k=5, bs='fs')+  # time varying 
            s(precip_anom_sd, vc, k=5, bs='fs')+  # time varying 
            s(month,vc, bs='fs'),
          data=d_train, 
          select=T, discrete=T)
summary(m1, re.test = T)
m1 %>% plot
getViz(m1) %>% plot(., allTerms=T)

viz1 <- getViz(m1)
plot(sm(viz1, 1)) + 
  # l_fitLine()+
  # l_points()+
  scale_x_continuous(limits=c(-5,5))

################################################################
names(d_train) %>% sort
m2 <- bam(nirv ~ 
            s(map)+
            s(matmax)+
            vc+
            ddate+ # proxy for CO2
            s(vpd3pm_anom_sd, vc, k=5, bs='fs')+  # time varying 
            s(precip_anom_sd, vc, k=5, bs='fs')+  # time varying 
            s(month,vc, bs='fs'),
          data=d_train, 
          select=T, discrete=T)
summary(m2, re.test = T)
m2 %>% plot
getViz(m2) %>% plot(., allTerms=T)

d_train %>% 
  sample_n(100) %>% 
  select(mcwd5_12mo, mcwd5_24mo, mcwd5_36mo) %>% 
  ggplot(data=., aes(mcwd5_36mo, mcwd5_12mo))+
  geom_point()
  na.omit() %>% 
  cor()


  
  
# --- SCRATCH ---------------------------------------------------------
  # nvis <- stars::read_stars("../data_general/NVIS/nvis51_majorVegClass_0p05.tif") %>% 
  #   as_tibble() %>% 
  #   set_names(., c("lon","lat","veg_class")) %>% 
  #   st_as_sf(., coords = c("lon","lat"))
  # # dat <- read_parquet("../data_general/AVHRR_EVI2_CDR_V5/EOz_AVHRR_EVI2_LAI_1981_2019.parquet")
  # dat <- arrow::read_arrow("../data_general/Oz_misc_data/ahvrr_clim_eastOz_2020-02-26.parquet") %>% 
  #   rename(evi2 = evi2.x, lai=lai.x)
  # codes <- readr::read_fwf("../data_general/NVIS/nvis51_majorVegClass_codes.txt", 
  #                          fwf_widths(c(2,100)), skip = 1) %>% 
  #   set_names(c("veg_class","veg_class_descrip")) %>% 
  #   mutate(vc = as.factor(veg_class_descrip))
  # 
  # 
  # coords_vi <- dat %>% select(lon,lat) %>% distinct()
  # 
  # coords_vi <- st_as_sf(coords_vi, coords = c("lon","lat"))
  # st_crs(coords_vi) <- st_crs(4326)
  # st_crs(nvis) <- st_crs(4326)
  # 
  # nn_coords <- RANN::nn2(
  #   nvis %>% st_coordinates(),
  #   coords_vi %>% st_coordinates()
  # ) 
  # nn_coords$nn.idx[,1] %>% hist
  # 
  # nn_coords %>% str
  # nn_coords$nn.idx[1,]
  # nn_coords$nn.idx %>% dim
  # dim(coords_vi)
  # 
  # 
  # coords_j <- inner_join(
  #   {coords_vi %>% 
  #   mutate(idx_nvis = nn_coords$nn.idx[,1]) %>% 
  #   mutate(lon=st_coordinates(.)[,1], 
  #          lat=st_coordinates(.)[,2]) %>% 
  #   as_tibble() %>% 
  #   select(-geometry)}, 
  #   {nvis %>% 
  #   rownames_to_column() %>% 
  #   as_tibble() %>% 
  #   mutate(idx_nvis = as.integer(rowname)) %>% 
  #   select(-geometry)}, 
  #   by='idx_nvis')
  # 
  # dat <- inner_join(dat, 
  #                   coords_j %>% select(lon,lat,veg_class), 
  #                   by=c("lon","lat"))
  # 
  # dat <- dat %>%  
  #   inner_join(., codes, by='veg_class')
  
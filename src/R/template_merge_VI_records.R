library(stars); library(tidyverse); library(data.table); library(lubridate)
library(dtplyr, warn.conflicts = FALSE)

# AVHRR SR ----------------------------------------------------------------
dat_s <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_longTermSdNDVI_EastOz_2007_2014.tif") %>% 
  as_tibble() %>% 
  as.data.table() %>% 
  set_names(c("x","y","sd"))
dat_red <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif") %>%
  slice('band', seq(1,by=2,length.out = 456)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1982-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>% 
  set_names(c("x","y","date","red"))
dat_nir <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif") %>%
  slice('band', seq(2,by=2,length.out = 456)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1982-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>% 
  set_names(c("x","y","date","nir"))
gc() 
dat_red <- dat_red[is.na(red)==F]
dat_nir <- dat_nir[is.na(nir)==F]
gc()
dat <- dat_red[dat_nir,on=.(x,y,date)]
rm(dat_red, dat_nir); gc()
dat <- dat_s[dat,on=.(x,y)]

# add the NVIS vegetation classes
base <- stars::read_stars('../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif', 
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
dat <- vc[dat, on=.(x,y)]

# Attach the solar zenith angle -------------------------------------------
sz <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_SolarZenithAngle_median_EastOz_1982_2019.tif") %>%
  # slice('band', seq(1,by=2,length.out = 456)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("1982-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>% 
  set_names(c("x","y","date","sz"))
sz <- sz[is.na(sz)==F]
sz <- sz[,`:=`(sz=(sz*0.01))]

dat <- sz[dat, on=.(x,y,date)]
# END **************************************************************************

# Spectral rotation function 2----------------------------------------------
# (1) Identify soil line pixels
soil_pix <- dat[,`:=`(ndvi=(nir-red)/(nir+red))][,.(ndvi_u = mean(ndvi), 
                                                    sd_u = mean(sd), 
                                                    red_u = mean(red), 
                                                    nir_u = mean(nir)),by=.(x,y)] 
# (2) Identify pixels that are sometimes water covered
water_pix <- dat[,`:=`(ndvi=(nir-red)/(nir+red))] %>% 
  .[,.(red_low = quantile(red,0.01), 
       red_nobs = sum(is.na(red)==F), 
       ndvi_u = mean(ndvi), 
       sd_u = mean(sd), 
       red_u = mean(red), 
       nir_u = mean(nir)),by=.(x,y)]

fn_rot2 <- function(din){
  # soil line pixels
  MP <- 0.7 # max reflectance
  
  d_soil <- soil_pix[is.na(sd_u)==F & 
                       red_u < 0.5 & 
                       ndvi_u < 0.025 & 
                       sd_u < 0.05]
  d_soil <- din[d_soil[,.(x,y)],on=.(x,y)][,`:=`(ndvi=(nir-red)/(nir+red))]
  d_soil <- d_soil[ndvi < 0.025 & ndvi > -0.025]

  d_water <- water_pix[red_low <= 0.03 & 
                         red_nobs > 400 & 
                         ndvi_u < 0.1]
  d_water <- din[water_pix[,.(x,y)],on=.(x,y)][,`:=`(ndvi=(nir-red)/(nir+red))]
  d_water <- d_water[ndvi < 0.1 & ndvi > -0.1 & red < 0.1]
  # d_water$class <- 'water'
  
  d_mod <- rbindlist(list(d_soil, d_water))
  d_mod <- unique(d_mod, on=.(x,y))
  
  # d_mod %>% lazy_dt() %>%
  #   # filter(between(ndvi,-0.05,0.05)) %>%
  #   filter((red >0.45 & !near(nir,red,tol=0.025))==F) %>%
  #   # filter((red <0.05 & (nir>0.05))==F) %>%
  #   as_tibble() %>%
  #   ggplot(data=.,aes(red,nir,color=ndvi))+
  #   geom_point()+geom_abline(aes(intercept=0,slope=1),col='red')+
  #   geom_smooth(method='lm')+
  #   scale_color_viridis_c()
  
  # d_mod <- d_mod %>% lazy_dt() %>% 
  #   filter(between(ndvi,-0.05,0.05)) %>%
  #   filter((red >0.5 & !near(nir,red,tol=0.025))==F) %>% 
  #   as.data.table()
  
  pr <- d_mod$red
  pn <- d_mod$nir
  
  # all pixels
  vr <- din$red
  vn <- din$nir
  
  # pr:ref red, pn: ref nir
  # prs <- pr - (min(pr)) # shift reference red to origin
  # pns <- pn - pn[dp_idx]
  f1 <- lm(pn~pr); 
  b_s <- coef(f1)[1] %>% unname()
  a_s <- coef(f1)[2] %>% unname()
  
  xpoint <- as.numeric(b_s/(1 - a_s))
  prs <- pr-xpoint
  pns <- pn-xpoint
  theta <- atan(1) - atan(a_s)
  pr_p <- prs*cos(theta) - pns*sin(theta)
  pn_p <- prs*sin(theta) + pns*cos(theta)
  # plot(pn_p~pr_p); abline(0,1)
  pr_pp <- pr_p+xpoint
  pn_pp <- pn_p+xpoint
  # plot(pn_pp~pr_pp); abline(0,1)
  
  # darkpoint from ECDF, 0.01 percentile
  # f_red <- ecdf(pr_pp)
  # f_nir <- ecdf(pn_pp)
  # dp <- c(quantile(f_red, 0.01), quantile(f_nir, 0.0)) %>% unname()
  
  
  # all pix coordinate transformation
  vrs <- vr - xpoint
  vns <- vn - xpoint
  vr_p <- vrs*cos(theta) - vns*sin(theta)
  vn_p <- vrs*sin(theta) + vns*cos(theta)
  vr_pp <- vr_p+xpoint
  vn_pp <- vn_p+xpoint
  # plot(vn_pp~vr_pp); abline(0,1)
  
  # filter for the dark point (Rd)
  # perhaps near minimum red and median nir?
  offsets <- data.table(red=vr_pp,nir=vn_pp)
  unname(quantile(offsets$red, 0.01))
  # offsets <- offsets[red <= (min(red)+0.005)]
  median(offsets[red < unname(quantile(offsets$red, 0.01))]$nir)
  
  offset_r <- min(offsets$red)
  offset_n <- offsets[min(pn_pp) & nir <0.5][red < unname(quantile(offsets$red,0.005))]$nir %>% median()
  offsets <- unname(c(offset_r, 0))
  
  # offsets <- c(min(offsets$red), 
  #              quantile(offsets[red < unname(quantile(offsets$red, 0.01))]$nir, 0.1)) %>%
  #   unname()
  M <- 0.02
  
  pr_bar <- (pr_pp-offsets[1]) + M
  pn_bar <- (pn_pp-offsets[2]) + M
  
  vr_bar <- (vr_pp-offsets[1]) + M
  vn_bar <- (vn_pp-offsets[2])
  
  
  # dp_idx <- which(pr_p==min(pr_p), arr.ind = T)
  # prd <- c(pr_pp[dp_idx],pn_pp[dp_idx])
  # pr_bar <- pr_pp+M-prd[1]
  # pn_bar <- pn_pp+M-prd[1]
  
  # out <- cbind(pr_bar, pn_bar)
  # out <- data.table(red=pr_bar, nir=pn_bar)
  # din$red_c <- vr_bar
  # din$nir_c <- vn_bar
  din$red_c <- vr_pp
  din$nir_c <- vn_pp
  
  
  din <- din[,`:=`(ndvi=(nir-red)/(nir+red), 
                   ndvi_c = (nir_c-red_c)/(nir_c+red_c))] %>% 
    .[,`:=`(nirv = (ndvi*nir), 
            nirv_c = (ndvi_c*nir))]
  din$a_s <- a_s
  din$b_s <- b_s
  
  din <- din[nir_c > 0]
  return(din)
}

#! Apply rotation function by date !#----
dat <- dat[,fn_rot2(.SD),by=date]
rm(base,codes,dat_s,soil_pix,sz,water_pix);
gc()
# END AVHRR ---------------------------------------------------------------





# MCD43  ------------------------------------------------------------
o1 <- stars::read_stars("../data_general/MCD43/MCD64A4_SR_5km_EastOz_mmean_NVISmask_2001_2019.tif") %>% 
  slice('band', seq(1,by=2,to = 456)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2001-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>% 
  set_names(c("x","y","date","red"))
o2 <- stars::read_stars("../data_general/MCD43/MCD64A4_SR_5km_EastOz_mmean_NVISmask_2001_2019.tif") %>% 
  slice('band', seq(2,by=2,to = 456)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2001-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>% 
  set_names(c("x","y","date","nir"))

o <- o1[o2,on=.(x,y,date)]
rm(o1,o2); gc()

o <- o[,`:=`(ndvi_mcd=(nir-red)/(nir+red))]
o <- o[,`:=`(evi2_mcd=2.5*(nir-red)/(nir+2.4*red+1))]
o <- o[,`:=`(nirv_mcd=ndvi_mcd*nir)]
o <- o[,`:=`(year=year(date))]
o <- o %>% lazy_dt() %>% 
  filter(is.na(ndvi_mcd)==F & red > 0 & nir > 0) %>% 
  as.data.table()
o <- o %>% lazy_dt() %>% select(x,y,date,ndvi_mcd,evi2_mcd,nirv_mcd) %>% as.data.table()
hyb <- o[dat,on=.(x,y,date)]

# hyb <- hyb[is.na(ndvi_mcd)==F & is.na(ndvi)==F]


# # MYD13A2 Aqua ------------------------------------------------------------
# o1 <- stars::read_stars("../data_general/MYD13A2/MYD13A2_SR_5km_EastOz_mmean_NVISmask_2003_2019.tif") %>% 
#   slice('band', seq(1,by=2,to = 408)) %>% 
#   st_set_dimensions(., 3, 
#                     values=seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"), 
#                     names = 'date') %>%  
#   as_tibble() %>% 
#   as.data.table() %>% 
#   set_names(c("x","y","date","red"))
# o2 <- stars::read_stars("../data_general/MYD13A2/MYD13A2_SR_5km_EastOz_mmean_NVISmask_2003_2019.tif") %>% 
#   slice('band', seq(2,by=2,to = 408)) %>% 
#   st_set_dimensions(., 3, 
#                     values=seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"), 
#                     names = 'date') %>%  
#   as_tibble() %>% 
#   as.data.table() %>% 
#   set_names(c("x","y","date","nir"))
# 
# o <- o1[o2,on=.(x,y,date)]
# rm(o1,o2); gc()
# 
# o <- o[,`:=`(ndvi_myd=(nir-red)/(nir+red))]
# o <- o[,`:=`(year=year(date))]
# o <- o %>% lazy_dt() %>% 
#   filter(is.na(ndvi_myd)==F & red > 0 & nir > 0) %>% 
#   as.data.table()
# o <- o %>% lazy_dt() %>% select(x,y,date,ndvi_myd) %>% as.data.table()
# hyb <- o[hyb,on=.(x,y,date)]
# 
# # Import MOD13A2 Terra -----------------------------------------------------------
# o1 <- stars::read_stars("../data_general/MOD13A2/MOD13A2_SR_5km_EastOz_mmean_NVISmask_2003_2019.tif") %>% 
#   slice('band', seq(1,by=2,to = 408)) %>% 
#   st_set_dimensions(., 3, 
#                     values=seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"), 
#                     names = 'date') %>%  
#   as_tibble() %>% 
#   as.data.table() %>% 
#   set_names(c("x","y","date","red"))
# o2 <- stars::read_stars("../data_general/MOD13A2/MOD13A2_SR_5km_EastOz_mmean_NVISmask_2003_2019.tif") %>% 
#   slice('band', seq(2,by=2,to = 408)) %>% 
#   st_set_dimensions(., 3, 
#                     values=seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"), 
#                     names = 'date') %>%  
#   as_tibble() %>% 
#   as.data.table() %>% 
#   set_names(c("x","y","date","nir"))
# 
# o <- o1[o2,on=.(x,y,date)]
# rm(o1,o2); gc()
# 
# o <- o[,`:=`(ndvi_mod=(nir-red)/(nir+red))] %>% 
#      .[,`:=`(year=year(date))]
# o <- o %>% lazy_dt() %>% 
#   filter(is.na(ndvi_mod)==F & red > 0 & nir > 0) %>% 
#   as.data.table()
# o <- o %>% lazy_dt() %>% select(x,y,date,ndvi_mod) %>% as.data.table()
# # End import *****************************************************************
# 
# # join MODIS Terra with the rest
# hyb <- hyb[o,on=.(x,y,date)]

# Fit calibration model ---------------------------------------------------
library(mgcv)
set.seed(333)
train <- hyb %>% lazy_dt() %>% 
  filter(str_detect(vc,"Eucalypt")==T |
           str_detect(vc,"Rainforests")==T |
           str_detect(vc,"Forest")==T | 
           str_detect(vc,"Woodlands")) %>% 
  filter(ndvi_mcd>0 & date<=ymd("2016-12-01") & is.na(ndvi_c)==F) %>% 
  sample_n(3e5) %>% 
  as.data.table()
test <- hyb %>% lazy_dt() %>% 
  filter(str_detect(vc,"Eucalypt")==T |
           str_detect(vc,"Rainforests")==T |
           str_detect(vc,"Forest")==T | 
           str_detect(vc,"Woodlands")) %>% 
  filter(ndvi_mcd>0 & date<=ymd("2016-12-01") & is.na(ndvi_c)==F) %>% 
  sample_n(3e5) %>% 
  as.data.table()

gc()
# m0_ndvi <- bam(ndvi_mcd~
#                  te(sz,ndvi_c)+
#                  s(x,y)+s(vc,bs='re'),
#                family=betar(link='logit'),
#                select = TRUE, discrete=TRUE, method='fREML', nthreads = 8,
#                data=train)
# m1_ndvi <- bam(ndvi_mcd~
#                  s(sz,by=nir_c)+s(sz,by=red_c)+te(red_c,nir_c)+
#                  s(x,y)+s(vc,bs='re'),
#                family=betar(link='logit'),
#                select = TRUE, discrete=TRUE, method='fREML', nthreads = 8,
#                data=train)
# m2_ndvi <- bam(ndvi_mcd~
#                  s(sz,by=ndvi_c)+
#                  s(x,y)+s(vc,bs='re'),
#                family=betar(link='logit'),
#                select = TRUE, discrete=TRUE, method='fREML', nthreads = 8,
#                data=train)
# summary(m2_ndvi)
# m3_ndvi <- bam(ndvi_mcd~
#                  s(sz)+s(nir_c)+s(red_c)+
#                  s(x,y)+s(vc,bs='re'),
#                family=betar(link='logit'),
#                select = TRUE, discrete=TRUE, method='fREML', nthreads = 8,
#                data=train)
m4_ndvi <- bam(ndvi_mcd~
                 s(sz,k=5)+s(nir_c,k=5)+s(red_c,k=5)+s(ndvi_c,k=5,m = 1)+
                 s(x,y)+s(vc,bs='re'),
               family=Gamma(link='log'),
               select = TRUE, discrete=TRUE, method='fREML', nthreads = 8,
               data=train)
# bbmle::AICtab(m0_ndvi, m1_ndvi, m2_ndvi, m3_ndvi, m4_ndvi)

summary(m4_ndvi)
# predict(m1_ndvi, type='response') %>% hist
# gam.check(m1_ndvi)
# gratia::qq_plot(m1_ndvi)
# gratia::appraise(m1_ndvi)
# bbmle::AICtab(m0_ndvi,m1_ndvi)

m4_evi2 <- bam(evi2_mcd~
                 s(sz,k=5)+s(nir_c,k=5)+s(red_c,k=5)+s(ndvi_c,k=5,m = 1)+
                 s(x,y)+s(vc,bs='re'),
               family=Gamma(link='log'),
               select = TRUE, discrete=TRUE, method='fREML', nthreads = 8,
               data=train)
m4_nirv <- bam(nirv_mcd~
                 s(sz,k=5)+s(nir_c,k=5)+s(red_c,k=5)+s(ndvi_c,k=5,m = 1)+
                 s(x,y)+s(vc,bs='re'),
               family=Gamma(link='log'),
               select = TRUE, discrete=TRUE, method='fREML', nthreads = 8,
               data=train)

oos_eval <- test %>% 
  mutate(pred_ndvi_mcd = predict(m4_ndvi, newdata=., type='response'), 
         pred_evi2_mcd = predict(m4_evi2, newdata=., type='response'), 
         pred_nirv_mcd = predict(m4_nirv, newdata=., type='response')) %>% 
  summarize(r2_ndvi = cor(ndvi_mcd, pred_ndvi_mcd)**2, 
            r2_evi2 = cor(evi2_mcd, pred_evi2_mcd)**2,
            r2_nirv = cor(nirv_mcd, pred_nirv_mcd)**2, 
            rmse_ndvi = sqrt(mean((pred_ndvi_mcd-ndvi_mcd)**2)), 
            rmse_evi2 = sqrt(mean((pred_evi2_mcd - evi2_mcd)**2)), 
            rmse_nirv = sqrt(mean((pred_nirv_mcd - nirv_mcd)**2)))
oos_eval %>% 
  mutate(eval_date = Sys.Date()) %>% 
  write_csv(.,path = paste0("outputs/OutOfSample_VI_merge_",Sys.Date(),".csv"))


hyb <- hyb %>% lazy_dt() %>% 
  filter(str_detect(vc,"Eucalypt")==T |
           str_detect(vc,"Rainforests")==T |
           str_detect(vc,"Forest")==T | 
           str_detect(vc,"Woodlands")) %>% 
  mutate(ndvi_hyb = predict(m4_ndvi, newdata=., n.threads = 8,type='response'), 
         evi2_hyb = predict(m4_evi2, newdata=., n.threads = 8,type='response'), 
         nirv_hyb = predict(m4_nirv, newdata=., n.threads = 8,type='response')) %>% 
  as.data.table()

out <- hyb %>% lazy_dt() %>% 
  mutate(ndvi_hyb = coalesce(ndvi_mcd, ndvi_hyb), 
         evi2_hyb = coalesce(evi2_mcd, evi2_hyb), 
         nirv_hyb = coalesce(nirv_mcd, nirv_hyb)) %>% 
  as.data.table() %>% 
  group_by(x,y) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup()

out %>% lazy_dt() %>% 
  filter(is.na(ndvi_hyb)==F) %>% 
  as.data.table() %>% 
  arrow::write_parquet(sink = 
          paste0("../data_general/MCD43/MCD43_AVHRR_NDVI_hybrid_",Sys.Date(),".parquet"))

# hyb[is.na(vc)==F] %>%
#   lazy_dt() %>%
#   filter(date >= ymd("2001-01-01")) %>%
#   filter(date < ymd("2015-01-01")) %>%
#   filter(veg_class %in% c(1:6)) %>% 
#   mutate(month=month(date)) %>%
#   group_by(vc,date) %>%
#   summarize(val1 = mean(ndvi_hyb, na.rm=TRUE), 
#             val2 = mean(ndvi_mcd, na.rm=TRUE)) %>% 
#   ungroup() %>% 
#   as_tibble() %>% 
#   ggplot(data=., aes(date,val1))+
#   geom_smooth(method='lm')+
#   geom_smooth(aes(date,val2),col='black',method='lm')+
#   facet_wrap(~vc,scales = 'free')

# 
# 
# 
# hyb[date >= ymd("2015-01-01")] %>% 
#   lazy_dt() %>% 
#   filter(veg_class %in% c(1:6)) %>% 
#   group_by(vc,date) %>% 
#   summarize(ndvi = mean(ndvi_mcd, na.rm=TRUE), 
#             nirv = mean(nirv_mcd, na.rm=TRUE)) %>% 
#   ungroup() %>%
#   as_tibble() %>% 
#   gather(-date,-vc, key='method',value = 'u') %>% 
#   as_tibble() %>% 
#   ggplot(data=., aes(date,u,color=method))+
#   geom_smooth(se=F,method='loess',span=0.1)+
#   scale_color_viridis_d(end=0.99)+
#   facet_wrap(~vc)
# 
# 
# 
# hyb %>% lazy_dt() %>% 
#   filter(is.na(ndvi_mcd)==F) %>% 
#   sample_n(1e6) %>% 
#   group_by(vc,veg_class) %>% 
#   summarize(val = mean(ndvi_mcd,na.rm=TRUE), 
#             nobs=n()) %>% 
#   ungroup() %>% 
#   view()
# 
# hyb[is.na(vc)==TRUE & is.na(ndvi_mcd)==F][,.(val=mean(ndvi_mcd,na.rm=TRUE)),by=.(x,y)] %>% 
#   ggplot(data=.,aes(x,y,fill=val))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c()
# 
# 
# 
# 
# 
# #########################################################
# summary(m0)
# gam.check(m0)
# plot(m0,pages = 1,scheme = 2,color='bw')
# vis.gam(m0, view=c("x","y"),plot.type="contour",color="cm")
# qq.gam(m0)
# 
# m1.1 <- bam(ndvi_mcd~ 
#               s(sz,by=nir_c)+s(sz,by=red_c)+te(red_c,nir_c,by=vc)+
#               s(x,y),
#             # family=Gamma(link='identity'),
#             select = TRUE, discrete=TRUE, method='fREML', 
#             data=train)
# summary(m1.1)
# m1.2 <- bam(ndvi_mcd~s(sz,by=nir_c)+s(sz,by=red_c)+te(red_c,nir_c)+
#               s(x,y,month), 
#             # family=Gamma(link='log'),
#             select = TRUE, discrete=TRUE, method='fREML', 
#             data=train %>% mutate(month=month(date)))
# summary(m1.2)
# gam.check(m1.2)
# plot(m1.2,select = 4,scheme = 2)
# m1.3 <- bam(ndvi_mcd~
#             s(sz,by=nir_c)+s(sz,by=red_c)+te(red_c,nir_c)+
#             s(x,y)+s(vc,bs='re'),
#           select = TRUE, discrete=TRUE, method='fREML', 
#           data=train)
# summary(m1.3); plot(m1.3)
# m1.4 <- bam(ndvi_mcd~s(sz,by=nir_c)+s(sz,by=red_c)+te(red_c,nir_c)+
#               s(x,y)+
#               s(vc,bs='re'),
#             # family=gaussian(link='log'),
#             select = TRUE, discrete=TRUE, method='fREML', 
#             data=train, nthreads = 8)
# # summary(m1.4)
# bbmle::AICtab(m0,m1.1,m1.2,m1.3,m1.4)
# # 
# test$veg_class %>% table %>% sort
# 
# test %>%
#   lazy_dt() %>%
#   mutate(month=month(date)) %>%
#   filter(veg_class %in% c(5,11,3,6,2,9,7,1,13)) %>%
#   rename(obs=ndvi_mcd) %>%
#   mutate(pred0 = predict(m0_ndvi,newdata=.,type='response'),
#          pred1 = predict(m1.2,newdata=.,type='response'),
#          pred2 = predict(m2_ndvi,newdata=.,type='response'),
#          pred3 = predict(m3_ndvi,newdata=.,type='response'),
#          pred4 = predict(m4_ndvi,newdata=.,type='response')
#          ) %>%
#   select(vc,date,obs,pred0,pred1,pred2,pred3,pred4) %>%
#   as_tibble() %>%
#   gather(-vc,-date,-obs, key='method',value='estimate') %>%
#   group_by(vc,date,method) %>%
#   summarize(rmse = sqrt(mean((estimate-obs)**2)),
#             r2 = cor(estimate,obs)) %>%
#   ungroup() %>%
#   ggplot(data=., aes(date,r2,color=method))+
#   # geom_point()+
#   geom_smooth(se=F,span=0.1)+
#   scale_color_brewer(type='qual')+
#   facet_wrap(~vc)+
#   theme_linedraw()
# 
# test %>% 
#   lazy_dt() %>% 
#   filter(veg_class %in% c(1:6)) %>% 
#   rename(a_obs=ndvi_mcd) %>% 
#   mutate(pred1 = predict(m1_ndvi,newdata=.,type='response'), 
#          pred3 = predict(m3_ndvi,newdata=.,type='response'), 
#          pred4 = predict(m4_ndvi,newdata=.,type='response'),
#          raw = ndvi, 
#          ndvi_c = ndvi_c) %>%
#   select(vc,date,a_obs,pred1,pred3,pred4) %>% 
#   as_tibble() %>% 
#   gather(-vc,-date,key='method',value='estimate') %>% 
#   group_by(vc,date,method) %>% 
#   summarize(u = mean(estimate)) %>% 
#   ungroup() %>% 
#   # filter(date <= ymd("2015-11-01")) %>% 
#   ggplot(data=., aes(date,u,color=method))+
#   # geom_point()+
#   geom_smooth(se=F,method='loess',span=0.1)+
#   scale_color_brewer(type='qual')+
#   facet_wrap(~vc)
# 
# 
# 
# 
# 
# 
# library(gratia)
# gratia::observed_fitted_plot(m2)+
#   ggpointdensity::geom_pointdensity()+
#   geom_abline(aes(intercept=0,slope=1),col='red')+
#   scale_color_viridis_c()
# #*******************************************************************************
# #*** END SECTION 
# #*******************************************************************************
# 
# 
# 
# 
# 
# 
# 
# out %>% names
# class(out)
# out <- out %>% lazy_dt() %>% 
#   mutate(ndvi_pred = predict(m1.2, newdata = ., type='response',n.threads = 8)) %>% 
#   as.data.table()
# 
# out %>% 
#   lazy_dt() %>% 
#   group_by(date,vc) %>% 
#   summarize(ndvi_c = mean(ndvi_c,na.rm=TRUE), 
#             ndvi_pred = mean(ndvi_pred,na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   as_tibble() %>% 
#   ggplot(data=., aes(date, ndvi_c))+
#   geom_smooth(method='lm')+
#   geom_smooth(method='lm',aes(date,ndvi_pred),col='purple')+
#   facet_wrap(~vc, scales = 'free')
# 
# 
# 
# hyb %>% sample_n(1000) %>% ggplot(data=., aes(ndvi,ndvi_myd))+
#   geom_point()+
#   geom_abline(aes(intercept=0,slope=1),col='red')
# 
# 
# m0 <- bam(ndvi_myd~red_c+nir_c+ndvi_c+sz, 
#           family=gaussian(link='log'),
#           select = TRUE, discrete=TRUE, method='fREML', 
#           data=train)
# summary(m0)
# m1 <- bam(ndvi_myd~
#             te(red,nir,sz),
#           select = TRUE, discrete=TRUE, method='fREML', 
#           data=train, nthreads = 8)
# summary(m1)
# plot(m1)
# 
# m1.1 <- bam(ndvi_myd~
#             te(red_c,nir_c,sz),
#           select = TRUE, discrete=TRUE, method='fREML', 
#           data=train, nthreads = 8)
# m1.2 <- bam(ndvi_myd~
#               te(red_c,nir_c,sz,month),
#             select = TRUE, discrete=TRUE, method='fREML', 
#             data=train, nthreads = 8)
# summary(m1.2)
# m1.3 <- bam(ndvi_myd~
#               te(red_c,nir_c,sz,month),
#             family=Gamma(link='log'),
#             select = TRUE, discrete=TRUE, method='fREML', 
#             data=train, nthreads = 8)
# bbmle::AICtab(m1,m1.1,m1.2,m1.3)
# 
# 
# m1.3 <- bam(ndvi_myd~
#               red_c+nir_c+
#               te(ndvi_c,sz,x),
#           select = TRUE, discrete=TRUE, method='fREML', 
#           data=train, nthreads = 8)
# bbmle::AICtab(m1, m1.2,m1.3)
# plot(m1.3)
# summary(m1.3)
# 
# m2 <- bam(ndvi_myd~
#             te(red,nir,sz,month)+
#             s(x,y), 
#           select = TRUE, discrete=TRUE, method='fREML', 
#           data=train, nthreads = 8)
# summary(m2)
# plot(m2)
# 
# m3 <- bam(ndvi_myd~
#             te(ndvi,sz,month,x),
#           select = TRUE, discrete=TRUE, method='fREML', 
#           data=train, nthreads = 8)
# summary(m3)
# plot(m3)
# 
# bbmle::AICtab(m0,m1,m2,m3)
# 
# 
# 
# 
# 
# unique(o$y) %in% unique(out$y)
# 
# o %>% lazy_dt() %>% 
#   group_by(year) %>% 
#   summarize(val = mean(ndvi,na.rm=TRUE)) %>% 
#   ungroup() %>% 
#   as_tibble() %>% 
#   ggplot(data=., aes(year,val))+geom_line()

library(stars); library(tidyverse); library(data.table); library(lubridate)
library(dtplyr, warn.conflicts = FALSE)
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


# Psuedo Invariant Features -----------------------------------------------
ref <- dat[,`:=`(ndvi=(nir-red)/(nir+red))][,.(ndvi_u = mean(ndvi), 
                                               sd_u = mean(sd), 
                                               red_u = mean(red), 
                                               nir_u = mean(nir)),by=.(x,y)] %>% 
  .[is.na(sd_u)==F & red_u < 0.5 & ndvi_u < 0.025 & sd_u < 0.05]
# din <- dat[date>=ymd("2011-04-01") & date<= ymd("2012-01-01")]
din <- dat[date==ymd("2011-12-01")]

fn_pif <- function(din){
  d_mod <- din[ref,on=.(x,y)]
  f_red <- lm(red_u~red, data=d_mod)
  f_nir <- lm(nir_u~nir, data=d_mod)
  din$red_c <- predict(f_red, newdata=din)
  din$nir_c <- predict(f_nir, newdata=din)
  din <- din[,`:=`(ndvi = (nir-red)/(nir+red), 
                   ndvi_c = (nir_c-red_c)/(nir_c+red_c))] %>% 
    .[,`:=`(nirv = ndvi*nir, 
            nirv_c = ndvi_c*nir_c)]
  return(din)
}


# Spectral rotation function ----------------------------------------------
fn_rot <- function(din){
  # soil line pixels
  MP <- 0.7 # max reflectance
  pr <- din[vc=="Inland Aquatic - freshwater, salt lakes, lagoons"&red<MP &nir<MP]$red
  pn <- din[vc=="Inland Aquatic - freshwater, salt lakes, lagoons"&red<MP &nir<MP]$nir
  
  # all pixels
  vr <- din$red
  vn <- din$nir
  
  # pr:ref red, pn: ref nir
  # prs <- pr - (min(pr)) # shift reference red to origin
  # pns <- pn - pn[dp_idx]
  f1 <- MASS::rlm(pn~pr); 
  b_s <- coef(f1)[1] %>% unname()
  a_s <- coef(f1)[2] %>% unname()
  xpoint <- as.numeric(b_s/(1 - a_s))
  prs <- pr-xpoint
  pns <- pn-xpoint
  theta <- atan(1) - atan(a_s)
  pr_p <- prs*cos(theta) - pns*sin(theta)
  pn_p <- prs*sin(theta) + pns*cos(theta)
  pr_pp <- pr_p+xpoint
  pn_pp <- pn_p+xpoint

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
  din$red_c <- vr_bar
  din$nir_c <- vn_bar
  
  din <- din[,`:=`(ndvi=(nir-red)/(nir+red), 
          ndvi_c = (nir_c-red_c)/(nir_c+red_c))] %>% 
    .[,`:=`(nirv = (ndvi*nir), 
            nirv_c = (ndvi_c*nir))]
  din$a_s <- a_s
  din$b_s <- b_s
  
  din <- din[nir_c > 0]
  return(din)
}

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
  # d_soil$class <- 'soil'
  
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
# ************************************************************************
system.time(fn_rot(dat[date==ymd("1982-01-01")]))
# Test the spectral rotation function -----
x1 <- ref[date==ymd("1982-01-01")]$red
y1 <- ref[date==ymd("1982-01-01")]$nir
# Testing coord transform function ---- 
# x1 <- rnorm(nobs,mean=0.3,sd=0.055); eps1 <- rnorm(nobs,mean=0,sd=0.01);
# y1 <- 0.5 + 0.05*x1 + eps1; 
pr <- x1; pn <- y1;

test <- dat[date==ymd("2004-04-01")]
din <- test
pr <- test$red; pn <- test$nir 
plot(nir~red,
     data=test,
     ylim=c(-0.05,max(test$nir)),
     xlim=c(-0.05,max(test$red)),pch=20,cex=0.15, 
     xlab=expression(paste(rho['red'])),
     ylab=expression(paste(rho['NIR'])));
abline(lm(nir~red,data=test),col='black'); abline(0,1,col='red')
o <- fn_rot(test) %>% as_tibble()
points(o$nir_c~o$red_c,col='gray',pch=20,cex=0.5)
abline(lm(o$nir_c~o$red_c), col='gray')
abline(v=0.02,h=0.02)
points(nir_c~red_c,data=o %>% 
         filter(veg_class==24), 
       col='blue',pch=20,cex=0.5)
abline(lm(nir_c~red_c,data=o %>% 
            filter(veg_class==24), 
),col='blue')

o$ndvi_c %>% hist         

o %>% 
  ggplot(data=., aes(red_c,nir_c,color=ndvi_c))+
  geom_point()+
  geom_abline(aes(intercept=b_s, slope=a_s))+
  scale_color_gradient2(limits=c(-0.1,0.9), midpoint = 0.1, 
                        high='green',low='yellow',mid = 'black',
                        oob=scales::squish)
# Apply rotation function by date -------------------------------------
# out <- dat[,fn_rot(.SD),by=date]
out <- dat[,fn_rot2(.SD),by=date]
# END *****

# calc anoms ---------------------------------------------------------
out <- out[ndvi>0]
out <- out[, `:=`(month = month(date))] # create month
out <- out[, `:=`(year = year(date))]   # create year
out <- out[, `:=`(nirv = ndvi*nir, 
                  nirv_c = ndvi_c*nir_c)]

norms_nirv <- out[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
                  .("nirv_u" = mean(nirv,na.rm=TRUE), 
                    "nirv_sd" = sd(nirv,na.rm=TRUE), 
                    "nirv_c_u" = mean(nirv_c,na.rm=TRUE), 
                    "nirv_c_sd" = sd(nirv_c,na.rm=TRUE), 
                    "ndvi_u" = mean(ndvi,na.rm=TRUE), 
                    "ndvi_c_u" = mean(ndvi_c, na.rm=TRUE), 
                    "ndvi_sd" = sd(ndvi,na.rm=TRUE), 
                    "ndvi_c_sd" = sd(ndvi_c, na.rm=TRUE)),
                  by=.(x,y,month)] # joining on x,y,month
out <- norms_nirv[out, on=.(x,y,month)]

out <- out[, `:=`(nirv_anom = nirv - nirv_u, 
                  nirv_c_anom = nirv_c - nirv_c_u, 
                  ndvi_anom = ndvi - ndvi_u, 
                  ndvi_c_anom = ndvi_c - ndvi_c_u),]
out <- out[, `:=`(nirv_anom_sd = nirv_anom/nirv_sd,
                  nirv_c_anom_sd = nirv_c_anom/nirv_c_sd, 
                  ndvi_anom_sd = ndvi_anom/ndvi_sd, 
                  ndvi_c_anom_sd = ndvi_c_anom/ndvi_c_sd)]

out[nirv_anom_sd < 5 & nirv_anom_sd > -5]$nirv_anom_sd %>% hist

#####################################################################
out %>% lazy_dt() %>% 
  filter(veg_class == 5) %>% 
  sample_n(1e5) %>% 
  # group_by(vc,date) %>% 
  # summarize(ndvi = mean(ndvi,na.rm=TRUE), 
  #           ndvi_c = mean(ndvi_c,na.rm=TRUE)) %>% 
  # ungroup() %>% 
  filter(date <= ymd("2017-12-31")) %>% 
  filter(ndvi < 0) %>%
  as.data.table() %>% 
  ggplot(data=., aes(date, ndvi))+
  geom_point()+
  geom_point(aes(date,ndvi_c),col='blue')+
  geom_smooth(method='lm',col='black')+
  geom_smooth(method='lm',col='blue',aes(date,ndvi_c))+
  facet_wrap(~vc, ncol=1)


out %>% lazy_dt() %>% 
  filter(veg_class <= 5) %>% 
  group_by(vc,date) %>% 
  summarize(val = mean(ndvi_c,na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(date <= ymd("2017-12-31")) %>% 
  as.data.table() %>% 
  ggplot(data=., aes(date, val))+
  geom_line()+
  geom_smooth(method='lm')+
  facet_wrap(~vc, ncol=1)

out %>% 
  sample_n(1e5) %>% 
  filter(between(ndvi_c,0,1)) %>% 
  # filter(str_detect(vc,'Forest')==T) %>% 
  ggplot(data=., aes(ndvi, ndvi_c))+
  geom_point(alpha=0.1)+
  # geom_density2d_filled()+
  geom_smooth(method='lm')+
  geom_abline(aes(intercept=0,slope=1),col='red',lty=3)+
  # geom_hline(aes(yintercept=0),col='red')+
  labs(x=expression(paste(NDVI["orig."])), 
       y=expression(paste(NDVI["rotation_cal"])))+
  facet_wrap(~vc)+
  theme_linedraw()

out[veg_class==24] %>% 
  filter(between(ndvi_c,0,1)) %>% 
  group_by(x,y) %>% 
  summarize(sd = sd(ndvi,na.rm=TRUE), 
            nobs = n(), 
            u = mean(ndvi,na.rm=TRUE), 
            sd2 = sd(ndvi_c,na.rm=TRUE), 
            u2 = mean(ndvi_c,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(u,sd))+
  geom_point()+
  geom_smooth(method='lm')+
  geom_point(aes(u2,sd2),col='red')+
  geom_smooth(method='lm',col='red',aes(u2,sd2))
  # geom_histogram()

coords_stable <- out[veg_class==24] %>% 
  filter(between(ndvi_c,0,1)) %>% 
  group_by(x,y) %>% 
  summarize(sd = sd(ndvi,na.rm=TRUE), 
            nobs = n(), 
            u = mean(ndvi,na.rm=TRUE), 
            sd2 = sd(ndvi_c,na.rm=TRUE), 
            u2 = mean(ndvi_c,na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(u < 0.1 & sd < 0.05) %>% 
  select(x,y) %>% 
  distinct() %>% 
  as.data.table()

library(dtplyr)
coords_stable_l <- lazy_dt(coords_stable)
out_l <- lazy_dt(out)
tmp <- inner_join(coords_stable_l, out_l, by=c("x","y")) %>% 
  as.data.table()

tmp %>% 
  sample_n(1e5) %>% 
  # filter(date==min(date)) %>% 
  ggplot(data=., aes(red,nir))+
  geom_point()+
  geom_point(aes(red_c,nir_c),col='red')+
  geom_abline(aes(intercept=0,slope=1))

suspect <- tmp %>% lazy_dt() %>% 
  # sample_n(1e5) %>% 
  filter(nir_c < -0.05) %>% 
  as.data.table()
suspect %>% filter(nir_c == min(nir_c))
suspect %>% ggplot(data=., aes(date, nir))+
  geom_point()+
  geom_point(aes(date,nir_c))


out[veg_class==24] %>% 
  sample_n(1e4) %>% 
  ggplot(data=., aes(date, a_s))+
  geom_line()

out[veg_class==24] %>% 
  sample_n(1e4) %>% 
  ggplot(data=., aes(sz, ndvi))+
  # geom_point()
  geom_density2d_filled()+
  geom_hline(aes(yintercept=0),col='red')

out[ndvi_c > 0 & ndvi_c < 1] %>% 
  .[veg_class==24] %>% 
  # .[veg_class>=2 & veg_class<=5] %>% 
  .[date <= ymd("2017-12-31")] %>% 
  .[,.(val = mean(nirv,na.rm=TRUE), 
  val_c = mean(nirv_c, na.rm=TRUE)),by=.(date,vc)] %>% 
  ggplot(data=., aes(date, val))+
  geom_point()+geom_smooth(method='lm',se=F,color='black')+
  geom_point(aes(date,val_c),col='red')+
  geom_smooth(aes(date,val_c),method='lm',se=F,color='red')+
  facet_wrap(~vc)


out[date==min(date)] %>% 
  .[vc=="Inland Aquatic - freshwater, salt lakes, lagoons"] %>% 
  pull(ndvi_c) %>% hist
  ggplot(data=., aes(x,y,fill=ndvi))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()

out %>% 
  sample_n(1000) %>% 
  ggplot(data=., aes(red,nir))+
  geom_density_2d_filled()+
  geom_abline(aes(intercept=0,slope=1),col='red')
  
  
vec_dates <- sort(unique(dat$date))
for(i in 1:length(vec_dates)){
  if(i == 1){
    out <- fn_rot(dat[date==vec_dates[1]])
  }else{
   out <- rbind(out, fn_rot(dat[date==vec_dates[i]]))  
  }
  print(vec_dates[i])
}

out <- out %>% 
  .[,`:=`(ndvi=(nir-red)/(nir+red), 
          ndvi_c = (nir_c-red_c)/(nir_c+red_c))] %>% 
  .[,`:=`(nirv = (ndvi*nir), 
          nirv_c = (ndvi_c*nir))]

#
#



test <- out[date <= ymd("1982-02-01")][,.(x,y,date,vc,red,nir)]
test2 <- test[,lapply(FUN=fn_rot), by=date]

idx <- sample.int(dim(out)[1],1000)
out$nir_c[idx]-out2$nir_c[idx]

tmp %>% 
  group_by(date) %>% 
  summarize(val = mean(precip_anom_12mo,na.rm=T)) %>% 
  ungroup() %>% 
  filter(date>=ymd("2014-01-01")) %>% 
  ggplot(data=., aes(date, val))+
  geom_line()



ref <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_ref_201402.tif") %>% 
  as_tibble() %>% 
  set_names(c('x','y','band','refl'))
  
ref <- raster::stack("../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_ref_201402.tif") %>% 
  raster::as.data.frame(xy=TRUE) %>% 
  as.data.table()
dyn <- raster::stack("../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_201902.tif") %>% 
  raster::as.data.frame(xy=TRUE) %>% 
  as.data.table()

ref %>% 
  sample_frac(0.1) %>%
  ggplot(data=., aes(red,nir))+
  ggpointdensity::geom_pointdensity()+
  geom_vline(aes(xintercept=0.02),col='red')+
  geom_smooth(method='lm',se=F)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  scale_color_viridis_c()

dyn %>% 
  sample_frac(0.1) %>%
  ggplot(data=., aes(red,nir))+
  ggpointdensity::geom_pointdensity()+
  geom_vline(aes(xintercept=0.02),col='red')+
  geom_smooth(method='lm',se=F)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  scale_color_viridis_c()

ref %>% 
  ggplot(data=.,aes(x,y,fill=nir))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()

dyn %>% 
  ggplot(data=.,aes(x,y,fill=nir))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()



ref %>% 
  sample_frac(0.1) %>%
  ggplot(data=., aes(red,nir))+
  geom_point(color=scales::muted('blue'),alpha=0.1)+
  geom_vline(aes(xintercept=0.02),col='red')+
  geom_smooth(method='lm',se=F,color='black')+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  scale_color_viridis_c()+
  geom_point(data=dyn %>% sample_frac(0.1), 
             aes(red,nir),color=scales::muted('red'),alpha=0.1)+
  geom_smooth(data=dyn %>% sample_frac(0.1), 
              method='lm',se=F,color='purple')
  

ref %>% 
  # sample_frac(0.1) %>%
  ggplot(data=., aes(red,nir))+
  stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
  scale_fill_viridis_c()+
  geom_vline(aes(xintercept=0.02),col='red')+
  geom_smooth(method='lm',se=F)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  scale_x_continuous(limits=c(0,0.433))+
  scale_y_continuous(limits=c(0,0.433))+
  scale_color_viridis_c()


dyn %>% 
  ggplot(data=., aes(red,nir))+
  geom_density_2d_filled(h=c(0.01,0.01))+
  geom_vline(aes(xintercept=0.02),col='red')+
  geom_smooth(method='lm',se=F)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  scale_x_continuous(limits=c(0,0.433),expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.433),expand=c(0,0))+
  scale_color_viridis_c()


nobs <- 1e3
x1 <- rnorm(nobs,mean=0.25,sd=0.1); 
x2 <- rnorm(nobs,mean=0.2,sd=0.1);
eps1 <- rnorm(nobs,mean=0,sd=0.1); 
eps2 <- rnorm(nobs,mean=0.2,sd=0.1)
y2 <- 0.5 + 0.5*x1 + eps2
f1 <- lm(y1~y2); summary(f1)
y2_cor <- predict(f1)
y2_cor2 <- -coef(f1)[1] + coef(f1)[2]*y2

plot(y1~x1, pch=20, col='#00000055'); abline(lm(y1~x1))
points(y2~x1, pch=20, col='#F0000055'); abline(lm(y2~x1),col='red')
points(y2_cor~x1, pch=20, col='#F000F055'); abline(lm(y2_cor~x1),col='purple')
points(y2_cor2~x1, pch=20, col='blue'); abline(lm(y2_cor2~x1),col='blue')

min(x1)+ -(min(x1) - 0.02)

curve(sin(x), 0,2*pi)
curve(cos((pi*x/180)), 0,365)

mean(y1/x1)

fn <- function(pr, pn, tr, tn){
  # pr:ref red, pn: ref nir, tr: target red, tn: target nir
  # dp_idx <- which(pr==min(pr), arr.ind = T)
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
  pr_pp <- pr_p+xpoint
  pn_pp <- pn_p+xpoint
  out <- cbind(pr_pp, pn_pp)
  # return(out)
  return(out)
}
# Testing coord transform function ---- 
# x1 <- rnorm(nobs,mean=0.3,sd=0.055); eps1 <- rnorm(nobs,mean=0,sd=0.01); 
# y1 <- 0.5 + 0.05*x1 + eps1; pr <- x1; pn <- y1;
# plot(y1~x1,ylim=c(0,max(x1)),xlim=c(0,max(y1)),pch=20,cex=0.5);
# abline(lm(y1~x1),col='black'); abline(0,1,col='red')
# o <- fn(pr=x1,pn=y1) %>% as_tibble()
# points(o$pn_pp~o$pr_pp, data=o, col='blue',pch=20,cex=0.5)
# abline(lm(o$pn_pp~o$pr_pp), col='blue')

#########################################################################
#########################################################################
curve(tan(x),0,89.9*pi/180)

abline(0,mean(y1/x1),col='blue')
abline(lm(y1~x1),col='purple')

?tan
tan(sqrt(2)/1)
tan(45/180)
curve(tan(x/180),0,180)
tan(pi/4)
tan(pi*45/180)
atan(pi/4)*180

tan(45/180)
pi/180 # convert degrees to radians
180/pi # convert radians to degrees



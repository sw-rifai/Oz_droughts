library(tidyverse); 
library(data.table); setDTthreads(8) 
library(lubridate)

tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet")
sort(names(tmp))

vi <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet") %>% 
  as.data.table() %>% 
  .[is.infinite(ndvi_mcd)==F]


# Tmax
d1 <- vi[date>=ymd("1981-01-01")&date<=ymd("1989-12-31")] %>% 
  .[,.(ndvi_c,x,y,veg_class,date)] %>% 
  .[tmp[date>=ymd("1981-01-01")&date<=ymd("1989-12-31")][,.(tmax,id,mape,matmax,map,x,y,veg_class,date)],on=.(x,y,veg_class,date)] %>% 
  .[is.na(ndvi_c)==F] %>% 
  .[veg_class == 3]
d2 <- vi[date>=ymd("1990-01-01")&date<=ymd("1999-12-31")] %>% 
  .[,.(ndvi_c,x,y,veg_class,date)] %>% 
  .[tmp[date>=ymd("1990-01-01")&date<=ymd("1999-12-31")][,.(tmax,id,mape,matmax,map,x,y,veg_class,date)],on=.(x,y,veg_class,date)] %>% 
  .[is.na(ndvi_c)==F] %>% 
  .[veg_class == 3]
d3 <- vi[date>=ymd("2000-01-01")&date<=ymd("2009-12-31")] %>% 
  .[,.(ndvi_c,x,y,veg_class,date)] %>% 
  .[tmp[date>=ymd("2000-01-01")&date<=ymd("2009-12-31")][,.(tmax,id,mape,matmax,map,x,y,veg_class,date)],on=.(x,y,veg_class,date)] %>% 
  .[is.na(ndvi_c)==F] %>% 
  .[veg_class == 3]
d4 <- vi[date>=ymd("2010-01-01")&date<=ymd("2017-12-31")] %>% 
  .[,.(ndvi_c,x,y,veg_class,date)] %>% 
  .[tmp[date>=ymd("2010-01-01")&date<=ymd("2017-12-31")][,.(tmax,id,mape,matmax,map,x,y,veg_class,date)],on=.(x,y,veg_class,date)] %>% 
  .[is.na(ndvi_c)==F] %>% 
  .[veg_class == 3]

d1 <- d1 %>% filter(veg_class %in% 3) %>% filter(ndvi_c > 0) %>% filter(matmax >= 25) 
d1 %>% filter(veg_class %in% 2:3) %>%
  filter(matmax >= 25) %>% 
  sample_n(1000) %>% ggplot(data=., aes(tmax,ndvi_c))+geom_point()+geom_smooth()

fit1 <- nls_multstart(
  ndvi_c ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                     (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = d1 %>% sample_n(10000) %>% mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0.5, Hd = 1, Ha = 1, Topt = 273+25),
  start_upper = c(kopt = 0.7, Hd = 500, Ha = 2.5, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0.4, Hd = 200, Ha = 0.1, Topt = 285), 
  upper = c(kopt = 0.9, Hd=200, Ha=100, Topt=310))
fit2 <- nls_multstart(
  ndvi_c ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                     (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = d2 %>% sample_n(10000) %>% mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0.5, Hd = 1, Ha = 1, Topt = 273+25),
  start_upper = c(kopt = 0.7, Hd = 500, Ha = 2.5, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0.4, Hd = 200, Ha = 0.1, Topt = 285), 
  upper = c(kopt = 0.9, Hd=200, Ha=100, Topt=310))
fit3 <- nls_multstart(
  ndvi_c ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                     (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = d3 %>% sample_n(10000) %>% mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0.5, Hd = 1, Ha = 1, Topt = 273+25),
  start_upper = c(kopt = 0.7, Hd = 500, Ha = 2.5, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0.4, Hd = 200, Ha = 0.1, Topt = 285), 
  upper = c(kopt = 0.9, Hd=200, Ha=100, Topt=310))
fit4 <- nls_multstart(
  ndvi_c ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                     (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = d4 %>% sample_n(10000) %>% mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0.5, Hd = 1, Ha = 1, Topt = 273+25),
  start_upper = c(kopt = 0.7, Hd = 500, Ha = 2.5, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0.4, Hd = 200, Ha = 0.1, Topt = 285), 
  upper = c(kopt = 0.9, Hd=200, Ha=100, Topt=310))
coef(fit1)
coef(fit2)
coef(fit3)
coef(fit4)

fn <- function(Tk,kopt,Topt,Hd=200,Ha){kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                                                 (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt)))))))}
curve(fn(Tk=x+273.15,kopt=coef(fit1)["kopt"],Topt=coef(fit1)["Topt"], Hd=200,Ha=coef(fit1)["Ha"]),10,40, 
      ylim=c(0.2,0.8),xlab=expression(paste(Tmax~(degree*C))),ylab="NDVI")
curve(fn(Tk=x+273.15,kopt=coef(fit2)["kopt"],Topt=coef(fit2)["Topt"], Hd=200,Ha=coef(fit2)["Ha"]),10,40,add=T,col='blue')
curve(fn(Tk=x+273.15,kopt=coef(fit3)["kopt"],Topt=coef(fit3)["Topt"], Hd=200,Ha=coef(fit3)["Ha"]),10,40,add=T,col='purple')
curve(fn(Tk=x+273.15,kopt=coef(fit4)["kopt"],Topt=coef(fit4)["Topt"], Hd=200,Ha=coef(fit4)["Ha"]),10,40,add=T,col='red')
abline(v=coef(fit1)["Topt"]-273.15,col='black');
abline(v=coef(fit2)["Topt"]-273.15,col='blue');
abline(v=coef(fit3)["Topt"]-273.15,col='purple');
abline(v=coef(fit4)["Topt"]-273.15,col='red')
points(ndvi_c~tmax, data=d1 %>% sample_n(100),cex=0.5,pch=20,col='black')
points(ndvi_c~tmax, data=d1 %>% sample_n(100),cex=0.5,pch=20,col='blue')
points(ndvi_c~tmax, data=d1 %>% sample_n(100),cex=0.5,pch=20,col='purple')
points(ndvi_c~tmax, data=d1 %>% sample_n(100),cex=0.5,pch=20,col='red')

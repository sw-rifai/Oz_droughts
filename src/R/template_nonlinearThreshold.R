# library(raster); library(rasterVis)
library(arrow)
library(sf); library(stars)
library(tidyverse); 
library(data.table); library(lubridate);
library(dtplyr)
setDTthreads(threads=8)
nvis <- stars::read_stars("../data_general/NVIS/nvis51_majorVegClass_0p05.tif")

o <- stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/awap_tmax_daily_masked_1980_2019.nc", 
                      ncsub=cbind(start=c(1,1,1), count=c(381,681,1)))
bbox <- st_bbox(o)
nvis <- st_crop(nvis, bbox)
nvis <- st_warp(src=nvis, dest=o[,,], use_gdal = F)
st_get_dimension_values(nvis,1) %in% st_get_dimension_values(o,1)
st_get_dimension_values(nvis,2) %in% st_get_dimension_values(o,2)


codes <- readr::read_fwf("../data_general/NVIS/nvis51_majorVegClass_codes.txt", 
                         fwf_widths(c(2,100)), skip = 1) %>% 
  set_names(c("veg_class","veg_class_descrip")) %>% 
  mutate(vc = as.factor(veg_class_descrip)) 
# vc <- left_join(nvis, codes, by='veg_class')

oo <- inner_join(as_tibble(nvis) %>% set_names(c("lon","lat","vc")),
           as_tibble(o))
oo <- oo %>% filter(vc <= 12)

oc <- oo %>% select(lon,lat,vc) %>% as.data.table()



# sum(is.na(oo$vc))
# sum(is.na(oo$tmax))
# 
# plot(o,col=viridis::inferno(20), breaks='equal')
# 
# 
# 
# 
# plot(nvis)
# dim(nvis)
# dim(o)
# 
# 
# plot(st_get_dimension_values(nvis,1)~st_get_dimension_values(o,1))
# abline(0,1,col='red')
# 
# cbind(st_get_dimension_values(nvis,1),st_get_dimension_values(o,1)) %>% head


jj <- stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/awap_tmax_daily_masked_1980_2019.nc", 
                      ncsub=cbind(start=c(30,300,1), count=c(1,1,10000)))#14609)))
as_tibble(jj) %>% pull(tmax) %>% hist

library(lubridate)
(ymd("2010-12-31")-ymd("1981-01-01"))/365


attmax <- c(
  stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/tmax_gt35.nc") %>% 
  set_names("t35"), 
stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/tmax_gt36.nc") %>% 
  set_names("t36"),
stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/tmax_gt37.nc") %>% 
  set_names("t37"),
stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/tmax_gt38.nc") %>% 
  set_names("t38"),
stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/tmax_gt39.nc") %>% 
  set_names("t39"),
stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/tmax_gt40.nc") %>% 
  set_names("t40"),
stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/tmax_gt41.nc") %>% 
  set_names("t41"),
stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/tmax_gt42.nc") %>% 
  set_names("t42"),
stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/tmax_gt43.nc") %>% 
  set_names("t43"),
stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/tmax_gt44.nc") %>% 
  set_names("t44"),
stars::read_ncdf("../data_general/clim_grid/awap/AWAP/daily/tmax_gt45.nc") %>% 
  set_names("t45")
    )

attmax <- as_tibble(attmax) %>% as.data.table()
attmax <- attmax[is.na(t35)==F]
attmax <- attmax[,`:=`(month=month(time))]
attmax <- attmax %>% 
  group_by(lon,lat) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup() %>% 
  as.data.table()



jj[id==1]

test <- jj[,`:=`(month=month(time))] %>% 
  .[month==11 & id==1]

vec <- test %>% select(starts_with("t")) %>% select(-time) %>% .[1] 
t(vec)
d <- data.frame(days=unname(t(vec)), degree=35:45)
# d <- data.frame(days=rep(0,11), degree=35:45)

microbenchmark::microbenchmark(
nls(days ~ B0*exp(B1*-(degree-35)), 
    data=d,
    # data = list(degree=35:45,
    #             days=as.numeric(unname(t(vec)))), 
    start = list(B0=max(d$days),B1=1), 
    algorithm = 'port'), 
  unit='us'
)
microbenchmark::microbenchmark(
  lm( log(I(days+0.05)) ~ I(degree-35), data=d) %>% coef %>% exp
)
library(RcppArmadillo)
microbenchmark::microbenchmark(
  exp(coef(fastLm(X=cbind(1,d$degree-35),y=log(d$days+0.05)))), 
  unit='us'
)
microbenchmark::microbenchmark(
  exp(coef(fastLm(log(I(days+0.05))~I(degree-35),data=d))), 
  unit='us'
)

RcppArmadillo::fastLm(X=cbind(1,d$degree-35),y=log(d$days+0.05))


nls(log(days) ~ log(B0) - B1*(degree-35), 
    data=d,
    # data = list(degree=35:45,
    #             days=as.numeric(unname(t(vec)))), 
    start = list(B0=max(d$days),B1=1), 
    algorithm = 'port')

lm( log(I(days+0.01)) ~ I(degree-35), data=d) %>% coef %>% exp

lm( log(I(days+1)) ~ I(degree-35), data=d) %>% coef


plot(days~degree,data=d)
curve(4.2*exp(0.44*-(x-35)), 35,45, add=T)
curve(3.83*exp(0.58*-(x-35)), 35,45, col='red',add=T)

c()
rnorm(10,mean=rep(0.5,10))

curve( (1-x)/(1+x), -0.99, 10)
B <- 2
curve(1 - exp(B*x)/(exp(B*x)+1), -5,5)

curve((1+exp(-x))**-1,-5,5)




library(tidyverse); library(lubridate); 
library(stars); library(data.table);
library(dtplyr); library(patchwork); library(RcppRoll)
# Load data ---------------------------------------------------------------
tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet") %>% 
  as.data.table()


tmp[ndvi_anom <= -0.25 & year == 2019] %>% pull(id)
d <- tmp[id%in%c(47151,45848,45309,45309,9960,41681)][year>2000]

d %>% ggplot(data=., aes(date, ndvi_anom))+geom_point()+
  geom_smooth()
ggplot(data=d, aes(tmax_anom_3mo, ndvi_anom))+geom_point()+
  geom_smooth(method='lm')


tmp[id %in% sample(unique(tmp[ndvi_anom_sd <= -2 & year == 2019]$id),10)] %>% 
  .[is.na(ndvi_anom)==F & year>2000 & date <= ymd("2019-09-01")] %>% 
  ggplot(data=., aes(tmax_anom, ndvi_anom_sd))+
  geom_point(alpha=0.1)+
  geom_smooth(col='red',se=F,method='gam',formula=y~s(x))+
  stat_smooth(method='nls', 
              formula='y ~ d + 1/(1 + exp(-a * (x - b)))', 
              method.args = list( #algorithm='port',
                start=list(a=1, 
                           b=1, 
                           d=1)),
              se=FALSE)+
  facet_wrap(~id)
tmp[id==4647][,.(vc, ndvi_anom)][is.na(ndvi_anom)==F]


d[is.na(ndvi_anom)==F] %>% 
 ggplot(data=., aes(tmax_anom_3mo, ndvi_anom,color=as.factor(id)))+
  geom_point(alpha=0.1)+
  stat_smooth(method='nls', 
              formula='y ~ d + 1/(1 + exp(-a * (x - b)))', 
              method.args = list( #algorithm='port',
                                 start=list(a=1, 
                                            b=1, 
                                            d=1)),
              se=FALSE)+
  facet_wrap(~id)

fn <- nls(y ~ 1/(1 + exp(-a * (x - b))), 
          start = list(a=1,b=1), 
          data=(data.frame(y=d$ndvi_anom, 
                           x=d$tmax_anom)), 
          algorithm = 'port')
fn %>% summary


fn <- nls(y~d+-1*sigmoid(x,a,b), 
          start = list(a=1,b=1,d=1), 
          data=(data.frame(y=d$ndvi_anom, 
                           x=d$tmax_anom)), 
          algorithm = 'port')
fn %>% summary



nls(ndvi_anom ~ 1 - (1+exp(tmax_anom_3mo))**alpha, 
    data=d,
    start = list(alpha=1), 
    algorithm = 'port')
plot(ndvi_anom~tmax_anom_3mo, data=d); 
curve(1 - (1+exp(x))**0.029,-1.5,2.5,add=T,col='red')

f <- nls(ndvi_anom ~ 1 - (1+exp(beta*tmax_anom_3mo))**alpha, 
    data=d,
    start = list(alpha=1,beta=1), 
    algorithm = 'port')
fn <- function(x)predict(f,newdata = data.frame(tmax_anom_3mo=x))
fn <- Vectorize(fn)
plot(ndvi_anom~tmax_anom_3mo, data=d); 
curve(fn(x),-3,3,add=T,col='red')


f <- nls(ndvi_anom ~ A + (K-A)/((C+Q*exp(-B*tmax_anom_3mo))^(1/V)), 
         data=d,
         start = list(A=0.05, # lower asymp
                      K=-0.3, # upper asymp
                      C=1,
                      Q=4,
                      B=1,
                      V=1), 
         algorithm = 'port')
fn <- function(x)predict(f,newdata = data.frame(tmax_anom_3mo=x))
fn <- Vectorize(fn)
plot(ndvi_anom~tmax_anom_3mo, data=d); 
curve(fn(x),-3,3,add=T,col='red')

# Generalized logistic function
A <- 0.075 # lower asymp
K <- -0.25 # upper asymp
C <- 1 # 
Q <- 4 # related to Y(0)
B <- 1
V <- 1
curve( A+(K-A)/(C+Q*exp(-B*x))**(1/V),-5, 5,add=F,col='red')
points(ndvi_anom~tmax_anom_3mo, xlim=c(-3,3),data=d);

mm <- function(V,S,Km){(V*S)/(Km+S)}
curve(mm(1,x,3),-3,3)

library(pracma)


x <- seq(-6, 6, length.out = 101)
y1 <- sigmoid(x)*-1
y2 <- sigmoid(x, a = 2)*-1
## Not run:
plot(x, y1, type = "l", col = "darkblue",
     xlab = "", ylab = "", main = "Sigmoid Function(s)")
lines(x, y2, col = "darkgreen")
grid()
## End(Not run)
# The slope in 0 (in x = b) is a/4
# sigmf with slope 1 and range [-1, 1].
sigmf <- function(x) 2 * sigmoid(x, a = 2) - 1


fn <- nls(y~d+-1*sigmoid(x,a,b), 
    start = list(a=1,b=1,d=1), 
    data=(data.frame(y=d$ndvi_anom, 
              x=d$tmax_anom)), 
    algorithm = 'port')
plot(d$ndvi_anom~predict(fn))
fn <- function(x) predict(fn,newdata = data.frame(tmax_anom_3mo=x))
fn <- Vectorize(fn)
plot(ndvi_anom~tmax_anom_3mo, data=d); 
curve(coef(fn)['d']-1*sigmoid(x = x,a = coef(fn)['a'], b = coef(fn)['b']),-3,3,add=T,col='red')


fn <- lm(y~1+I(x)+I(x**2), data=data.frame(y=d$ndvi_anom, x=d$tmax_anom_3mo))
fnv <- Vectorize(function(x)predict(fn, newdata=data.frame(x)))
curve(fnv, -3,3,add=T,col='red')

          

curve(sigmoid(-(x-2), 1, 0),-5,5,ylim=c(0,1)); abline(v=2,h=0.5)



library(mgcv)
jj <- tmp[vc=="Eucalypt Tall Open Forests"][id %in% 
            sample(unique(tmp[ndvi_anom_sd <= -2 & year == 2019]$id),1000)] %>% 
  .[is.na(ndvi_anom)==F & year>2000 & date <= ymd("2019-09-01")] %>% 
  as_tibble() %>% 
  mutate(idf = as.factor(id))
m1 <- gam(ndvi_anom_sd~
            # s(pet_anom)+
            s(pet_anom_3mo)+
            # s(pet_anom_12mo)+
            # s(precip_anom_3mo)+
            # s(precip_anom_12mo)+
            # s(precip_anom_36mo)+
            s(precip_anom_3mo, precip_anom_12mo, precip_anom_36mo)+
            # s(tmax_anom)+
            # s(tmax_anom_3mo)+
            # s(tmax_anom_12mo)+
            s(idf,bs='re'),
            # s(x,y),
          data=jj %>% filter(month==4), select=TRUE, method='REML')
summary(m1)
plot(m1)

jj %>% as_tibble() %>% 
  mutate(pred = predict(m1)) %>% 
  group_by(vc) %>% 
  summarize(r2 = cor(pred,ndvi_anom_sd)**2) %>% 
  ungroup() %>% 
  View


  ggplot(data=., aes(tmax_anom, ndvi_anom_sd))+
  geom_point(alpha=0.1)+
  geom_smooth(col='red',se=F,method='gam',formula=y~s(x))+
  stat_smooth(method='nls', 
              formula='y ~ d + 1/(1 + exp(-a * (x - b)))', 
              method.args = list( #algorithm='port',
                start=list(a=1, 
                           b=1, 
                           d=1)),
              se=FALSE)+
  facet_wrap(~id)



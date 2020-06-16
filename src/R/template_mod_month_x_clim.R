library(tidyverse)
library(data.table)
library(lubridate); 
library(mgcv); library(mgcViz); library(gratia)
###########################################################################
tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id","season","precip_anom", 
                             "precip_anom_3mo","precip_anom_36mo",
                             "precip_anom_12mo","map", 
                             "tmax","tmax_anom","tmax_anom_sd", "matmax",
                             "tmin",
                             "vpd15","vpd15_anom","vpd15_anom_sd","mavpd15",
                             "vpd15_u",
                             "pet","mapet","pet_anom","pet_anom_3mo","pet_u","pet_sd",
                             "pet_anom_sd",
                             "pe","mape",
                             "ndvi_anom",
                             "ndvi_anom_12mo","ndvi_anom_sd",
                             "ndvi_mcd",
                             'vc','veg_class',
                             'month',
                             "x", "y", "year")) %>% 
   as.data.table() %>% 
  .[is.infinite(mape)==F]
unique(tmp[,.(vc,veg_class)]) %>% View
tmp <- tmp[order(x,y,date)][,tmean := (tmax+tmin)/2]
tmp <- tmp[order(x,y,date)][, ndvi_3mo := frollmean(ndvi_mcd,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmax_3mo := frollmean(tmax,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmean_3mo := frollmean(tmean,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmin_3mo := frollmean(tmin,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmax_12mo := frollmean(tmax,n = 12,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pet_3mo := frollmean(pet,n = 3,fill = NA,align='center'), by=.(x,y)]



vec_ids <- unique(tmp[veg_class %in% c(2:3)][,(id)])

dat1 <- tmp  %>% 
  # .[[id %in% sample(vec_ids,500)]] %>% 
  .[y> -35 & y < -23.5] %>% 
  .[ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5] %>% 
  .[ndvi_mcd > 0] %>% 
  # .[mape > 1 & mape < 1.5] %>%
  .[year>=2000]
dim(dat1)
vec_ids <- unique(dat1[,(id)])
# dat2 <- dat1[id %in% sample(vec_ids,100)] %>% as_tibble() 
# dim(dat2)  

fit <- bam(ndvi_3mo~
             te(mape, tmean)+
             # te(hydro_year,tmean_3mo,k=c(3,4),bs='gp')+
             # s(precip_anom_12mo,by=map,bs='gp')+
             # s(id, bs='re')
             s(x,y,fx = F)
           , 
           data=dat1 %>% mutate(ddate=decimal_date(date), 
                                id = as.factor(id)), 
           discrete = T, select=TRUE)
summary(fit)
print(plot(getViz(fit),allTerms=TRUE),pages=1)
plot(sm(getViz))

plotSlice(sm(getViz(fit),1),
          fix=list("mape"=seq(0.1,2,length.out = 8)))+
  l_fitRaster()+
  l_fitContour()+
  scale_fill_gradient2()

plot(fit,scheme = 2)
plotRGL(sm(getViz(fit),1))
o <- evaluate_smooth(fit, "te(mape,tmean)")
o %>% 
  mutate(epoch = cut_interval(mape,n = 4)) %>%
  group_by(epoch,tmean) %>% 
  summarize(est = mean(est,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(tmean,est,color=as.factor(epoch)))+
  geom_line()+
  scale_color_viridis_d()+
  # facet_wrap(~epoch,ncol = 1)+
  theme(legend.position = 'bottom')

appraise(fit)
# Does tmax impose a benefit as time increases? 
fit <- bam(ndvi_mcd~te(year,tmax_3mo,mape,k=c(3,4,3)), 
           data=dat1 %>% mutate(ddate=decimal_date(date)), 
           discrete = T, select=TRUE)



dat2 %>% 
  as_tibble() %>% 
  filter(year>=2000) %>% 
  # select(ndvi_3mo, tmax_3mo,mape) %>%  
  ggplot(data=., aes(tmin_3mo,ndvi_3mo,color=as.factor(id)))+
  # ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_point(alpha=0.1)+
  geom_smooth(se=F)

dat2 <- dat1[id==42613]

fit <- nls_multstart(
  ndvi_3mo ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                       (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = dat2 %>%
    mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0, Hd = 1, Ha = 1, Topt = 290),
  start_upper = c(kopt = 1, Hd = 900, Ha = 300, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0, Hd = 0, Ha = 0, Topt = 280))

fit
summary(fit)

vec_cols <- viridis::inferno(6)
dat2 %>% 
  ggplot(data=., aes(tmax,ndvi_3mo,color=as.factor(id)))+
  # ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_point(alpha=0.5,col='black')+
  geom_smooth(se=F)+
  geom_vline(aes(xintercept=coef(fit)["Topt"]-273.15),col=vec_cols[1])+
  geom_hline(aes(yintercept=coef(fit)["kopt"]),col=vec_cols[1])
  
fit <- nlme(
  ndvi_3mo ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                       (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = dat2 %>%
    mutate(Tk=tmax+273.15) %>% 
    filter(is.na(ndvi_3mo)==F),
  fixed = list(Hd~1, Ha~1, Topt~1, kopt~1), #Hd + Ha + Topt + kopt ~ 1,
  # random = kopt ~ 1,
  start = c(kopt = 0, Hd = 1, Ha = 1, Topt = 290)
  # start_upper = c(kopt = 1, Hd = 900, Ha = 300, Topt = 320),
  # supp_errors = 'Y',
  # na.action = na.omit,
  #convergence_count = 500,
  # lower = c(kopt = 0, Hd = 0, Ha = 0, Topt = 280))
)
# Asym-Drop*exp(-exp(lrc)*x^pwr)
curve(SSweibull(x, Asym = 0.25,Drop = -0.7,lrc = -15.5,pwr = 2), 0,40)
points(ndvi_3mo~tmax_3mo, data=dat2)
curve(SSweibull(x, Asym = 0.3,Drop = -0.5,lrc = -15.5,pwr = 5), 0,40,add=T,
      col='red')

lme4::nlmer(ndvi_3mo~SSweibull(tmax_3mo,Asym,Drop,lrc, pwr) ~ Drop|id, 
            data=dat2,
            start=c(Asym=0.2, Drop=-0.5,lrc=-15.5,pwr=5))

out <- nls_multstart(ndvi_3mo~SSweibull(tmax_3mo,Asym,Drop,lrc, pwr), 
            data=dat2,
            iter=1000,
            na.action = na.omit,
            start_lower=c(Asym=0.2, Drop=-0.52,lrc=-15.5,pwr=5), 
            start_upper=c(Asym=0.3, Drop=-0.6,lrc=0,pwr=10), 
            lower = c(Asym=0.2,Drop=-0.4))
out

start_lower = c(kopt = 0, Hd = 1, Ha = 1, Topt = 290),
start_upper = c(kopt = 1, Hd = 900, Ha = 300, Topt = 320),
# supp_errors = 'Y',
na.action = na.omit,
#convergence_count = 500,
lower = c(kopt = 0, Hd = 0, Ha = 0, Topt = 280)


train <- tmp[veg_class%in%c(2:3)]  %>% 
  .[y> -35 & y < -23.5] %>% 
  .[ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5] %>% 
  # .[hydro_year >= min_year & hydro_year <= max_year] %>% 
  .[,`:=`(id=as.factor(id))] %>% 
  .[sample(.N,50000)]

train <- train %>% lazy_dt() %>% 
  mutate(x_p12 = (precip_anom_12mo)/(precip_anom_12mo+map), 
         # x_v = (vpd15_anom - vpd15_u)/(vpd15_anom+vpd15_u), 
         x_v = (vpd15-vpd15_u)/(vpd15+vpd15_u),
         x_p3 = (precip_anom_3mo-map)/(precip_anom_3mo+map), 
         x_pet3 = -(pet_anom_3mo - pet_u)/(pet_u) - 1) %>% 
  as_tibble()

m1 <- bam(ndvi_anom_sd~ s(pet_anom),
          data=train %>% filter(season=='SON'), 
          select=T, discrete = T, method='fREML')
plot(getViz(m1))

m2 <- bam(ndvi_anom_sd~ 
            x_v*season + x_p12*season + x_p3*season + 
            s(x_pet3,by=season, k=5),
          data=train, 
          select=T, discrete = T, method='fREML')
summary(m2)
print(plot(getViz(m2),allTerms=T),pages=1)

m3 <- bam(ndvi_anom_sd~ 
            # s(month, by=pet_anom,k=5)+
            ti(month,pet_anom,pet_u,k=5)+
            s(month,by=x_p3,bs='cc',k=5)+
            s(month,by=x_p12, bs='cc', k=5)+
            # s(month,by=x_v, bs='cc', k=5)+
            s(month,by=x_v, bs='cc', k=5)+
            s(month,by=vc,bs='fs', xt=list(bs='cc')),
            # s(mandvi,ndvi_u)+
            # s(x,y,fx = TRUE, k=60),
          # s(x_v,by=season,bs='fs') + 
          # x_p12*season + x_p3*season + 
          # t2(x_pet3,season, k=5, bs='fs', xt=list(bs='cr'), full=TRUE),
          data=train, 
          select=T, discrete = T, method='fREML')
summary(m3)
print(plot(getViz(m3),allTerms=T),pages=1)
plot(m3, scale=0, pages=1)
appraise(m3)

fn <- function(x,x_u) x/(x_u)
curve(fn(x,50),-50,50,col=viridis::inferno(6)[1])
curve(fn(x,100),-50,50,add=T,col=viridis::inferno(6)[2])
curve(fn(x,150),-50,50,add=T,col=viridis::inferno(6)[4])
curve(fn(x,200),-50,50,add=T,col=viridis::inferno(6)[5])
curve(fn(x,200),-50,150,add=F,col=viridis::inferno(6)[3])

fn <- function(x,x_u,x_sd) x/((x-x_sd)*sqrt(2*pi))
fn <- Vectorize(fn)
curve(fn(x,x_u=100,x_sd=25),50,200,col=viridis::inferno(6)[1])

curve(fn(x,100),50,200,add=T,col=viridis::inferno(6)[2])
curve(fn(x,150),50,200,add=T,col=viridis::inferno(6)[4])
curve(fn(x,200),50,200,add=T,col=viridis::inferno(6)[5])
curve(fn(x,200),50,200,add=F,col=viridis::inferno(6)[3])


plot(getViz(m1))
fn <- function(x,x_u) (x-x_u)**2/(x_u+x+x**2) 
curve(fn(x,x_u=100),-100,200,col=viridis::inferno(6)[1])

# Ricker
m1 <- bam(ndvi_anom_sd~ s(pet,k=5),
          data=train %>% filter(season=='SON'), 
          select=T, discrete = T, method='fREML')
plot(getViz(m1))
dev.new()
fn <- function(x,a,b) a*x*exp(-b*x)
curve(fn(x,a=100,b=1/50),0,200,col=viridis::inferno(6)[1])

train %>% 
  mutate(val = (1/pet_u)*pet*exp((-1/sqrt((pet_anom-pet_u)**2)))*pet) %>% 
  ggplot(data=., aes(pet_anom,val))+geom_point()+
  geom_smooth()+
  geom_vline(aes(xintercept=0),col='red')


train %>% 
  filter(year>=1990) %>% 
  mutate(x = pet_anom/pet_u) %>% 
  ggplot(data=., aes(pet,ndvi_anom_sd))+
  ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_smooth()+
  # stat_smooth(method='nls', formula=y~alpha*x*exp(-beta*x),
  #             method.args=list(start=list("alpha"=1,
  #                                         "beta"=1)),
  #             se=F,col='yellow',lwd=1)+
  geom_vline(aes(xintercept=0),col='red')

nls(y~alpha*x*exp(-beta*x), 
    data=list(y=train$ndvi_anom_sd, 
              x=train$pet_anom_sd), 
    start=list("alpha"=3.5,
               "beta"=1), 
    # upper=list("beta"=0), 
    algorithm = 'port')


# Gaussian 
nls(y~(1/(sigma*sqrt(2*pi)))*exp(-0.5*((x-u)/sigma)**2), 
    data=list(y=train$ndvi_anom_sd, 
              x=train$pet_anom_sd), 
    start=list("u"=0,
               "sigma"=1),
    lower=c(-1,0.001),
    upper=c(0.1,1),
    # lower=list("u"=-5,
    #            "sigma"=0.1),
    # upper=list("u"=0,
    #            "sigma"=5),
    algorithm = 'port')
fn <- function(x,sigma=0.1,u=-0.25)(1/(sigma*sqrt(2*pi)))*exp(-0.5*((x-u)/sigma)**2)
curve(fn(x),-5,5)


fn <- function(x,u,sigma)(1/(sigma*sqrt(2*pi)))*exp(-0.5*((x-u)/sigma)**2)
train %>% 
  mutate(x = pet_anom/pet_u) %>% 
  ggplot(data=., aes(x,ndvi_anom_sd))+
  ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_smooth()+
  stat_smooth(method='nls', 
              formula=y~fn(x,u,sigma),
              method.args=list(start=list("sigma"=0.001,
                                          "u"=-0.1)),
              se=F,col='yellow',lwd=1)+
  geom_vline(aes(xintercept=0),col='red')



#
test <- tmp[veg_class%in%c(2:3)]  %>% 
  .[y> -35 & y < -23.5] %>% 
  .[ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5]
vec_ids <- unique(test[,(id)])
test[id %in% sample(vec_ids,10)] %>% 
  filter(ndvi_mcd > 0) %>% 
  ggplot(data=., aes(vpd15,ndvi_mcd,color=as.factor(id)))+
  # ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_point(alpha=0.1)+
  geom_smooth(se=F)


# Generalized logistic function
fn <- function(A,K,C,Q,B,v,x) A + (K-A)/(C+Q*exp(-B*x))**(1/v)
curve(fn(1,0,1,0.5,0.1,1,x),-250,250)
train %>% 
  mutate(x = pet_anom/pet_u) %>% 
  ggplot(data=., aes(x,ndvi_anom_sd))+
  ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_smooth()+
  stat_smooth(method='nls', 
              formula=y~fn(x,u,sigma),
              method.args=list(start=list("sigma"=0.001,
                                          "u"=-0.1)),
              se=F,col='yellow',lwd=1)+
  geom_vline(aes(xintercept=0),col='red')


# SSlogis(input, Asym, xmid, scal)
curve(SSlogis(x,-1,0.5,1),-5,5)
train %>% 
  mutate(x = tmax) %>% 
  ggplot(data=., aes(x,ndvi_anom_sd))+
  ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_smooth()+
  stat_smooth(method='nls', 
              formula=y~SSlogis(x,Asym,xmid,scal),
              method.args=list(start=list("Asym"=-3.5,
                                          "scal"=0.01,
                                          "xmid"=0)),
              se=F,col='yellow',lwd=1)+
  geom_vline(aes(xintercept=0),col='red')


m <- nls(y~SSlogis(x, Asym, xmid, scal), 
    data=list(y=train$ndvi_anom_sd, 
              x=train$pet_anom_sd), 
    start=list("Asym"=-1.5,
               "xmid"=0,
               "scal"=0.1),
    # lower=c(-1,0.001),
    # upper=c(0.1,1),
    # lower=list("u"=-5,
    #            "sigma"=0.1),
    # upper=list("u"=0,
    #            "sigma"=5),
    algorithm = 'port')
summary(m)
plot(m)
predict(m, newdata=tibble(x=seq(-3,3,length.out = 100))) %>% plot


fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
            data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
summary(fm1)
fm2 <- update(fm1, random = pdDiag(Asym + lrc ~ 1))
summary(fm2)
bbmle::AICctab(fm1,fm2)



# 
test <- tmp[veg_class%in%c(2:3)]  %>% 
  .[y> -35 & y < -23.5] %>% 
  .[ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5] %>% 
  .[ndvi_mcd > 0]
vec_ids <- unique(test[,(id)])
test[id %in% sample(vec_ids,10)] %>% 
  filter(ndvi_mcd > 0) %>% 
  ggplot(data=., aes(vpd15,ndvi_3mo,color=as.factor(id)))+
  # ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_point(alpha=0.1)+
  geom_smooth(se=F)

dat <- test[id %in% sample(vec_ids,3)] %>% as_tibble()
dat %>% 
  filter(id==41027) %>%
  ggplot(data=., aes(pet,ndvi_mcd,color=as.factor(id)))+
  # ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_point(alpha=0.1)+
  geom_smooth(se=F)


dat1 <- dat %>% filter(id==41027)
  
library(nls.multstart)
fit_med80 <- nls_multstart(
  ndvi_mcd ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                      (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
                   data = dat1 %>% filter(year<= 1989) %>% 
    mutate(Tk=tmax+273.15),
            iter = 1000,
            start_lower = c(kopt = 0, Hd = 1, Ha = 1, Topt = 290),
            start_upper = c(kopt = 1, Hd = 900, Ha = 300, Topt = 320),
                 # supp_errors = 'Y',
                 na.action = na.omit,
                         #convergence_count = 500,
          lower = c(kopt = 0, Hd = 0, Ha = 0, Topt = 280))
fit_med90 <- nls_multstart(
  ndvi_mcd ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                       (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = dat1 %>% filter(year< 2000 & year>= 1990) %>% 
    mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0, Hd = 1, Ha = 1, Topt = 290),
  start_upper = c(kopt = 1, Hd = 900, Ha = 300, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0, Hd = 0, Ha = 0, Topt = 280))
fit_med00 <- nls_multstart(
  ndvi_mcd ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                       (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = dat1 %>% filter(year< 2010 & year>= 2000) %>% 
    mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0, Hd = 1, Ha = 1, Topt = 290),
  start_upper = c(kopt = 1, Hd = 900, Ha = 300, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0, Hd = 0, Ha = 0, Topt = 280))
fit_med10 <- nls_multstart(
  ndvi_mcd ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                       (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = dat1 %>% filter(year>= 2010) %>% 
    mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0, Hd = 1, Ha = 1, Topt = 290),
  start_upper = c(kopt = 1, Hd = 900, Ha = 300, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0, Hd = 0, Ha = 0, Topt = 290))

fit_med
vec_cols <- viridis::inferno(5)
dat1 %>% 
  ggplot(data=., aes(tmax,ndvi_3mo,color=as.factor(id)))+
  # ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_point(alpha=0.5,col='black')+
  geom_smooth(se=F)+
  geom_vline(aes(xintercept=coef(fit_med80)["Topt"]-273.15),col=vec_cols[1])+
  geom_hline(aes(yintercept=coef(fit_med80)["kopt"]),col=vec_cols[1])+
  geom_vline(aes(xintercept=coef(fit_med90)["Topt"]-273.15),col=vec_cols[2])+
  geom_hline(aes(yintercept=coef(fit_med90)["kopt"]),col=vec_cols[2])+
  geom_vline(aes(xintercept=coef(fit_med00)["Topt"]-273.15),col=vec_cols[3])+
  geom_hline(aes(yintercept=coef(fit_med00)["kopt"]),col=vec_cols[3])+
  geom_vline(aes(xintercept=coef(fit_med10)["Topt"]-273.15),col=vec_cols[4])+
  geom_hline(aes(yintercept=coef(fit_med10)["kopt"]),col=vec_cols[4])





fit <- nls(
  ndvi_mcd ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                       (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = dat1 %>% filter(year>= 2010) %>% 
    mutate(Tk=tmax+273.15), 
  start=list("kopt"=0.5,
             "Hd"=100, 
             "Ha"=100,
             "Topt"=273.15+20))
  # iter = 1000,
  # start_lower = c(kopt = 0, Hd = 1, Ha = 1, Topt = 290),
  # start_upper = c(kopt = 1, Hd = 900, Ha = 300, Topt = 320),
  # # supp_errors = 'Y',
  # na.action = na.omit,
  # #convergence_count = 500,
  # lower = c(kopt = 0, Hd = 0, Ha = 0, Topt = 290))

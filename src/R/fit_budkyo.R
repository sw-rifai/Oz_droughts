library(tidyverse)
library(data.table); setDTthreads(threads = 8)
library(lubridate); 
library(mgcv); library(mgcViz); library(gratia)
library(dtplyr)
###########################################################################
tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id","season",
                             "precip",  "precip_anom", 
                             "precip_anom_3mo","precip_anom_36mo",
                             "precip_anom_12mo","map", 
                             "precip_12mo",
                             "tmax","tmax_anom","tmax_anom_sd", "matmax",
                             "tmin",
                             "vpd15","vpd15_anom","vpd15_anom_sd","mavpd15",
                             "vpd15_u",
                             "pet","mapet","pet_anom","pet_anom_3mo","pet_u","pet_sd",
                             "pet_anom_sd", "pet_12mo",
                             "pe","mape",
                             "ndvi_anom",
                             "ndvi_anom_12mo","ndvi_anom_sd",
                             "ndvi_mcd",
                             'vc','veg_class',
                             'month',
                             "x", "y", "year")) %>% 
  as.data.table() %>% 
  .[is.infinite(mape)==F]
# unique(tmp[,.(vc,veg_class)]) %>% View
tmp <- tmp[order(x,y,date)][,tmean := (tmax+tmin)/2]
tmp <- tmp[order(x,y,date)][, ndvi_3mo := frollmean(ndvi_mcd,n = 3,fill = NA,align='center',na.rm=TRUE), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmax_3mo := frollmean(tmax,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmean_3mo := frollmean(tmean,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmin_3mo := frollmean(tmin,n = 3,fill = NA,align='center'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, tmax_12mo := frollmean(tmax,n = 12,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pet_3mo := frollmean(pet,n = 3,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, precip_3mo := frollmean(precip,n = 3,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, vpd15_3mo := frollmean(vpd15,n = 3,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[,`:=`(pe_3mo = precip_3mo/pet_3mo)]



# NDVI ~ PE through time --------------------------------------------------
fn_xmid <- function(indat){
  fit <- nls_multstart(ndvi_3mo ~ SSlogis(pe_3mo, Asym, xmid, scal),
                       data = indat[sample(.N, 5000)],
                       iter = 20,
                       start_lower = c(Asym=0.5, xmid=0.1, scal=0.01),
                       start_upper = c(Asym=1, xmid=0.9, scal=1),
                       # supp_errors = 'Y',
                       na.action = na.omit)
  bfit <- broom::tidy(fit)
  bfit$hydro_year <- unique(indat$hydro_year); 
  bfit$season <- unique(indat$season)
  return(bfit)  
}

xdat <- tmp[hydro_year %in% c(1983:2019)] %>% 
  # .[season=="DJF"] %>%
  .[season=="SON"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_mcd>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  # .[sample(.N,5000)] %>% 
  .[,`:=`(Tk=tmax+273.15)] %>% 
  .[,fn_xmid(.SD),by=.(season,hydro_year)]

xdat %>% 
  as_tibble() %>% 
  # filter(p.value < 0.1) %>% 
  select(season,hydro_year,term,estimate) %>% 
  spread(key = term, value=estimate) %>% 
  filter(!hydro_year %in% c(2001)) %>%
  # filter(hydro_year >= 2000) %>%
  ggplot(data=., aes(hydro_year, xmid))+
  geom_point()+
  geom_smooth(method='lm')+
  geom_label(aes(label=hydro_year))

xdat %>% 
  as_tibble() %>% 
  # filter(p.value < 0.1) %>% 
  select(season,hydro_year,term,estimate) %>% 
  spread(key = term, value=estimate) %>% 
  # filter(!hydro_year %in% c(2012)) %>%
  # filter(hydro_year >= 2000) %>%
  ggplot(data=., aes(Asym, xmid))+
  geom_point()+
  # geom_smooth(method='lm')+
  geom_label(aes(label=hydro_year))


# Plot Budkyo -------------------------------------------------------------
tmp[hydro_year %in% c(2015:2014)] %>% 
  # .[season=="DJF"] %>%
  .[season=="SON"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_mcd>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  # .[veg_class %in% c(1,2)] %>% 
  .[is.na(veg_class)==F] %>% 
  .[pe_3mo <= 2] %>% 
  .[sample(.N,50000)] %>%
  .[,`:=`(Tk=tmax+273.15)] %>% 
  ggplot(data=., aes(pe_3mo,ndvi_3mo,color=as_factor(hydro_year)))+
  geom_point(alpha=0.1)+
  geom_smooth(se=F)+
  # geom_smooth(method='nls', se=F, #color='purple',          #
  #             formula = y~SSlogis(input = x,Asym,xmid,scal),
  #             method.args=list(
  #               start=list("Asym"=0.8,
  #                          "xmid"=0.5,
  #                          "scal"=1)))+
  scale_color_viridis_d()


  # geom_smooth()+
  # geom_smooth(method='nls', se=F, color='red',          # WORKS!
  #             formula = y~SSmicmen(x,Vm,K),
  #             method.args=list(
  #               start=list("Vm"=1,
  #                        "K"=1)))+
  geom_smooth()+
  geom_smooth(method='nls', se=F, color='orange',          # WORKS!
              formula = y~SSweibull(x,Asym,Drop,lrc,pwr),
              method.args=list(
                start=list("Asym"=0.8,
                           "Drop"=1,
                           "lrc"=1,
                           "pwr"=1)))+
  labs(x="Precip:PET")+theme_linedraw()


l_pe_son <- tmp[hydro_year %in% c(2001)] %>% 
    # .[season=="DJF"] %>%
    .[season=="SON"] %>%
    .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
    .[ndvi_mcd>0] %>%
    .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
    # .[veg_class %in% c(1,2)] %>% 
    .[is.na(veg_class)==F] %>% 
    # .[pe_3mo <= 2] %>% 
    .[sample(.N,10000)] %>% 
  nls_multstart(ndvi_3mo ~ SSlogis(pe_3mo, Asym, xmid, scal),
                         data = .,
                         iter = 100,
                         start_lower = c(Asym=0.7, xmid=0.01, scal=0.01),
                         start_upper = c(Asym=1, xmid=1, scal=1),
                         # supp_errors = 'Y',
                         na.action = na.omit)
summary(l_pe_son)
plot(ndvi_3mo ~ pe_3mo, data=tmp[hydro_year %in% c(2001)] %>% 
       .[season=="SON"] %>%
       .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
       .[ndvi_mcd>0] %>%
       .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
       .[is.na(veg_class)==F] %>% 
       .[pe_3mo <= 2] %>% 
       .[sample(.N,1000)] )
curve(SSlogis(x,Asym=coef(l_pe_son)["Asym"],
              xmid=coef(l_pe_son)["xmid"],
              scal=coef(l_pe_son)["scal"]), 0.01,3, 
      add=T, col='red')

  
  

o <- tmp %>% 
  lazy_dt() %>% 
  filter(hydro_year < 2019) %>% 
  filter(ndvi_mcd > 0) %>% 
  group_by(hydro_year,id) %>% 
  summarize(ndvi = mean(ndvi_mcd,na.rm=TRUE), 
            pe = mean(precip_12mo/pet_12mo, na.rm=TRUE),
            precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE), 
            map = mean(map,na.rm=TRUE),
            tmax_max = max(tmax,na.rm=TRUE), 
            tmean = mean((tmax+tmin)/2,na.rm=TRUE),
            matmax = mean(matmax,na.rm=TRUE),
            vpd15 = mean(vpd15,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  filter(is.na(ndvi)==F & is.na(pe)==F)

dat_train <- o %>% 
  filter(pe <= 3) %>% 
  filter(hydro_year==2000)

o %>%
  filter(pe <= 3) %>%
  filter(hydro_year==2004) %>%
  ggplot(data=., aes(vpd15,ndvi))+
  geom_point()

o %>%
  filter(pe <= 3) %>%
  filter(hydro_year==2004) %>%
  ggplot(data=., aes(pe,ndvi))+
  geom_point(alpha=0.1)+
  # geom_smooth()+
  # geom_smooth(method='nls', se=F, color='red',          # WORKS!
  #             formula = y~SSmicmen(x,Vm,K),
  #             method.args=list(
  #               start=list("Vm"=1,
  #                        "K"=1)))+
  geom_smooth(method='nls', se=F, color='orange',          # WORKS!
              formula = y~SSweibull(x,Asym,Drop,lrc,pwr),
              method.args=list(
                start=list("Asym"=1,
                           "Drop"=1,
                           "lrc"=1,
                           "pwr"=1)))+
  labs(x="Precip:PET")+theme_linedraw()
  # geom_smooth(method='nls', se=F, color='purple',          # 
  #           formula = y~SSasymp(input = x,Asym,R0,lrc),
  #           method.args=list(
  #             start=list("Asym"=0.9,
  #                        "R0"=0.1,
  #                        "lrc"=1)))+
# geom_smooth(method='nls', se=F, color='purple',          #
#             formula = y~SSlogis(input = x,Asym,xmid,scal),
#             method.args=list(
#               start=list("Asym"=0.9,
#                          "xmid"=0.5,
#                          "scal"=1)))+
  # geom_smooth(method='nls', se=F, color='purple',          # 
  #             formula = y~SSgompertz(x,Asym,b2,b3),
  #             method.args=list(
  #               start=list("Asym"=0.9,
  #                          "b2"=3,
  #                          "b3"=0.005)))

m1 <- nls(ndvi~SSlogis(pe,Asym,xmid,scal), 
    start=list("Asym"=0.9,
               "xmid"=0.5,
               "scal"=0.1), 
    data=o %>% 
      filter(pe <= 3) %>% 
      filter(hydro_year==2000))
m2 <- nls(ndvi~SSgompertz(pe, Asym,b2,b3), 
          start=list(Asym=0.9, 
                     b2=0.3,
                     b3=0.005), 
          data=o %>% 
            filter(pe <= 3) %>% 
            filter(hydro_year==2000))
m3 <- nls(ndvi~SSasymp(pe,Asym,R0,lrc), 
         start=list(Asym=0.9, 
                    R0=0.1,
                    lrc=1), 
         data=o %>% 
           filter(pe <= 3) %>% 
           filter(hydro_year==2000))
m4 <- nls(ndvi~SSweibull(pe,Asym,Drop,lrc,pwr), 
         start=list(Asym=1, 
                    Drop=1, 
                    lrc=1,
                    pwr=1), 
         data=dat_train)
m5 <- nls(ndvi~SSmicmen(pe,Vm,K), 
         start=list(Vm=1,
                    K=1), 
         data=o %>% 
           filter(pe <= 3) %>% 
           filter(hydro_year==2000))
m <- nls(ndvi~, 
         start=list(), 
         data=o %>% 
           filter(pe <= 3) %>% 
           filter(hydro_year==2000))
bbmle::AICtab(m1,m2,m3,m4,m5)


o %>% 
  filter(pe <= 3) %>% 
  filter(hydro_year==2000) %>% 
  mutate(pred1 = predict(m1,newdata=.), 
         pred2 = predict(m2,newdata=.), 
         pred3 = predict(m3,newdata=.), 
         pred4 = predict(m4,newdata=.), 
         pred5 = predict(m5,newdata=.)) %>% 
 summarize(r2_1 = cor(ndvi,pred1)**2,
           r2_2 = cor(ndvi,pred2)**2,
           r2_3 = cor(ndvi,pred3)**2,
           r2_4 = cor(ndvi,pred4)**2, 
           r2_5 = cor(ndvi,pred5)**2, 
           rmse_1 = sqrt(mean((ndvi-pred1)**2)),
           rmse_2 = sqrt(mean((ndvi-pred2)**2)),
           rmse_3 = sqrt(mean((ndvi-pred3)**2)),
           rmse_4 = sqrt(mean((ndvi-pred4)**2)),
           rmse_5 = sqrt(mean((ndvi-pred5)**2)))


o %>% filter(pe<=3 & hydro_year==2000) %>% 
  mutate(pred4 = predict(m4,newdata=.)) %>% 
  mutate(resids4 = pred4-ndvi) %>% 
  ggplot(aes(precip_anom_12mo/map, resids4))+
  geom_point()+
  geom_smooth()

curve(SSlogis(x,1.04,0.52,0.24),0,3)
curve(SSgompertz(x,0.8,3,0.005),-1,3,ylim=c(0,1))


m4_2 <- nls(ndvi~Asym-Drop*exp(-exp(lrc)*pe^pwr)+beta*(precip_anom_12mo/map), 
          start=list(Asym=1, 
                     Drop=1, 
                     lrc=1,
                     pwr=1, 
                     beta=-0.2), 
          lower=c(0,0,0,0,-1),
          upper=c(1,1,1,1,1),
          algorithm = 'port',
          data=dat_train)
m4_2
bbmle::AICtab(m4, m4_2)

m4_3 <- nls(ndvi~Asym-Drop*exp(-exp(lrc)*pe^pwr)+
              beta*(precip_anom_12mo/map)+
              alpha*(tmax_max), 
            start=list(Asym=1, 
                       Drop=1, 
                       lrc=1,
                       pwr=1, 
                       beta=-0.2,
                       alpha=0), 
            lower=c(0,0,0,0,-1,-0.1),
            upper=c(1.5,1,1,1,1,0.1),
            algorithm = 'port',
            data=dat_train)
m4_3

mg <- bam(ndvi~s(pe)+s(I(precip_anom_12mo/map))+s(I(tmax_max/mat)), 
    data=dat_train, select=TRUE, method='fREML',discrete = T)
bbmle::AICtab(m4, m4_2, m4_3,mg)
summary(mg)
cor(predict(m4,newdata=dat_train),dat_train$ndvi)**2
cor(predict(m4_2,newdata=dat_train),dat_train$ndvi)**2
cor(predict(m4_3,newdata=dat_train),dat_train$ndvi)**2

dat_train %>% 
  mutate(pred = predict(m4_3,newdata=.), 
         pred0 = predict(m4,newdata=.)) %>% 
  mutate(resids = pred-ndvi) %>% 
  ggplot(data=., aes(pred,ndvi))+
  # geom_point(inherit.aes = F, aes(pred0, ndvi), alpha=0.1,color='red')+
  geom_point(alpha=0.05,color='blue')+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='black')
  

res <- tibble(year=1983:2018, Asym=NA,Drop=NA,lrc=NA,pwr=NA,beta=NA,alpha=NA)

for(i in 1982:2019){
  m <- nls(ndvi~Asym-Drop*exp(-exp(lrc)*pe^pwr)+
                beta*(precip_anom_12mo/map)+
                alpha*(tmax_max), 
              start=list(Asym=1, 
                         Drop=1, 
                         lrc=1,
                         pwr=1, 
                         beta=-0.2,
                         alpha=0), 
              lower=c(0,0,0,0,-1,-0.1),
              upper=c(1.5,1,1,1,1,0.1),
              algorithm = 'port',
              data=o %>% filter(pe<=3 & hydro_year==i))

  res[res$year==i,] <- c(i,coef(m))
  print(i)
}
which(res$year==i)
bind_rows(c(i,coef(m)),c(i,coef(m)))


tmp %>% 
  filter(year <= 2017) %>% 
  sample_n(5e5) %>% 
  filter(mape <5) %>%
  mutate(val = precip_3mo/pet_3mo) %>% 
  filter(val <= 5) %>% 
  mutate(epoch = cut_interval(hydro_year, n = 4)) %>% 
  ggplot(data=., aes(val, ndvi_3mo, color=as.factor(epoch)))+
  # geom_point(aes(mape,ndvi_3mo,color=vc),inherit.aes = F)+
  geom_smooth(se=F)+
  geom_vline(aes(xintercept=1),col='red')+
  scale_color_viridis_d()+
  facet_wrap(~season)
  # geom_smooth(method='gam',se=F,
  #             formula=y~x,
  #             method.args=list(family=betar(link='probit')))


library(brms)

sdat <- tmp[date>= ymd("2004-01-01") & date <= "2004-03-01"][is.na(ndvi_mcd)==F] %>% 
  .[ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5]
sdat <- sdat %>% mutate(k=8.62e-5)
sdat <- sdat[sample(.N,10000)]
sdat <- sdat %>% as.data.frame()

#   bf(ndvi_mcd ~ lnc+log(exp(E/8.62e-5*(1/(25) - 1/(tmax+273.15)))) + log(1/(1 + exp(Eh/8.62e-5*(1/Th - 1/(tmax+273.15))))), 

# bprior5 <- prior(gamma(1,1), nlpar = lnc, lb=0, ub=1.1) + #! need to specify lb=0 when using gamma prior
#   prior(gamma(1,1), nlpar = E, lb=0)+
#   prior(gamma(1,1), nlpar=Eh, lb=0)+
#   prior(normal(32.11,5), nlpar=Th, lb=10, ub=50)
bprior5 <- prior(normal(1,0.5), nlpar = lnc, lb=0, ub=1.5) + #! need to specify lb=0 when using gamma prior
  prior(normal(0.3,0.05), nlpar = E, lb=0.01)+    # VERY SENSITIVE 
  prior(normal(1,0.5), nlpar=Eh, lb=0.01)+
  prior(normal(301,3), nlpar=Th, lb=280, ub=320)

options(mc.cores=parallel::detectCores()-3) 
f <- bf(ndvi_mcd ~ lnc + log(exp(E/k*(1/298.15 - 1/(tmax+273.15)))) + 
          log(1/(1 + exp(Eh/k*(1/Th - 1/(tmax+273.15))))),
        lnc + E + Eh + Th ~ 1, 
        nl=TRUE)
make_stancode(f, prior=bprior5, family=gaussian(),data=sdat)

fit5 <- brm(f,
            data = sdat[sample(.N, 1000)], 
            prior = bprior5, 
            # sample_prior = 'only')
            algorithm = 'sampling', 
            control = list(adapt_delta=0.99, 
                           max_treedepth=15),
            chains = 3, 
            iter = 500
)
# fit5 <- update(fit5, algorithm='meanfield', iter=2000)
fit5 <- update(fit5, chains=4,iter=2000,algorithm='sampling',
               control=list(adapt_delta=0.99, 
                                                    max_treedepth=15)) 
summary(fit5)
plot(fit5)

blah <- summary(fit5, prob = 0.5)
blah$fixed[1]
j3 <- print(fit5,digits = 2)
j3$fixed["Th_Intercept","l-95% CI"]
curve(schoolfield_high(lnc =blah$fixed[1],E=blah$fixed[2],
                       Eh = blah$fixed[3],Th=blah$fixed[4], 
                       temp=x+273.15,Tc = 20),10,43, ylim=c(0,1), 
      ylab='NDVI',xlab=expression(paste(Tmax~(degree*C))))
abline(v=blah$fixed[4]-273.15)
abline(v=j3$fixed["Th_Intercept","l-95% CI"]-273.15,lty=3)
abline(v=j3$fixed["Th_Intercept","u-95% CI"]-273.15,lty=3)
points(ndvi_mcd~tmax, data=sdat, pch=20,
       col='#0000FF1A')




fit <- nls_multstart(ndvi_mcd ~ lnc + log(exp(E/k*(1/298.15 - 1/(tmax+273.15)))) + 
      log(1/(1 + exp(Eh/k*(1/Th - 1/(tmax+273.15))))), 
    data = sdat[sample(.N, 1000)] %>% 
      lazy_dt() %>% mutate(k=8.62e-5) %>% 
      as_tibble(),
    iter = 1000,
    start_lower = c(lnc = 0.5, E = 0.25, Eh = 0.2, Th = 273.15+15),
    start_upper = c(lnc = 5, E = 1, Eh = 3, Th = 310),
    # supp_errors = 'Y',
    na.action = na.omit,
    lower = c(lnc = 0.5, E = 0.01, Eh = 0.01, Th = 283))
summary(fit)

schoolfield_high <- function(lnc, E, Eh, Th, temp, Tc) {
  Tc <- 273.15 + Tc
  k <- 8.62e-5
  boltzmann.term <- lnc + log(exp(E/k*(1/Tc - 1/temp)))
  inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))
  return(boltzmann.term + inactivation.term)
}
library(nls.multstart)
fit <- nls_multstart(ndvi_mcd ~ schoolfield_high(lnc, E, Eh, Th, temp = tmax+273.15, Tc = 25),
                     data = sdat[sample(.N, 1000)],
                     iter = 100,
                     start_lower = c(lnc = 0.5, E = 0.25, Eh = 0.2, Th = 273.15+15),
                     start_upper = c(lnc = 5, E = 1, Eh = 3, Th = 310),
                     # supp_errors = 'Y',
                     na.action = na.omit,
                     lower = c(lnc = 0.5, E = 0.01, Eh = 0.01, Th = 283))
summary(fit)
curve(schoolfield_high(lnc = coef(fit)["lnc"],E=coef(fit)["E"],
                       Eh = coef(fit)["Eh"],Th=coef(fit)["Th"], 
                       temp=x+273,Tc = 20),10,43, ylim=c(0,1))
points(ndvi_mcd~tmax, data=sdat, pch=20,
       col='#0000FF80')
alpha('blue',alpha=0.1)

blah <- summary(fit5, prob = 0.5)
blah$fixed[1]
curve(schoolfield_high(lnc =blah$fixed[1],E=blah$fixed[2],
                       Eh = blah$fixed[3],Th=blah$fixed[4], 
                       temp=x+273,Tc = 20),10,43, ylim=c(0,1), 
      ylab='NDVI',xlab=expression(paste(Tmax~(degree*C))))
points(ndvi_mcd~tmax, data=sdat, pch=20,
       col='#0000FF1A')



schoolfield_high <- function(lnc, E, Eh, Th, temp, Tc) {
  Tc <- 273.15 + Tc
  k <- 8.62e-5
  boltzmann.term <- lnc + log(exp(E/k*(1/Tc - 1/temp)))
  inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))
  return(boltzmann.term + inactivation.term)
}
nobs <- 1000
vec_temp <- rnorm(nobs,mean=305,sd=5)
vec_eps <- rnorm(nobs, mean=0, sd=0.2)
vec_sim <- schoolfield_high(lnc = rnorm(nobs,1.3,0.1), 
                 E = rnorm(nobs,0.3,0.015),
                 Eh = rnorm(nobs,1,0.1),
                 Th = rnorm(nobs,301,1),
                 temp = vec_temp, 
                 Tc = 15)+vec_eps

sfit <- nls_multstart(ndvi_mcd ~ schoolfield_high(lnc, E, Eh, Th, temp, Tc = 15),
                     data = data.frame(ndvi_mcd = vec_sim, 
                                       temp = vec_temp),
                     iter = 1000,
                     start_lower = c(lnc = 0, E = 0.2, Eh = 0, Th = 295),
                     start_upper = c(lnc = 2, E = 0.4, Eh = 3, Th = 305),
                     # supp_errors = 'Y',
                     na.action = na.omit,
                     lower = c(lnc = 0, E = 0.2, Eh = 0, Th = 290), 
                     upper = c(lnc = 2, E = 0.4, Eh = 2, Th = 315))
summary(sfit)
curve(schoolfield_high(lnc =1.3,E=0.3,
                       Eh = 1,Th=301, 
                       temp=x,Tc = 15),290,325, ylim=c(0,2))
points(vec_sim~vec_temp,pch=20)
abline(v=301)
curve(schoolfield_high(lnc = coef(sfit)["lnc"],E=coef(sfit)["E"],
                       Eh = coef(sfit)["Eh"],Th=coef(sfit)["Th"], 
                       temp=x,Tc = 15),290,325, col='red', add=TRUE)
abline(v=coef(sfit)["Th"],col='red')




sdat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5][ndvi_mcd>0]
gfit <- gam(log(ndvi_mcd) ~ s(tmax_3mo,bs = "gp", m = c(3, 1)), 
    data=tmp[date>=ymd("2004-12-01") & date <= ymd("2005-11-01")] %>% 
      .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
      .[ndvi_mcd>0] %>%
      .[veg_class %in% 1:5] %>% 
      .[sample(.N,5000)], 
    method='REML', select=T)
plot(gfit)
summary(gfit)

tmp[date>=ymd("2004-12-01") & date <= ymd("2005-11-01")] %>% 
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_mcd>0] %>%
  .[veg_class %in% 1:5] %>% 
  .[sample(.N,5000)] %>% 
  ggplot(data=., aes(tmax_3mo, log(ndvi_mcd)))+
  geom_point()+
  geom_smooth()


library(devRate)
?devRate::devRateInfo
devRateInfo(eq = campbell_74)


gen_af <- function(tk, Ea, Topt){
  R <- 8.3145
  ft <- exp(Ea/(R*Topt) - Ea/(R*tk))
  Ga <- 2*ft/(1+(ft^2))
  Ga <- unname(Ga)
  return(ft)
}
curve(gen_af(tk=x, Ea=2, Topt=311), 273,330)
gen_af(tk=273:330, Ea=1, Topt=311)

sea <- function(tk,A,Ea,B){A*exp(-(Ea/(8.62e-5*tk))**B)}
curve(sea(x,0.8,Ea = 0.1,B = 1.5),273.15,330)


cherry <- function(tk){exp(9.5e3 * (tk-288.2)/(288.2*tk))}
cherry(273:330)
curve(cherry(tk=x),273,350)


schoolfield_high <- function(lnc, E, Eh, Th, temp, Tc) {
  k <- 8.62e-5
  boltzmann.term <- lnc + log(exp(E/k*(1/Tc - 1/temp)))
  inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))
  return(boltzmann.term + inactivation.term)
}

curve(schoolfield_high(lnc=1, 
                       E=0.5, Eh = 5, Th = 300, 
                       temp = x,Tc = 283+25),
      273,320)


curve(0.5*x/(273+25) - (0.5+x)/(273+25), 273,310)


mod_arr <- function(kopt, Ha, Hd, Tk, Topt){
  kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
          (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt)))))))
}
curve(mod_arr(kopt=1,Ha=4,Hd=200,Tk=x,Topt=273+25),273,325)


library(nls.multstart)
tmp %>% lazy_dt() %>% group_by(vc,veg_class) %>% 
  summarize(val = diff(range(tmax,na.rm=TRUE)), 
            nobs = sum(is.na(ndvi_mcd)==F)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  View()
sdat <- tmp[hydro_year==2014] %>% 
  # .[season=="DJF"] %>%
  .[season=="SON"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_mcd>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  # .[sample(.N,5000)] %>% 
  .[,`:=`(Tk=tmax+273.15)]
sdat %>% ggplot(data=., aes(tmax,ndvi_mcd,color=vc))+#geom_point(alpha=0.2)+
  geom_smooth(se=F)
fit <- nls_multstart(ndvi_mcd ~ mod_arr(kopt,Ha,Hd,Tk,Topt),
                     data = sdat[sample(.N, 2500)],
                     iter = 100,
                     start_lower = c(kopt = 0.7, Ha=3, Hd=200, Topt=273.15+20),
                     start_upper = c(kopt = 0.8, Ha=5, Hd=201, Topt=273.15+30),
                     # supp_errors = 'Y',
                     na.action = na.omit,
                     lower = c(kopt = 0.5, Ha=1, Hd=80, Topt=273.15+10))
summary(fit)
plot(ndvi_mcd~I(tmax+273.15), data=sdat[sample(.N,5000)], pch=20, cex=1, col='#00000055', 
     ylab='NDVI',xlab='tmax (K)',ylim=c(0,1))
curve(mod_arr(kopt=coef(fit)["kopt"], 
              Ha=coef(fit)["Ha"], 
              # Hd=coef(fit)["Hd"], 
              Hd=100,
              Tk=x,
              Topt=coef(fit)["Topt"]), 
      280,273+42,add=T,col='red')
abline(v=coef(fit)["Topt"],
       h=coef(fit)["kopt"],col='blue')

cor(predict(fit, newdata = sdat), sdat$ndvi_mcd)**2



dat <- dat[,fn_rot2(.SD),by=date]

fn_topt <- function(dat){
  fit <- nls_multstart(ndvi_mcd ~ mod_arr(kopt,Ha,Hd,Tk,Topt),
                       data = dat[sample(.N, 2500)],
                       # data=dat,
                       iter = 20,
                       start_lower = c(kopt = 0.7, Ha=3, Hd=200, Topt=273.15+20),
                       start_upper = c(kopt = 0.8, Ha=5, Hd=201, Topt=273.15+30),
                       # supp_errors = 'Y',
                       na.action = na.omit,
                       lower = c(kopt = 0.5, Ha=1, Hd=80, Topt=285.5))
  bfit <- broom::tidy(fit)
  bfit$hydro_year <- unique(dat$hydro_year); 
  bfit$season <- unique(dat$season)
  return(bfit)  
}

cdat <- tmp[hydro_year %in% c(1983:2019)] %>% 
  # .[season=="DJF"] %>%
  .[season=="SON"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_mcd>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  # .[sample(.N,5000)] %>% 
  .[,`:=`(Tk=tmax+273.15)] %>% 
  .[,fn_topt(.SD),by=.(season,hydro_year)]

cdat %>% filter(term=="Hd") %>% 
  ggplot(data=., aes(hydro_year, estimate))+
  geom_point()+
  geom_smooth(method='lm')

cdat %>% 
  as_tibble() %>% 
  # filter(p.value < 0.1) %>% 
  select(season,hydro_year,term,estimate) %>% 
  spread(key = term, value=estimate) %>% 
  # filter(Hd > 100) %>% 
  ggplot(data=., aes(hydro_year, Topt,color=as_factor(hydro_year)))+
  geom_point()+
  geom_label(aes(label=hydro_year))
  # geom_smooth(method='lm')
  # geom_smooth(method='loess')

tmp[hydro_year %in% c(2009, 2014)] %>% 
  # .[season=="DJF"] %>%
  .[season=="SON"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_mcd>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  .[sample(.N,5000)] %>%
  .[,`:=`(Tk=tmax+273.15)] %>% 
  ggplot(data=., aes(Tk,ndvi_mcd,color=as_factor(hydro_year)))+
  geom_point()+
  geom_smooth(se=F)+
  stat_function(fun=mod_arr, args=list(kopt=coef(fit)["kopt"], 
                                       Ha=coef(fit)["Ha"],
                                       Hd=coef(fit)["Hd"], 
                                       Topt=coef(fit)["Topt"]), 
                color='black')


sdat
Asym/(1+exp((xmid-input)/scal))
l_fit <- nls_multstart(ndvi_mcd ~ SSlogis(Tk, Asym, xmid, scal),
                     data = sdat[sample(.N, 2500)],
                     iter = 100,
                     start_lower = c(Asym=0.8, xmid=290, scal=-1),
                     start_upper = c(Asym=0.8, xmid=300, scal=-1),
                     # supp_errors = 'Y',
                     na.action = na.omit)
                     # lower = c(kopt = 0.5, Ha=1, Hd=80, Topt=273.15+10))
plot(ndvi_mcd~I(tmax+273.15), data=sdat[sample(.N,5000)], pch=20, cex=1, col='#00000055', 
     ylab='NDVI',xlab='tmax (K)',ylim=c(0,1))
curve(mod_arr(kopt=coef(fit)["kopt"], 
              Ha=coef(fit)["Ha"], 
              Hd=coef(fit)["Hd"],
              Tk=x,
              Topt=coef(fit)["Topt"]), 
      280,273+42,add=T,col='blue')
curve(SSlogis(x,Asym=coef(l_fit)["Asym"],
              xmid=coef(l_fit)["xmid"],
              scal=coef(l_fit)["scal"]), 273, 330, 
      add=TRUE, col='red')
abline(v=coef(l_fit)["xmid"], h=coef(l_fit)["Asym"],col='red')

hillfunc <- function(Tk, a, b, c) {
  1 - ( ( (1-a) * Tk^b) / ( c + (Tk)^b ) )
} 
curve(hillfunc(Tk=x,0.2,0.5,2), -100,500)
h_fit <- nls_multstart(ndvi_mcd ~ hillfunc(Tk, a,b,c),
                       data = sdat[sample(.N, 2500)],
                       iter = 100,
                       start_lower = c(a = 0.1, b=0.5, c =2),
                       start_upper = c(a = 0.9, b=2, c=1),
                       # supp_errors = 'Y',
                       na.action = na.omit)
h_fit
# lower = c(kopt = 0.5, Ha=1, Hd=80, Topt=273.15+10))


bbmle::AICtab(fit,l_fit)


fn_xmid <- function(dat){
  fit <- nls_multstart(ndvi_mcd ~ SSlogis(Tk, Asym, xmid, scal),
                       data = sdat[sample(.N, 2500)],
                       iter = 30,
                       start_lower = c(Asym=0.8, xmid=290, scal=-1),
                       start_upper = c(Asym=0.8, xmid=300, scal=-1),
                       # supp_errors = 'Y',
                       na.action = na.omit)
  bfit <- broom::tidy(fit)
  bfit$hydro_year <- unique(dat$hydro_year); 
  bfit$season <- unique(dat$season)
  return(bfit)  
}

xdat <- tmp[hydro_year %in% c(1983:2019)] %>% 
  # .[season=="DJF"] %>%
  .[season=="SON"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_mcd>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  # .[sample(.N,5000)] %>% 
  .[,`:=`(Tk=tmax+273.15)] %>% 
  .[,fn_xmid(.SD),by=.(season,hydro_year)]

xdat %>% 
  as_tibble() %>% 
  # filter(p.value < 0.1) %>% 
  select(season,hydro_year,term,estimate) %>% 
  spread(key = term, value=estimate) %>% 
  filter(!hydro_year %in% c(2012)) %>%
  filter(hydro_year >= 2000) %>% 
  ggplot(data=., aes(hydro_year, xmid-273.15))+
  geom_point()+
  geom_smooth(method='lm')+
  geom_label(aes(label=hydro_year))


tmp[hydro_year %in% c(2017)] %>% 
  # .[season=="DJF"] %>%
  .[season=="SON"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_mcd>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  # .[veg_class %in% c(1,2)] %>% 
  .[is.na(veg_class)==F] %>% 
  # .[sample(.N,25000)] %>%
  .[,`:=`(Tk=tmax+273.15)] %>% 
  ggplot(data=., aes(Tk, ndvi_mcd,color=vc))+
  geom_point(alpha=0.1)+
  geom_smooth(method='nls', se=F,           #
            formula = y~SSlogis(input = x,Asym,xmid,scal),
            method.args=list(
              start=list("Asym"=0.8,
                         "xmid"=306,
                         "scal"=-6.1)))
  # geom_smooth(method='nls', se=F, color='orange',          # WORKS!
  #             formula = y~SSweibull(x,Asym,Drop,lrc,pwr),
  #             method.args=list(
  #               start=list("Asym"=1,
  #                          "Drop"=1,
  #                          "lrc"=1,
  #                          "pwr"=1)))
  



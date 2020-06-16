dat_fit <- tmp[hydro_year %in% c(1983:2019)] %>% 
  .[, `:=`(sc_co2_int = scale(co2_int, center=T, scale=F), 
           pe_12mo = precip_12mo/pet_12mo)] %>% 
  .[season=="DJF"] %>%
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
  .[ndvi_m>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  # .[veg_class %in% c(1,2)] %>% 
  .[is.na(veg_class)==F] %>% 
  # .[pe_3mo <= 2] %>% 
  .[sample(.N,10000)]

Asym-Drop*exp(-exp(lrc)*x^pwr)
curve(-SSweibull(x, Asym = -1,Drop = -1,lrc = -10,pwr = 1),0,40)

w_fit <- nls_multstart(ndvi_3mo ~ SSweibull(precip_12mo/pet_12mo, Asym, Drop,lrc,pwr),
                       data = dat_fit,
                       iter = 1000,
                       start_lower = c(Asym=0.7, Drop=-1, lrc=-20, pwr=-1),
                       start_upper = c(Asym=1, Drop=1, lrc=20, pwr=1),
                       # supp_errors = 'Y',
                       na.action = na.omit)
# lower = c(kopt = 0.5, Ha=1, Hd=80, Topt=273.15+10))
w_fit
summary(w_fit)
curve(SSweibull(x, Asym=coef(w_fit)["Asym"], Drop=coef(w_fit)["Drop"], 
                lrc=coef(w_fit)["lrc"], pwr=coef(fit)['pwr']),0.1,3, ylim=c(0,1))

SSweibull(0.5, Asym=coef(w_fit)["Asym"], Drop=coef(w_fit)["Drop"], 
          lrc=coef(w_fit)["lrc"], pwr=coef(fit)['pwr'])

fn <- function(x,Asym,Drop,lrc,pwr) Asym-Drop*exp(-exp(lrc)*x^pwr)
curve(fn(x,0.8,0.6,0.5,1),-0.1,2)
fn_mid <- function(A,R,L) (log((A - R)/A) + 0.693147180559945)*exp(-L)
fn_mid(A=0.8, R)


fn <- function(x,lambda,k) -(-1 - exp(-(x/lambda)**k))
curve(fn(x,lambda = 1, k=5))

# modified michaelis-menten
mmm <- function(x, a, b){ a*(1 - exp(-b*x))}
curve(mmm(x,1,1),0,45) 


h3 <- function(x,a,b,scal,offset){
  -(a*x**2 / scal) / (b**2 + x**2/(scal)) + offset
  }
curve(h3(x,1,1,scal=30,offset=1),-1,40)




tmp[hydro_year %in% c(2016)] %>% 
  # .[season %in% c( "SON", "DJF")] %>%
  # .[season=="SON"] %>%
  # .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_mcd>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  # .[veg_class %in% c(1,2)] %>% 
  .[is.na(veg_class)==F] %>% 
  .[pe_3mo <= 2] %>% 
  # .[sample(.N,50000)] %>%
  .[,`:=`(Tk=tmax+273.15)] %>% 
  ggplot(data=., aes(#(precip_12mo-pet_12mo)/(pet_12mo+precip_12mo), 
                    x=tmax_3mo, 
                    y=ndvi_mcd,
                    color=as_factor(season)))+
  # geom_point(alpha=0.1)+
  geom_smooth(formula=y~s(x,k=4), se=F,
              method='gam',
              method.args=list(select=TRUE 
                               # discrete=TRUE, 
                               # method='fREML'
                               ))+
  scale_x_continuous(limits=c(25,35))+
  scale_y_continuous(limits=c(0.25,0.75))+
  scale_color_viridis_d()+
  facet_wrap(~vc)

tmp %>% 
  lazy_dt() %>% 
  group_by(hydro_year) %>% 
  summarize(val = mean(precip_anom_12mo,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  ggplot(data=., aes(hydro_year, val))+
  geom_point()



b3 <- tmp[hydro_year %in% c(2016:2019)] %>% 
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
  bam(ndvi_3mo ~ s(pet_3mo)+ 
                 s(tmax_3mo)+
                 s(precip_3mo)+
                 s(precip_12mo), 
      select=TRUE, discrete = TRUE, data=.)
summary(b3)
plot(b3,scale=0)





tmp[hydro_year %in% c(1982:2019)] %>% 
  .[season %in% c("DJF")] %>%
  .[ndvi_hyb>0] %>%
  .[ndvi_anom_sd >= -3 & ndvi_anom_sd <= 3] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  .[pe_3mo <= 2] %>% 
  .[sample(.N,10000)] %>%
  .[,`:=`(Tk=tmax+273.15)] %>% 
  ggplot(data=., aes(precip_12mo/pet_12mo, ndvi_3mo,
                     color=as_factor(hydro_year)))+
  # geom_point(alpha=0.25)+
  geom_smooth(method='nls', se=F,           #
              formula = y~SSlogis(input = x,Asym,xmid,scal),
              method.args=list(
                start=list("Asym"=coef(fit12)["Asym"],
                           "xmid"=coef(fit12)["xmid"],
                           "scal"=coef(fit12)["scal"])), 
              lwd=0.5)+
  # scale_color_gradient2(expression(paste(Tmax~(degree*C))), 
  #                       mid='gray70')+
  scale_color_viridis_d()+
  labs(x=expression(paste(Precip["12 mo"]*":"*PET["12 mo"])), 
       y=expression(paste(NDVI[" 3 mo"])))+
  scale_x_continuous(limits=c(0,1.5), expand=c(0,0))+
  scale_y_continuous(limits=c(0,1), expand=c(0,0))+
  theme_linedraw()+
  theme(legend.position = 'bottom')


fit <- dat_fit %>%  
  nls_multstart(ndvi_3mo ~ Asym/(1+exp((xmid-(precip_12mo/pet_12mo))/scal))+
                  Asym2/(1+exp((xmid2-vpd15)/scal2)),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01, 
                                Asym2=0.7, xmid2=0.01, scal2=0.01),
                start_upper = c(Asym=1, xmid=1, scal=1, 
                                Asym2=1, xmid2=1, scal2=1),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(fit)

pdat <- tmp[season=='DJF' & is.na(ndvi_3mo)==F][sample(.N,10000)]
o <- predict(fit, newdata=pdat, na.action=na.omit)
o %>% unname()
(cbind(o[1:10000], pdat$ndvi_3mo) %>% na.omit() %>% cor)**2
pdat$

tmp[season=='DJF'][sample(.N,10000)] %>% 
  ggplot(data=., aes(vpd15, ndvi_3mo))+geom_smooth()

fit2 <- dat_fit %>% 
  nls_multstart(ndvi_3mo ~ Asym/(1+exp((xmid-(precip_12mo/pet_12mo))/scal))+beta*tmax_anom,
                data = .,
                iter = 30,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01, 
                                beta=-1),
                start_upper = c(Asym=1, xmid=1, scal=1, 
                                beta=1),
                # supp_errors = 'Y',
                na.action = na.omit)

fit3 <- dat_fit %>% 
  nls_multstart(ndvi_3mo ~ Asym/(1+exp((xmid-(precip_12mo/pet_12mo))/scal))+beta*vpd15_anom,
                data = .,
                iter = 30,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01, 
                                beta=-1),
                start_upper = c(Asym=1, xmid=1, scal=1, 
                                beta=1),
                # supp_errors = 'Y',
                na.action = na.omit)

fit4 <- dat_fit %>%  
  nls_multstart(ndvi_3mo ~ Asym/(1+exp((xmid-(precip_12mo/pet_12mo))/scal))+
                  Asym2/(1+exp((xmid2-tmax)/scal2)),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01, 
                                Asym2=0.7, xmid2=0.01, scal2=0.01),
                start_upper = c(Asym=1, xmid=1, scal=1, 
                                Asym2=1, xmid2=1, scal2=1),
                # supp_errors = 'Y',
                na.action = na.omit)

fit5 <- dat_fit %>%  
  nls_multstart(ndvi_3mo ~ Asym/(1+exp((xmid-(precip_12mo/pet_12mo))/scal))+
                  Asym2/(1+exp((xmid2-vpd15_3mo)/scal2)),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01, 
                                Asym2=0.7, xmid2=0.01, scal2=0.01),
                start_upper = c(Asym=1, xmid=1, scal=1, 
                                Asym2=1, xmid2=1, scal2=1),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(fit5)

fit6 <- dat_fit %>%  
  nls_multstart(ndvi_3mo ~ Asym/(1+exp((xmid-(precip_12mo/pet_12mo))/scal))+
                  Asym2/(1+exp((xmid2-tmax_3mo)/scal2)),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01, 
                                Asym2=0.7, xmid2=0.01, scal2=0.01),
                start_upper = c(Asym=1, xmid=1, scal=1, 
                                Asym2=1, xmid2=1, scal2=1),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(fit6)

fit7 <- dat_fit %>%  
  nls_multstart(ndvi_3mo ~ Asym/(1+exp((xmid-(precip_12mo/pet_12mo))/scal))+
                  Drop2*exp(-exp(lrc2)*vpd15_3mo^pwr2),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01, 
                                Drop2=0, lrc2=0.01, pwr2=0.01),
                start_upper = c(Asym=1, xmid=1, scal=1, 
                                Drop2=0.5, lrc2=1, pwr2=2),
                # supp_errors = 'Y',
                na.action = na.omit)
fit7

fit7 <- dat_fit %>%  
  nls_multstart(ndvi_3mo ~ Asym/(1+exp((xmid-(precip_12mo/pet_12mo))/scal))+
                  Drop2*exp(-exp(lrc2)*vpd15_3mo^pwr2)+ 
                  beta*hydro_year,
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01, 
                                Drop2=-0.5, lrc2=0.01, pwr2=-5, 
                                beta=-0.5),
                start_upper = c(Asym=1, xmid=1, scal=1, 
                                Drop2=0.5, lrc2=10, pwr2=5, 
                                beta=0.5),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(fit7)

fit_weibull <- dat_fit %>% 
  nls_multstart(ndvi_3mo ~ SSweibull(precip_12mo/pet_12mo, Asym, Drop,lrc,pwr),
                       data = .,
                       iter = 1000,
                       start_lower = c(Asym=0.5, Drop=0, lrc=0, pwr=0),
                       start_upper = c(Asym=1, Drop=1, lrc=1, pwr=2),
                       # supp_errors = 'Y',
                       na.action = na.omit)

fit_asymp <- dat_fit %>% 
  nls_multstart(ndvi_3mo ~ SSasymp(input = precip_12mo/pet_12mo, Asym, R0,lrc),
                data = .,
                iter = 1000,
                start_lower = c(Asym=0.5, R0=0, lrc=0),
                start_upper = c(Asym=1, R0=0.25, lrc=1),
                # supp_errors = 'Y',
                na.action = na.omit)
curve(SSasymp(input=x, Asym=coef(fit_asymp)["Asym"], R0=coef(fit_asymp)["R0"], 
              lrc=coef(fit_asymp)["lrc"]),0,2)
A <- coef(fit_asymp)["Asym"]; R <- coef(fit_asymp)["R0"]; L <- coef(fit_asymp)["lrc"]
abline(v=(log((A - R)/A) + 0.693147180559945)*exp(-L),
       col='blue')
abline(h=0.5*A, col='blue')
abline(v=0.5)

fit_logis <- dat_fit %>%  
  nls_multstart(ndvi_3mo ~ Asym/(1+exp((xmid-(pe_12mo))/scal)),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, xmid=0.01, scal=0.01), 
                start_upper = c(Asym=1, xmid=1, scal=1),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(fit_logis)

fit_gompertz <- dat_fit %>%  
  nls_multstart(ndvi_3mo ~ SSgompertz(x=(precip_12mo/pet_12mo),Asym,b2,b3),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.7, b2=0.01, b3=0.01), 
                start_upper = c(Asym=1, b2=1, b3=1),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(fit_gompertz)

fit_micmen <- dat_fit %>%  
  nls_multstart(ndvi_3mo ~ SSmicmen(input=(precip_12mo/pet_12mo),Vm ,K ),
                data = .,
                iter = 100,
                start_lower = c(Vm=0.5, K=0.1), 
                start_upper = c(Vm=1, K=0.75),
                # supp_errors = 'Y',
                na.action = na.omit)
summary(fit_micmen)

fit_fpl <- dat_fit %>%  
  nls_multstart(ndvi_3mo ~ SSfpl(input=(precip_12mo/pet_12mo),A,B,xmid,scal),
                data = .,
                iter = 100,
                start_lower = c(A=0, B=0.7, xmid=0.1, scal=0.1), 
                start_upper = c(A=0.3,B=1,xmid=0.9,scal=1),
                # supp_errors = 'Y',
                na.action = na.omit, lower=c(A=-1,0,0,0))
summary(fit_fpl)

fit_asymp_2 <- dat_fit %>% 
  mutate(delta_year = hydro_year-1983) %>% 
  nls_multstart(ndvi_3mo ~ (Asym+beta3*delta_year)+(R0-(Asym+beta3*delta_year))*exp(-exp(lrc)*(precip_12mo/pet_12mo))+
                           beta1*precip_anom_3mo + 
                           beta2*pet_anom_3mo,
                data = .,
                iter = 100,
                start_lower = c(Asym=0.5, R0=0, lrc=0,beta1=-1,beta2=-1,beta3=-0.1),
                start_upper = c(Asym=1, R0=0.25, lrc=1,beta2=1,beta2=1,beta3=0.1),
                supp_errors = 'Y',
                na.action = na.omit)
summary(fit_asymp_2)
(cbind(predict(fit_asymp_2, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2

bbmle::AICtab(fit_logis, fit_weibull, fit_asymp, fit_gompertz, fit_micmen, fit_fpl, 
              fit_asymp_2)
bbmle::AICctab(fit_logis, fit_weibull, fit_asymp, fit_gompertz, fit_micmen, fit_fpl)
bbmle::BICtab(fit_logis, fit_weibull, fit_asymp, fit_gompertz, fit_micmen, fit_fpl)

bbmle::AICtab(fit_logis, fit,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit_gompertz,
              fit_micmen)
(cbind(predict(fit5, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2
(cbind(predict(fit8, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2
(cbind(predict(fit_logis, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2
(cbind(predict(fit_micmen, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2
(cbind(predict(fit_fpl, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2
(cbind(predict(fit_asymp_2, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2

cbind(predict(fit5, newdata=dat_fit), dat_fit$ndvi_3mo) %>% plot

dat_fit %>% ggplot(data=., aes(log10(precip_12mo/pet_12mo), ndvi_3mo))+geom_point(alpha=0.05)+geom_smooth()

dat_fit %>% 
  mutate(pred5 = predict(fit5, newdata=.)) %>% 
  mutate(res5 = pred5-ndvi_3mo) %>% 
  gam(res5~s(precip_3mo), 
      data=., 
      select=TRUE) %>% 
  plot()

dat_fit %>% 
 ggplot(data=., aes(precip_12mo/pet_12mo, ndvi_3mo,color=vpd15_anom))+
  geom_point(alpha=0.25)+
  geom_smooth(method='nls', se=F, color='black',          #
              formula = y~SSlogis(input = x,Asym,xmid,scal),
              method.args=list(
                start=list("Asym"=coef(fit12)["Asym"],
                           "xmid"=coef(fit12)["xmid"],
                           "scal"=coef(fit12)["scal"])))+
  geom_smooth(method='nls', se=F, color='red',          #
              formula = y~SSweibull(x, Asym, Drop,lrc,pwr),
              method.args=list(
                start=list("Asym"=coef(fit8)["Asym"],
                           "Drop"=coef(fit8)["Drop"],
                           "lrc"=coef(fit8)["lrc"], 
                           "pwr"=coef(fit8)["pwr"])))+
  geom_smooth(method='nls', se=F, color='blue',          #
              formula = y~SSgompertz(x, Asym, b2,b3),
              method.args=list(
                start=list("Asym"=coef(fit_gompertz)["Asym"],
                           "b2"=coef(fit_gompertz)["b2"],
                           "b3"=coef(fit_gompertz)["b3"])))+
  geom_smooth(method='nls', se=F, color='orange',          #
              formula = y~SSfpl(input=x, A , B , xmid, scal),
              method.args=list(
                start=list("A"=coef(fit_fpl)["A"],
                           "B"=coef(fit_fpl)["B"],
                           "xmid"=coef(fit_fpl)["xmid"], 
                           "scal"=coef(fit_fpl)["scal"])))+
  geom_smooth(method='nls', se=F, color='purple', 
              formula = y~SSmicmen(input=x,Vm,K), 
              method.args=list(
                start=list("Vm"=coef(fit_micmen)["Vm"], 
                           "K"=coef(fit_micmen)["K"])
              ))+
  scale_color_gradient2(expression(paste(Tmax~(degree*C))), 
                        mid='gray70')

dat_fit %>% 
  mutate(res = predict(fit_asymp, newdata=.) - ndvi_3mo) %>% 
  bam(res ~ 
        s(precip_3mo)+
        s(vpd15_anom), 
      data=., select=TRUE) %>% 
  plot


dat_fit %>% 
  mutate(res = predict(fit_asymp, newdata=.) - ndvi_3mo) %>% 
  ggplot(data=., aes(precip_anom_3mo, res))+
  ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_smooth()

dat_fit %>% 
  mutate(res = predict(fit_asymp, newdata=.) - ndvi_3mo) %>% 
  nls_multstart(res ~ nlraa::SSricker(vpd15_anom, a,b), 
                data=.,
                iter=100,
                start_lower = list(a=0,b=0), 
                start_upper = list(a=1,b=1))
curve(nlraa::SSricker(x,-0.045,0.62),-3,3)



# Compare additive nonlinear models ---------------------------------------

fit_asymp <- dat_fit %>% filter(year %in% c(1982:2019)) %>% 
  nls_multstart(ndvi_3mo ~ SSasymp(input = precip_12mo/pet_12mo, Asym, R0,lrc),
                data = .,
                iter = 1000,
                start_lower = c(Asym=0.5, R0=0, lrc=0),
                start_upper = c(Asym=1, R0=0.25, lrc=1),
                # supp_errors = 'Y',
                na.action = na.omit, 
                lower = c(0.5, 0,0))
fit_asymp_1 <- dat_fit %>% filter(year %in% c(1982:2019)) %>% 
  nls_multstart(ndvi_3mo ~ ((Asym)+(R0-(Asym))*exp(-exp(lrc)*(precip_12mo/pet_12mo)))+
                  beta1*precip_anom_3mo + 
                  beta2*pet_anom_3mo,
                data = .,
                iter = 1000,
                start_lower = c(Asym=0.5, R0=0, lrc=0,beta1=-0.1,beta2=-0.1),
                start_upper = c(Asym=1, R0=0.25, lrc=1,beta2=0.1,beta2=0.1),
                # supp_errors = 'Y',
                na.action = na.omit, 
                lower = c(0.5, -0.1, 0, -0.1, -0.1))
fit_asymp_2 <- dat_fit %>% filter(year %in% c(1982:2019)) %>% 
  mutate(delta_year = hydro_year-1983, 
         sc_co2_int = scale(co2_int, center=T, scale=F)) %>% 
  nls_multstart(ndvi_3mo ~ (Asym)+
                  (R0-(Asym))*exp(-exp(lrc)*(precip_12mo/pet_12mo))+
                  beta1*precip_anom_3mo + 
                  beta2*pet_anom_3mo+
                  beta3*sc_co2_int,
                data = .,
                iter = 1000,
                start_lower = c(Asym=0.5, R0=0, lrc=0,beta1=-0.1,beta2=-0.1,beta3=-0.1),
                start_upper = c(Asym=1, R0=0.25, lrc=1,beta2=0.1,beta2=0.1,beta3=0.1),
                # supp_errors = 'Y',
                na.action = na.omit, 
                lower = c(0.5, -0.1, 0, -0.1, -0.1,-0.1))
fit_asymp_2


fit_asymp_3 <- dat_fit %>% filter(year %in% c(1982:2019)) %>% 
  mutate(delta_year = hydro_year-1983, 
         sc_co2_int = scale(co2_int, center=T, scale=F)) %>% 
  nls_multstart(ndvi_3mo ~ (Asym)+
                  (R0-(Asym))*exp(-exp(lrc)*(precip_12mo/pet_12mo))+
                  Asym2/(1+exp((xmid2-precip_anom_3mo)/scal2)),
                data = .,
                iter = 1000,
                start_lower = c(Asym=0.5, R0=0, lrc=0,
                                Asym2=-1, xmid=-200, scal=-100),
                start_upper = c(Asym=1, R0=0.25, lrc=1,
                                Asym2=1, xmid=200, scal=300),
                # supp_errors = 'Y',
                na.action = na.omit, 
                lower = c(0.5, -0.1, 0, -0.5, -300, -100))
summary(fit_asymp_3)

fit_asymp_4 <- dat_fit %>% filter(year %in% c(1982:2019)) %>% 
  mutate(delta_year = hydro_year-1983, 
         sc_co2_int = scale(co2_int, center=T, scale=F)) %>% 
  nls_multstart(ndvi_3mo ~ (Asym)+
                  (R0-(Asym))*exp(-exp(lrc)*(precip_12mo/pet_12mo))+
                  Asym2/(1+exp((xmid2-precip_anom_3mo)/scal2))+
                  Asym3/(1+exp((xmid3-pet_anom_3mo)/scal3)),
                data = .,
                iter = 1000,
                start_lower = c(Asym=0.5, R0=0, lrc=0,
                                Asym2=-1, xmid2=-200, scal2=-100, 
                                Asym3=-0.5, xmid3=-100, scal3=-50),
                start_upper = c(Asym=1, R0=0.25, lrc=1,
                                Asym2=1, xmid2=200, scal2=300, 
                                Asym3=0.5, xmid3=100, scal3=50),
                # supp_errors = 'Y',
                na.action = na.omit, 
                lower = c(0.5, -0.1, 0, 
                          -0.5, -300, -100,
                          -0.5, -300, -100))
summary(fit_asymp_4)

fit_asymp_5 <- dat_fit %>% filter(year %in% c(1982:2019)) %>% 
  mutate(delta_year = hydro_year-1983, 
         sc_co2_int = scale(co2_int, center=T, scale=F)) %>% 
  nls_multstart(ndvi_3mo ~ ((Asym)+(R0-(Asym))*exp(-exp(lrc)*(pe_12mo)))+
                  (Asym2/(1+exp((xmid2-precip_anom_3mo)/scal2)))+
                  (Asym3/(1+exp((xmid3-pet_anom_3mo)/scal3))) + 
                  (beta4*sc_co2_int),
                data = .,
                iter = 100,
                start_lower = c(Asym=0.5, R0=0, lrc=0,
                                Asym2=-1, xmid2=-200, scal2=-100, 
                                Asym3=-0.5, xmid3=-100, scal3=-50, 
                                beta4=-0.1),
                start_upper = c(Asym=1, R0=0.25, lrc=1,
                                Asym2=1, xmid2=200, scal2=300, 
                                Asym3=0.5, xmid3=100, scal3=50, 
                                beta4=0.1),
                supp_errors = 'Y',
                na.action = na.omit, 
                lower = c(0.5, -0.1, 0, 
                          -0.5, -300, -100,
                          -0.5, -300, -100, 
                          -0.1))
summary(fit_asymp_5)

fit_asymp_6 <- dat_fit %>% 
  nls_multstart(ndvi_3mo ~  (Asym1/(1+exp((xmid1-pe_12mo)/scal1)))+
                  (Asym2/(1+exp((xmid2-precip_anom_3mo)/scal2)))+
                  (Asym3/(1+exp((xmid3-pet_anom_3mo)/scal3))) +
                  (beta4*sc_co2_int),
                data = .,
                iter = 1000,
                start_lower = c(Asym1=0.5, xmid1=0.1,  scal1=0,
                                Asym2=-1,  xmid2=-200, scal2=-100, 
                                Asym3=-0.5,xmid3=-100, scal3=-50, 
                                beta4=-0.1),
                start_upper = c(Asym1=1,   xmid1=0.75, scal1=100,
                                Asym2=1,   xmid2=200,  scal2=300, 
                                Asym3=0.5, xmid3=100,  scal3=50, 
                                beta4=0.1),
                supp_errors = 'Y',
                na.action = na.omit, 
                lower = c(0, 0, 0, 
                          -0.5, -300, -100,
                          -0.5, -300, -100, 
                          -0.1))
summary(fit_asymp_6)


# Generalized Logistic ----------------------------------------------------
# Asym/(1+exp((xmid-input)/scal))
# 
fit_glogis <- dat_fit %>% 
  nls_multstart(ndvi_3mo ~  A + (K-A)/(1+Q*exp(-B*pe_12mo)^(1/v)),
                data = .,
                iter = 100,
                start_lower = c(A=-0.25, K=0.5, Q=0,   B=10, v=1),
                start_upper = c(A=0.25, K=1.5,  Q=500, B=30, v=5),
                supp_errors = 'Y',
                na.action = na.omit, 
                #         -0.5:0.5  0.3:1  0.1:10    1:1e3  
                lower = c(A=0.0975, K=0.6,     Q=0.1,    B=1,   v=1),
                upper = c(A=0.125,  K=0.9,     Q=1000,    B=2000, v=1000))
fit_glogis
summary(fit_glogis)
fn_glogis <- function(x, A,K,B,v,Q,C) A + (K-A)/(C+Q*exp(-B*x)^(1/v))
plot(ndvi_3mo~pe_12mo,data=dat_fit[sample(.N,1000)], ylim=c(0,1),pch=20,col='gray')
curve(fn_glogis(x, A=coef(fit_glogis)["A"], # lower asymp
                   K=coef(fit_glogis)["K"], # upper asymp
                   B=coef(fit_glogis)["B"], # growth rate
                   v=coef(fit_glogis)["v"],
                   Q=coef(fit_glogis)["Q"], 
                   C=1),#coef(fit_glogis)["C"])
      ,from = 0.1,to = 3, 
      ylab='NDVI',add=T,col='purple',lwd=2)
plot(ndvi_3mo~pe_12mo,data=dat_fit[sample(.N,1000)], ylim=c(0,1))
curve(fn_glogis(x,A=-0.3,K=0.7,B=1500,v=301,Q=1.15,C=1),0,3,ylim=c(0,1),add=T,col='blue')

curve(fn_glogis(x,A=0,K=0.8,B=20,v=3,Q=100,C=1),0,3,ylim=c(0,1))


AIC(fit_asymp_6)
bbmle::AICtab(fit_asymp, fit_asymp_1, fit_asymp_2, fit_asymp_3, fit_asymp_4,fit_asymp_5,fit_asymp_6)
bbmle::BICtab(fit_asymp, fit_asymp_1, fit_asymp_2, fit_asymp_3, fit_asymp_4,fit_asymp_5)


curve(SSasymp(input=x, Asym=coef(fit_asymp)["Asym"], R0=coef(fit_asymp)["R0"], 
              lrc=coef(fit_asymp)["lrc"]),0,2, ylim=c(0,0.9))
curve(SSasymp(input=x, Asym=coef(fit_asymp_1)["Asym"], R0=coef(fit_asymp_1)["R0"], 
              lrc=coef(fit_asymp_1)["lrc"]),0,2, add=T, col='blue')
curve(SSasymp(input=x, Asym=coef(fit_asymp_2)["Asym"], R0=coef(fit_asymp_2)["R0"], 
              lrc=coef(fit_asymp_2)["lrc"]),0,2, add=T, col='red')
curve(SSasymp(input=x, Asym=coef(fit_asymp_3)["Asym"], R0=coef(fit_asymp_3)["R0"], 
              lrc=coef(fit_asymp_3)["lrc"]),0,2, add=T, col='purple')
curve(SSasymp(input=x, Asym=coef(fit_asymp_4)["Asym"], R0=coef(fit_asymp_4)["R0"], 
              lrc=coef(fit_asymp_4)["lrc"]),0,2, add=T, col='orange')


(cbind(predict(fit_asymp, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2
(cbind(predict(fit_asymp_1, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2
(cbind(predict(fit_asymp_2, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2
(cbind(predict(fit_asymp_3, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2
(cbind(predict(fit_asymp_4, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2
(cbind(predict(fit_asymp_5, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2
(cbind(predict(fit_asymp_6, newdata=dat_fit), dat_fit$ndvi_3mo) %>% na.omit() %>% cor)**2


expand_grid(pe_12mo=seq(0.1,2,length.out=100), 
            precip_anom_3mo=c(-200,0,200), 
            pet_anom_3mo=0, 
            sc_co2_int=c(-40,0,40)) %>% 
  mutate(pred = predict(fit_asymp_5, newdata=.)) %>% 
  ggplot(data=., aes(pe_12mo, pred,color=as_factor(sc_co2_int)))+
  geom_line()



A <- coef(fit_asymp)["Asym"]; R <- coef(fit_asymp)["R0"]; L <- coef(fit_asymp)["lrc"]
abline(v=(log((A - R)/A) + 0.693147180559945)*exp(-L),
       col='blue')
abline(h=0.5*A, col='blue')
abline(v=0.5)


b1 <- bam(ndvi_3mo ~ s(I(precip_12mo/pet_12mo)), 
          data=dat_fit, select=TRUE)
b2 <- bam(ndvi_3mo ~ s(I(precip_12mo/pet_12mo))+s(precip_anom_3mo)+s(pet_anom_3mo), 
          data=dat_fit, select=TRUE)
b3 <- bam(ndvi_3mo ~ s(I(precip_12mo/pet_12mo))+s(sc_co2_int), 
          data=dat_fit %>% mutate(sc_co2_int = scale(co2_int, center=T, scale=F)), 
          select=TRUE)
b4 <- bam(ndvi_3mo ~ s(I(precip_12mo/pet_12mo))+
            s(precip_anom_3mo)+s(pet_anom_3mo)+sc_co2_trend, 
          data=dat_fit %>% mutate(sc_co2_trend = scale(co2_trend, center=T, scale=F)), 
          select=TRUE)
b5 <- bam(ndvi_3mo ~ te(I(precip_12mo/pet_12mo),precip_anom_3mo,pet_anom_3mo,sc_co2_trend),
          data=dat_fit %>% mutate(sc_co2_trend = scale(co2_trend, center=T, scale=F)), 
          select=TRUE)

plot(b1)
plot(b2)
plot(b3)
plot(b4, pages = 1,scale=0)
bbmle::AICtab(b1,b2,b3,b4)
bbmle::BICtab(b1,b2,b3,b4)
print(plot(getViz(b4),allTerms=TRUE),pages=1)


fit_asymp_3 <- nls_multstart(ndvi_3mo~Asym/(1+exp((xmid-precip_anom_3mo)/scal)), 
                             start_lower = c(Asym=0.5, xmid=-100, scal=0), 
                             start_upper = c(Asym=1, xmid=100, scal=1), 
                             data=dat_fit %>% filter(year %in% c(1982:2019)), 
                             na.action = na.omit,
                             iter=100)
curve(SSlogis(input=x, Asym=coef(fit_asymp_3)["Asym2"], xmid=coef(fit_asymp_3)["xmid2"], 
              scal=coef(fit_asymp_3)["scal2"]),-2000,2000, add=F, col='red')

curve(SSlogis(input=x, Asym=coef(fit_asymp_4)["Asym3"], xmid=coef(fit_asymp_4)["xmid3"], 
              scal=coef(fit_asymp_4)["scal3"]),-200,200, add=F, col='red'); abline(v=0)
theta <- 0.5
curve(cos(theta)*SSlogis(input=x, Asym=coef(fit_asymp_4)["Asym3"], xmid=coef(fit_asymp_4)["xmid3"], 
              scal=coef(fit_asymp_4)["scal3"])+
      sin(theta)*SSlogis(input=x, Asym=coef(fit_asymp_4)["Asym3"], xmid=coef(fit_asymp_4)["xmid3"], 
                scal=coef(fit_asymp_4)["scal3"]),-200,200, add=T, col='blue'); abline(v=0)





b_low <- bam(ndvi_3mo ~ s(I(precip_12mo/pet_12mo))+s(precip_anom_3mo)+s(pet_anom_3mo)+
               sc_co2_trend,
          data=dat_fit %>% mutate(sc_co2_trend = scale(co2_trend, center=T, scale=F)) %>% 
            filter(pe_12mo < 0.5), 
          select=TRUE)
b_mod <- bam(ndvi_3mo ~ s(I(precip_12mo/pet_12mo))+s(precip_anom_3mo)+s(pet_anom_3mo)+
               sc_co2_trend,
             data=dat_fit %>% mutate(sc_co2_trend = scale(co2_trend, center=T, scale=F)) %>% 
               filter(pe_12mo > 0.5 & pe_12mo<1), 
             select=TRUE)
b_high <- bam(ndvi_3mo ~ s(I(precip_12mo/pet_12mo))+s(precip_anom_3mo)+s(pet_anom_3mo)+
                   sc_co2_trend,
                 data=dat_fit %>% mutate(sc_co2_trend = scale(co2_trend, center=T, scale=F)) %>% 
                   filter(pe_12mo > 1), 
                 select=TRUE)
b_all <- bam(ndvi_3mo ~ s(I(precip_12mo/pet_12mo))+s(precip_anom_3mo)+s(pet_anom_3mo)+
                sc_co2_trend,
              data=dat_fit %>% mutate(sc_co2_trend = scale(co2_trend, center=T, scale=F)), 
              select=TRUE)

summary(b_low)
summary(b_mod)
summary(b_high)
coef(b_low)["sc_co2_trend"]/coef(b_all)["sc_co2_trend"]
coef(b_mod)["sc_co2_trend"]/coef(b_all)["sc_co2_trend"]
coef(b_high)["sc_co2_trend"]/coef(b_all)["sc_co2_trend"]


curve(1*exp(-0.001*x),-500,500)

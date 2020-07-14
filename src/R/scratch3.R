curve(SSfpl(input = x,A = -8, B=0.8,xmid = -1.4, scal = 0.55),0,2, 
      ylim=c(0,1))
curve(predict(n_fpl, newdata=tibble(pe_12mo=x)), 0,2, 
      ylim=c(0,1))
points(ndvi_3mo~pe_12mo, data=train_dat[1:1000,],cex=0.1,pch=20,col='red')

curve(-1/exp(1.5*x)+(0.75),0,2)


curve(predict(n_fpl, newdata=tibble(pe_12mo=x)), 0,2, 
      ylim=c(0,1))
curve(predict(n_exp, newdata=tibble(pe_12mo=x)), 0,2, 
      ylim=c(0,1),add=T,col='red');
curve(predict(n_exp_x2_6p, newdata=tibble(pe_12mo=x,co2_trend=360)), 0,2, 
      ylim=c(0,1),add=T,col='blue')
curve(predict(n_exp_x2_6p, newdata=tibble(pe_12mo=x,co2_trend=420)), 0,2, 
      ylim=c(0,1),add=T,col='purple')



train_dat %>% 
  as_tibble() %>% 
  mutate(tmax_frac_anom = tmax_anom_3mo/matmax) %>% 
  mutate(pred_ric = predict(n_ric, newdata=.),
         pred3 = predict(n3, newdata=.), 
         pred4 = predict(n4, newdata=.), 
         pred5 = predict(n_exp_x2_6p_tmax, newdata=.), 
         pred6 = predict(n_exp_co2_tmax, newdata=.), 
         pred7 = predict(n_exp_co2_tmax_2, newdata=.), 
         pred8 = predict(n_exp_co2_tmax_3, newdata=.)) %>% 
  summarize(rmse_ric = sqrt(mean((pred_ric-ndvi_3mo)**2,na.rm=TRUE)), 
            rmse3 = sqrt(mean((pred3-ndvi_3mo)**2,na.rm=TRUE)), 
            rmse4 = sqrt(mean((pred4-ndvi_3mo)**2,na.rm=TRUE)), 
            rmse5 = sqrt(mean((pred5-ndvi_3mo)**2,na.rm=TRUE)), 
            rmse6 = sqrt(mean((pred6-ndvi_3mo)**2,na.rm=TRUE)),
            rmse7 = sqrt(mean((pred7-ndvi_3mo)**2,na.rm=TRUE)), 
            rmse8 = sqrt(mean((pred8-ndvi_3mo)**2,na.rm=TRUE)))


em_fit <- bam(tmax_3mo ~ te(pe_12mo,tmax_anom_3mo), data=train_dat, 
              select=TRUE, method='fREML',discrete=TRUE)
summary(em_fit)
pred_dat <- expand_grid(
  co2_trend = seq(min(tmp$co2_trend),max(tmp$co2_trend),length.out=3),
  # co2_trend=c(340,390,440),
  tmax_anom_3mo = c(-5,0,5),
  pe_12mo = seq(0.01,2,length.out = 100)) %>% 
  mutate(tmax_3mo = predict(em_fit, newdata=.))
pred_dat$pred <- predict(n_exp_co2_tmax_3, 
                         newdata=pred_dat)

pred_dat %>% 
  mutate(combo = paste("co2:",co2_trend," tmax:",tmax_anom_3mo)) %>% 
  ggplot(data=., aes(pe_12mo,pred,
                     linetype=as_factor(tmax_anom_3mo), 
                     color=as_factor(co2_trend)))+
  geom_line(alpha=1)+
  geom_hline(aes(yintercept=0.75),lty=3)+
  scale_color_viridis_d(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2), expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  # facet_wrap(co2_trend~tmax_anom_3mo, nrow = 3)+
  theme_linedraw()
# theme(#panel.grid = element_blank(), 
#       axis.text = element_text(size=10), 
#       # legend.position = c(0.7,0.1), 
#       legend.key.width = unit(1,'cm'),
#       legend.direction = 'horizontal')


curve(log(1+exp(x))/exp(x),-5,5)
curve(exp(1+x)/(log(1+exp(x))),-5,5)

curve((x+1)/(x+8),-8,8)


library(mgcv)
o <- train_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_exp,newdata=.)) %>% 
  mutate(res = ndvi_3mo-pred) %>% 
  bam(res~s(I(tmax_anom/matmax), bs='cs'), 
      data=.,
      select=TRUE, discrete = TRUE)
summary(o)
plot(o)

o <- train_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_exp,newdata=.)) %>% 
  mutate(res = ndvi_3mo-pred) %>% 
  bam(res~s(tmax_3mo, bs='cs'), 
      data=.,
      select=TRUE, discrete = TRUE)
summary(o)
plot(o)


train_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_exp,newdata=.)) %>% 
  mutate(res = pred-ndvi_3mo) %>% 
  ggplot(data=., aes(pred,res,color=tmax_anom))+
  geom_point()+
  geom_smooth(se=F,color='red')+
  scale_color_gradient2(trans='reverse',mid='gray')


mo <- train_dat %>% as_tibble() %>% 
  bam(ndvi_3mo~
        s(pe_12mo, bs='cs')+
        s(tmax_3mo,bs='cs')+
        s(co2_trend, bs='cs'), 
      data=.,
      select=TRUE, discrete = TRUE)
summary(mo)
getViz(mo) %>% plot(allTerms=TRUE) %>% print(pages=1)
sqrt(mean(test_dat$ndvi_3mo - predict(mo, newdata=test_dat))**2)
train_dat %>% as_tibble() %>% 
  mutate(pred=predict(mo, newdata=.)) %>%
  filter(is.na(pred)==T) %>% 
  select(ndvi_3mo, pe_12mo, tmax_3mo)

train_dat %>% as_tibble() %>% 
  ggplot(data=., aes(co2_trend, precip_anom_12mo))+
  geom_point()+
  geom_smooth(se=F)

1.6*(g0 + g1/sqrt(D))*A/CO2

n_exp_co2_tmax_5
test_dat %>% 
  as_tibble() %>% 
  mutate(pred=predict(n_gs_3,newdata=.)) %>%
  mutate(pred_alt=predict(n_gs_2,newdata=.)) %>% 
  filter(is.na(pred)==F) %>% 
  summarize(rmse=mean((pred-ndvi_3mo)**2, na.rm=TRUE), 
            r2 = cor(pred,ndvi_3mo)**2, 
            rmse_alt=mean((pred_alt-ndvi_3mo)**2, na.rm=TRUE), 
            r2_alt = cor(pred_alt,ndvi_3mo)**2)


expand_grid(pe_12mo=0.5, 
            pet_12mo = 1000,
            mape=0.5,
            precip_12mo=500,
            rad_3mo=250,
            tmax_3mo=25,
            tmax_anom_3mo=0,
            matmax=25,
            vpd15_3mo=2,
            co2_trend=seq(320,450,length.out = 100)) %>% 
  mutate(pred=predict(n_exp_co2_tmax_6,newdata=.)) %>% 
  ggplot(data=., aes(co2_trend,pred))+
  geom_line()

expand_grid(pe_12mo=seq(0.1,2,length.out = 100), 
            mape=1,
            rad_3mo=250,
            # tmax_3mo=seq(10,45,length.out=100),
            tmax_3mo=25,
            # tmax_3mo = seq(10,40,length.out = 100),
            tmax_anom_3mo=c(0),
            vpd15_anom_sd=c(0),
            vpd15=1,
            matmax=25,
            co2_trend=c(380,400,420)) %>%
  mutate(cco2 = co2_trend-380) %>% 
  mutate(pred=predict(n_ric_co2_tmax,newdata=.)) %>% 
  ggplot(data=., aes(pe_12mo,pred,color=co2_trend,group=co2_trend))+
  geom_line()+
  scale_color_viridis_c()

expand_grid(pe_12mo=seq(0.1,2,length.out=100), 
            mape=1,
            rad_3mo=250,
            # tmax_3mo=seq(10,45,length.out=100),
            tmax_3mo=25,
            # tmax_anom_3mo=seq(-5,5,length.out = 100),
            tmax_anom_3mo=0,
            matmax=25,
            co2_trend=c(400)) %>% 
  mutate(pred=predict(n_gs_3,newdata=.)) %>% 
  ggplot(data=., aes(pe_12mo,pred))+
  geom_line()

expand_grid(pe_12mo=0.5, 
            mape=1,
            rad_3mo=250,
            precip_12mo=1000,
            # tmax_3mo=seq(10,45,length.out=100),
            tmax_3mo=25,
            tmax_anom_3mo=0,
            vpd15_3mo=seq(0.1,8,length.out = 100),
            matmax=25,
            co2_trend=c(400)) %>% 
  mutate(pred=predict(n_gs_3,newdata=.)) %>% 
  ggplot(data=., aes(vpd15_3mo,pred))+
  geom_line()
# g0*(1+g1/(1/pe_12mo))*pe_12mo/(co2_trend) + g2*tmax_anom_3mo,


train_dat %>% mutate(val=(pe_12mo*rad_3mo)**0.5) %>% pull(val) %>% hist
train_dat %>% mutate(val=(sqrt(mape)/sqrt(pe_12mo))) %>% pull(val) %>% hist

                     





expand_grid(pe_12mo=seq(0.25,0.75,length.out=100), 
            mape=0.5,
            rad_3mo=250,
            # tmax_3mo=seq(10,45,length.out=100),
            tmax_3mo=25,
            # tmax_anom_3mo=seq(-5,5,length.out = 100),
            tmax_anom_3mo=0,
            matmax=25,
            co2_trend=c(350,400,450)) %>% 
  mutate(pred=predict(n_gs_2,newdata=.)) %>% 
  mutate(pe_anom_12mo = pe_12mo-mape) %>% 
  ggplot(data=., aes(pe_anom_12mo,pred,color=as_factor(co2_trend)))+
  geom_line()


















i_tmax <- lm(tmax_3mo~pe_12mo, data=train_dat)
pred_djf <- expand_grid(#season=unique(tmp$season), 
  co2_trend = seq(min(tmp$co2_trend),max(tmp$co2_trend),length.out=50),
  pe_12mo = seq(0.01,2.05,length.out = 200)) %>%
  mutate(tmax_3mo = predict(i_tmax, newdata=.)) %>%  
  mutate(pred = predict(n_exp_co2_tmax, newdata=.), 
         season = "DJF")

pred_djf %>% 
  ggplot(data=., aes(pe_12mo,pred,color=(co2_trend), 
                     group=co2_trend))+
  geom_line(alpha=1)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2.05),
                     breaks=c(0,0.5,1,1.5,2),
                     labels = c(0,0.5,1,1.5,2),
                     expand=c(0,0)
                     # guide = guide_axis(n.dodge=1, angle=0,check.overlap = TRUE)
  )+
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  facet_wrap(~season,nrow=2)+
  theme_linedraw()+
  guides(color=guide_colorbar(title.position = 'top'))+
  theme(panel.grid = element_blank(),
        # panel.spacing.x = unit(6, "mm"),
        axis.text = element_text(size=10),
        # axis.text.x = element_text(angle=45, vjust=-0.5),
        legend.position = c(0.85,0.1), 
        legend.key.width = unit(0.65,'cm'),
        legend.direction = 'horizontal', 
        legend.background = element_rect(fill=NA))
# ggsave("figures/ndvi3mo_PE12mo_richard_x2_nlsFit_bySeason.png", 
#        width=15, height=12, units='cm', dpi=350, type='cairo')






ric_x2_son <- nls(ndvi_3mo ~ 
                    mod_grad_ric_x2(x = pe_12mo,x2=cco2, 
                                    Asym=Asym,Asym2=Asym2, 
                                    xmid=xmid, xmid2=xmid2,
                                    scal=scal,scal2=scal2,
                                    lpow=lpow, lpow2=lpow2),
                  data=train_son, 
                  control=nls.control(maxiter=1000),
                  start = c(Asym=0.7561,Asym2=5e-4,
                            xmid=0.27,xmid2=3e-4,
                            scal=0.28,scal2=0,
                            lpow=-0.28,lpow2=0), 
                  lower=c(0.5, -0.01, 
                          -0.5, -0.01, 
                          0.15, -0.001, 
                          -5, -0.01), 
                  algorithm = 'port')
summary(ric_x2_son)


sqrt(mean((predict(ric_x2_son, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(ric_x2_son, newdata=test_dat),test_dat$ndvi_3mo)**2

test_dat %>% as_tibble() %>% 
  sample_n(1000) %>% 
  mutate(pred=predict(ric_x2_son, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')

expand_grid(#season=unique(tmp$season), 
  co2 = seq(min(tmp$co2_int),max(tmp$co2_int),length.out=50),
  # co2_int = seq(min(tmp$co2_int),max(tmp$co2_int),length.out=50),
  # co2_trend = seq(min(tmp$co2_trend),max(tmp$co2_trend),length.out=50),
  pe_12mo = seq(0.01,2.05,length.out = 200)) %>% 
  mutate(cco2 = co2-center_co2) %>%
  mutate(pred = predict(ric_x2_son, newdata=.), 
         season=train_son$season[1]) %>% 
  ggplot(data=., aes(pe_12mo,pred,color=(co2), group=co2))+
  geom_line(alpha=1)+
  # geom_vline(aes(xintercept=p50, color=hydro_year), 
  #            data=wdat %>% 
  #        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
  scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
  scale_x_continuous(limits=c(0,2.05),
                     breaks=c(0,0.5,1,1.5,2),
                     labels = c(0,0.5,1,1.5,2),
                     expand=c(0,0)
                     # guide = guide_axis(n.dodge=1, angle=0,check.overlap = TRUE)
  )+
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
  labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
       y=expression(paste(NDVI["3 mo"])))+
  facet_wrap(~season,nrow=2)+
  theme_linedraw()+
  guides(color=guide_colorbar(title.position = 'top'))+
  theme(panel.grid = element_blank(),
        # panel.spacing.x = unit(6, "mm"),
        axis.text = element_text(size=10),
        # axis.text.x = element_text(angle=45, vjust=-0.5),
        legend.position = c(0.85,0.1), 
        legend.key.width = unit(0.65,'cm'),
        legend.direction = 'horizontal', 
        legend.background = element_rect(fill=NA))








fn <- function(x,x2,Asym,Asym2,xmid,xmid2,scal,scal2,lpow,lpow2){
    (Asym+Asym2*x2) * 
      (1+exp(((xmid+xmid2*x2) - x)/(scal+scal2*x2)))^(-exp(-(lpow+lpow2*x2)))
}
grad <- deriv(body(fn)[[2]], 
  namevec = c("Asym","Asym2","xmid","xmid2","scal","scal2","lpow","lpow2"),
  function.arg = fn
)

n_ric_x2_1 <- nls_multstart(ndvi_3mo ~ grad(x = pe_12mo,x2=cco2, 
                                          Asym=Asym,Asym2=Asym2, 
                                          xmid=xmid, xmid2=xmid2,
                                          scal=scal,scal2=scal2, 
                                          lpow=lpow, lpow2=lpow2),
                          data=train_dat,
                          iter=10,
                          supp_errors = 'Y',
                          control=nls.control(maxiter=50),
                          start_upper = c(Asym=0.7561,Asym2=5e-4,
                                          xmid=0.27,xmid2=3e-4,
                                          scal=0.28,scal2=0.1,
                                          lpow=1, lpow2 =0.1), 
                          start_lower=c(0.5, -0.01, 
                                        -0.5, -0.01, 
                                        0.15,0, 
                                        -2, 0), 
                          lower=c(0.5, -0.01, 
                                  -0.5, -0.01, 
                                  0.15, -0.001, 
                                  -0.9, -0.01),
                          upper=c(1,0.01, 
                                  1, 0.1, 
                                  1, 0.1, 
                                -0.3, 0.01),
                          algorithm='port')
n_ric_x2_1 %>% summary
n_ric_x2 %>% summary


n_ric_x2 <- nls_multstart(ndvi_3mo ~ grad(x = pe_12mo,x2=cco2, 
                                         Asym=Asym,Asym2=Asym2, 
                                         xmid=xmid, xmid2=xmid2,
                                         scal=scal,scal2=scal2, 
                                        lpow=lpow, lpow2=lpow2),
                          data=train_dat,
                          iter=10,
                          supp_errors = 'Y',
                          control=nls.control(maxiter=100),
                          start_upper = c(Asym=0.7561,Asym2=5e-4,
                                          xmid=0.27,xmid2=3e-4,
                                          scal=0.28,scal2=0.1,
                                          lpow=1, lpow2 =0.1), 
                          start_lower=c(0.5, -0.01, 
                                        -0.5, -0.01, 
                                        0.15,0, 
                                        -2, 0), 
                          lower=c(0.5, -0.01, 
                                  -0.5, -0.01, 
                                  0.15, -0.001, 
                                  -0.9, -0.01),
                          upper=c(1,0.01, 
                                  1, 0.1, 
                                  1, 0.1, 
                                  -0.3, 0.01),
                          algorithm='port')
summary(n_ric_x2)
sqrt(mean((predict(n_ric_x2, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_ric_x2, newdata=test_dat),test_dat$ndvi_3mo)**2

expand_grid(#season=unique(tmp$season), 
# co2_trend = seq(min(tmp$co2_int),max(tmp$co2_int),length.out=50),
co2_trend = seq(min(tmp$co2_trend),max(tmp$co2_trend),length.out=50),
pe_12mo = seq(0.01,2.05,length.out = 200)) %>% 
mutate(cco2 = co2_trend-center_co2) %>%
mutate(pred = predict(n_ric_x2_1, newdata=.), 
       season=train_son$season[1]) %>% 
ggplot(data=., aes(pe_12mo,pred,color=(co2_trend), group=co2_trend))+
geom_line(alpha=1)+
# geom_vline(aes(xintercept=p50, color=hydro_year), 
#            data=wdat %>% 
#        mutate(p50 = (log((Asym - R0)/Asym) + 0.693147180559945)*exp(-lrc)))+
scale_color_viridis_c(expression(paste(CO[2]~ppm)), option='B',end=0.85)+
scale_x_continuous(limits=c(0,2.05),
                   breaks=c(0,0.5,1,1.5,2),
                   labels = c(0,0.5,1,1.5,2),
                   expand=c(0,0)
                   # guide = guide_axis(n.dodge=1, angle=0,check.overlap = TRUE)
)+
scale_y_continuous(limits=c(0,0.9), expand=c(0,0))+
labs(x=expression(paste(paste(sum(Precip[t], "1 mo", "12 mo")," / ", sum(PET[t], "1 mo", "12 mo")))), 
     y=expression(paste(NDVI["3 mo"])))+
facet_wrap(~season,nrow=2)+
theme_linedraw()+
guides(color=guide_colorbar(title.position = 'top'))+
theme(panel.grid = element_blank(),
      # panel.spacing.x = unit(6, "mm"),
      axis.text = element_text(size=10),
      # axis.text.x = element_text(angle=45, vjust=-0.5),
      legend.position = c(0.85,0.1), 
      legend.key.width = unit(0.65,'cm'),
      legend.direction = 'horizontal', 
      legend.background = element_rect(fill=NA))



# ndvi_3mo ~ a3 * cco2 + (a0 + a4 * cco2) * tanh(a1 * pe_12mo +     b1 * mape + a2 * cco2) + b0 * matmax + b2 * tmax_3mo + b3 *     tmax_3mo^2
curve(tanh(x*-8.578e-03),-40,40)
n_co2_topt <- nls_multstart(ndvi_3mo ~ 
              (a0*cco2+a1+a5*tmax_anom_3mo)*tanh(a2*pe_12mo+a3*tmax_anom_3mo+a4*cco2)+
              b0*(cco2)+b1*tmax_anom_3mo,
              # b3*tmax_anom_3mo**2,
              data=train_dat,
              iter=10,
              supp_errors = 'Y',
              control=nls.control(maxiter=100),
              start_lower = c(a0=0, a1=0,a2=0,a3=0,a4=0, a5=0,
                              b0=0,b1=0
                               # b0=0, b1=0,b2=0,b3=0
                              ), 
              start_upper = c(a0=1, a1=1, a2=0.001,a3=0.001,a4=0.1, a5=0.1,
                              b0=1,b1=1
                              # b0=0.8,b1=0.5,b2=1,b=1
                              ))
summary(n_co2_topt)
sqrt(mean((predict(n_co2_topt, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n_co2_topt, newdata=test_dat),test_dat$ndvi_3mo)**2

test_dat %>% as_tibble() %>% 
  sample_n(10000) %>% 
  mutate(pred=predict(n_co2_topt, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')


expand_grid(matmax = c(20),
            map = 1200,
            mape=0.8,
  tmax_anom_3mo = c(-2,0,4),
  cco2 = c(-40,0,40),
  pe_12mo = seq(0.1,2,length.out=50)) %>% 
  mutate(pred=predict(n_co2_topt,newdata=.)) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  ggplot(data=., aes(pe_12mo, pred, 
                     color=as_factor(co2),
                     # group=cco2, 
                     linetype=as_factor(tmax_anom_3mo)))+
  geom_line()+
  scale_color_viridis_d(option='B', end=0.8)+
  scale_x_continuous(expand=c(0,0))+
  facet_wrap(~matmax, scales = 'free')+
  theme_linedraw()+
  theme(panel.grid = element_blank())


#







# ndvi_3mo ~ a3 * cco2 + (a0 + a4 * cco2) * tanh(a1 * pe_12mo +     b1 * mape + a2 * cco2) + b0 * matmax + b2 * tmax_3mo + b3 *     tmax_3mo^2
n1 <- nls_multstart(ndvi_3mo ~ 
    a3 * cco2 + 
      (a0 + a4 * cco2) * tanh(a1 * pe_12mo +b1 * mape + a2 * cco2) + 
      b0 * matmax + b2 * tmax_3mo + b3 *tmax_3mo^2, 
          data=train_dat,
          iter=10,
          supp_errors = 'Y',
          control=nls.control(maxiter=100),
          start_lower = c(a0=0, a1=0,a2=0,a3=0,a4=0, 
                          b0=0,b1=0,b2=0,b3=0
                          # b0=0, b1=0,b2=0,b3=0
          ), 
          start_upper = c(a0=1, a1=1, a2=0.001,a3=0.001,a4=0.1, 
                          b0=1,b1=1,b2=1,b3=1
                          # b0=0.8,b1=0.5,b2=1,b=1
          ))
sqrt(mean((predict(n1, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n1, newdata=test_dat),test_dat$ndvi_3mo)**2





n2 <- nls_multstart(ndvi_3mo ~ 
              a3*cco2+
              (a0+a4*cco2)*tanh(a1*pe_12mo + b1*mape + a2*cco2)+
              b0*matmax + b2*tmax_3mo + b3*tmax_3mo**2,
            # a3*cco2+
            # a0*tanh(a1*pe_12mo+a2*cco2)+
            # b0*tanh(b1*mape+a4*cco2)+
            # b2*matmax+b3*tmax_anom_3mo**2,
                            data=train_dat,
                            iter=5,
                            supp_errors = 'Y',
                            control=nls.control(maxiter=100),
                            start_lower = c(a0=0, a1=0,a2=0,a3=0,a4=0,
                                            b0=0, b1=0,b2=0,b3=0), 
                            start_upper = c(a0=1, a1=1, a2=0.001,a3=0.001,a4=0.001,
                                            b0=0.8,b1=0.5,b2=1, b3=1))
summary(n2)
sqrt(mean((predict(n2, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n2, newdata=test_dat),test_dat$ndvi_3mo)**2

n3 <- nls_multstart(ndvi_3mo ~ 
      a3*cco2+
      (a0+a4*cco2)*tanh(a1*pe_12mo + b1*mape + a2*cco2)+
      b2*tmax_3mo + b3*tmax_3mo**2+
      b0*precip_anom_12mo+
      c1*precip_anom_12mo**2 + c2*precip_anom_12mo**3,
                    # a3*cco2+
                    # a0*tanh(a1*pe_12mo+a2*cco2)+
                    # b0*tanh(b1*mape+a4*cco2)+
                    # b2*matmax+b3*tmax_anom_3mo**2,
                    data=train_dat,
                    iter=5,
                    supp_errors = 'Y',
                    control=nls.control(maxiter=100),
                    start_lower = c(a0=0, a1=0,a2=0,a3=0,a4=0,
                                    b0=0, b1=0,b2=0,b3=0, 
                                    c1=0,c2=0), 
                    start_upper = c(a0=1, a1=1, a2=0.001,a3=0.001,a4=0.001,
                                    b0=0.8,b1=0.5,b2=1, b3=1, 
                                    c1=1,c2=1))
summary(n3)
sqrt(mean((predict(n3, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n3, newdata=test_dat),test_dat$ndvi_3mo)**2

n4 <- nls_multstart(ndvi_3mo ~ 
          a0*tanh(a1*cco2+a2*mape+a3*pe_12mo+a6*tmax_anom_3mo)+
          a4*cco2 + a5*tmax_anom_3mo,
          # d0*tanh(d1*tmax_anom_3mo+d2*matmax),
    data=train_dat,
    iter=5,
    supp_errors = 'Y',
    control=nls.control(maxiter=100),
    start_lower = c(a0=0, a1=0, a2=0, a3=0, a4=0, a5=0,a6=0),
                    # d0=0,d1=0,d2=0), 
    start_upper = c(a0=1, a1=1, a2=1, a3=1, a4=1, a5=1,a6=1))
summary(n4)
sqrt(mean((predict(n4, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n4, newdata=test_dat),test_dat$ndvi_3mo)**2

test_dat %>% as_tibble() %>% 
  sample_n(10000) %>% 
  mutate(pred=predict(n4, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')

expand_grid(matmax = 
              c(10),
            map = 1200,
            mape=seq(0.1,2,length.out=50),
            tmax_anom_3mo = c(-4,0,4),
            tmax_3mo = c(25,27,30),
            cco2 = c(-40,0,40),
            pe_12mo = seq(0.1,2,length.out=1)) %>% 
  mutate(pred=predict(n4,newdata=.)) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  ggplot(data=., aes(mape, pred, 
                     color=as_factor(co2),
                     # group=cco2, 
                     linetype=as_factor(tmax_anom_3mo)))+
  geom_line()+
  scale_color_viridis_d(option='B', end=0.8)+
  scale_x_continuous(expand=c(0,0))+
  facet_wrap(~pe_12mo, scales = 'free')+
  theme_linedraw()+
  theme(panel.grid = element_blank())




n5 <- nls_multstart(ndvi_3mo ~ 
        (a0*cco2+a1*pe_12mo+a2*tmax_3mo)+
        b0*tanh(b1*mape)+
        b2*log(pe_12mo),
                    data=train_dat,
                    iter=5,
                    supp_errors = 'Y',
                    control=nls.control(maxiter=100),
                    start_lower = c(a0=0, a1=0, a2=0, 
                                    b0=0, b1=0, b2=0),
                    # d0=0,d1=0,d2=0), 
                    start_upper = c(a0=1, a1=1, a2=1, 
                                    b0=1, b1=1, b2=1))
summary(n5)
sqrt(mean((predict(n5, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n5, newdata=test_dat),test_dat$ndvi_3mo)**2

test_dat %>% as_tibble() %>% 
  sample_n(10000) %>% 
  mutate(pred=predict(n5, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')

expand_grid(matmax = 
              c(10),
            map = 1200,
            mape=seq(0.1,2,length.out=50),
            tmax_anom_3mo = c(0),
            tmax_3mo = c(25),
            cco2 = c(-40,0,40),
            pe_12mo = seq(0.1,2,length.out=50)) %>% 
  mutate(pred=predict(n5,newdata=.)) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  ggplot(data=., aes(mape, pred, 
                     color=as_factor(co2),
                     # group=cco2, 
                     linetype=as_factor(co2)))+
  geom_line()+
  scale_color_viridis_d(option='B', end=0.8)+
  scale_x_continuous(expand=c(0,0))+
  # facet_wrap(~pe_12mo, scales = 'free')+
  theme_linedraw()+
  theme(panel.grid = element_blank())



curve(x*2.2e-02 + -4.574e-04*x**2, 0, 40)
# ndvi_3mo ~ a3 * cco2 + (a0 + a4 * cco2) * tanh(a1 * pe_12mo +     b1 * mape + a2 * cco2) + b0 * matmax + b2 * tmax_3mo + b3 *     tmax_3mo^2

# good model, but co2 effect seems too high in wet forests
# a3*cco2 + 
#   (a0+a4*cco2) * tanh(a1*mape +  a2*cco2) +
#   b1*pe_anom_12mo+
#   b0*matmax + b2*tmax_3mo + b3*tmax_3mo^2,

curve(tanh(0.1*x),-50,50)
curve(400**x,0.1,2)
curve((1.125e-03*440*x)/(1+x),0.1,2)
curve(()/(1+x),0.1,2)
curve(log((x)/(x+1)),300,500)

n6 <- nls_multstart(ndvi_3mo ~ 
    (a3*cco2)+
  (a0+
       a4*(pe_anom_12mo/cco2)+
       b1*pe_anom_12mo) * tanh(a1*mape + 
                                          a2*cco2 +
                                          a5*pe_anom_12mo) +
  # b1*pe_anom_12mo+
  b0*matmax + b2*tmax_3mo + b3*tmax_3mo^2,
                    data=train_dat,
                    iter=5,
                    supp_errors = 'Y',
                    control=nls.control(maxiter=100),
                    start_lower = c(a0=0, a1=0,a2=0, 
                                    a3=0,
                                    a4=0,
                                    a5=0,
                                    b0=0, b1=0, b2=0,b3=0),
                    # d0=0,d1=0,d2=0), 
                    start_upper = c(a0=1, a1=1,a2=1, 
                                    a3=1,
                                    a4=1,
                                    a5=1,
                                    b0=1, b1=1, b2=0.1, b3=1))
summary(n6)
sqrt(mean((predict(n6, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n6, newdata=test_dat),test_dat$ndvi_3mo)**2
curve(coef(n6)["b0"]*25 + coef(n6)["b2"]*x + coef(n6)["b3"]*x**2, 20,36)

test_dat %>% as_tibble() %>% 
  sample_n(10000) %>% 
  mutate(pred=predict(n6, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')

f_matmax <- lm(matmax~mape, data=train_dat)
f_tmax_3mo <- lm(tmax_3mo~mape+pe_anom_12mo, data=train_dat)
f_vpd15_3mo <- lm(vpd15_3mo~mape+pe_anom_12mo, data=train_dat)

expand_grid(map = 1200,
            mape=seq(0.1,2,length.out=50),
            tmax_anom_3mo = c(0),
            cco2 = c(-40,0,40),
            pe_anom_12mo=c(-0.5,0,0.5),
            # pe_12mo = seq(0.1,2,length.out=50)
            ) %>% 
  mutate(pe_12mo = mape) %>% 
  mutate(matmax = predict(f_matmax, newdata=. )) %>% 
  mutate(tmax_3mo = predict(f_tmax_3mo, newdata=.)) %>% 
  mutate(vpd15_3mo = predict(f_vpd15_3mo, newdata=.)) %>% 
  mutate(co2_trend = cco2+center_co2) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  mutate(pred=predict(n6,newdata=.)) %>% 
  ggplot(data=., aes(mape, pred, 
                     color=as_factor(co2),
                     # group=cco2, 
                     linetype=as_factor(pe_anom_12mo)))+
  geom_line()+
  scale_color_viridis_d(option='B', end=0.8)+
  scale_x_continuous(expand=c(0,0))+
  facet_wrap(~pe_anom_12mo, scales = 'fixed',nrow = 1)+
  theme_linedraw()+
  theme(panel.grid = element_blank())

expand_grid(matmax = c(25),
            map = 1200,
            mape=seq(0.1,2,length.out=50),
            tmax_anom_3mo = c(0),
            tmax_3mo = c(25),
            cco2 = c(0),
            pe_anom_12mo=c(-0.2,0,0.2),
            # pe_12mo = seq(0.1,2,length.out=50)
) %>% 
  mutate(pe_12mo = mape) %>% 
  mutate(pred=predict(n6,newdata=.)) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  ggplot(data=., aes(mape, pred, 
                     color=as_factor(pe_anom_12mo),
                     # group=cco2, 
                     linetype=as_factor(pe_anom_12mo)))+
  geom_line()+
  scale_color_viridis_d(option='B', end=0.8)+
  scale_x_continuous(expand=c(0,0))+
  # facet_wrap(~pe_12mo, scales = 'free')+
  theme_linedraw()+
  theme(panel.grid = element_blank())





n7 <- nls_multstart(ndvi_3mo ~ 
        a3*cco2 + a4*cco2**2+
        (a0)* tanh(a1*mape) +
        b1*pe_anom_12mo+
        b0*matmax + b2*(tmax_3mo/matmax) + b3*(tmax_3mo/matmax)^2,
                    data=train_dat,
                    iter=5,
                    supp_errors = 'Y',
                    control=nls.control(maxiter=100),
                    start_lower = c(a0=0, a1=0, a3=0,a4=0, 
                                    b0=0, b1=0, b2=0,b3=0),
                    # d0=0,d1=0,d2=0), 
                    start_upper = c(a0=1, a1=1, a3=1,a4=1,
                                    b0=1, b2=0.1, b2=1,b3=1))
summary(n7)
sqrt(mean((predict(n7, newdata=test_dat)-test_dat$ndvi_3mo)**2))
cor(predict(n7, newdata=test_dat),test_dat$ndvi_3mo)**2


curve(coef(n7)["b0"]*25 + coef(n7)["b2"]*x + coef(n7)["b3"]*x**2, 0.8,1.2)
test_dat %>% as_tibble() %>% 
  sample_n(10000) %>% 
  mutate(pred=predict(n7, newdata=.)) %>% 
  ggplot(data=., aes(pred, ndvi_3mo))+
  geom_point()+
  geom_smooth()+
  geom_abline(aes(intercept=0,slope=1),color='red')

i_matmax <- lm(matmax~mape, data=train_dat)
i_tmax <- lm(tmax_3mo~pe_anom_12mo, data=train_dat)
expand_grid(
       map=1200, 
       mape=seq(0.1,2,length.out = 50),
       pe_anom_12mo=c(-0.25,0,0.25),
       cco2=c(-40,0,40)) %>% 
  mutate(matmax = predict(i_matmax,newdata=.)) %>% 
  mutate(tmax_3mo = predict(i_tmax,newdata=.)) %>% 
  mutate(pe_12mo = mape) %>% 
  mutate(pred=predict(n6,newdata=.)) %>% 
  mutate(co2 = round(cco2+center_co2)) %>% 
  mutate(tmax_frac_anom = tmax_3mo/matmax) %>% 
  ggplot(data=., aes(mape, pred, 
                     color=as_factor(co2),
                     # group=cco2, 
                     # linetype=as_factor(tmax_frac_anom)
                     )
         )+
  geom_line()+
  scale_color_viridis_d(option='B', end=0.8)+
  scale_x_continuous(expand=c(0,0))+
  # facet_wrap(~pe_12mo, scales = 'free')+
  theme_linedraw()+
  theme(panel.grid = element_blank())







bbmle::AICtab(n_co2_topt, 
              n1,n2,n3,n4,n5,n6,n7)
bbmle::BICtab(n_co2_topt, 
              n1,n2,n3,n4,n5,n6)
yardstick::rmse_vec(test_dat$ndvi_3mo, predict(n1,newdata = test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo, predict(n2,newdata = test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo, predict(n3,newdata = test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo, predict(n4,newdata = test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo, predict(n5,newdata = test_dat))
yardstick::rmse_vec(test_dat$ndvi_3mo, predict(n6,newdata = test_dat))

yardstick::rsq_vec(test_dat$ndvi_3mo, predict(n1,newdata = test_dat))
yardstick::rsq_vec(test_dat$ndvi_3mo, predict(n2,newdata = test_dat))
yardstick::rsq_vec(test_dat$ndvi_3mo, predict(n3,newdata = test_dat))
yardstick::rsq_vec(test_dat$ndvi_3mo, predict(n4,newdata = test_dat))
yardstick::rsq_vec(test_dat$ndvi_3mo, predict(n5,newdata = test_dat))
yardstick::rsq_vec(test_dat$ndvi_3mo, predict(n6,newdata = test_dat))

yardstick::mase_vec(test_dat$ndvi_3mo, predict(n1,newdata = test_dat))
yardstick::mase_vec(test_dat$ndvi_3mo, predict(n2,newdata = test_dat))
yardstick::mase_vec(test_dat$ndvi_3mo, predict(n3,newdata = test_dat))
yardstick::mase_vec(test_dat$ndvi_3mo, predict(n4,newdata = test_dat))
yardstick::mase_vec(test_dat$ndvi_3mo, predict(n5,newdata = test_dat))
yardstick::mase_vec(test_dat$ndvi_3mo, predict(n6,newdata = test_dat))

yardstick::rpiq_vec(test_dat$ndvi_3mo, predict(n1,newdata = test_dat))
yardstick::rpiq_vec(test_dat$ndvi_3mo, predict(n2,newdata = test_dat))
yardstick::rpiq_vec(test_dat$ndvi_3mo, predict(n3,newdata = test_dat))
yardstick::rpiq_vec(test_dat$ndvi_3mo, predict(n4,newdata = test_dat))
yardstick::rpiq_vec(test_dat$ndvi_3mo, predict(n5,newdata = test_dat))
yardstick::rpiq_vec(test_dat$ndvi_3mo, predict(n6,newdata = test_dat))


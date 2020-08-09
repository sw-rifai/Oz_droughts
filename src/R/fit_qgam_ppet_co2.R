library(qgam); library(tidyverse); library(lubridate); 
library(colorspace)

quSeq <- c(0.25, 0.5, 0.99)
fit <- mqgam(ndvi_3mo ~ s(pe_12mo, bs='cs',k=5) + 
               s(I(pe_anom_12mo/mape),bs='cs',k=5) + 
               s(cco2,bs='cs',k=2)+
               epoch, 
            data=train_dat[sample(.N, 100000)], 
            qu = quSeq)

zz <- expand_grid(season=unique(train_dat$season),
            co2=c(340,380,420),
            # co2 = seq(min(dat$co2_trend),max(dat$co2_trend),length.out=3),
            mape = seq(0.05,1.5,length.out = 100), 
            epoch=train_dat$epoch[1],
            pct_anom = c(-50,0,50), 
            iq = quSeq) %>% 
  mutate(pe_anom_12mo = 0.01*pct_anom*mape) %>%
  mutate(pe_12mo = pe_anom_12mo+mape) %>%
  mutate(cco2 = co2-center_co2)

zz <- zz %>% 
  mutate(pred_high = qdo(fit, qu=0.99, predict, newdata=.)) %>% 
  mutate(pred_med = qdo(fit, qu=0.5, predict, newdata=.)) %>% 
  mutate(pred_low = qdo(fit, qu=0.25, predict, newdata=.))

zz %>% 
  select(co2,mape,pct_anom,pred_high, pred_med, pred_low) %>% 
  gather(-co2,-mape,-pct_anom, key='key',value='value') %>% 
  mutate(key=factor(key,levels = c('pred_high','pred_med','pred_low'),ordered = T)) %>% 
  ggplot(data=., aes(mape, value,color=key,group=key))+
  geom_hline(aes(yintercept=c(0.9)),color='red',lty=3)+
  geom_vline(aes(xintercept=0.25),color='blue',lty=3)+
  geom_vline(aes(xintercept=0.5),color='blue',lty=3)+
  geom_vline(aes(xintercept=1),color='blue',lty=3)+
  geom_line()+
  # scale_color_discrete_sequential('ag_GrnYl',rev = TRUE)+
  scale_color_viridis_d(end=0.7,option='D',direction = -1)+
  scale_y_continuous(limits=c(0,1),expand=c(0,0.01))+
  facet_grid(pct_anom~co2,labeller = label_both)+
  theme_linedraw()

qdo(fit,0.99,predict)



xSeq <- data.frame(cbind("accel" = rep(0, 1e3), 
                         "times" = seq(2, 58, length.out = 1e3)))
plot(mcycle$times, mcycle$accel, xlab = "Times", ylab = "Acceleration", ylim = c(-150, 80))

# Predict each quantile curve and plot
for(iq in quSeq){
  pred <- qdo(fit, iq, predict, newdata = xSeq)
  lines(xSeq$times, pred, col = 2)
}




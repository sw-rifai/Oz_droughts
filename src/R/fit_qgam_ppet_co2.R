library(qgam)

quSeq <- c(0.25, 0.5, 0.99)
fit <- mqgam(ndvi_3mo ~ s(pe_12mo, bs='cs',k=5) + 
               s(I(pe_anom_12mo/mape),bs='cs',k=5) + 
               s(cco2,bs='cs',k=2)+
               epoch, 
            data=train_dat[sample(.N, 100000)], 
            qu = quSeq)

zz <- expand_grid(season=unique(train_dat$season),
            co2 = seq(min(dat$co2_trend),max(dat$co2_trend),length.out=100),
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
  ggplot(data=., aes(mape, value,color=co2,group=co2))+
  geom_line()+
  scale_color_viridis_c(end=0.9,option='B')+
  facet_grid(key~pct_anom,labeller = label_both)

qdo(fit,0.99,predict)



xSeq <- data.frame(cbind("accel" = rep(0, 1e3), 
                         "times" = seq(2, 58, length.out = 1e3)))
plot(mcycle$times, mcycle$accel, xlab = "Times", ylab = "Acceleration", ylim = c(-150, 80))

# Predict each quantile curve and plot
for(iq in quSeq){
  pred <- qdo(fit, iq, predict, newdata = xSeq)
  lines(xSeq$times, pred, col = 2)
}



x <- rnorm(1e4, mean=300, sd=50)
eps <- rnorm(1e4, mean=0, sd=20)
y <- 100 + 0.5*x + eps

cor(x,y)
mean(y)/mean(x)
lm(y~x)
lm(log(y)~log(x))
lm(log(y)~x) %>% coef %>% exp

mean(log10(y)/log10(x))
mean(log(y)/log(x))


fpar <- stars::read_stars("../data_general/MCD15/MCD15A3_fpar_5000m_EastOz_mMean_maskFireDefor_2001_2020.tif")

ggplot()+
  stars::geom_stars(data=fpar[,,,235])



dat[vc=="Eucalypt Tall Open Forests"][ndvi_c>0] %>% 
  ggplot(data=.,aes(ndvi_c,ndvi_mcd))+
  geom_smooth()

out %>% filter(vc=="Eucalypt Tall Open Forests") %>% 
  filter(year==2003) %>% 
  filter(ndvi_c>0) %>% 
  sample_n(10000) %>% 
  ggplot(data=.,aes(ndvi_c,ndvi_mcd))+
  geom_point() + 
  geom_smooth()

out$ndvi_c %>% hist

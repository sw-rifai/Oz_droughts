library(mgcv); 
library(mgcViz); library(tidyverse); library(lubridate); 
library(data.table)
library(dtplyr)

# source("src/R/template_fast_calc_anoms.R")
tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet") 
tmp <- setDT(tmp)
tmp <- tmp[is.na(ndvi_anom_sd)==F]
tmp <- tmp[veg_class %in% c(2:5)]
gc()
 
# tmp %>% lazy_dt() %>% 
#   sample_frac(0.01) %>% 
#   as_tibble() %>% 
#   select(vc,veg_class) %>% 
#   distinct() %>% 
#   View

sdat <- tmp %>% 
  lazy_dt() %>% 
  # filter(season=="DJF") %>% 
  filter(ndvi_mcd > 0) %>% 
  mutate(ddate=decimal_date(date)) %>% 
  filter(between(ndvi_anom_sd,-5,5)) %>% 
  filter(between(pe_anom_12mo,-4,4)) %>% 
  filter(is.na(precip_anom_12mo)==F) %>% 
  filter(is.na(pet_anom_12mo)==F) %>% 
  sample_n(1e5) %>% 
  as_tibble()

stest <- tmp %>% 
  lazy_dt() %>% 
  # filter(season=="DJF") %>% 
  filter(ndvi_mcd > 0) %>% 
  mutate(ddate=decimal_date(date)) %>% 
  filter(between(ndvi_anom_sd,-5,5)) %>% 
  filter(between(pe_anom_12mo,-4,4)) %>% 
  filter(is.na(precip_anom_12mo)==F) %>% 
  filter(is.na(pet_anom_12mo)==F) %>% 
  sample_n(1e5) %>% 
  as_tibble()



fit_st0 <- sdat %>% 
  bam(ndvi_anom_sd ~ 
        s(x,y,month,bs=c("tp","tp","cc"))+
        s(vc,bs='re')+
        te(precip_u, pet_u, tmax_u)+
        te(map, mapet, matmax),
      data=.,
      discrete=T,select=T)
fit_st1 <- sdat %>% 
  bam(ndvi_anom_sd ~ 
        s(vpd15_anom_sd)+
        s(x,y,month,bs=c("tp","tp","cc"))+
        s(vc,bs='re')+
        te(precip_u, pet_u, tmax_u)+
        te(precip_anom, pet_anom, tmax_anom)+
        te(map, mapet, matmax),
      data=.,
      discrete=T,select=T)
summary(fit_st1)
plot(fit_st1)
fit_st2 <- sdat %>% 
  bam(ndvi_anom_sd ~ 
        s(x,y,month,bs=c("tp","tp","cc"))+
        s(vc,bs='re')+
        te(precip_u, pet_u, tmax_u)+
        te(precip_anom, pet_anom, tmax_anom)+
        te(precip_anom_3mo, pet_anom_3mo, tmax_anom_3mo)+
        te(map, mapet, matmax),
      data=.,
      discrete=T,select=T)
fit_st3 <- sdat %>% 
  bam(ndvi_anom_sd ~ 
        s(x,y,month,bs=c("tp","tp","cc"))+
        s(vc,bs='re')+
        te(precip_u, pet_u, tmax_u)+
        te(precip_anom, pet_anom, tmax_anom)+
        te(tmax_anom_3mo,pet_anom_3mo,precip_anom_3mo)+
        te(tmax_anom_12mo,pet_anom_12mo,precip_anom_12mo)+
        te(map, mapet, matmax),
      data=.,
      discrete=T,select=T)
fit_st4 <- sdat %>% 
  bam(ndvi_anom_sd ~ 
        s(x,y,month,bs=c("tp","tp","cc"))+
        s(vc,bs='re')+
        te(precip_u, pet_u, tmax_u)+
        te(tmax_anom,pet_anom,precip_anom)+
        te(tmax_anom_3mo,pet_anom_3mo,precip_anom_3mo)+
        te(tmax_anom_12mo,pet_anom_12mo,precip_anom_12mo)+
        te(tmax_anom_36mo,pet_anom_36mo,precip_anom_36mo)+
        te(map, mapet, matmax),
      data=.,
      discrete=T,select=T)

fit_st1 %>% summary
fit_st2 %>% summary
fit_st3 %>% summary
fit_st4 %>% summary

getViz(fit_st3) %>% plot(allTerms=T) %>% print(pages=2)
b <- getViz(fit_st2)
b %>% plot()
plot(sm(b,1))+l_fitRaster()+coord_equal()
plot(sm(b,2))+
  l_fitRaster()+
  l_rug()+
  scale_fill_gradient2(high='blue',low='red')+
  geom_hline(aes(yintercept=0))+
  geom_vline(aes(xintercept=0))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))


tibble(x=152,y=-30,
       vc="Eucalypt Tall Open Forests",
       ddate=1982:2019, 
       pet_anom_12mo=-100,
       precip_anom_12mo=500, 
       pe_anom_12mo=5) %>% 
  mutate(pred1 = predict(fit_st1, newdata=.)) %>% 
  mutate(pred2 = predict(fit_st2, newdata=.)) %>% 
  gather(-ddate,-x,-y,-vc, 
         -precip_anom_12mo,-pet_anom_12mo, 
         -pe_anom_12mo, 
         key = 'model',value='pred') %>% 
  ggplot(data=., aes(ddate,pred,color=model))+
  geom_point()

tmp %>% 
  filter(near(x,152,tol = 0.1)) %>% 
  filter(near(y,-30,tol=0.1)) %>% 
  mutate(ddate= decimal_date(date)) %>% 
  filter(ddate>=2015) %>% 
  mutate(pred0 = predict(fit_st0, newdata=.)) %>%
  mutate(pred1 = predict(fit_st1, newdata=.)) %>%
  mutate(pred2 = predict(fit_st2, newdata=.)) %>% 
  mutate(pred3 = predict(fit_st3, newdata=.)) %>% 
  mutate(pred4 = predict(fit_st4, newdata=.)) %>% 
  select(ddate,starts_with('pred'),ndvi_anom_sd) %>% 
  gather(-ddate,
         key = 'model',value='pred') %>% 
  ggplot(data=., aes(ddate,pred,color=model))+
  geom_hline(aes(yintercept=-2),col='red')+
  geom_point(alpha=0.1)+
  geom_smooth()


tmp %>% 
  filter(near(x,148,tol = 0.1)) %>% 
  filter(near(y,-36.5,tol=0.1)) %>% 
  mutate(ddate= decimal_date(date)) %>% 
  filter(ddate>=2015) %>% 
  mutate(pred0 = predict(fit_st0, newdata=.)) %>%
  mutate(pred1 = predict(fit_st1, newdata=.)) %>%
  mutate(pred2 = predict(fit_st2, newdata=.)) %>% 
  mutate(pred3 = predict(fit_st3, newdata=.)) %>% 
  mutate(pred4 = predict(fit_st4, newdata=.)) %>% 
  select(ddate,starts_with('pred'),ndvi_anom_sd) %>% 
  gather(-ddate,
         key = 'model',value='pred') %>% 
  ggplot(data=., aes(ddate,pred,color=model))+
  geom_hline(aes(yintercept=-2),col='red')+
  geom_point(alpha=0.1)+
  geom_smooth()+
  scale_y_continuous(limits = c(-5,2))


tmp[date==min(date)] %>% ggplot(data=.,aes(x,y,tmax))+geom_tile()+
  geom_rect(aes(xmin = 147.5, 
                xmax = 148.5,
                ymin = -36,
                ymax = -37),color='red',fill=NA)

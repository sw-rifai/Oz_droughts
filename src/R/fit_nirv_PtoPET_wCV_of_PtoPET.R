library(arrow); library(tidyverse); library(lubridate);
library(ggridges)
library(sf); 
library(mgcv); library(mgcViz)
mlo <- read_table("../data_general/CO2_growth_rate/co2_mm_mlo_20200405.txt", 
                  skip = 72, col_names = F)
names(mlo) <- c("year","month","ddate","co2_avg","co2_int","trend","ndays")


# Read subset -------------------------------------------------------------
dat <- read_parquet("../data_general/AVHRR_CDRv5_VI/ard_nirv_2020-04-04.parquet", 
                    ### dput(names(dat)) ### to get an R vector input representation
                    col_select = 
                      c("x", "y", "date", 
                        "pet", "precip",
                        # "veg_class",
                        "vc", 
                        "nirv", 
                        # "month", "pet_u", "precip_u", 
                        "nirv_u", 
                        #"pet_sd", "precip_sd", 
                        # "nirv_sd",
                        "map", "mapet", 
                        # "pet_anom", "precip_anom", 
                        "nirv_anom", 
                        # "pet_anom_sd", "precip_anom_sd", 
                        "nirv_anom_sd", "ap_sd", "apet_sd", 
                        "pet_12mo", "precip_12mo",
                        # "pet_anom_min_3mo", "pet_anom_min_6mo", 
                        # "pet_anom_min_9mo", "pet_anom_min_12mo", "pet_anom_max_3mo", 
                        # "pet_anom_max_6mo", "pet_anom_max_9mo", "pet_anom_max_12mo", 
                        # "precip_anom_min_3mo", "precip_anom_min_6mo", "precip_anom_min_9mo", 
                        # "precip_anom_min_12mo", "precip_anom_max_3mo", "precip_anom_max_6mo", 
                        # "precip_anom_max_9mo", "precip_anom_max_12mo", 
                        "pet_anom_12mo", 
                        "precip_anom_12mo", 
                        # "lai", "lai_u", "lai_sd", "lai_min", "lai_max", 
                        # "lai_amp_u", 
                        "ddate", "precip_anom_sd_12mo", "pet_anom_sd_12mo",
                        # "pet_precip", "pet_precip_anom_12mo", 
                        "ma_pet_precip"
                      ))




#******************************************************************
# Calc IA Coef of Variation 
#******************************************************************
library(data.table); library(dtplyr)
dat <- as.data.table(dat)
ldat <- lazy_dt(dat)
tmp <- ldat %>% 
  mutate(year=year(date)) %>% 
  filter(year>=1983 & year<= 2018) %>% 
  group_by(x,y,year) %>% 
  summarize(ap = sum(precip), 
            apet = sum(pet)) %>% 
  mutate(ap_apet = ap/apet) %>% 
  ungroup()
tmp2 <- tmp %>% 
  group_by(x,y) %>% 
  summarize(p_u = mean(ap), 
            pet_u = mean(apet), 
            p_pet_u = mean(ap_apet), 
            p_sd = sd(ap), 
            pet_sd = sd(apet), 
            p_pet_sd = sd(ap_apet)) %>% 
  ungroup() %>% 
  as_tibble()
tmp_cv <- tmp %>% 
  group_by(x,y) %>% 
  summarize(cv2_p = sqrt(exp(sd(log1p(ap))**2) - 1), 
            cv2_pet = sqrt(exp(sd(log1p(apet))**2) - 1), 
            cv2_p_pet = sqrt(exp(sd(log1p(ap_apet))**2) - 1)) %>% 
  ungroup() %>% 
  as_tibble()
# Percentile division
tmp_pd <- tmp %>% 
  group_by(x,y) %>% 
  summarize(p90 = quantile(ap,0.9), 
            p10 = quantile(ap,0.1), 
            pet90 = quantile(apet,0.9), 
            pet10 = quantile(apet,0.1), 
            p_pet90 = quantile(ap_apet, 0.9), 
            p_pet10 = quantile(ap_apet, 0.1)) %>% 
  ungroup()
gc(reset = T, full = T)
#******************************************************************
# END SECTION *** 
#******************************************************************

#******************************************************************
# DROUGHT EPOCH CROSS-VALIDATION 
#******************************************************************
dat_80s <- dat %>% 
  filter(date <= ymd('1990-01-01')) %>% 
  as_tibble() %>% 
  left_join(., tmp_pd %>% as_tibble(), by=c("x","y"))
gc(reset = T, full = T)
dat_90s <- dat %>% 
  filter(date >= ymd('1990-01-01') & 
           date <= ymd("2000-01-01")) %>% 
  as_tibble() %>% 
  inner_join(., tmp_pd %>% as_tibble(), by=c("x","y"))
gc()
dat_00s <- dat %>% 
  filter(date >= ymd('2000-01-01') & 
           date <= ymd("2010-01-01")) %>% 
  as_tibble() %>% 
  inner_join(., tmp_pd %>% as_tibble(), by=c("x","y"))
gc()
dat_10s <- dat %>% 
  filter(date >= ymd('2010-01-01') & 
           date <= ymd("2019-10-01")) %>% 
  as_tibble() %>% 
  inner_join(., tmp_pd %>% as_tibble(), by=c("x","y"))
gc()

nobs <- 5e5
fit80s <- bam(nirv_anom_sd ~ 
                te(p90p10, p_pet_12mo_anom, ma_p_pet)+
                s(nirv_u),
                # te(p_pet_12mo,
                #             cv2_p_pet,
                #             ma_p_pet,
                #             nirv_u
                            # ma_p_pet, nirv_u #some 
                            # p_pet_12mo,nirv_u
 method='fREML', select=T, discrete = T, 
 data=dat_80s %>% 
  sample_n(nobs) %>% 
  mutate(ma_p_pet = map/mapet, 
         p90p10 = p90/p10) %>% 
   mutate(p_pet_12mo_anom = (precip_12mo-pet_12mo)-(ma_p_pet)
          ))
fit90s <- bam(nirv_anom_sd ~ 
                te(p90p10, p_pet_12mo_anom, ma_p_pet)+
                s(nirv_u),
              method='fREML', select=T, discrete = T, 
              data=dat_90s %>% 
                sample_n(nobs) %>% 
                mutate(ma_p_pet = map/mapet, 
                       p90p10 = p90/p10) %>% 
                mutate(p_pet_12mo_anom = (precip_12mo-pet_12mo)-(ma_p_pet)
                ))
fit00s <- bam(nirv_anom_sd ~ 
                te(p90p10, p_pet_12mo_anom, ma_p_pet)+
                s(nirv_u),
              method='fREML', select=T, discrete = T, 
              data=dat_00s %>% 
                sample_n(nobs) %>% 
                mutate(ma_p_pet = map/mapet, 
                       p90p10 = p90/p10) %>% 
                mutate(p_pet_12mo_anom = (precip_12mo-pet_12mo)-(ma_p_pet)
                ))
fit10s <- bam(nirv_anom_sd ~ 
                te(p90p10, p_pet_12mo_anom, ma_p_pet)+
                s(nirv_u),
              method='fREML', select=T, discrete = T, 
              data=dat_10s %>% 
                sample_n(nobs) %>% 
                mutate(ma_p_pet = map/mapet, 
                       p90p10 = p90/p10) %>% 
                mutate(p_pet_12mo_anom = (precip_12mo-pet_12mo)-(ma_p_pet)
                ))
summary(fit80s)
summary(fit90s)
summary(fit00s)
summary(fit10s)
bind_rows(dat_80s %>% sample_n(nobs), 
          dat_90s %>% sample_n(nobs), 
          dat_00s %>% sample_n(nobs), 
          dat_10s %>% sample_n(nobs)) %>% 
  sample_n(nobs) %>% 
  mutate(ma_p_pet = map/mapet, 
         p90p10 = p90/p10) %>% 
  mutate(p_pet_12mo_anom = (precip_12mo-pet_12mo)-(ma_p_pet)) %>% 
  mutate(pred80 = predict(fit80s, newdata = .), 
         pred90 = predict(fit90s, newdata = .), 
         pred00 = predict(fit00s, newdata = .), 
         pred10 = predict(fit10s, newdata = .)) %>% 
  group_by(date) %>% 
  summarize(rmse_80 = sqrt(mean((pred80-nirv_anom_sd)**2, na.rm = T)), 
            rmse_90 = sqrt(mean((pred90-nirv_anom_sd)**2, na.rm = T)), 
            rmse_00 = sqrt(mean((pred00-nirv_anom_sd)**2, na.rm = T)), 
            rmse_10 = sqrt(mean((pred10-nirv_anom_sd)**2, na.rm = T))) %>% 
  ungroup %>% 
  select(date, rmse_80, rmse_90, rmse_00, rmse_10) %>% 
  gather(-date, key = 'epoch', value = "rmse") %>% 
  ggplot(data=., aes(date, rmse, color=epoch))+
  geom_line()+
  scale_color_viridis_d()
#******************************************************************
# END SECTION *** 
#******************************************************************




nobs <- 1e5
dat <- dat %>% 
  mutate(p_pet_12mo = precip_12mo/pet_12mo) %>% 
  mutate(ma_p_pet = map/mapet)
dat_train <- dat %>% sample_n(nobs); 
dat_test <- dat %>% sample_n(nobs)

dat_train <- inner_join(dat_train, tmp_cv, by=c("x","y"))
dat_test <- inner_join(dat_test, tmp_cv, by=c("x","y"))
dat_train <- inner_join(dat_train, tmp_pd, by=c("x","y"))
dat_test <- inner_join(dat_test, tmp_pd, by=c("x","y"))

dat_train %>% 
  sample_n(100) %>% 
  select(ma_p_pet, map, mapet)


library(ggpointdensity)
vec_vc <- unique(dat_train$vc)
dat_train %>% 
  select(cv2_p_pet, ma_p_pet, vc) %>% 
  remove_missing() %>% 
  filter(vc %in% vec_vc[1:12]) %>% 
  ggplot(data=., aes(ma_p_pet, cv2_p_pet))+
  geom_pointdensity()+
  scale_color_viridis_c("nobs")+
  labs(x="Mean Annual P:PET",
       y='Coefficient of Variation P:PET')+
  theme_linedraw()+
  facet_wrap(~vc)
ggsave(filename = "figures/scatter_cv_ma_p_pet_byNVIS.png", 
       width = 1.5*16, height=1.5*9, units = 'cm', dpi='retina', type='cairo')

dat_train %>% 
  select(p_pet90,p_pet10, ma_p_pet, vc) %>% 
  remove_missing() %>% 
  filter(vc %in% vec_vc[1:12]) %>% 
  ggplot(data=., aes(ma_p_pet, p_pet90/p_pet10))+
  geom_pointdensity()+
  scale_color_viridis_c("nobs")+
  labs(x="Mean Annual P:PET",
       y=' 90percentile/10percentile  P:PET')+
  theme_linedraw()+
  facet_wrap(~vc)
ggsave(filename = "figures/scatter_p90p10_ma_p_pet_byNVIS.png", 
       width = 1.5*16, height=1.5*9, units = 'cm', dpi='retina', type='cairo')

g0 <- bam(nirv_anom_sd ~ cv2_p_pet, 
          method='fREML', select=T, discrete = T, 
          data=dat_train)
summary(g0)
getViz(g0) %>% plot(allTerms=T)

g1 <- bam(nirv_anom_sd ~ p_pet_12mo+cv2_p_pet, 
          method='fREML', select=T, discrete = T, 
          data=dat_train)
summary(g1)
getViz(g1) %>% plot(allTerms=T)
g2 <- bam(nirv_anom_sd ~ p_pet_12mo*cv2_p_pet, 
          method='fREML', select=T, discrete = T, 
          data=dat_train)
summary(g2)
getViz(g2) %>% plot(allTerms=T)


g3 <- bam(nirv_anom_sd ~ te(p_pet_12mo,
                            cv2_p_pet,
                            ma_p_pet,
                            nirv_u
                         # ma_p_pet, nirv_u #some 
                         # p_pet_12mo,nirv_u
                         ), 
          method='fREML', select=T, discrete = T, 
          data=dat_train %>% 
            mutate(ma_p_pet = map/mapet))
summary(g3)
getViz(g3) %>% plot(allTerms=T)
b3 <- getViz(g3)
plotSlice(sm(b3,1), 
          fix=list(
            # "ma_p_pet"=c(0.05,0.2,1)
            "cv2_p_pet"=c(0.025,0.05,0.075,0.1), 
            "nirv_u"=c(0.1,0.2,0.3)
            ))+
  l_fitRaster(noiseup = T, mul = 2)+
  l_fitContour(lwd=0.5)+
  l_rug()+
  scale_fill_gradient2(expression(eta),
                       # limits=c(-5,5)
  )+
  coord_flip()+
  geom_abline(aes(intercept=0,slope=1),color='purple')

#******************************************************************
# VI response to P:PET & CV P:PET 
#******************************************************************




library(data.table); library(dtplyr)
dat <- as.data.table(dat); 
ldat <- lazy_dt(dat)

system.time(
  fit_p_pet <- ldat %>% 
    mutate(p_pet_12mo = precip_12mo/pet_12mo) %>% 
    # mutate(p_pet_anom_12mo = precip_anom_12mo/pet_anom_12mo) %>% 
    select(x,y,nirv_anom_sd,p_pet_12mo) %>% 
    filter(is.na(p_pet_12mo)==F) %>% 
    filter(is.na(nirv_anom_sd)==F) %>% 
    filter(nirv_anom_sd >= -10 & 
             nirv_anom_sd <= 10) %>% 
    as_tibble() %>% 
    inner_join(., tmp_cv, by=c("x","y")) %>% 
    group_by(x,y) %>%
    summarize(fit = list(lm(nirv_anom_sd ~ p_pet_12mo*cv2_p_pet))) %>%
    ungroup() %>%
    as_tibble()
)

tmp_lt$beta1 <- as_vector(tmp_lt$fit %>% 
                            lapply(.,
                                   FUN=function(x) coef(x) %>% 
                                     "["("p_pet_12mo"))) %>% 
  as.numeric()
tmp_lt$beta0 <- as_vector(tmp_lt$fit %>% 
                            lapply(.,
                                   FUN=function(x) coef(x) %>% 
                                     "["("(Intercept)"))) %>% 
  as.numeric()

p1 <- tmp_lt %>% 
  ggplot(data=., aes(x,y,fill=beta0))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2("Intercept",
                       limits=c(-5,5))+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        legend.position = 'bottom')

p2 <- tmp_lt %>% 
  ggplot(data=., aes(x,y,fill=beta1))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(paste(Beta[1])),
                       limits=c(-10,10),
                       oob=scales::squish
  )+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        # panel.grid = element_blank(),
        legend.position = 'bottom')
library(patchwork)
ggsave(
  p1+p2+plot_layout(nrow = 1)+
    plot_annotation(title=
                      expression(paste(NIR[V]~Anom[sigma]%~%Intercept+Beta[1]*"(12 month (P:PET))")), 
                    subtitle = "Data: AVHRR 1982-2019"), 
  filename="figures/map_eastOz_longTerm_linearSensitivity_PtoPET.png", 
  width=0.7*30, height=22, units = 'cm', dpi='retina', type='cairo')

# tmp_cv <- ldat %>% 
#   filter(date>=ymd("1981-01-01") & 
#            date<=ymd("2018-12-31")) %>% 
#   mutate(p_pet_anom_12mo = precip_anom_12mo/pet_anom_12mo)
# group_by(x,y) %>% 
#   summarize(cv2_p = sqrt(exp(sd(log1p(ap))**2) - 1), 
#             cv2_pet = sqrt(exp(sd(log1p(apet))**2) - 1), 
#             cv2_p_pet = sqrt(exp(sd(log1p(ap_apet))**2) - 1)) %>% 
#   ungroup() %>% 
#   as_tibble()
# 
# tmp_cv <- dat %>% 
#   filter(date>=ymd("1981-01-01") & 
#            date<=ymd("2018-12-31"))
#   mutate(p_pet_anom_12mo = precip_anom_12mo/pet_anom_12mo)

#******************************************************************
# END SECTION *** 
#******************************************************************

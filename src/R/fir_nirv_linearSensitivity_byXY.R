library(arrow); library(tidyverse); library(lubridate);
library(ggridges)
library(sf); library(mgcv); library(mgcViz)
mlo <- read_table("../data_general/CO2_growth_rate/co2_mm_mlo_20200405.txt", 
                  skip = 72, col_names = F)
names(mlo) <- c("year","month","ddate","co2_avg","co2_int","trend","ndays")

dat <- read_parquet("../data_general/AVHRR_CDRv5_VI/ard_nirv_2020-04-04.parquet", 
                    ### dput(names(dat)) ### to get an R vector input representation
                    col_select = 
                      c("x", "y", "date", 
                        "pet", "precip", 
                        "veg_class", "vc", 
                        "nirv", 
                        # "month", "pet_u", "precip_u", "nirv_u", "pet_sd", "precip_sd", 
                         "nirv_sd",
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
# P:PET Longterm sensitivity --------------------------------------
#******************************************************************
library(data.table); library(dtplyr)
dat <- as.data.table(dat); 
ldat <- lazy_dt(dat)

system.time(
  tmp_lt <- ldat %>% 
    mutate(p_pet_12mo = precip_12mo/pet_12mo) %>% 
    # mutate(p_pet_anom_12mo = precip_anom_12mo/pet_anom_12mo) %>% 
    select(x,y,nirv_anom_sd,p_pet_12mo) %>% 
    filter(is.na(p_pet_12mo)==F) %>% 
    filter(is.na(nirv_anom_sd)==F) %>% 
    filter(nirv_anom_sd >= -10 & 
             nirv_anom_sd <= 10) %>% 
    as_tibble() %>% 
    group_by(x,y) %>%
    summarize(fit = list(lm(nirv_anom_sd ~ p_pet_12mo))) %>%
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








system.time(
tmp <- dat %>% 
  select(x,y,nirv_anom_sd,pet_anom_sd_12mo,precip_anom_sd_12mo) %>% 
  na.omit() %>% 
  group_by(x,y) %>% 
  summarize(fit = list(lm(nirv_anom_sd ~ pet_anom_sd_12mo + precip_anom_sd_12mo))) %>% 
  ungroup()
)
tmp$beta_pet <- as_vector(tmp$fit %>% 
  lapply(.,
    FUN=function(x) coef(x) %>% 
      "["("pet_anom_sd_12mo"))) %>% 
  as.numeric()
tmp$beta_precip <- as_vector(tmp$fit %>% 
                            lapply(.,
                                   FUN=function(x) coef(x) %>% 
                                     "["("precip_anom_sd_12mo"))) %>% 
  as.numeric()

vec_cols <- RColorBrewer::brewer.pal(9, "BrBG")
p_pet <- tmp %>% 
  ggplot(data=., aes(x,y, fill=beta_pet))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma),
                       low=vec_cols[1],
                       mid=vec_cols[5],
                       high = vec_cols[9],
                       limits=c(-1,1),oob=scales::squish)+
  labs(x=NULL,y=NULL,
       title=expression(paste("Longterm Coef. \n WRT PET anom"~(sigma))))+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
p_precip <- tmp %>% 
  ggplot(data=., aes(x,y, fill=beta_precip))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma),
                       low=vec_cols[1],
                       mid=vec_cols[5],
                       high = vec_cols[9],
                       limits=c(-1,1),oob=scales::squish)+
  labs(x=NULL,y=NULL,
       title=expression(paste("Longterm Coef. \n WRT Precip. anom"~(sigma))))+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
library(patchwork)
p_pet+p_precip
ggsave(p_pet+p_precip, 
       filename = 'figures/map_eastOz_longterm_linearSensitivity_PET_Precip.png', 
       width=20, height = 22, units='cm', dpi = 'retina', type='cairo')



#****************************************************************************
# Compare Drought Epochs -------------------------------------------------------------system.time(
#****************************************************************************

tmp_80s <- dat %>% 
  filter(date >= ymd("1982-01-01")& 
           date <= ymd("1989-12-31")) %>% 
  select(x,y,nirv_anom_sd,pet_anom_sd_12mo,precip_anom_sd_12mo) %>% 
  na.omit() %>% 
  group_by(x,y) %>% 
  summarize(fit = list(lm(nirv_anom_sd ~ pet_anom_sd_12mo + precip_anom_sd_12mo))) %>% 
  ungroup()
tmp_90s <- dat %>% 
  filter(date >= ymd("1992-01-01")& 
           date <= ymd("1996-12-31")) %>% 
  select(x,y,nirv_anom_sd,pet_anom_sd_12mo,precip_anom_sd_12mo) %>% 
  na.omit() %>% 
  group_by(x,y) %>% 
  summarize(fit = list(lm(nirv_anom_sd ~ pet_anom_sd_12mo + precip_anom_sd_12mo))) %>% 
  ungroup()
tmp_00s <- dat %>% 
  filter(date >= ymd("2002-01-01")& 
           date <= ymd("2007-12-31")) %>% 
  select(x,y,nirv_anom_sd,pet_anom_sd_12mo,precip_anom_sd_12mo) %>% 
  na.omit() %>% 
  group_by(x,y) %>% 
  summarize(fit = list(lm(nirv_anom_sd ~ pet_anom_sd_12mo + precip_anom_sd_12mo))) %>% 
  ungroup()
tmp_10s <- dat %>% 
  filter(date >= ymd("2015-01-01")& 
           date <= ymd("2019-12-31")) %>% 
  select(x,y,nirv_anom_sd,pet_anom_sd_12mo,precip_anom_sd_12mo) %>% 
  na.omit() %>% 
  group_by(x,y) %>% 
  summarize(fit = list(lm(nirv_anom_sd ~ pet_anom_sd_12mo + precip_anom_sd_12mo))) %>% 
  ungroup()

tmp_80s$beta_pet <- as_vector(tmp_80s$fit %>% 
                            lapply(.,
                                   FUN=function(x) coef(x) %>% 
                                     "["("pet_anom_sd_12mo"))) %>% 
  as.numeric()
tmp_80s$beta_precip <- as_vector(tmp_80s$fit %>% 
                               lapply(.,
                                      FUN=function(x) coef(x) %>% 
                                        "["("precip_anom_sd_12mo"))) %>% 
  as.numeric()
tmp_90s$beta_pet <- as_vector(tmp_90s$fit %>% 
                                lapply(.,
                                       FUN=function(x) coef(x) %>% 
                                         "["("pet_anom_sd_12mo"))) %>% 
  as.numeric()
tmp_90s$beta_precip <- as_vector(tmp_90s$fit %>% 
                                   lapply(.,
                                          FUN=function(x) coef(x) %>% 
                                            "["("precip_anom_sd_12mo"))) %>% 
  as.numeric()
tmp_00s$beta_pet <- as_vector(tmp_00s$fit %>% 
                                lapply(.,
                                       FUN=function(x) coef(x) %>% 
                                         "["("pet_anom_sd_12mo"))) %>% 
  as.numeric()
tmp_00s$beta_precip <- as_vector(tmp_00s$fit %>% 
                                   lapply(.,
                                          FUN=function(x) coef(x) %>% 
                                            "["("precip_anom_sd_12mo"))) %>% 
  as.numeric()
tmp_10s$beta_pet <- as_vector(tmp_10s$fit %>% 
                                lapply(.,
                                       FUN=function(x) coef(x) %>% 
                                         "["("pet_anom_sd_12mo"))) %>% 
  as.numeric()
tmp_10s$beta_precip <- as_vector(tmp_10s$fit %>% 
                                   lapply(.,
                                          FUN=function(x) coef(x) %>% 
                                            "["("precip_anom_sd_12mo"))) %>% 
  as.numeric()




vec_cols <- RColorBrewer::brewer.pal(9, "BrBG")
p_pet_80s <- tmp_80s %>% 
  ggplot(data=., aes(x,y, fill=beta_pet))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma),
                       low=vec_cols[1],
                       mid=vec_cols[5],
                       high = vec_cols[9],
                       limits=c(-1,1),oob=scales::squish)+
  labs(x=NULL,y=NULL,
  title=expression(paste("1982-1989 Coef. \n WRT 12mo PET anom"~(sigma))))+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
p_pet_90s <- tmp_90s %>% 
  ggplot(data=., aes(x,y, fill=beta_pet))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma),
                       low=vec_cols[1],
                       mid=vec_cols[5],
                       high = vec_cols[9],
                       limits=c(-1,1),oob=scales::squish)+
  labs(x=NULL,y=NULL,
       title=expression(paste("1992-1996 Coef. \n WRT 12mo PET anom"~(sigma))))+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
p_pet_00s <- tmp_00s %>% 
  ggplot(data=., aes(x,y, fill=beta_pet))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma),
                       low=vec_cols[1],
                       mid=vec_cols[5],
                       high = vec_cols[9],
                       limits=c(-1,1),oob=scales::squish)+
  labs(x=NULL,y=NULL,
       title=expression(paste("2002-2007 Coef. \n WRT 12mo PET anom"~(sigma))))+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
p_pet_10s <- tmp_10s %>% 
  ggplot(data=., aes(x,y, fill=beta_pet))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma),
                       low=vec_cols[1],
                       mid=vec_cols[5],
                       high = vec_cols[9],
                       limits=c(-1,1),oob=scales::squish)+
  labs(x=NULL,y=NULL,
       title=expression(paste("2015-2019 Coef. \n WRT 12mo PET anom"~(sigma))))+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
ggsave(p_pet_80s+p_pet_90s+p_pet_00s+p_pet_10s+plot_layout(ncol = 4), 
       filename = 'figures/map_eastOz_droughtEpoc_linearSensitivity_PET.png', 
       width=40, height = 22, units='cm', dpi = 'retina', type='cairo')



vec_cols <- RColorBrewer::brewer.pal(9, "BrBG")
p_precip_80s <- tmp_80s %>% 
  ggplot(data=., aes(x,y, fill=beta_precip))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma),
                       low=vec_cols[1],
                       mid=vec_cols[5],
                       high = vec_cols[9],
                       limits=c(-1,1),oob=scales::squish)+
  labs(x=NULL,y=NULL,
       title=expression(paste("1982-1989 Coef. \n WRT 12mo Precip anom"~(sigma))))+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
p_precip_90s <- tmp_90s %>% 
  ggplot(data=., aes(x,y, fill=beta_precip))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma),
                       low=vec_cols[1],
                       mid=vec_cols[5],
                       high = vec_cols[9],
                       limits=c(-1,1),oob=scales::squish)+
  labs(x=NULL,y=NULL,
       title=expression(paste("1992-1996 Coef. \n WRT 12mo Precip anom"~(sigma))))+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
p_precip_00s <- tmp_00s %>% 
  ggplot(data=., aes(x,y, fill=beta_precip))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma),
                       low=vec_cols[1],
                       mid=vec_cols[5],
                       high = vec_cols[9],
                       limits=c(-1,1),oob=scales::squish)+
  labs(x=NULL,y=NULL,
       title=expression(paste("2002-2007 Coef. \n WRT 12mo Precip anom"~(sigma))))+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
p_precip_10s <- tmp_10s %>% 
  ggplot(data=., aes(x,y, fill=beta_precip))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma),
                       low=vec_cols[1],
                       mid=vec_cols[5],
                       high = vec_cols[9],
                       limits=c(-1,1),oob=scales::squish)+
  labs(x=NULL,y=NULL,
       title=expression(paste("2015-2019 Coef. \n WRT 12mo Precip anom"~(sigma))))+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
# p_precip_80s+p_precip_90s+p_precip_00s+p_precip_10s
ggsave(p_precip_80s+p_precip_90s+p_precip_00s+p_precip_10s+plot_layout(ncol = 4), 
       filename = 'figures/map_eastOz_droughtEpoc_linearSensitivity_Precip.png', 
       width=40, height = 22, units='cm', dpi = 'retina', type='cairo')




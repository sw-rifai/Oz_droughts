library(arrow); library(tidyverse); library(lubridate); 
library(ggridges)
library(sf);
dat <- read_parquet("../data_general/AVHRR_CDRv5_VI/ard_nirv_2020-04-04.parquet", 
                    ### dput(names(dat)) ### to get an R vector input representation
                    col_select = 
                      c("x", "y", "date", 
                        "pet", "precip", 
                        "veg_class", "vc", 
                        "nirv", 
                        # "month", "pet_u", "precip_u", 
                        "nirv_u", 
                        #"pet_sd", "precip_sd", 
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
  ungroup() %>% 
  as_tibble()

dat <- dat %>% 
  mutate(p_pet_12mo = precip_12mo/pet_12mo) %>% 
  mutate(ma_p_pet = map/mapet)
dat <- dat %>% 
  mutate(p_pet_anom_12mo = p_pet_12mo-ma_p_pet)
#******************************************************************
# END SECTION *** 
#******************************************************************



# split data --------------------------------------------------------------
nobs_mod <- 5e6
d_train <- dat %>% sample_n(nobs_mod)
d_test <- dat %>% sample_n(nobs_mod)


library(RcppRoll)
vec_vc <- unique(dat$vc)
dat %>% 
  # sample_n(1e6) %>% 
  filter(vc %in% vec_vc[1:12]) %>% 
  group_by(date, vc) %>% 
  summarize(val = mean(nirv_anom_sd,na.rm=T), 
            val2 = mean(p_pet_anom_12mo,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(vc) %>% 
  arrange(date) %>% 
  mutate(val_12mo = roll_meanr(val, n=12,fill=NA)) %>% 
  ungroup() %>% 
  remove_missing() %>% 
  ggplot(data=., aes(date, val_12mo, color=vc, fill=val2))+
  geom_rect(aes(xmin=date,xmax=date+months(1),
                ymax=3,ymin=-3, 
                fill=val2),lty=0)+
  geom_hline(aes(yintercept=0),col='black')+
  geom_line()+
  scale_fill_gradient2("Anom. 12mo P:PET", oob=scales::squish)+
  scale_color_viridis_d(end=0.9)+
  scale_y_continuous(limits=c(-3,3), expand=c(0,0))+
  scale_x_date(expand=c(0,0))+
  labs(x=NULL, 
       y=expression(paste("12 Month ",NIR[V]~Anom~(sigma))))+
  facet_wrap(~vc, nrow = 3)+
  theme_linedraw()+
  guides(color=F)+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(fill='grey'),
        panel.grid = element_blank())
ggsave(filename = "figures/timeseries_nirvAnomSigma_byNVIS.png", 
       width = 1.5*16, height=1.5*9, units = 'cm', dpi='retina', type='cairo')




# Tiled Time Series of Precip Anom 12 mo ---------------------------------------
round(rnorm(1000, mean=1000, sd=100),-1)
tmp <- dat %>% 
  mutate(map=round(map,-1)) %>% 
  group_by(ddate,map) %>% 
  summarize(val = mean(precip_anom_sd_12mo,na.rm=T)) %>% 
  ungroup()
p <- tmp %>% 
  filter(is.na(val)==F) %>% 
  ggplot(data=., aes(ddate, map, fill=val))+
  geom_tile()+
  scale_fill_gradient2(expression(paste(Anom.~sigma)), 
                       limits=c(-2.5,2.5), oob=scales::squish)+
  theme_linedraw()+
  scale_x_continuous(expand=c(0,0), 
                     breaks=c(1982,1985,1990,1995,2000,2005,
                              2010,2015,2019))+
  scale_y_continuous(expand=c(0,0))+
  labs(x=NULL, y='Mean Annual Precip (mm)', 
       title="Tree Containing Regions of Eastern Australia - 12 Month Precip Anomaly by MAP", 
       subtitle = 'Data: ERA5')
ggsave(p, 
       filename = 'figures/tileTimeSeries_eastOz_mean12moPrecipAnomSD.png', 
       width=35.5, height = 20, units='cm', dpi = 'retina', type='cairo')

# Tiled Time Series of PET Anom 12 mo ---------------------------------------
round(rnorm(1000, mean=1000, sd=100),-1)
tmp <- dat %>% 
  mutate(mapet=round(mapet,-1)) %>% 
  group_by(ddate,mapet) %>% 
  summarize(val = mean(pet_anom_sd_12mo,na.rm=T)) %>% 
  ungroup()
vec_cols <- RColorBrewer::brewer.pal(n=9, "PuOr")
vec_cols <- rev(vec_cols)
p <- tmp %>% 
  filter(is.na(val)==F) %>% 
  ggplot(data=., aes(ddate, mapet, fill=val))+
  geom_tile()+
  scale_fill_gradient2(expression(paste(Anom.~sigma)), 
                       high=vec_cols[9], mid=vec_cols[5],low=vec_cols[1],
                       limits=c(-2.5,2.5), oob=scales::squish)+
  theme_linedraw()+
  scale_x_continuous(expand=c(0,0), 
                     breaks=c(1982,1985,1990,1995,2000,2005,
                              2010,2015,2019))+
  scale_y_continuous(expand=c(0,0), limits = c(2500,15000))+
  labs(x=NULL, y='Mean Annual PET (mm)', 
       title="Tree Containing Regions of Eastern Australia - 12 Month PET Anomaly by MAPET", 
       subtitle = 'Data: ERA5')
ggsave(p, 
       filename = 'figures/tileTimeSeries_eastOz_mean12moPETAnomSD.png', 
       width=35.5, height = 20, units='cm', dpi = 'retina', type='cairo')




# NVIS classes by LAI -----------------------------------------------------
d_train %>% 
  mutate(year=year(date)) %>% 
  select(year, 
         vc, 
         lai_u,
         lai_amp_u,
         nirv_anom_sd, 
         precip_anom_sd_12mo, 
         pet_anom_sd_12mo) %>% 
  na.omit() %>% 
  group_by(vc) %>% 
  summarize(lai_u = mean(lai_u), 
            lai_amp = mean(lai_amp_u)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(lai_u, lai_amp,color=vc))+
  geom_point()+
  geom_text(aes(label=vc),angle=45,
            position = position_stack(vjust =1.1))+
  labs(x='Mean annual LAI', 
       y='Mean annual LAI amplitude')+
  scale_color_viridis_d(end=0.9)+
  theme_linedraw()+
  theme(axis.text.x = element_text(angle=90))

d_train %>% 
  mutate(year=year(date)) %>% 
  select(year, 
         vc, 
         lai,
         lai_u,
         lai_amp_u,
         nirv,
         nirv_u,
         nirv_sd, 
         precip_anom_sd_12mo, 
         pet_anom_sd_12mo) %>% 
  na.omit() %>% 
  ggplot(data=., aes(vc, lai_u,fill=vc))+
  geom_violin(draw_quantiles = T)+
  scale_fill_brewer("NVIS major veg. class",
                    palette = 'Paired') +
  labs(x=NULL,y=expression(Mean~LAI))+
  theme_linedraw()+
  theme(axis.text.x = element_blank())


# NVIS classes by NIR-V ---------------------------------------------------
d_train %>% 
  mutate(year=year(date)) %>% 
  select(year, 
         vc, 
         lai_u,
         lai_amp_u,
         nirv,
         nirv_u,
         nirv_sd, 
         precip_anom_sd_12mo, 
         pet_anom_sd_12mo) %>% 
  na.omit() %>% 
  ggplot(data=., aes(vc, nirv,fill=vc))+
  geom_violin(draw_quantiles = T)+
  scale_fill_brewer("NVIS major veg. class",
                    palette = 'Paired') +
  labs(x=NULL,y=expression(paste(NIR[V])))+
  theme_linedraw()+
  theme(axis.text.x = element_blank())
  # geom_col()+
  # geom_errorbar(aes(ymin=val1-val2, ymax=val1+val3))+
  # geom_text(aes(label=vc),angle=45,
  #           position = position_stack(vjust =1.1))+
  # labs(x='Mean annual LAI', 
  #      y='Mean annual LAI amplitude')+
  scale_color_viridis_d(end=0.9)+
  theme_linedraw()+
  theme(axis.text.x = element_text(angle=90))




d_train %>% 
  mutate(year=year(date)) %>% 
  select(year, 
         nirv_anom_sd, 
         precip_anom_sd_12mo, 
         pet_anom_sd_12mo) %>% 
  na.omit() %>% 
  group_by(year) %>% 
  summarize(rho = cor(precip_anom_sd_12mo, pet_anom_sd_12mo)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year, rho))+
  geom_line()+
  geom_point()

vec_vc <- d_train$vc %>% unique %>% sort
d_train %>% 
  filter(nirv_anom_sd >= -5 & 
           nirv_anom_sd <= 5) %>% 
  mutate(year=year(date)) %>% 
  filter(vc==vec_vc[1:6]) %>% 
  ggplot(., aes(x = nirv_anom_sd, 
                y = as.factor(year), 
                fill=stat(x))) +
  geom_density_ridges_gradient(
    scale=3,
    rel_min_height=0.05,
    quantile_lines = TRUE, 
    quantiles = c(0.025, 0.975), alpha = 0.7)+
  geom_vline(aes(xintercept=0),color='grey')+
  scale_fill_viridis_c(expression(paste(sigma)), 
                       option='B')+
  scale_x_continuous(limits=c(-5,5))+
  labs(x=expression(paste(NIR[V]~Anomaly~(sigma))), 
       y=NULL)+
  theme_linedraw()+
  facet_wrap(~vc)

d_train %>% 
  filter(nirv_anom_sd >= -5 & 
           nirv_anom_sd <= 5) %>% 
  mutate(year=year(date)) %>% 
  filter(vc==vec_vc[7:12]) %>% 
  ggplot(., aes(x = nirv_anom_sd, 
                y = as.factor(year), 
                fill=stat(x))) +
  geom_density_ridges_gradient(
    scale=3,
    rel_min_height=0.05,
    quantile_lines = TRUE, 
    quantiles = c(0.025, 0.975), alpha = 0.7)+
  geom_vline(aes(xintercept=0),color='grey')+
  scale_fill_viridis_c(expression(paste(sigma)), 
                       option='B')+
  scale_x_continuous(limits=c(-5,5))+
  labs(x=expression(paste(NIR[V]~Anomaly~(sigma))), 
       y=NULL)+
  theme_linedraw()+
  facet_wrap(~vc)


p <- dat %>% 
  mutate(year=year(date)) %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(precip_anom_sd_12mo,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(x,y, fill=val))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma), 
                       limits=c(-4,4),oob=scales::squish)+
  labs(x=NULL,y=NULL,title=expression(paste("Mean 12 Monthly Precip. Anom."~(sigma))))+
  facet_wrap(~year, nrow = 4)+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
ggsave(p, 
       filename = 'figures/map_eastOz_mean12moPrecipAnomSD.png', 
       width=18, height = 18, units='cm', dpi = 'retina', type='cairo')

vec_cols <- RColorBrewer::brewer.pal(n=9, name="PuOr")
vec_cols <- rev(vec_cols)
p <- dat %>% 
  mutate(year=year(date)) %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(pet_anom_sd_12mo,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(x,y, fill=val))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(sigma),
                       low=vec_cols[1], 
                       mid=vec_cols[5],
                       high = vec_cols[9],
                       limits=c(-4,4),oob=scales::squish)+
  labs(x=NULL,y=NULL,title=expression(paste("Mean 12 Monthly PET Anom."~(sigma))))+
  facet_wrap(~year, nrow = 4)+
  theme_linedraw()+
  theme(legend.position = 'bottom', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
ggsave(p, 
       filename = 'figures/map_eastOz_mean12moPETAnomSD.png', 
       width=18, height = 18, units='cm', dpi = 'retina', type='cairo')



p <- dat %>% 
  group_by(x,y) %>% 
  summarize(val = first(vc)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(x,y, fill=val))+
  geom_tile()+
  coord_equal()+
  scale_fill_brewer("Major Veg. Class", 
                    palette = "Paired")+
  # scale_fill_gradient2(expression(sigma),
  #                      low=vec_cols[1], 
  #                      mid=vec_cols[5],
  #                      high = vec_cols[9],
  #                      limits=c(-4,4),oob=scales::squish)+
  labs(x=NULL,y=NULL,
       title=expression(paste("Tree Containing Major Veg. Classes of NVIS v5.1")))+
  theme_linedraw()+
  theme(legend.position = 'right', 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank())
ggsave(p, 
       filename = 'figures/map_eastOz_NVIS.png', 
       width=20, height = 22, units='cm', dpi = 'retina', type='cairo')





# Coefficient of Variation of P, PET, P:PET -------------------------------
library(arrow); library(tidyverse); library(lubridate); 
library(ggridges)
library(sf);
dat <- read_parquet("../data_general/AVHRR_CDRv5_VI/ard_nirv_2020-04-04.parquet",
                    col_select = c("x","y","date","precip","pet"))

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
p1 <- tmp2 %>% 
  ggplot(data=., aes(x,y,fill=p_pet_sd/p_pet_u))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c("CV P:PET")+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom')
p2 <- tmp2 %>% 
  ggplot(data=., aes(x,y,fill=p_sd/p_u))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c("CV Precip")+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom')
p3 <- tmp2 %>% 
  ggplot(data=., aes(x,y,fill=pet_sd/pet_u))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c("CV PET")+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom')
library(patchwork)
p2+p3+p1+plot_layout(nrow=1)

# Log normal formulation of CV: https://en.wikipedia.org/wiki/Coefficient_of_variation
tmp4 <- tmp %>% 
  group_by(x,y) %>% 
  summarize(cv2_p = sqrt(exp(sd(log1p(ap))**2) - 1), 
            cv2_pet = sqrt(exp(sd(log1p(apet))**2) - 1), 
            cv2_p_pet = sqrt(exp(sd(log1p(ap_apet))**2) - 1)) %>% 
  ungroup() %>% 
  as_tibble()
p1 <- tmp4 %>% 
  ggplot(data=., aes(x,y,fill=cv2_p_pet))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c("CV P:PET")+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1,'cm'))
p2 <- tmp4 %>% 
  ggplot(data=., aes(x,y,fill=cv2_p))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c("CV Precip")+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1,'cm'))
p3 <- tmp4 %>% 
  ggplot(data=., aes(x,y,fill=cv2_pet))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c("CV PET")+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1,'cm'))
library(patchwork)
p_out <- p2+p3+p1+plot_layout(nrow=1)+
  plot_annotation(title="Interannual Coefficient of Variation (1983-2018)")
ggsave(p_out, 
       filename = 'figures/map_CVlnorm_P_PET_PtoPET.png', 
       width=35.5, height = 20, units='cm', dpi = 'retina', type='cairo')

# Percentile division
tmp %>% 
  group_by(x,y) %>% 
  summarize(val90 = quantile(ap,0.9), 
            val10 = quantile(ap,0.1)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  ggplot(data=., aes(x,y,fill=val90/val10))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c("Precip p90/p10")+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom', 
        legend.key.width = unit(1,'cm'))



# plot trend in precip and 5yr annual precip sd
library(RcppRoll)
ttmp <- tmp %>% as_tibble() 
tmp6 <- ttmp %>% 
 group_by(x,y) %>% 
 arrange(year) %>% 
 mutate(p_sd_5yr = roll_sdr(ap, n=5, fill=NA)) %>% 
 ungroup() %>% 
  filter(is.na(p_sd_5yr)==F) %>% 
  group_by(x,y) %>% 
  summarize(trend_p_sd_5yr = coef(lm(p_sd_5yr~year))[2], 
            trend_ap = coef(lm(ap~year))[2]) %>% 
  ungroup() 

p10 <- tmp6 %>% 
  ggplot(data=., aes(x,y,fill=trend_ap))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(paste(mm~yr**-1)), 
                       limits=c(-10,10), 
                       oob=scales::squish)+
  labs(x=NULL,y=NULL, 
       title="Annual Precip. Trend")+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(1,'cm'))
p11 <- tmp6 %>% 
  ggplot(data=., aes(x,y,fill=trend_p_sd_5yr))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(paste(mm~yr**-1)), 
                       limits=c(-7.5,7.5), oob=scales::squish)+
  labs(x=NULL,y=NULL, 
       title="5-yr SD(Annual P) Trend")+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(1,'cm'))
p10+p11+plot_layout(nrow = 1)
ggsave(p10+p11+plot_layout(nrow = 1), 
       filename = 'figures/map_trend_P_5yrSDofP.png', 
       width=0.5*35.5, height = 20, units='cm', dpi = 'retina', type='cairo')

library(tidyverse); library(lubridate); library(arrow)
library(ranger); 
library(pdp); library(vip); 
dat <- read_parquet("../data_general/AVHRR_CDRv5_VI/ard_nirv_2020-04-04.parquet", 
                    ### dput(names(dat)) ### to get an R vector input representation
                    col_select = 
                      c("x", "y", "date", 
                        "pet", "precip",
                        # "veg_class",
                        "vc", 
                        "nirv", 
                         "month", 
                        # "pet_u", "precip_u", 
                        "nirv_u", 
                        #"pet_sd", "precip_sd", 
                        # "nirv_sd",
                        "map", "mapet", 
                        # "pet_anom", "precip_anom", 
                        "nirv_anom", 
                        "pet_anom_sd", "precip_anom_sd",
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
                        # "lai", 
                        "lai_u", 
                        # "lai_sd", "lai_min", "lai_max", 
                         "lai_amp_u", 
                        "ddate", "precip_anom_sd_12mo", "pet_anom_sd_12mo",
                        # "pet_precip", "pet_precip_anom_12mo", 
                        "ma_pet_precip"
                      ))

gc()
# split data --------------------------------------------------------------
nobs_mod <- 5e4
d_train <- dat %>% sample_n(nobs_mod)
d_test <- dat %>% sample_n(nobs_mod)

r_train <- d_train %>% 
  select(nirv_anom_sd, 
         vc,month,ddate,
         # x,y, 
         lai_amp_u,
         lai_u,
         pet_anom_sd, 
         precip_anom_sd, 
         pet_anom_sd_12mo,
         precip_anom_sd_12mo) %>% 
  na.omit()
r_test <- d_test %>% 
  select(nirv_anom_sd, 
         vc,month,ddate,
         # x,y, 
         lai_amp_u,
         lai_u,
         pet_anom_sd_12mo, 
         precip_anom_sd_12mo) %>% 
  na.omit()

r1 <- ranger::ranger(nirv_anom_sd ~ .,
                     data=r_train,
                # importance = 'impurity', 
               # regularization.usedepth = T
               )
r1
vip::vip(r1)

r1 %>%
  partial(pred.var = c("pet_anom_sd_12mo","precip_anom_sd_12mo"), 
          type='regression',grid.resolution = 10) %>%
  autoplot(rug = TRUE, 
           train = d_train)


x <- data.matrix(subset(r_test))  # training features
p1 <- partial(r1, 
              pred.var = "ddate", 
              ice = TRUE, # false
              center = TRUE, 
              plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "ggplot2", 
              train = x)
p2 <- partial(r1, 
              pred.var = c("pet_anom_sd_12mo","precip_anom_sd_12mo"), 
              ice = F, # false
              # center = TRUE, 
              plot = TRUE, 
              rug = TRUE, alpha = 0.1, plot.engine = "ggplot2",
              chull = T,
              train = x)
p3 <- partial(ames_xgb, pred.var = c("Overall_Qual", "Gr_Liv_Area"),
              plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = x)

# Figure 2
grid.arrange(p1, p2, p3, ncol = 3)

pdp::partial(r1, pred.var=c("vc"))

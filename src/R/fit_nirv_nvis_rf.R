library(tidyverse); library(lubridate); library(arrow)
library(ranger); library(tuneRanger) 
library(pdp); library(vip); 
dat <- read_parquet("../data_general/AVHRR_CDRv5_VI/ard_nirv_2020-04-04.parquet", 
                    ### dput(names(dat)) ### to get an R vector input representation
                    col_select = 
                      c( 
                        "x", "y", "date", 
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
nobs_mod <- 1e5
d_train <- dat %>% sample_n(nobs_mod)
d_test <- dat %>% sample_n(nobs_mod)

r_train <- d_train %>% 
  select(nirv_anom_sd, 
         vc,month,
         # ddate,
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



res <- tuneMtryFast(nirv_anom_sd ~ ., 
                    data = r_train, 
                    mtryStart = 7,
                    num.treesTry = 500,
                    stepFactor = 1.5,
                    doBest = F)
res

## Not run:
library(tuneRanger); library(mlr)
# A mlr task has to be created in order to use the package
nirv_task = makeRegrTask(data = r_train, target = "nirv_anom_sd")
# Estimate runtime
estimateTimeTuneRanger(nirv_task,num.threads = 8,
                       num.tree=500, 
                       iters=3)
# Tuning
res = tuneRanger(nirv_task, 
                 num.trees = 500,
                 num.threads = 8, 
                 iters = 3, 
                 save.file.path = NULL)
# Mean of best 5 % of the results
res
# Model with the new tuned hyperparameters
res$model
# Prediction
predict(res$model, newdata = iris[1:10,])
## End(Not run)



r1 <- ranger::ranger(nirv_anom_sd ~ .,
                     data=r_train,
                importance = 'impurity',
               regularization.usedepth = T
               )
r1
vip::vip(r1)

r1 %>%
  partial(pred.var = c("pet_anom_sd_12mo","precip_anom_sd_12mo"), 
          type='regression',grid.resolution = 10) %>%
  autoplot(rug = TRUE, 
           train = d_train)
partial(r1,'pet_anom_sd_12mo',
        pred.grid=data.frame(pet_anom_sd_12mo=seq(-4,4,length.out = 20)),
        plot=T)
partial(r1,'precip_anom_sd_12mo',
        pred.grid=data.frame(precip_anom_sd_12mo=seq(-4,4,length.out = 20)),
        plot=T)
partial(r1,'ddate',
        pred.grid=data.frame(ddate=1981:2019),
        plot=T)





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

list.files("../data_general/clim_grid/awap/parquet/")
list.files("../data_general/Oz_misc_data/", pattern = '.parquet',full.names = T)
# library(mgcv)
library(tidyverse); library(lubridate)
library(data.table); library(mgcv); library(mgcViz)
setDTthreads(threads=10)

# import
tmp <- arrow::read_parquet("../data_general/Oz_misc_data//nirv_nvis_clim_2020-04-04.parquet", 
                           col_select = c("x","y","vc","date",
                                          "nirv_anom_sd",
                                           "precip","pet"))

# data.table 
tmp <- setDT(tmp) # OR: tmp <- as.data.table(tmp)

# Subsetting to just one type of vegetation class
vec_vc <- unique(tmp$vc) %>% sort
tmp <- tmp[vc=="Eucalypt Tall Open Forests"]


# Calculate Climatology --------------------------------------------------------
# calc norms *******************************************************************
tmp <- tmp[, `:=`(month = month(date))] # create month
tmp <- tmp[, `:=`(year = year(date))]   # create year
tmp <- tmp[,`:=`("pe" = precip/pet)]
norms_p <- tmp[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
               .("precip_u" = mean(precip), 
                 "precip_sd" = sd(precip)),
               by=.(x,y,month)]
norms_pet <- tmp[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
                 .(pet_u = mean(pet), 
                   pet_sd = sd(pet)),
                 by=.(x,y,month)]
norms_pe <- tmp[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
                 .(pe_u = mean(pe), 
                   pe_sd = sd(pe)),
                 by=.(x,y,month)]

norms_map <- tmp[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
                 .("ap" = sum(precip)),
                 by=.(x,y,year)][,.("map"=mean(ap), 
                                    "ap_sd"=sd(ap)),by=.(x,y)]
norms_mapet <- tmp[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
                 .("apet" = sum(pet)),
                 by=.(x,y,year)][,.("mapet"=mean(apet), 
                                    "apet_sd"=sd(apet)),by=.(x,y)]
norms_mape <- tmp[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
                 .("ape" = sum(pe)),
                 by=.(x,y,year)][,.("mape"=mean(ape), 
                                    "ape_sd"=sd(ape)),by=.(x,y)]

# join all the data frames *****************************************************
norms <- norms_p[norms_pet, on=.(x,y,month)] # join data.tables
norms <- norms[norms_pe, on=.(x,y,month)] # join data.tables
norms <- norms[norms_map, on=.(x,y)]
norms <- norms[norms_mapet, on=.(x,y)]
norms <- norms[norms_mape, on=.(x,y)]
tmp <- tmp[norms, on=.(x,y,month)]

# calculate the anomalies ******************************************************
tmp <- tmp[, `:=`(precip_anom = precip-precip_u,  # calc raw anomaly 
        pet_anom = pet-pet_u, 
        pe_anom = pe-pe_u)]
tmp <- tmp[, `:=`(precip_anom_sd = precip_anom/precip_sd,  # calc sd anomaly 
                  pet_anom_sd = pet_anom/pet_sd, 
                  pe_anom_sd = pe_anom/pe_sd)]

# calculate the rolling 12-month sums & anomalies
tmp <- tmp[order(x,y,date)][, precip_12mo := frollsum(precip,n = 12,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pet_12mo := frollsum(pet,n = 12,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pe_12mo := frollsum(pe,n = 12,fill = NA,align='right'), by=.(x,y)]

tmp <- tmp[, `:=`(precip_anom_12mo = precip_12mo-map)]
tmp <- tmp[, `:=`(pet_anom_12mo = pet_12mo-mapet)]
tmp <- tmp[, `:=`(pe_anom_12mo = pe_12mo-mape)]


# Cast the variables to lagged matrices -----------------------------------
gc(reset = TRUE)
tmp <- tmp[order(x,y,date), c(paste0("precip_",1:36)) := shift(precip, 1:36) , .(x,y)][order(date)]
gc(verbose = T, reset = T, full = T)

system.time(
  tmp <- tmp[order(x,y,date), c(paste0("pet_",1:36)) := shift(pet, 1:36) , .(x,y)][order(date)]
)
gc(verbose = T, reset = T, full = T)

tmp <- tmp[order(x,y,date), c(paste0("pe_",1:36)) := shift(pe, 1:36) , .(x,y)][order(date)]


# do it for the anom
gc(reset = TRUE)
tmp <- tmp[order(x,y,date), c(paste0("precip_anom_",1:36)) := shift(precip_anom, 1:36) , .(x,y)][order(date)]
gc(verbose = T, reset = T, full = T)

system.time(
  tmp <- tmp[order(x,y,date), c(paste0("pet_anom_",1:36)) := shift(pet_anom, 1:36) , .(x,y)][order(date)]
)
gc(verbose = T, reset = T, full = T)

tmp <- tmp[order(x,y,date), c(paste0("pe_anom_",1:36)) := shift(pe_anom, 1:36) , .(x,y)][order(date)]


d1 <- tmp[vc=="Eucalypt Tall Open Forests"]
lag_n <- 0:36 ## create wavelength matrix...
j <- 3
# d2 <- d1[x >= (152+j) & x<(152+j) & y>= (-32+j) & y < (30+j)] # TEMPORARY BREAKING UP DF
# d2 <- d1[x %in% d1$x[1:10]]
d2 <- d1
tmp_mat <- t(matrix(lag_n,length(lag_n),length(d2$x)))
d2 <- as_tibble(d2)
d2$lag_month <- tmp_mat # lag index is needed for GAM
d2 <- d2 %>% rename(precip_0 = precip, 
              pet_0 = pet, 
              pe_0 = pe)
d2$lag_precip <- d2 %>% select(paste0('precip_',0:36))
d2$lag_pet <- d2 %>% select(paste0('pet_',0:36))
d2$lag_pe <- d2 %>% select(paste0('pe_',0:36))

d2 <- d2 %>% mutate(precip_anom_0 = precip_anom, 
                    pet_anom_0 = pet_anom, 
                    pe_anom_0 = pe_anom)
d2$lag_precip_anom <- d2 %>% select(paste0('precip_anom_',0:36))
d2$lag_pet_anom <- d2 %>% select(paste0('pet_anom_',0:36))
d2$lag_pe_anom <- d2 %>% select(paste0('pe_anom_',0:36))


# d2 <- d2 %>% select(x,y,vc,date,nirv_anom_sd, lag_month, 
#                     lag_precip, lag_pet, 
#                     lag_precip_anom, lag_pet_anom)
d2$lag_precip <- d2$lag_precip %>% as.matrix()
d2$lag_pet <- d2$lag_pet %>% as.matrix()
d2$lag_pe <- d2$lag_pe %>% as.matrix()

d2$lag_precip_anom <- d2$lag_precip_anom %>% as.matrix()
d2$lag_pet_anom <- d2$lag_pet_anom %>% as.matrix()
d2$lag_pe_anom <- d2$lag_pe_anom %>% as.matrix()

# filter extreme outliers
d2 <- d2 %>% 
  filter(is.na(nirv_anom_sd)==F) %>% 
  filter(between(nirv_anom_sd,-3.5,3.5))


library(mgcv); library(mgcViz)
library(parallel)  
nc <- 6
if (detectCores()>1) { ## no point otherwise
  cl <- makeCluster(nc) 
  ## could also use makeForkCluster, but read warnings first!
} else cl <- NULL
fit <- bam(nirv_anom_sd ~ 
             ddate+
             # s(month)+
             s(x,y,fx = T)+
             # s(map,mapet)+
             # s(precip_anom_12mo)+
             # s(pet_anom_12mo)+
             # s(mape, pe_anom_12mo)+
             s(precip_anom_12mo, pet_anom_12mo)+
             s(lag_month,by=lag_precip_anom, bs='gp')+
             s(lag_month,by=lag_pet_anom, bs='gp'),
           data=d2 %>% filter(between(nirv_anom_sd,-3.5,3.5)) %>% 
             mutate(ddate = decimal_date(date)) %>% 
             filter(month %in% c(12,1,2)), 
           select=T, method='fREML', discrete = T, 
           gamma=50, nthreads = 6
           # cluster = cl # does not work with discrete=T
           )
summary(fit)

getViz(fit) %>% plot(allTerms=T)
plot(fit,rug=F,shade=T, select = 1, scale=0);abline(h=0,col='red',lty=3)
plot(fit,rug=F,shade=T, select = 2, scale=0);abline(h=0,col='red',lty=3)
plot(fit,rug=F,shade=T, select = 3, scale=0);abline(h=0,col='red',lty=3)
plot(fit$model$nirv_anom_sd~fitted(fit)); abline(0,1,col='red')

df_preds <- d2 %>% 
  filter(month %in% c(12,1,2)) %>% 
  mutate(ddate=decimal_date(date)) %>% 
  mutate(pred = predict(fit,newdata=., type='response')) 
df_preds %>% 
  sample_frac(0.1) %>% 
  ggplot(data=., aes(date, nirv_anom_sd))+
  geom_point(alpha=0.1)+
  geom_point(aes(date, pred),color='darkgreen',alpha=0.1)+
  geom_smooth(color='black')+
  geom_smooth(aes(date, pred),color='darkgreen')

  # group_by(date) %>% 
  # summarize(obs = mean(nirv_anom_sd,na.rm=T), 
  #           pred = pred) %>% 
  

(d2 %>% select(x,y) %>% distinct() %>% dim())[1]*(as.numeric((ymd("2019-01-01")-ymd("1986-01-01")))/30)/36

fit2 <- bam(nirv_anom_sd ~ 
             ddate+
             s(x,y,fx = TRUE)+ # unpenalized spatial term
             s(lag_month,by=lag_precip_anom, bs='gp')+
             s(lag_month,by=lag_pet_anom, bs='gp'),
           data=d2 %>% filter(between(nirv_anom_sd,-6,6)) %>% 
             mutate(ddate = decimal_date(date)) %>% 
             filter(month %in% c(12,1,2)), 
           select=T, method='fREML', 
           gamma=33, # n/gamma
           discrete = T
)
summary(fit2)


plot(fit2,rug=F,shade=T, select = 1, scale=0);abline(h=0,col='red',lty=3)
plot(fit2,rug=F,shade=T, select = 2, scale=0);abline(h=0,col='red',lty=3)
plot(fit2,rug=F,shade=T, select = 3, scale=0);abline(h=0,col='red',lty=3)
plot(fit2$model$nirv_anom_sd~fitted(fit2)); abline(0,1,col='red')
b <- getViz(fit2)
plot(b, allTerms = T)



# xgboost -----------------------------------------------------------------
library(xgboost)
d2 <- d2 %>% filter(is.na(precip_anom_36)==F & is.na(nirv_anom_sd)==F) %>% 
  filter(between(nirv_anom_sd,-4,4))
idx_train <- sample.int(dim(d2)[1],10000)
vec_label <- d2$nirv_anom_sd[idx_train]
mat_dat <- d2 %>% select(starts_with(c("m","precip_anom","pet_anom"))) %>% as.matrix()
mat_dat <- mat_dat[idx_train,]

vec_test <- d2$nirv_anom_sd[-idx_train]
mat_test <- as.matrix(d2 %>% 
                        select(starts_with(c("m",
                                             "precip_anom",
                                             "pet_anom"))))[-idx_train,] 

x_1 <- xgboost(data = mat_dat, 
               label = vec_label, 
               max_depth = 5, 
               eta = 0.1, 
               nthread = 10, 
               nrounds = 300,
               params=list(booster="dart"),
               objective = "reg:squarederror")
summary(x_1)
x_1
xgboost::xgb.importance(model=x_1)
xgboost::xgb.plot.importance(xgboost::xgb.importance(model=x_1))
cor(vec_test,predict(x_1,newdata=mat_test))**2

preds <- tibble(pred=predict(x_1,newdata=mat_test), 
       obs = vec_test)
preds %>% 
  sample_frac(0.1) %>% 
  ggplot(data=., aes(pred, obs))+
  ggpointdensity::geom_pointdensity()+
  geom_smooth()+
  scale_color_viridis_c(option='B')+
  geom_abline(aes(intercept=0,slope=1),col='red')

preds %>% 
  mutate(precip_anom_12mo = mat_test[,7]) %>% 
  sample_frac(0.1) %>% 
  ggplot(data=., aes(precip_anom_12mo, obs))+
  geom_point(alpha=0.05)+
  geom_point(aes(precip_anom_12mo,pred),color='green',alpha=0.05)+
  geom_smooth(color='black',method='lm')+
  geom_smooth(aes(precip_anom_12mo,pred),color='darkgreen',method='lm')
  
# scratch ----------------------------------------------------------------------
fit <- bam(nirv_anom_sd ~ 
             # s(ddate)+
             # s(lag_month,by=lag_precip, bs='gp',k=7)+
             s(lag_pet,lag_precip,by=lag_n, bs='gp', k=7),
           data=d2 %>% filter(between(nirv_anom_sd,-6,6)) %>% 
             mutate(ddate = decimal_date(date)), 
           select=T, method='fREML', discrete = T
)
summary(fit)
plot(fit)
b <- getViz(fit)

#






gas$NIR %>% class
gas$nm %>% class
d2$lag_n %>% class
d2$lag_precip %>% class




d2[paste0('lag_n', names(d2)[0:36])] <- tmp_mat


d2$lag_n <- t(matrix(lag_n,length(lag_n),length(d2$x)))

lag_n <- matrix(0:36,nrow = 1, byrow = T)
d1$lag_n <- lag_n

fit <- bam(nirv_anom_sd ~ 
             s(lag_n_precip,by=lag_precip,bs="gp")+
             s(lag_n_pet,by=lag_pet,bs="gp"),
           data=v1, 
           select=T, method='fREML', discrete = T)
summary(fit)

# scratch 
fit <- bam(nirv_anom_sd ~ 
             # s(lag_n_precip,by=lag_precip,bs="gp")+
             s(lag_n_pet,lag_pet,bs="gp"),
           data=v1, 
           select=T, method='fREML', discrete = T)
summary(fit)










# define all important function -------------------------------------------
jetlag2 <- function(data, variable, n=10){
  
  # https://purrple.cat/blog/2018/03/02/multiple-lags-with-tidy-evaluation/
  variable <- enquo(variable)
  
  indices <- seq(from=1, to=n, by=1)
  quosures <- map( indices, ~quo(lag(!!variable, !!.x)) ) %>% 
    set_names(sprintf("lag_%02d", indices))
  out_lag_var <- mutate( data, !!!quosures ) %>% as.matrix()
  
  out_lag_var <- out_lag_var[,1:n]
  out_lag_n <- t(matrix(rep(0:(n-1),dim(data)[1]),n)) %>% as.matrix()
  colnames(out_lag_n) <- vctrs::vec_cast(paste0("_",c(1:n)), character())
  
  out <- data
  out$lag_var <- out_lag_var
  out$lag_n <- out_lag_n
  return(out)
}

library(data.table)
jetlag2_dt <- function(data, variable, n=10){
  
  # https://purrple.cat/blog/2018/03/02/multiple-lags-with-tidy-evaluation/
  variable <- enquo(variable)
  
  indices <- seq(from=1, to=n, by=1)
  quosures <- map( indices, ~quo(shift(!!variable, !!.x)) ) %>% 
    set_names(sprintf("lag_%02d", indices))
  out_lag_var <- mutate( data, !!!quosures ) %>% as.matrix()
  
  out_lag_var <- out_lag_var[,1:n]
  out_lag_n <- t(matrix(rep(0:(n-1),dim(data)[1]),n)) %>% as.matrix()
  colnames(out_lag_n) <- vctrs::vec_cast(paste0("_",c(1:n)), character())
  
  out <- data
  out$lag_var <- out_lag_var
  out$lag_n <- out_lag_n
  return(out)
}

microbenchmark::microbenchmark(
junk %>%
  select(x,y,date,vc,nirv_anom_sd,precip,pet) %>% 
  group_by(x,y,vc) %>% 
  arrange(date) %>% 
  jetlag2(., precip, 36)
)

microbenchmark::microbenchmark(
junk_dt %>%
  select(x,y,id,date,vc,nirv_anom_sd,precip,pet) %>% 
  group_by(x,y) %>% 
  arrange(date) %>% 
  jetlag2_dt(., precip, 36)
)

library(dtplyr)
junk_dt <- data.table(junk)
junk_l <- lazy_dt(junk_dt)
junk_dt %>%
  select(x,y,id,date,vc,nirv_anom_sd,precip,pet) %>% 
  group_by(x,y) %>% 
  arrange(date) %>% 
  jetlag2_dt(., precip, 36)

junk_l %>% select(x,y,id,date,vc,nirv_anom_sd,precip,pet) %>% 
  group_by(x,y) %>% 
  arrange(date)

# Data Table approach
o1 <- junk_dt[, .(x, y, id, date, vc, nirv_anom_sd, precip, pet)][order(date)]  
jetlag2_dt(o1, precip, 36)

#setDT(df)[order(time), c("a", "b", "c") := shift(x, 1:3) , id][order(id, time)]

junk_dt[order(date), c(paste0("precip_",1:10)) := shift(precip, 1:10) , id][order(date)]


junk_dt[, .(.N), by = .(id)]


tmp3 <- junk_dt[order(date), c("p1","p2","p3") := shift(precip, 1:3) , id][order(date)]
tmp3 %>% 
  as_tibble() %>% 
  filter(id==vec_id[1]) %>% 
  filter(date <= ymd('1985-01-01')) %>% 
  ggplot(data=., aes(date,precip))+
  geom_line()+
  geom_line(aes(date,p1),col='blue')+
  geom_line(aes(date,p2),col='blue')+
  geom_line(aes(date,p3),col='blue')


microbenchmark::microbenchmark(
  junk_dt[order(date), c(paste0("precip_",1:36)) := shift(precip, 1:36) , id][order(date)]
)
microbenchmark::microbenchmark(
  junk %>%
    select(x,y,id,date,vc,nirv_anom_sd,precip,pet) %>% 
    group_by(id) %>% 
    arrange(date) %>% 
    jetlag2(., precip, 36)
)

junk_dt <- data.table(junk)
tmp3 <- junk_dt[order(date), c(paste0("precip_",1:36)) := shift(precip, 1:36) , .(x,y)][order(date)]
tmp3 %>% 
  as_tibble() %>% 
  # filter(id==vec_id[1]) %>% 
  filter(date <= ymd('1987-01-01')) %>% 
  ggplot(data=., aes(date,precip))+
  geom_line()+
  geom_line(aes(date,precip_1),col='blue')+
  geom_line(aes(date,precip_12),col='purple')+
  geom_line(aes(date,precip_36),col='red')+
  facet_wrap(~id)

  
# parallel processing -----------------------------------------------------
library(foreach); library(doParallel)
n_cores <- 10
cl <- makeCluster(n_cores)
registerDoParallel(cl)
tmp2 <- foreach(i = 1:10, 
               .packages = c("tidyverse"),
               .combine=rbind) %dopar% {
                 out <- tmp %>%
                   filter(id == vec_id[i]) %>% 
                   select(x,y,vc,nirv_anom_sd,precip,pet) %>% 
                   group_by(x,y,vc) %>% 
                   jetlag2(., precip, 36) %>% 
                   ungroup() %>% 
                   rename(lag_precip = lag_var, 
                          lag_n_precip = lag_n) %>% 
                   jetlag2(., pet, 12) %>% 
                   rename(lag_pet = lag_var, 
                          lag_n_pet = lag_n)
                 out
                 
               }

#*******************************************************************************

jetlag2 <- function(data, variable, n=10){
  
  # https://purrple.cat/blog/2018/03/02/multiple-lags-with-tidy-evaluation/
  variable <- enquo(variable)
  
  indices <- seq(from=1, to=n, by=1)
  quosures <- map( indices, ~quo(lag(!!variable, !!.x)) ) %>% 
    set_names(sprintf("lag_%02d", indices))
  out_lag_var <- mutate( data, !!!quosures ) %>% as.matrix()
  
  out_lag_var <- out_lag_var[,1:n]
  out_lag_n <- t(matrix(rep(0:(n-1),dim(data)[1]),n)) %>% as.matrix()
  colnames(out_lag_n) <- vctrs::vec_cast(paste0("_",c(1:n)), character())
  
  out <- data
  out$lag_var <- out_lag_var
  out$lag_n <- out_lag_n
  return(out)
}




library(microbenchmark)
junk <- tmp$`Acacia Forests and Woodlands` %>% 
  filter(x==
       tmp$`Acacia Forests and Woodlands`$x[1],
       y == tmp$`Acacia Forests and Woodlands`$y[1])

microbenchmark(
  junk %>% 
    select(x,y,vc,nirv_anom_sd,precip,pet) %>% 
    group_by(x,y,vc) %>% 
    jetlag2(., precip, 36) %>% 
    ungroup() %>% 
    rename(lag_precip = lag_var, 
           lag_n_precip = lag_n) %>% 
    jetlag2(., pet, 12) %>% 
    rename(lag_pet = lag_var, 
           lag_n_pet = lag_n)
)

microbenchmark({
o <-  junk %>% select(x,y,vc,nirv_anom_sd,precip,pet)
o <- o %>% group_by(x,y,vc) %>% 
    jetlag2(., precip, 36) %>% 
    ungroup()
o <- o %>% 
    rename(lag_precip = lag_var, 
           lag_n_precip = lag_n)  
o <- o %>% jetlag2(., pet, 12) 
o <- o %>% rename(lag_pet = lag_var, 
           lag_n_pet = lag_n)
})


library(data.table); library(dtplyr); 
junk_dt <- data.table(junk)
junk_l <- lazy_dt(junk_dt)
microbenchmark({
  o <-  junk_l %>% select(x,y,vc,nirv_anom_sd,precip,pet)
  o <- o %>% group_by(x,y,vc) %>% 
    jetlag2(., precip, 36) %>% 
    ungroup()
  o <- o %>% 
    rename(lag_precip = lag_var, 
           lag_n_precip = lag_n)  
  o <- o %>% jetlag2(., pet, 12) 
  o <- o %>% rename(lag_pet = lag_var, 
                    lag_n_pet = lag_n)
})




vec_vc <- sort(tmp$vc %>% unique)
tmp <- split(tmp, tmp$vc)

system.time(
tmp$`Acacia Forests and Woodlands` %>% 
  select(x,y,vc,nirv_anom_sd,precip,pet) %>% 
  group_by(x,y,vc) %>% 
  jetlag2(., precip, 36) %>% 
  ungroup() %>% 
  rename(lag_precip = lag_var, 
         lag_n_precip = lag_n) %>% 
  jetlag2(., pet, 12) %>% 
  rename(lag_pet = lag_var, 
         lag_n_pet = lag_n)
)




for(i in 1:1){
}
v1 <- tmp %>% 
  # filter(vc == vec_vc[1]) %>% 
  select(x,y,vc,nirv_anom_sd,precip,pet) %>% 
  group_by(x,y,vc) %>% 
  jetlag2(., precip, 36) %>% 
  ungroup() %>% 
  rename(lag_precip = lag_var, 
         lag_n_precip = lag_n) %>% 
  jetlag2(., pet, 12) %>% 
  rename(lag_pet = lag_var, 
         lag_n_pet = lag_n)

fit <- bam(nirv_anom_sd ~ 
             s(lag_n_precip,by=lag_precip,bs="gp")+
             s(lag_n_pet,by=lag_pet,bs="gp"),data=v1, 
           select=T, method='fREML', discrete = T)
summary(fit)
plot(fit,rug=F,shade=T, select = 1, scale=0);abline(h=0,col='red',lty=3)
plot(fit,rug=F,shade=T, select = 2, scale=0);abline(h=0,col='red',lty=3)

plot(fit,rug=FALSE,shade=TRUE,main="Estimated function"); abline(h=0,col='red')
summary(fit)








tmp <- tmp %>% 
  mutate(ppet = precip/pet)



tmp <- tmp %>% filter(is.na(nirv)==F)
vec_vc <- tmp$vc %>% unique %>% sort
junk <- tmp %>% 
  # filter(x==tmp$x[1000], y==tmp$y[1000]) %>% 
  filter(vc == as.character(vec_vc[9])) %>% 
  arrange(date)
jetlag <- function(data, variable, n=10){
  variable <- enquo(variable)
  
  indices <- seq_len(n)
  quosures <- map( indices, ~quo(lag(!!variable, !!.x)) ) %>% 
    set_names(sprintf("lag_%02d", indices))
    mutate( data, !!!quosures )
}

junk2 <- junk %>% 
  group_by(x,y) %>% 
  jetlag(., precip, 36)



pmat <- jetlag(junk, precip, 35) %>% 
  select(nirv_anom_sd, precip, starts_with('lag'))
names(pmat) <- c('nirv',paste0("precip",0:35))
pmat <- pmat %>% filter(precip35 >= 0)
pmat$p <- pmat[,2:37] %>% as.matrix()
pmat$l <- t(matrix(rep(1:36,dim(pmat)[1]),36))
fit <- gam(nirv ~ s(l,by=p,bs="gp"),data=pmat)
plot(fit,rug=FALSE,shade=TRUE,main="Estimated function")
plot(fitted(fit),pmat$nirv)





jetlag2 <- function(data, variable, n=10){

  # https://purrple.cat/blog/2018/03/02/multiple-lags-with-tidy-evaluation/
  variable <- enquo(variable)
  
  indices <- seq(from=1, to=n, by=1)
  quosures <- map( indices, ~quo(lag(!!variable, !!.x)) ) %>% 
    set_names(sprintf("lag_%02d", indices))
  out_lag_var <- mutate( data, !!!quosures ) %>% as.matrix()
  
  out_lag_var <- out_lag_var[,1:n]
  out_lag_n <- t(matrix(rep(0:(n-1),dim(data)[1]),n)) %>% as.matrix()
  colnames(out_lag_n) <- vctrs::vec_cast(paste0("_",c(1:n)), character())
  
  out <- data
  out$lag_var <- out_lag_var
  out$lag_n <- out_lag_n
  return(out)
}
jj <- jetlag2(tibble(xx=rnorm(50, mean=sin(seq(-3,3,length.out = 50)), 
                                  sd=1+sin(seq(-3,3,length.out=50)))), xx, 10)
jj
names(jj)
fit <- gam(xx ~ s(lag_n,by=lag_var,bs="ad"),data=jj)
plot(fit,rug=FALSE,shade=TRUE,main="Estimated function")

fit <- bam(xx ~ s(lag_n,by=lag_var,bs="ad"),data=jj,method='REML',select=T,discrete=T)
plot(fit,rug=FALSE,shade=TRUE,main="Estimated function")
plot(fitted(fit),fit$model$xx); abline(0,1,col='red')


junk2 <- junk %>% 
  # filter(x > 151) %>% 
  select(x,y,nirv_anom_sd,precip,pet) %>% 
  group_by(x,y) %>% 
  jetlag2(., precip, 36) %>% 
  ungroup() %>% 
  rename(lag_precip = lag_var, 
         lag_n_precip = lag_n) %>% 
  jetlag2(., pet, 12) %>% 
  rename(lag_pet = lag_var, 
         lag_n_pet = lag_n)
  
fit <- bam(nirv_anom_sd ~ s(lag_n_precip,by=lag_precip,bs="gp")+
             s(lag_n_pet,by=lag_pet,bs="gp"),data=junk2, 
           select=T, method='fREML', discrete = T)
plot(fit,rug=FALSE,shade=TRUE,main="Estimated function"); abline(h=0,col='red')
plot(fit,rug=F,shade=T)
summary(fit)


gas$NIR %>% image
gas$nm %>% image

dd %>% 
  rename(date=time) %>% 
  group_by(date) %>% 
  summarize(awap = mean(pet_pt,na.rm=T), 
            e5 = mean(pet_pm,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(awap_sd = scale(awap)[,1], 
         e5_sd = scale(e5)[,1]) %>% 
  select(awap_sd, e5_sd, date) %>% 
  gather(-date, key='type',value='estimate') %>% 
  ggplot(data=., aes(date,estimate,color=type))+
  geom_line()


tmp %>% 
  group_by(x,y) %>% 
  filter(pet == min(pet)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(x,y,fill=pet))+
  geom_tile()+
  scale_fill_viridis_c(limits=c(0,200))+
  coord_equal()


library(pls); data(gasoline);gas <- gasoline
nm <- seq(900,1700,by=2) ## create wavelength matrix...
gas$nm <- t(matrix(nm,length(nm),length(gas$octane)))
b <- gam(octane~s(nm,by=NIR,bs="ad"),data=gas)
plot(b,rug=FALSE,shade=TRUE,main="Estimated function")
plot(fitted(b),gas$octane)


tmp[x == tmp$x[1] & y == tmp$y[1], ] %>%
  ggplot(data=., aes(date, precip_anom_12mo))+
  geom_line(lwd=1)

old <- o[x == tmp$x[1] & y == tmp$y[1], ] %>% 
  as_tibble() %>% 
  group_by(x,y) %>% 
  arrange(date) %>% 
  mutate(precip_12mo = RcppRoll::roll_sumr(precip,n=12,fill=NA)) %>% 
  ungroup()


o[x == tmp$x[1] & y == tmp$y[1], ] %>%
  ggplot(data=., aes(date, precip_12mo))+
  geom_line(lwd=2)+
  geom_line(data=old, aes(date, precip_12mo),col='blue')


# tmp <- tmp[, .(precip_anom = precip-precip_u,  # calc raw anomaly 
#         pet_anom = pet-pet_u)]
# tmp[, .(precip_anom_sd = precip_anom/precip_sd)]

# norms[,'precip']




fit2 <- bam(nirv_anom_sd ~ 
             ddate+
              # s(pe_anom_36)
              # s(log(pe),k=5)
             # s(month)+
             s(x,y,fx = T)+
             # s(pe_anom_12mo,k=5)   # nonlinear
             # s(map,mapet)+
             # s(precip_anom_12mo,k=5)     # very nonlinear
             # s(pet_anom_12mo,k=5)        #
             # s(mape, pe_anom_12mo)+
             s(precip_anom_12mo, pet_anom_12mo)+
             # s(lag_month,by=lag_precip_anom, bs='gp')+
             # s(lag_month,by=lag_pet_anom, bs='gp')+
            s(lag_month,by=lag_pe_anom, bs='gp',m=c(2,12,2))
            ,
           data=d2 %>% filter(between(nirv_anom_sd,-3.5,3.5)) %>% 
             mutate(ddate = decimal_date(date)) %>% 
             filter(month %in% c(12,1,2)), 
           select=T, method='fREML', discrete = T, 
           gamma=30,
           nthreads = 6
           # cluster = cl # does not work with discrete=T
)
summary(fit2)
getViz(fit2) %>% plot(allTerms=F)


curve(log10(1000/x),500,10000)
d2$pe %>% hist
d2$pe %>% log %>% hist
d2$pe %>% log10 %>% hist
d2$pe_anom_12mo %>% hist




DT[, c(t(outer(names(DT), sz, paste0))) := {
  
  #use frollsum with centering alignment
  C <- matrix(unlist(frollsum(.SD, 2L*sz + 1L, align="center")), nrow=.N)
  
  #largest window size
  winsz <- 2L*last(sz)+1L
  
  #extract head and tail of data and reverse row order of tail
  H <- head(.SD, winsz)
  B <- tail(.SD, winsz)[.N:1L]
  
  #calculate sums of those head and tail using frollmean and cumsum
  U <- matrix(unlist(frollsum(H, sz+1L, align="left")), nrow=winsz) +
    rep(H[, as.matrix(lapply(.SD, cumsum) - .SD)], length(sz))
  D <- matrix(unlist(frollsum(B, sz+1L, align="left")), nrow=winsz) +
    rep(B[, as.matrix(lapply(.SD, cumsum) - .SD)], length(sz))
  D <- D[rev(seq_len(nrow(D))), ]
  
  #update NAs in C with values from U and D
  C[is.na(C) & row(C) <= winsz] <- U[is.na(C) & row(C) <= winsz]
  C[is.na(C) & row(C) >= .N - winsz] <- D[is.na(C) & row(C) >= .N - winsz]
  as.data.table(C)
}]


sz <- seq(1:12)
tmp[x==tmp$x[1] & y==tmp$y[1]][,c("precip")][, c(t(outer(names(tmp), sz, paste0))) := {
  
  #use frollsum with centering alignment
  C <- matrix(unlist(frollsum(.SD, 2L*sz + 1L, align="right")), nrow=.N)
  
  #largest window size
  winsz <- 2L*last(sz)+1L
  
  #extract head and tail of data and reverse row order of tail
  H <- head(.SD, winsz)
  B <- tail(.SD, winsz)[.N:1L]
  
  #calculate sums of those head and tail using frollmean and cumsum
  U <- matrix(unlist(frollsum(H, sz+1L, align="left")), nrow=winsz) +
    rep(H[, as.matrix(lapply(.SD, cumsum) - .SD)], length(sz))
  D <- matrix(unlist(frollsum(B, sz+1L, align="left")), nrow=winsz) +
    rep(B[, as.matrix(lapply(.SD, cumsum) - .SD)], length(sz))
  D <- D[rev(seq_len(nrow(D))), ]
  
  #update NAs in C with values from U and D
  C[is.na(C) & row(C) <= winsz] <- U[is.na(C) & row(C) <= winsz]
  C[is.na(C) & row(C) >= .N - winsz] <- D[is.na(C) & row(C) >= .N - winsz]
  as.data.table(C)
}]

require(data.table)
setDT(data)[,ID2:=.GRP,by=c("ID")]
Ref <-data[,list(Compare_Value=list(I(USD)),Compare_Date=list(I(Date))),by=c("ID2")]
data[,Roll.Val := mapply(RD = Date,NUM=ID2, function(RD, NUM) {
  d <- as.numeric(Ref[ID2 == NUM,]$Compare_Date[[1]] - RD)
  sum((d <= 0 & d >= -7)*Ref[ID2 == NUM,]$Compare_Value[[1]])})]


# rolling sum
tmp <- tmp[order(x,y,date)][, pe_12mo := frollsum(pe,n = 12,fill = NA,align='right'), by=.(x,y)]

# cast lags
tmp <- tmp[order(x,y,date), c(paste0("precip_",1:36)) := shift(precip, 1:36) , .(x,y)][order(date)]

tmp[x==tmp$x[1] & y==tmp$y[1]][,c("precip")][,precip_sum := apply(
  var=precip, FUN=function(precip){
    frollsum(precip,n = 12,fill = NA,align='right')
  }
)]




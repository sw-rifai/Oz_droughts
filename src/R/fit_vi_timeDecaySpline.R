list.files("../data_general/clim_grid/awap/parquet/")
list.files("../data_general/Oz_misc_data/", pattern = '.parquet',full.names = T)
library(mgcv)
library(tidyverse); library(lubridate)
tmp <- arrow::read_parquet("../data_general/Oz_misc_data//nirv_nvis_clim_2020-04-04.parquet", 
                           col_select = c("x","y","vc","date",
                                          "nirv_anom_sd",
                                          "precip","pet"))
library(data.table)
setDTthreads(threads=10)
# system.time(tmp$id <- paste0(tmp$x,"_",tmp$y))
# vec_id <- unique(tmp$id)
# junk <- tmp %>% filter(id %in% sample(vec_id,size = 4))
tmp <- as.data.table(tmp)
tmp <- tmp[order(date), c(paste0("precip_",1:36)) := shift(precip, 1:36) , .(x,y)][order(date)]
gc(verbose = T, reset = T, full = T)

system.time(
  tmp <- tmp[order(date), c(paste0("pet_",1:36)) := shift(pet, 1:36) , .(x,y)][order(date)]
)
gc(verbose = T, reset = T, full = T)


d1 <- tmp[vc=='Eucalypt Woodlands']
d1$x[1:5]
lag_n <- 0:36 ## create wavelength matrix...
d2 <- d1[x >= 140 & x<141] # TEMPORARY BREAKING UP DF
# d2 <- d1[x %in% d1$x[1:10]]
tmp_mat <- t(matrix(lag_n,length(lag_n),length(d2$x)))
d2 <- as_tibble(d2)
d2$lag_n <- tmp_mat # lag index is needed for GAM
d2 <- d2 %>% rename(precip_0 = precip, 
              pet_0 = pet)
d2$lag_precip <- d2 %>% select(starts_with('precip_'))
d2$lag_pet <- d2 %>% select(starts_with('pet_'))
d2 <- d2 %>% select(x,y,vc,date,nirv_anom_sd, lag_n, lag_precip, lag_pet)
d2$lag_precip <- d2$lag_precip %>% as.matrix()
d2$lag_pet <- d2$lag_pet %>% as.matrix()

fit <- bam(nirv_anom_sd ~ 
             s(lag_n,by=lag_precip,bs="gp",k=5)+
             s(lag_n,by=lag_pet,bs="gp",k=5),data=d2, 
           select=T, method='fREML', discrete = T)
summary(fit)


plot(fit,rug=F,shade=T, select = 1, scale=0);abline(h=0,col='red',lty=3)
plot(fit,rug=F,shade=T, select = 2, scale=0);abline(h=0,col='red',lty=3)
plot(fit$model$nirv_anom_sd~fitted(fit)); abline(0,1,col='red')




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
             s(lag_n_pet,by=lag_pet,bs="gp"),data=v1, 
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

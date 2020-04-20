library(arrow)
library(tidyverse); library(lubridate);
library(data.table)
setDTthreads(threads=8)

#*******************************************************************************
# Get data  ---------------------------------------------------------
#*******************************************************************************
tmp <- arrow::read_parquet("/home/sami/srifai@gmail.com/work/research//data_general/Oz_misc_data/ARD_nirv_aclim_2020-04-18.parquet", 
)

# data.table 
tmp <- setDT(tmp) # OR: tmp <- as.data.table(tmp)
tmp <- tmp %>% rename(x=x_vi, y=y_vi)

# Subsetting to just one type of vegetation class
tmp_vc <- tmp[date==ymd("2000-01-01"),.(x,y,vc)]
vec_vc <- unique(tmp$vc) %>% sort
tmp_vc <- tmp_vc[vc=="Eucalypt Open Forests"]
tmp <- tmp[tmp_vc,on=.(x,y)] # subset to just the selected vegetation class
#*******************************************************************************
#* END SECTION
#*******************************************************************************

#*******************************************************************************
# Calculate Climatology --------------------------------------------------------
#*******************************************************************************
# calc norms 
tmp <- tmp[, `:=`(month = month(date))] # create month
tmp <- tmp[, `:=`(year = year(date))]   # create year
tmp <- tmp[,`:=`("pe" = precip/pet)]
norms_nirv <- tmp[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
                  .("nirv_u" = mean(nirv), 
                    "nirv_sd" = sd(nirv)),
                  by=.(x,y,month)] # joining on x,y,month
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

# join all the data frames ***
norms <- norms_p[norms_pet, on=.(x,y,month)] # join data.tables
norms <- norms[norms_pe, on=.(x,y,month)] # join data.tables
norms <- norms[norms_map, on=.(x,y)]
norms <- norms[norms_mapet, on=.(x,y)]
norms <- norms[norms_mape, on=.(x,y)]
norms <- norms[norms_nirv, on=.(x,y,month)]
tmp <- norms[tmp, on=.(x,y,month)]
#*******************************************************************************
#* END SECTION
#*******************************************************************************

#*******************************************************************************
# Calculate the anomalies ------
#*******************************************************************************
tmp <- tmp[, `:=`(nirv_anom = nirv - nirv_u, 
                  precip_anom = precip-precip_u,  # calc raw anomaly 
                  pet_anom = pet-pet_u, 
                  pe_anom = pe-pe_u)]
tmp <- tmp[, `:=`(nirv_anom_sd = nirv_anom/nirv_sd,
                  precip_anom_sd = precip_anom/precip_sd,  # calc sd anomaly 
                  pet_anom_sd = pet_anom/pet_sd, 
                  pe_anom_sd = pe_anom/pe_sd)]
#*******************************************************************************
#* END SECTION
#*******************************************************************************

#*******************************************************************************
# Calculate the multi-year anomalies ------
#*******************************************************************************
# calculate the rolling 12-month sums 
tmp <- tmp[order(x,y,date)][, nirv_12mo := frollmean(nirv,n = 12,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, precip_12mo := frollsum(precip,n = 12,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pet_12mo := frollsum(pet,n = 12,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pe_12mo := frollsum(pe,n = 12,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, nirv_anom_12mo := frollmean(nirv_anom,n = 12,fill = NA,align='right'), by=.(x,y)]

# rolling 2 year sums
tmp <- tmp[order(x,y,date)][, precip_24mo := frollsum(precip,n = 24,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pet_24mo := frollsum(pet,n = 24,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pe_24mo := frollsum(pe,n = 24,fill = NA,align='right'), by=.(x,y)]

# rolling 3 year sums
tmp <- tmp[order(x,y,date)][, precip_36mo := frollsum(precip,n = 36,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pet_36mo := frollsum(pet,n = 36,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pe_36mo := frollsum(pe,n = 36,fill = NA,align='right'), by=.(x,y)]

# rolling 4 year sums
tmp <- tmp[order(x,y,date)][, precip_48mo := frollsum(precip,n = 48,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pet_48mo := frollsum(pet,n = 48,fill = NA,align='right'), by=.(x,y)]
tmp <- tmp[order(x,y,date)][, pe_48mo := frollsum(pe,n = 48,fill = NA,align='right'), by=.(x,y)]

# calc anoms of rolling sums
tmp <- tmp[, `:=`(precip_anom_12mo = precip_12mo-map)]
tmp <- tmp[, `:=`(pet_anom_12mo = pet_12mo-mapet)]
tmp <- tmp[, `:=`(pe_anom_12mo = pe_12mo-mape)]

tmp <- tmp[, `:=`(precip_anom_24mo = precip_24mo-2*map)]
tmp <- tmp[, `:=`(pet_anom_24mo = pet_24mo-2*mapet)]
tmp <- tmp[, `:=`(pe_anom_24mo = pe_24mo-2*mape)]

tmp <- tmp[, `:=`(precip_anom_36mo = precip_36mo-3*map)]
tmp <- tmp[, `:=`(pet_anom_36mo = pet_36mo-3*mapet)]
tmp <- tmp[, `:=`(pe_anom_36mo = pe_36mo-3*mape)]

tmp <- tmp[, `:=`(precip_anom_48mo = precip_48mo-4*map)]
tmp <- tmp[, `:=`(pet_anom_48mo = pet_48mo-4*mapet)]
tmp <- tmp[, `:=`(pe_anom_48mo = pe_48mo-4*mape)]

tmp_my_anoms <- tmp[date >= ymd("1978-01-01")] %>% 
               select(starts_with("precip_anom_"),
               starts_with("pet_anom_"), 
               starts_with("pe_anom_"))
names(tmp_my_anoms) <- paste0(names(tmp_my_anoms),"_my")
#*******************************************************************************
#* END SECTION
#*******************************************************************************


#!!! The following is for for a form of lagged variable GAM where the lagged
# covariates are organized into matrices. 
# This can/will def blow up the memory... even 64 GB...  
#*******************************************************************************
# Cast the variables to lagged matrices -----------------------------------
#*******************************************************************************
# Because this is so memory intensive I'm subsetting in time
tmp <- tmp[date >= ymd("1978-01-01")]

# precip anom lags
gc(reset = TRUE,full=T)
mat_p <- tmp[,.(x,y,date,precip_anom)][order(x,y,date), c(paste0("precip_anom_",1:36)) := shift(precip_anom, 1:36) , .(x,y)][order(date)]
mat_p <- mat_p %>% rename(precip_anom_0 = precip_anom) %>% select(-x,-y,-date)
gc(verbose = T, reset = T, full = T)

# pet anom lags
gc(reset = TRUE,full=T)
mat_pet <- tmp[,.(x,y,date,pet_anom)][order(x,y,date), c(paste0("pet_anom_",1:36)) := shift(pet_anom, 1:36) , .(x,y)][order(date)]
mat_pet <- mat_pet %>% rename(pet_anom_0 = pet_anom) %>% select(-x,-y,-date)
gc(verbose = T, reset = T, full = T)

# P:PET anom lags
mat_pe <- tmp[,.(x,y,date,pe_anom)][order(x,y,date), c(paste0("pe_anom_",1:36)) := shift(pe_anom, 1:36) , .(x,y)][order(date)]
mat_pe <- mat_pe %>% rename(pe_anom_0 = pe_anom) %>% select(-x,-y,-date)
gc(verbose = T, reset = T, full = T)


lag_n <- 0:36 ## create time lag matrix...

tmp_mat <- t(matrix(lag_n,length(lag_n),length(tmp$x)))
tmp <- as_tibble(tmp)
tmp$lag_month <- tmp_mat # lag index is needed for GAM
# tmp <- tmp %>% rename(precip_0 = precip, 
#                     pet_0 = pet, 
#                     pe_0 = pe)
# tmp$lag_precip <- tmp %>% select(paste0('precip_',0:36))
# tmp$lag_pet <- tmp %>% select(paste0('pet_',0:36))
# tmp$lag_pe <- tmp %>% select(paste0('pe_',0:36))

# tmp <- tmp %>% mutate(precip_anom_0 = precip_anom, 
#                     pet_anom_0 = pet_anom, 
#                     pe_anom_0 = pe_anom)
# tmp$lag_precip_anom <- tmp %>% select(paste0('precip_anom_',0:36))
# tmp$lag_pet_anom <- tmp %>% select(paste0('pet_anom_',0:36))
# tmp$lag_pe_anom <- tmp %>% select(paste0('pe_anom_',0:36))

tmp$lag_precip_anom <- as.matrix(mat_p)
rm(mat_p); gc()
tmp$lag_pet_anom <- as.matrix(mat_pet)
rm(mat_pet); gc()
tmp$lag_pe_anom <- as.matrix(mat_pe)
rm(mat_pe); gc()

# tmp <- tmp %>% select(x,y,vc,date,nirv_anom_sd, lag_month, 
#                     lag_precip, lag_pet, 
#                     lag_precip_anom, lag_pet_anom)
# tmp$lag_precip <- tmp$lag_precip %>% as.matrix()
# tmp$lag_pet <- tmp$lag_pet %>% as.matrix()
# tmp$lag_pe <- tmp$lag_pe %>% as.matrix()

# tmp$lag_precip_anom <- tmp$lag_precip_anom %>% as.matrix(); gc(reset = T, full=T)
# tmp$lag_pet_anom <- tmp$lag_pet_anom %>% as.matrix(); gc(reset = T, full=T)
# tmp$lag_pe_anom <- tmp$lag_pe_anom %>% as.matrix(); gc(reset = T, full=T)
#*******************************************************************************
#* END SECTION
#*******************************************************************************




#*******************************************************************************
#  Rejoin with multi year anoms -----
#*******************************************************************************
tmp <- bind_cols(tmp, tmp_my_anoms %>% as_tibble())
#*******************************************************************************
#* END SECTION
#*******************************************************************************


#*******************************************************************************
#  -----
#*******************************************************************************
#*******************************************************************************
#* END SECTION
#*******************************************************************************


#*******************************************************************************
#  -----
#*******************************************************************************
#*******************************************************************************
#* END SECTION
#*******************************************************************************


#*******************************************************************************
#  -----
#*******************************************************************************
#*******************************************************************************
#* END SECTION
#*******************************************************************************


#*******************************************************************************
#  -----
#*******************************************************************************
#*******************************************************************************
#* END SECTION
#*******************************************************************************


#*******************************************************************************
#  -----
#*******************************************************************************
#*******************************************************************************
#* END SECTION
#*******************************************************************************


#*******************************************************************************
#  -----
#*******************************************************************************
#*******************************************************************************
#* END SECTION
#*******************************************************************************


#*******************************************************************************
#  -----
#*******************************************************************************
#*******************************************************************************
#* END SECTION
#*******************************************************************************

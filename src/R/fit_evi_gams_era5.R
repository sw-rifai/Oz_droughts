library(tidyverse); library(lubridate); library(arrow)

#***************************************************************************
# Import EVI2 and clim (ERA5-Land) -----------------------------------------
#***************************************************************************
avhrr <- read_parquet(file="../data_general/AVHRR_EVI2_CDR_V5/EOz_AVHRR_EVI2_LAI_1981_2019.parquet")
ex_pet <- read_parquet(file='../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_PET_1979_2019.parquet')
ex_vpd <- read_parquet(file="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_vpd_1979_2019.parquet")
ex_precip <- read_parquet(file='../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_precip_1979_2019.parquet')
ex <- inner_join(ex_pet %>% select(-days_in_month), 
                 ex_vpd, 
                 by=c("id","date"))
ex <- inner_join(ex, ex_precip %>% select(-days_in_month), 
                 by=c("id","date"))
d <- inner_join(ex, 
                avhrr, 
                by=c("id","date"))
d <- d %>% 
  mutate(quarter = quarter(date, fiscal_start = 11))
d <- d %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"OSN"))
d <- d %>% mutate(season = factor(q,
                                  levels=c("DJF","MAM","JJA","OSN"), 
                                  ordered=T))
d <- d %>% 
  mutate(hydro_year = year(date+months(1)))
ds <- d %>% 
  filter(hydro_year >= 1981 & hydro_year <= 2019) %>% 
  group_by(hydro_year, season,lon,lat) %>% 
  summarize(evi2 = mean(evi2, na.rm=T), 
            evi2_anom = mean(evi2_anom, na.rm=T), 
            evi2_anom_sd = mean(evi2_anom_sd, na.rm=T), 
            precip = sum(precip), 
            t2m = mean(t2m, na.rm = T), 
            vpd = mean(vpd,na.rm = T), 
            pet = mean(pet,na.rm=T)) %>% 
  ungroup()
ds_clim <- d %>% 
  filter(hydro_year >= 1982 & hydro_year <= 2010) %>% 
  group_by(season,lon,lat) %>% 
  summarize(evi2_u = mean(evi2, na.rm=T), 
            evi2_sd = sd(evi2, na.rm=T), 
            precip_u = mean(precip)*3, 
            precip_sd = sd(precip)*3, 
            t2m_u = mean(t2m, na.rm = T), 
            t2m_sd = sd(t2m, na.rm = T), 
            vpd_u = mean(vpd,na.rm = T),
            vpd_sd = sd(vpd,na.rm=T),
            pet_u = mean(pet,na.rm=T), 
            pet_sd = sd(pet,na.rm=T)) %>% 
  ungroup()

ds <- inner_join(ds, ds_clim, by=c("lon","lat","season"))
rm(d); 
gc();
#********************************************************************************

# seasonal anoms
ds <- ds %>% 
  mutate(precip_anom = precip-precip_u, 
         pet_anom = pet-pet_u) %>% 
  mutate(precip_anom_sd = precip_anom/precip_sd,
         pet_anom_sd = pet_anom/pet_sd)


# Split data --------------------------------------------------------------
set.seed(333)
tmp_train <- ds %>% 
  filter(hydro_year <= 2000) %>% 
  sample_n(1e5)
tmp_test <- ds %>% 
  filter(hydro_year <= 2000) %>% 
  sample_n(1e6)



# model1 seasonal evi2 anom sd --------------------------------------------
library(mgcv); library(mgcViz)
ms1 <- bam(evi2_anom_sd ~ 
             te(lon,lat,by=season)+
             te(precip_anom_sd, pet_anom_sd), 
           data=tmp_train,
           select=T, 
           discrete=T)
summary(ms1)
getViz(ms1) %>% plot
plotSlice(sm(getViz(ms1),1), 
          fix=list("season"=unique(tmp_train$season)))
  # l_fitRaster()+
  # l_fitContour()


library(tidyverse); library(lubridate); library(arrow); 
library(RcppRoll)

#***************************************************************************
# Import EVI2 and clim (ERA5-Land) -----------------------------------------
#***************************************************************************
avhrr <- read_parquet(file="../data_general/AVHRR_EVI2_CDR_V5/EOz_AVHRR_EVI2_LAI_1981_2019.parquet")
ex_pet <- read_parquet(file='../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_PET_1979_2019.parquet')
ex_vpd <- read_parquet(file="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_vpd_1979_2019.parquet")
ex_precip <- read_parquet(file='../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_precip_1979_2019.parquet')
ex_precip <- ex_precip %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(precip_12mo = roll_sumr(precip,n=12,fill=NA)) %>% 
  ungroup()
ex_precip <- ex_precip %>% 
  inner_join(., 
             {.} %>% 
               filter(is.na(precip_12mo)==F) %>% 
               filter(date>=ymd('1982-01-01') & 
                        date<=ymd('2011-12-31')) %>% 
               group_by(id) %>% 
               summarize(map = mean(precip_12mo)) %>% 
               ungroup(), 
             by=c("id")) %>% 
  mutate(precip_anom_12mo = precip_12mo - map)


ex <- inner_join(ex_pet %>% select(-days_in_month), 
                 ex_vpd, 
                 by=c("id","date"))
ex <- inner_join(ex, ex_precip %>% select(-days_in_month), 
                 by=c("id","date"))
d <- inner_join(ex, 
                avhrr, 
                by=c("id","date"))
d_dates <- d %>% 
  select(date) %>% distinct() %>% 
  mutate(quarter = quarter(date, fiscal_start = 11))
d_dates <- d_dates %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"OSN"))
d_dates <- d_dates %>% mutate(season = factor(q,
                                  levels=c("DJF","MAM","JJA","OSN"), 
                                  ordered=T))
d_dates <- d_dates %>% 
  mutate(hydro_year = year(date+months(1)))
d <- inner_join(d, d_dates, by=c("date"))
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
#**************************************************************************




# library(mgcv)
# tmp_train %>% 
#   # mutate(month=month(date)) %>% 
#   bam(evi2_anom_sd ~ season+hydro_year, data=.) %>% 
#   coef
# 
# 
# 


mtcars %>% 
  group_by(cyl) %>% 
  nest %>% 
  mutate(data=map(data, ~.x %>% 
                    summarize(model=list(broom::tidy(lm(mpg~wt+hp)))))) %>% 
  unnest() %>% 
  unnest()

# nesting approach
# ds %>% 
#   filter(near(lon,141.6868,tol=0.0025)) %>% 
#   group_by(lon,lat,season) %>% 
#   summarize(coef = list(coef(lm(evi2_anom_sd~precip_anom_sd+pet_anom_sd)))) %>% 
  # ungroup() %>% 
  # {set_names(.$season, .$coef)} %>% 
  # map_df(~ .x %>% 
  #          as.data.frame() %>% 
  #          rownames_to_column("term"), .id=c("season"))
  # unnest(#cols = c('coef'), 
  #        # names_set=c("b1","b2","b3"), 
  #        names_repair="unique")

tmp <- ds %>% 
  filter(near(lon,141.6868,tol=10)) %>% 
  filter(near(lat, -30, tol=10)) %>% 
  select(lon,lat,evi2_anom_sd,precip_anom_sd,pet_anom_sd) %>% 
  na.omit() %>% 
  group_by(lon,lat) %>% 
    nest %>% 
    mutate(data=map(data, ~.x %>% 
          summarize(model=
            list(broom::tidy(lm(evi2_anom_sd~scale(precip_anom_sd)+scale(pet_anom_sd))))))) %>% 
    unnest() %>% 
    unnest() %>% 
  ungroup()

blah_theme <- theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
      panel.grid = element_blank(), 
      axis.ticks = element_blank(), 
      axis.title = element_blank(), 
      axis.text = element_blank())

tmp %>% 
  # filter(term == "pet_anom_sd") %>%
  ggplot(data=., aes(lon,lat,fill=statistic))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(
    'scaled estimate',
    limits=c(-1,1))+
  facet_wrap(~term)+
  blah_theme
  
tmp %>% 
  # filter(term == "pet_anom_sd") %>%
  ggplot(data=., aes(lon,lat,fill=p.value))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(
    'scaled estimate',
    limits=c(0,0.05), direction = -1)+
  facet_wrap(~term)+
  blah_theme


# library(foreach)
# # Parallelized processing -------------------------------------------------
# library(foreach); library(doParallel)
# cl <- makeCluster(n_cores)
# registerDoParallel(cl)
# fit_out <- foreach(i = 1:length(vec_dates), 
#                       .packages = c("tidyverse","lubridate"),
#                       .combine=rbind) %dopar% {
#                         
# }

library(tidyverse); library(sf); library(stars); library(lubridate)
library(arrow);
library(disk.frame)
setup_disk.frame()
options(future.globals.maxSize=Inf)

mod <- stars::read_stars("../data_general/MOD13Q1/MOD13Q1_NDVI_1km_SEAUS_monmean_2000_2019-0000000000-0000000000.tif", 
                         along="time")
vec_dates <- seq(ymd("2002-01-01"), 
                 ymd("2019-12-01"), by="1 month")
vec_dates <- tibble(date=vec_dates, band=1:length(vec_dates))
# Group by Hydro Year and Season ------------------------------------------
vec_dates <- vec_dates %>% 
  mutate(quarter = quarter(date, fiscal_start = 11))
vec_dates <- vec_dates %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON"))
vec_dates <- vec_dates %>% mutate(season = factor(q,
                                                  levels=c("DJF","MAM","JJA","SON"), 
                                                  ordered=T))
vec_dates <- vec_dates %>% 
  mutate(hydro_year = year(date+months(1)))
ls()

dim(mod)
mod <- mod %>% 
  as.data.frame()
names(mod) <- c("lon","lat","band","ndvi")
mod <- mod %>% filter(is.na(ndvi)==F)
mod <- inner_join(mod, vec_dates, by='band')
mod %>% write_parquet(., 
                      sink="../data_general/MOD13Q1/MOD13Q1_NDVI_1km_SEAUS_monmean_2000_2019.parquet", 
                      compression="GZIP")
mod <- mod %>% select(-quarter, -q, -band)


mod <- as.disk.frame(read_parquet("../data_general/MOD13Q1/MOD13Q1_NDVI_1km_SEAUS_monmean_2000_2019.parquet"))

# Summarize By Hydro Year & Season---------------------------------------------------------------
norms <- mod %>% 
  group_by(lon,lat,season) %>% 
  summarize(ndvi_u = mean(ndvi, na.rm=T), 
            ndvi_sd = sd(ndvi, na.rm=T)) %>% 
  ungroup()

norms %>% write_parquet(., "../data_general/MOD13Q1/MOD13Q1_NDVI_norms_SEOz_2000-2019.parquet")

mod <- mod %>% 
  group_by(lon,lat,season,hydro_year) %>% 
  summarize(ndvi = mean(ndvi, na.rm=T)) %>% 
  ungroup() %>% 
  as_tibble()
mod %>% write_parquet(., "../data_general/MOD13Q1/MOD13Q1_NDVI_seasonal_SEOz_2000-2019.parquet")

mod <- inner_join(mod, norms, by=c("lon","lat","season"))
rm(norms); gc(); 

mod <- mod %>% 
  mutate(ndvi_anom = ndvi - ndvi_u) %>% 
  mutate(ndvi_anom_sd = ndvi_anom/ndvi_sd)

mod %>% write_parquet(., "../data_general/MOD13Q1/MOD13Q1_NDVI_Anom_seasonal_SEOz_2000-2019.parquet")


# norms %>% 
#   # filter(date==min(date)) %>% 
#   ggplot(data=., aes(lon,lat,fill=ndvi_u))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c()+
#   facet_wrap(~season)
mod %>% 
  filter(hydro_year %in% c(2019)) %>% 
  ggplot(data=., aes(lon,lat,fill=ndvi_anom))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2()+
  facet_grid(hydro_year~season)

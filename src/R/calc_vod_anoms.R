library(arrow); library(tidyverse); library(lubridate); 
library(sf); library(lubridate)
# ascending ~ 1:30am 
# descending ~ 1:30pm

# Import ------------------------------------------------------------------
fp <- list.files("../data_general/lpdr_test/LPDR_VOD/", full.names = T, pattern='parquet')


library(foreach); library(doParallel)
n_cores <- 10
cl <- makeCluster(n_cores)
registerDoParallel(cl)


# Filter out daily values -------------------------------------------------------
out <- foreach(i = 1:length(fp), 
               .packages = c("tidyverse","lubridate","arrow"),
               .combine=rbind) %dopar% {
                 o <- arrow::read_parquet(fp[i])
                 test <- o %>% 
                   filter(band==5) %>% 
                   rename(vod_day = val_desc, 
                          vod_night = val_asc) %>% 
                   select(x,y,date,vod_day)

                 test          
               }


# Group by Hydro Year and Season ------------------------------------------
out_dates <- out %>% 
  select(date) %>% distinct() %>% 
  mutate(quarter = quarter(date, fiscal_start = 11))
out_dates <- out_dates %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON"))
out_dates <- out_dates %>% mutate(season = factor(q,
                                      levels=c("DJF","MAM","JJA","SON"), 
                                      ordered=T))
out_dates <- out_dates %>% 
  mutate(hydro_year = year(date+months(1)))
ls()
out <- inner_join(out, out_dates, by='date')


# test plot ---------------------------------------------------------------

j <- out %>% 
  inner_join(., 
             {.} %>% 
               filter(hydro_year %in% c(2003:2018)) %>% 
               group_by(x,y,season) %>% 
               summarize(u = mean(vod_day,na.rm=T)) %>% 
               ungroup(), 
             by=c("x","y","season"))
j %>% 
  filter(hydro_year %in% c(2015:2019)) %>% 
  mutate(vod_day_anom = vod_day - u) %>% 
  ggplot(data=., aes(x,y,fill=vod_day_anom))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2()+
  facet_grid(hydro_year~season)+
  blah_theme

p <- j %>% 
  filter(x >=1.35e07) %>% 
  filter(y <= -3401228) %>% 
  filter(hydro_year %in% c(2015:2019)) %>% 
  mutate(vod_day_anom = vod_day - u) %>% 
  ggplot(data=., aes(x,y,fill=vod_day_anom))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2()+
  facet_grid(hydro_year~season)+
  blah_theme
ggsave(p, 
       filename = paste0('figures/map_vod_day_anom_SEOz_',Sys.Date(),".png"), 
       units='cm',width=15, height=15, dpi='retina')

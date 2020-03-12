library(tidyverse); library(lubridate); library(arrow)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE);
source("src/R/helper_funs_Oz_droughts.R")
list.files("../data_general/AVHRR_LAI_FAPAR_CDR_V5/")
avhrr <- read_parquet("../data_general/AVHRR_LAI_FAPAR_CDR_V5/Oz_AVHRR_LAI_CDR_1981_2019.parquet")
avhrr <- avhrr %>% 
  filter(lon>=140 & lat <= -28 & lon<=154 & lat>= -40 )
vec_dates <- avhrr %>% distinct(date) %>% 
  mutate(quarter = lubridate::quarter(date, fiscal_start = 11))
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
vec_dates <- vec_dates %>% select(date, season, hydro_year)
avhrr <- inner_join(avhrr, vec_dates, by='date')
avhrr <- as.data.table(avhrr)
gc()

# data.table stuff
dt_avhrr <- lazy_dt(avhrr)

dt_avhrr %>% 
  group_by(season) %>% 
  summarize(val = mean(lai)) %>% 
  ungroup() %>% 
  as_tibble()

tmp_norms <- dt_avhrr %>% 
  filter(hydro_year %in% 1982:2011) %>% 
  group_by(lon,lat,season) %>% 
  summarize(lai_u = mean(lai),
            lai_sd = sd(lai)) %>% 
  ungroup() %>% 
  as_tibble() # important to tell data.table to do the calculation
tmp <- dt_avhrr %>% 
  group_by(lon,lat,hydro_year,season) %>% 
  summarize(lai = mean(lai)) %>% 
  ungroup() %>% 
  as_tibble() # important to tell data.table to do the calculation

tmp <- inner_join(tmp,tmp_norms,by=c('lon','lat','season'))


vec_cols <- RColorBrewer::brewer.pal(n=7, "BrBG")
p <- tmp%>% 
  mutate(lai_anom = lai - lai_u) %>% 
  mutate(lai_anom_sd = lai_anom/lai_sd) %>% 
  filter(hydro_year %in% c(2002:2019)) %>% 
  ggplot(data=., aes(lon,lat,fill=lai_anom_sd))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(paste(LAI~Anom.~(sigma))),
    high=vec_cols[7], low=vec_cols[1], 
                       mid=vec_cols[4], 
                       limits=c(-3,3))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  facet_grid(season~hydro_year)+
  theme_linedraw()+
  blah_theme
ggsave(p, filename = paste0("figures/map_avhrr_LAI_anom_seasonal_SEOZ_",Sys.Date(),".png"), 
       width=42,height=12,units='cm',dpi='retina')

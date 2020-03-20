# Part 1 - Import the TIFF ------------------------------------------------
library(tidyverse); library(lubridate); library(arrow)
library(sf); library(stars)
mod <- stars::read_stars("../data_general/MOD13A2/MOD13A2_NDVI_1km_SEAUS_monmean_2000_2019-0000000000-0000000000.tif")
vec_dates <- seq(ymd("2000-02-01"),ymd("2019-12-01"),by="1 month")
vec_dates <- tibble(date=vec_dates, 
                    band=1:dim(mod)[3])
vec_dates <- vec_dates %>% distinct(date,band) %>% 
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
vec_dates <- vec_dates %>% select(date,season, hydro_year)
vec_dates$band <- 1:dim(vec_dates)[1]
mod <- mod %>% as_tibble()
names(mod) <- c("lon","lat","band","ndvi")
mod <- mod %>% filter(is.na(ndvi)==F); 
gc();
mod <- mod %>% filter(lon>=140 & lat <= -28 & lon<=154 & lat>= -40 )
gc(reset = T);
mod %>% dim
mod <- inner_join(mod, vec_dates, by='band')
gc()




# Part 2 - Process the Anomaly --------------------------------------------
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE);
source("src/R/helper_funs_Oz_droughts.R")

mod <- mod %>% 
  as.data.table() %>% 
  mutate(lon = 0.125*round(lon/0.125), 
         lat = 0.125*round(lat/0.125))
gc()

# data.table stuff
dt_mod <- lazy_dt(mod)

dt_mod %>% 
  group_by(season) %>% 
  summarize(val = mean(ndvi)) %>% 
  ungroup() %>% 
  as_tibble()

tmp_norms <- dt_mod %>% 
  filter(hydro_year %in% 2001:2018) %>% 
  group_by(lon,lat,season) %>% 
  summarize(ndvi_u = mean(ndvi),
            ndvi_sd = sd(ndvi)) %>% 
  ungroup() %>% 
  as_tibble() # important to tell data.table to do the calculation
tmp <- dt_mod %>% 
  group_by(lon,lat,hydro_year,season) %>% 
  summarize(ndvi = mean(ndvi)) %>% 
  ungroup() %>% 
  as_tibble() # important to tell data.table to do the calculation

tmp <- inner_join(tmp,tmp_norms,by=c('lon','lat','season'))
# rm(mod); rm(dt_mod); gc(); 







# Part 3 - Map the Anomaly ------------------------------------------------
vec_cols <- RColorBrewer::brewer.pal(n=7, "BrBG")
p <- tmp%>% 
  mutate(ndvi_anom = ndvi - ndvi_u) %>% 
  mutate(ndvi_anom_sd = ndvi_anom/ndvi_sd) %>% 
  filter(hydro_year %in% c(2000:2019)) %>% 
  ggplot(data=., aes(lon,lat,fill=ndvi_anom_sd))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(expression(paste(NDVI~Anom.~(sigma))),
                       high=vec_cols[7], low=vec_cols[1], 
                       mid=vec_cols[4], 
                       limits=c(-3,3))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(title="MOD13A2 - NDVI")+
  facet_grid(season~hydro_year)+
  theme_linedraw()+
  blah_theme+
  theme(legend.position = 'bottom', 
        legend.key.width = unit(2,'cm'))
ggsave(p, filename = paste0("figures/map_mod13a2_ndvi_anom_seasonal_SEOZ_",Sys.Date(),".png"), 
       width=42,height=12,units='cm',dpi='retina')

# Part 1 - Import the TIFF ------------------------------------------------
library(tidyverse); library(lubridate); library(arrow)
library(sf); library(stars)
avhrr <- stars::read_stars("../data_general/AVHRR_NDVI_CDR_V5/AVHRR_NDVI_monmean_SEAUS_1982_2019.tif")
vec_dates <- seq(ymd("1982-01-01"),ymd("2019-12-01"),by="1 month")
vec_dates <- tibble(date=vec_dates, 
                    band=1:dim(avhrr)[3])
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
avhrr <- avhrr %>% as_tibble()
names(avhrr) <- c("lon","lat","band","ndvi")
avhrr <- avhrr %>% filter(is.na(ndvi)==F); 
gc();
avhrr <- avhrr %>% filter(lon>=140 & lat <= -28 & lon<=154 & lat>= -40 )
gc(reset = T);
avhrr %>% dim
avhrr <- inner_join(avhrr, vec_dates, by='band')
gc()




# Part 2 - Process the Anomaly --------------------------------------------
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE);
source("src/R/helper_funs_Oz_droughts.R")

mod <- mod %>% 
  mutate(lon = 0.125*round(lon/0.125), 
         lat = 0.125*round(lat/0.125))

mod <- as.data.table(mod)
gc()

# data.table stuff
dt_mod <- lazy_dt(mod)

dt_mod %>% 
  group_by(season) %>% 
  summarize(val = mean(ndvi)) %>% 
  ungroup() %>% 
  as_tibble()

tmp_norms <- dt_mod %>% 
  filter(hydro_year %in% 2002:2018) %>% 
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

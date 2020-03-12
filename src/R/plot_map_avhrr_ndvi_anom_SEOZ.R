library(tidyverse); library(lubridate);
# CHANGE THIS STUFF -------------------------------------------------------
product_name_for_fig <- "AVHRR CDR v5 - NDVI"
region <- "SEAUS"
ref_years <- 1982:2010
deg_coarse <- 0.125
color_theme <- "BrBG"

# Part 1 - Import the TIFF ------------------------------------------------
library(arrow)
library(sf); library(stars)
dat <- stars::read_stars("../data_general/AVHRR_NDVI_CDR_V5/AVHRR_NDVI_monmean_SEAUS_1982_2019.tif")
vec_dates <- seq(ymd("1982-01-01"),ymd("2019-12-01"),by="1 month")
vec_dates <- tibble(date=vec_dates, 
                    band=1:dim(dat)[3])
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
dat <- dat %>% as_tibble()
names(dat) <- c("lon","lat","band","ndvi")
dat <- dat %>% filter(is.na(ndvi)==F); 
gc();
dat <- dat %>% filter(lon>=140 & lat <= -28 & lon<=154 & lat>= -40 )
gc(reset = T);
dat %>% dim
dat <- inner_join(dat, vec_dates, by='band')
gc()




# Part 2 - Process the Anomaly --------------------------------------------
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE);
source("src/R/helper_funs_Oz_droughts.R")

dat <- as.data.table(dat)
gc()

# data.table stuff
dt_dat <- lazy_dt(dat)

dat <- dat %>% 
  mutate(lon = deg_coarse*round(lon/deg_coarse), 
         lat = deg_coarse*round(lat/deg_coarse))


dt_dat %>% 
  group_by(season) %>% 
  summarize(val = mean(ndvi)) %>% 
  ungroup() %>% 
  as_tibble()

tmp_norms <- dt_dat %>% 
  filter(hydro_year %in% ref_years) %>% 
  group_by(lon,lat,season) %>% 
  summarize(ndvi_u = mean(ndvi),
            ndvi_sd = sd(ndvi)) %>% 
  ungroup() %>% 
  as_tibble() # important to tell data.table to do the calculation
tmp <- dt_dat %>% 
  group_by(lon,lat,hydro_year,season) %>% 
  summarize(ndvi = mean(ndvi)) %>% 
  ungroup() %>% 
  as_tibble() # important to tell data.table to do the calculation

tmp <- inner_join(tmp,tmp_norms,by=c('lon','lat','season'))
# rm(dat); rm(dt_dat); gc(); 







# Part 3 - Map the Anomaly ------------------------------------------------
vec_cols <- RColorBrewer::brewer.pal(n=7, name = color_theme)
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
  labs(title=product_name_for_fig)+
  facet_grid(season~hydro_year)+
  theme_linedraw()+
  blah_theme+
  theme(legend.position = 'bottom', 
        legend.key.width = unit(2,'cm'))
fig_filename <- paste0(
  "figures/",
  "map_", 
  gsub(product_name_for_fig,pattern=' ',replacement = "_"), 
  "_anom_seasonal_",region,"_",
  Sys.Date(),".png"
 )

ggsave(p, filename = fig_filename, 
       width=42,height=12,units='cm',dpi='retina')

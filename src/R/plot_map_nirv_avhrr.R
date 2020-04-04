library(raster); library(rasterVis);     #  LOAD THESE FIRST
library(sf); library(stars)
library(tidyverse); library(lubridate)   #  THEN LOAD TIDYWHATEVER STUFF
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE);

o <- stars::read_stars("../data_general/AVHRR_NDVI_CDR_V5/AVHRR_CDRv5_NIRV_seasonalMedian_Australia_1982_2019.tif")
o

# Set the time dimension
vec_dates <- seq(ymd("1982-01-01"), ymd("2019-11-01"), by="3 months")
# raster::setZ(o, vec_dates, name='time')
stars::st_get_dimension_values(o, 'band')
o <- st_set_dimensions(o, 3, values = vec_dates, names = "date")


o <- o %>% 
  as.data.table() %>% 
  set_names(c("lon","lat","date","nirv")) %>% 
  lazy_dt(.) %>% 
  filter(is.na(nirv)==F)

o_norms <- o %>% 
  filter(date >= ymd("1982-01-1") & 
           date <= ymd("2011-12-31")) %>% 
  mutate(month=month(date)) %>% 
  group_by(lon,lat,month) %>% 
  summarize(nirv_u = mean(nirv, na.rm=TRUE), 
            nirv_sd = sd(nirv, na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble()

o <- inner_join(o %>% 
                  mutate(month=month(date)), 
                o_norms, by=c("lon","lat","month"))
o <- o %>% 
  mutate(nirv_anom = nirv - nirv_u)
o <- o %>% mutate(nirv_anom_sd = nirv_anom/nirv_sd)

ot <- o %>% as_tibble()

# Plot Seasonal Means -----------------------------------------------------
p <- o_norms %>% 
  # filter(month == 1) %>% 
  select(lon,lat,month, nirv_u) %>% 
  pivot_wider(#id_cols = c("lon","lat"), 
              names_from=month,
              values_from = nirv_u) %>% 
  rasterFromXYZ(.) %>% 
  `names<-`(c("DJF","MAM","JJA","SON")) %>% 
    levelplot(., margin=F, 
              ylim=c(-44,-10), 
              xlim=c(112.5,154.5), 
              main='NIR-V Seasonal Means (AVHRR CDR v5 1982-2011)',
              par.settings=infernoTheme(),
              at=seq(0,0.25,length.out = 15))
p
png(filename = 'figures/map_avhrr_nirv_seasonal_means_1982_2011.png', 
    width = 20, height = 16, units='cm',res=350); p; dev.off()



# Plot seasonal anomalies -------------------------------------------------
# convert to raster stack
r <- ot %>% 
  mutate(year=year(date)) %>% 
  mutate(q = case_when(month==1~"DJF",
                       month==4~"MAM",
                       month==7~"JJA",
                       month==10~"SON")) %>% 
  mutate(season = factor(q,levels=c("DJF","MAM","JJA","SON"), 
                         ordered=T)) %>% 
  select(lon,lat,year,season,nirv_anom_sd) %>% 
  na.omit() %>% 
  arrange(year, season) %>% 
  pivot_wider(#id_cols = c("lon","lat"), 
    names_from=c(year,season),
    values_from = nirv_anom_sd) %>% 
  rasterFromXYZ(.)

# Divide into four time epochs
names(r)
vec_rn <- paste(rep(c("DJF","MAM", "JJA","SON"), 2019-1982+1),
          sort(rep(1982:2019,4))
      )
vec_rn <- vec_rn[-length(vec_rn)]
names(r) <- vec_rn
map_theme <- rasterTheme(region=RColorBrewer::brewer.pal(11,"BrBG"), 
                        fill='grey')
map_theme$panel.background$col = 'gray' 

# 1980s ------------------------------------------------------
p_80s <- levelplot(r,
                   layers=c(which(str_detect(string=vec_rn, pattern=c("(?=.*DJF*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.198)"))), 
                            which(str_detect(string=vec_rn, pattern=c("(?=.*MAM*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.198)"))),
                            which(str_detect(string=vec_rn, pattern=c("(?=.*JJA*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.198)"))), 
                            which(str_detect(string=vec_rn, pattern=c("(?=.*SON*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.198)")))),
                   as.table=T,
                   margin=F, 
                   ylim=c(-44,-10), 
                   xlim=c(112.5,154.5), 
                   # main='NIR-V Anomalies (AVHRR CDR v5)',
                   par.settings=map_theme,
                   at=seq(-3,3,length.out = 15), 
                   layout = c(10, 4) # columns, rows
)
png(filename = 'figures/map_avhrr_nirv_seasonal_anoms_1980s.png', 
    width = 40, height = 15, units='cm',res=350); p_80s; dev.off()

# 1990s ------------------------------------------------------
p_90s <- levelplot(r,
          layers=c(which(str_detect(string=vec_rn, pattern=c("(?=.*DJF*)"))&
                           str_detect(string=vec_rn, pattern=c("(?=.199)"))), 
                   which(str_detect(string=vec_rn, pattern=c("(?=.*MAM*)"))&
                           str_detect(string=vec_rn, pattern=c("(?=.199)"))),
                   which(str_detect(string=vec_rn, pattern=c("(?=.*JJA*)"))&
                           str_detect(string=vec_rn, pattern=c("(?=.199)"))), 
                   which(str_detect(string=vec_rn, pattern=c("(?=.*SON*)"))&
                           str_detect(string=vec_rn, pattern=c("(?=.199)")))),
                   as.table=T,
                   margin=F, 
                   ylim=c(-44,-10), 
                   xlim=c(112.5,154.5), 
                   # main='NIR-V Anomalies (AVHRR CDR v5)',
                   par.settings=map_theme,
                   at=seq(-3,3,length.out = 15), 
                   layout = c(10, 4) # columns, rows
)
png(filename = 'figures/map_avhrr_nirv_seasonal_anoms_1990s.png', 
    width = 40, height = 15, units='cm',res=350); p_90s; dev.off()

# 2000s ------------------------------------------------------
p_00s <- levelplot(r,
                   layers=c(which(str_detect(string=vec_rn, pattern=c("(?=.*DJF*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.200)"))), 
                            which(str_detect(string=vec_rn, pattern=c("(?=.*MAM*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.200)"))),
                            which(str_detect(string=vec_rn, pattern=c("(?=.*JJA*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.200)"))), 
                            which(str_detect(string=vec_rn, pattern=c("(?=.*SON*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.200)")))),
                   as.table=T,
                   margin=F, 
                   ylim=c(-44,-10), 
                   xlim=c(112.5,154.5), 
                   # main='NIR-V Anomalies (AVHRR CDR v5)',
                   par.settings=map_theme,
                   at=seq(-3,3,length.out = 15), 
                   layout = c(10, 4) # columns, rows
)
png(filename = 'figures/map_avhrr_nirv_seasonal_anoms_2000s.png', 
    width = 40, height = 15, units='cm',res=350); p_00s; dev.off()

# 2010s ------------------------------------------------------
p_10s <- levelplot(r,
                   layers=c(which(str_detect(string=vec_rn, pattern=c("(?=.*DJF*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.201)"))), 
                            which(str_detect(string=vec_rn, pattern=c("(?=.*MAM*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.201)"))),
                            which(str_detect(string=vec_rn, pattern=c("(?=.*JJA*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.201)"))), 
                            which(str_detect(string=vec_rn, pattern=c("(?=.*SON*)"))&
                                    str_detect(string=vec_rn, pattern=c("(?=.201)")))),
                   as.table=T,
                   margin=F, 
                   ylim=c(-44,-10), 
                   xlim=c(112.5,154.5), 
                   # main='NIR-V Anomalies (AVHRR CDR v5)',
                   par.settings=map_theme,
                   at=seq(-3,3,length.out = 15), 
                   layout = c(10, 4) # columns, rows
)
png(filename = 'figures/map_avhrr_nirv_seasonal_anoms_2010s.png', 
    width = 40, height = 15, units='cm',res=350); p_10s; dev.off()

# All years --------------------------------------------------------------------------
# All years - does not look great
p <- levelplot(r, margin=F, 
               ylim=c(-44,-10), 
               xlim=c(112.5,154.5), 
               main='NIR-V Anomalies (AVHRR CDR v5)',
               par.settings=RdBuTheme(fill='grey'),
               at=seq(-3,3,length.out = 15), 
               as.table=T, 
               layout = c(12, 13) # columns, rows
)
p
png(filename = 'figures/map_avhrr_nirv_seasonal_anoms.png', 
    width = 40, height = 30, units='cm',res=350); p; dev.off()


vec_names <- str_replace(str_replace(names(r), pattern = "X", replacement = ""), 
            pattern='_', replacement = " ")
names(r) <- vec_names
  # `names<-`(c("DJF","MAM","JJA","SON")) %>% 
r[[1:12]]
paste(sort(rep(1982:2019,4)), 
      rep(c("DJF","MAM", "JJA","SON"), 2019-1982+1))



myTheme <- BTCTheme()
myTheme$panel.background$col = 'gray'



  
o_norms %>% 
  filter(month == 1) %>% 
  select(lon,lat,nirv_u) %>% 
  rasterFromXYZ(.) %>% 
  levelplot(., margin=F, 
            ylim=c(-44,-10), 
            xlim=c(112.5,154.5))





# SCRATCH -----------------------------------------------------------------


levelplot(as(o[,,,1], "Raster"), par.settings = GrTheme, margin=F)

o[,,,1]
rasterVis::levelplot(as(o[,,,1], "Raster"), margin=F, 
                     ylim=c(-45,-10), xlim=c(110,155))


rasterVis::levelplot(o[[1]], margin=F, 
                     ylim=c(-45,-10), xlim=c(110,155))

o[[1:2]] %>% 
  as.data.frame(xy=T) %>% 
  filter(is.na(NIRV)==F) %>% 
  dim


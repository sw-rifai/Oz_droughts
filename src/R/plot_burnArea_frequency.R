library(tidyverse); library(stars); library(sf); library(data.table); 
library(lubridate); 

kop <- arrow::read_parquet("../data_general/Koppen_climate/BOM_Koppen_simplified7.parquet")
pa <- stars::read_stars("../data_general/MCD64/MCD64_Oz/MCD64A1_AVHRRres_pixel_area_.tif", 
                        proxy = F) %>% 
  as_tibble() %>% 
  set_names(c("x","y","area_m2"))
ba <- stars::read_stars("../data_general/MCD64/MCD64_Oz/MCD64A1_BurnArea_reducedToAVHRRres_2001_2019_.tif", 
                        proxy = F) %>% 
  stars::st_set_dimensions(., 3, values=seq(ymd("2001-01-01"),ymd("2019-12-01"),by='1 month')) %>% 
  as_tibble() %>% 
  purrr::set_names(c('x','y','date','ba'))
ba <- ba %>% inner_join(., kop, by=c("x","y"))

ba %>% mutate(hydro_year=year(date+months(1))) %>% 
  group_by(hydro_year,zone) %>% 
  summarize(ba_km2 = sum(ba,na.rm=TRUE)/(1000**2)) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(hydro_year, ba_km2))+
  # geom_line()+
  geom_smooth(method='lm',color='black')+
  scale_x_continuous(expand=c(0,0))+
  labs(y=expression(paste('Total Burn Area ',(km**2))))+
  facet_wrap(~zone,scales='free')+
  theme_linedraw()

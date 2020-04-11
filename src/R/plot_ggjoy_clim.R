library(arrow); library(tidyverse); library(lubridate); library(ggridges)
dat <- read_parquet(file = "../data_general/MYD11A2_C6_LST/MYD11A2_C6_maxMonLST_1km_EastOz_NVIStreeClassMask_2003_2019.parquet") %>% 
  rename(date=time)

dat %>% 
  sample_n(10000) %>% 
  mutate(year=year(date), 
         ddate = decimal_date(date)) %>%   
  ggplot(data=., aes(x=lst, y=as.factor(year),color=year))+
  geom_density_ridges(fill=NA)+
  labs(x=expression(paste(Land~Surface~Temp.~degree*C)), 
       y=NULL)+
  scale_color_viridis_c(option='B', begin = 0.2)+
  theme_dark()+
  theme(panel.background = element_rect(fill='black'), 
        plot.background = element_rect(fill='black'), 
        legend.position = 'none', 
        panel.grid = element_blank(), 
        axis.title.x = element_text(color='grey30',face = 'bold'))
  

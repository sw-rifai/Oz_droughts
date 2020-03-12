library(tidyverse); library(lubridate); 

blah_theme <- theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank())

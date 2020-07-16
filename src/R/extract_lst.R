library(tidyverse); library(data.table); 
library(stars); library(sf); library(lubridate)
library(viridisLite)

lst <- stars::read_stars("../data_general/MYD11A1_C6_LST/MYD11A1_C6_LST_5km_Australia_2002_2019.tif")
class(lst)


# THIS IS A SUPER MEMORY HEAVY OPERATION -------------------------
library(tidyverse); library(arrow); library(lubridate);
avhrr <- read_parquet("../data_general/AVHRR_EVI2_CDR_V5/Oz_AVHRR_EVI2_CDR_1981_2019.parquet")
clim <- read_parquet("../data_general/clim_grid/awap/parquet/awap_clim_2020-02-24.parquet")
d <- inner_join(avhrr, 
                clim %>% 
                  mutate(lon=longitude, 
                         lat=latitude), 
                by=c('date','lon','lat'))
rm(avhrr); rm(clim); gc(); 

lai <- read_parquet("../data_general/AVHRR_LAI_FAPAR_CDR_V5/Oz_AVHRR_LAI_CDR_1981_2019.parquet")
d <- inner_join(d, 
                lai, 
                by=c('date','lon','lat'))
rm(lai); gc(); 
gc(reset = T)

d %>% 
  write_arrow(., sink=paste0("../data_general/Oz_misc_data/ahvrr_clim_",Sys.Date(),".parquet"))

d <- d %>% filter(c(lat> -15 & lon > 145)==F)
# 
# d_train <- d %>% 
#   sample_n(50000)
# d_test <- d %>%
#   sample_n(100000) %>% 
#   anti_join(., d_train, by=c("lon","lat","date")) %>% 
#   sample_n(50000)
# 
# # Base mod on constants ---------------------------------------------------
# m_base <- bam(evi2 ~ s(lon,lat)+s(map)+s(matmax), 
#               data=d_train, 
#               method='fREML', 
#               discrete=T, 
#               select=T ,
#               family=gaussian(link='identity'))
# 
# 
# d %>% 
#   filter(c(lat> -15 & lon > 145)==F) %>%
#   sample_n(10000) %>% 
#   ggplot(data=., aes(lon,lat,color=evap))+
#   geom_point(size=0.1)+
#   coord_equal()+
#   scale_color_viridis_c()

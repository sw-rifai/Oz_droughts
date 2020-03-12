# THIS IS A SUPER MEMORY HEAVY OPERATION -------------------------
library(tidyverse); library(arrow); library(lubridate);

# These are the eastern Australia coords for the drought 
# model residuals analysis
coords <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv")
coords <- coords %>% filter(longitude>=140) %>% 
  rename(lon=longitude, lat=latitude) %>% 
  rename(norm_lai_avg = lai_avg, 
         norm_lai_min = lai_min, 
         norm_lai_max = lai_max, 
         norm_lai_amp = lai_amp)

# subsetting avhrr evi to the coords. It's massive as it is. 
avhrr <- read_parquet("../data_general/AVHRR_EVI2_CDR_V5/Oz_AVHRR_EVI2_CDR_1981_2019.parquet")
avhrr <- avhrr %>% filter(c(lat>= -15 & lon >= 140)==F); gc()
avhrr <- inner_join(coords, avhrr, by=c("lon","lat"))

# subsetting lai
lai <- read_parquet("../data_general/AVHRR_LAI_FAPAR_CDR_V5/Oz_AVHRR_LAI_CDR_1981_2019.parquet")
lai <- lai %>% filter(c(lat>= -15 & lon >= 140)==F); gc()
avhrr <- inner_join(avhrr, 
                    lai %>% select(-band), by=c("lon","lat","date"))
rm(lai); gc()


norms_evi <- avhrr %>% 
  filter(date>= ymd('1982-01-01') & date<= ymd("2010-12-31")) %>% 
  mutate(month=month(date)) %>% 
  group_by(lon,lat,month) %>% 
  summarize(evi2_u = mean(evi2, na.rm=T), 
            evi2_sd = sd(evi2, na.rm=T)) %>% 
  ungroup()
avhrr <- inner_join(avhrr %>% mutate(month=month(date)), 
                    norms_evi, by=c("lon","lat","month"))
avhrr <- avhrr %>% 
  mutate(evi2_anom = evi2-evi2_u) %>% 
  mutate(evi2_anom_sd = evi2_anom/evi2_sd)

avhrr %>% arrow::write_parquet(., 
                               sink="../data_general/AVHRR_EVI2_CDR_V5/EOz_AVHRR_EVI2_LAI_1981_2019.parquet", 
                               compression = 'snappy')

# End main section --------------------------------------------------------
#
#
#
# Scratch -----------------------------------------------------------------


# clim <- read_parquet("../data_general/clim_grid/awap/parquet/awap_wDroughtMets_2020-02-26.parquet") %>% 
#   select(-evi2, -lai)
# clim <- clim %>% filter(c(lat>= -15 & lon >= 145)==F); gc()
# 
# d <- inner_join(avhrr, 
#                 clim %>% 
#                   mutate(lon=longitude, 
#                          lat=latitude), 
#                 by=c('date','lon','lat'))
# rm(avhrr); rm(clim); gc(); 
# 
# lai <- read_parquet("../data_general/AVHRR_LAI_FAPAR_CDR_V5/Oz_AVHRR_LAI_CDR_1981_2019.parquet")
# lai <- lai %>% filter(c(lat>= -15 & lon >= 145)==F); gc()
# 
# d <- inner_join(d, 
#                 lai, 
#                 by=c('date','lon','lat'))
# rm(lai); gc(); 
# gc(reset = T)
# 
# d %>% 
#   write_arrow(., sink=paste0("../data_general/Oz_misc_data/ahvrr_clim_eastOz_",Sys.Date(),".parquet"))
# 
# # 
# # d_train <- d %>% 
# #   sample_n(50000)
# # d_test <- d %>%
# #   sample_n(100000) %>% 
# #   anti_join(., d_train, by=c("lon","lat","date")) %>% 
# #   sample_n(50000)
# # 
# # # Base mod on constants ---------------------------------------------------
# # m_base <- bam(evi2 ~ s(lon,lat)+s(map)+s(matmax), 
# #               data=d_train, 
# #               method='fREML', 
# #               discrete=T, 
# #               select=T ,
# #               family=gaussian(link='identity'))
# # 
# # 
# # d %>% 
# #   filter(c(lat> -15 & lon > 145)==F) %>%
# #   sample_n(10000) %>% 
# #   ggplot(data=., aes(lon,lat,color=evap))+
# #   geom_point(size=0.1)+
# #   coord_equal()+
# #   scale_color_viridis_c()

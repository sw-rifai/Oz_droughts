d1 <- read_parquet("../data_general/AVHRR_EVI2_CDR_V5/Oz_AVHRR_EVI2_CDR_1981_2019.parquet"); 
d2 <- read_parquet("../data_general/AVHRR_LAI_FAPAR_CDR_V5/Oz_AVHRR_LAI_CDR_1981_2019.parquet")

d1 %>% 
  select(lon,lat) %>% 
  distinct()

unique(d1$lon) %in% unique(d2$lon)
unique(d1$lat) %in% unique(d2$lat)

d1 %>% dim

d1 %>%
  sample_n(100000) %>% 
  mutate(month=month(date)) %>% 
  group_by(lon,lat,month) %>% 
  summarize(val = mean(evi2,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(lon,lat,color=val))+
  geom_point(size=0.1)+
  scale_color_viridis_c()+
  coord_equal()+
  facet_wrap(~month)

d2 %>%
  sample_n(100000) %>% 
  mutate(month=month(date)) %>% 
  group_by(lon,lat,month) %>% 
  summarize(val = mean(lai,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(lon,lat,color=val))+
  geom_point(size=0.1)+
  scale_color_viridis_c()+
  coord_equal()+
  facet_wrap(~month)


# rsync -r z3530735@monsoon.ccrc.unsw.edu.au:/srv/ccrc/data41/z3530735/ERA5-Land/Oz Oz


arrow::write_arrow(data.frame(x=1:1e6), sink="delete_me.parquet")
file.remove("delete_me.parquet")

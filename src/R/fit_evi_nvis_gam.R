library(tidyverse); library(lubridate); library(arrow); 
library(sf); library(mgcv); library(mgcViz)
nvis <- stars::read_stars("../data_general/NVIS/nvis51_majorVegClass_0p05.tif") %>% 
  as_tibble() %>% 
  set_names(., c("lon","lat","veg_class")) %>% 
  st_as_sf(., coords = c("lon","lat"))
# dat <- read_parquet("../data_general/AVHRR_EVI2_CDR_V5/EOz_AVHRR_EVI2_LAI_1981_2019.parquet")
dat <- arrow::read_arrow("../data_general/Oz_misc_data/ahvrr_clim_eastOz_2020-02-26.parquet") %>% 
  rename(evi2 = evi2.x, lai=lai.x)
codes <- readr::read_fwf("../data_general/NVIS/nvis51_majorVegClass_codes.txt", 
                         fwf_widths(c(2,100)), skip = 1) %>% 
  set_names(c("veg_class","veg_class_descrip")) %>% 
  mutate(vc = as.factor(veg_class_descrip))


coords_vi <- dat %>% select(lon,lat) %>% distinct()

coords_vi <- st_as_sf(coords_vi, coords = c("lon","lat"))
st_crs(coords_vi) <- st_crs(4326)
st_crs(nvis) <- st_crs(4326)

nn_coords <- RANN::nn2(
  nvis %>% st_coordinates(),
  coords_vi %>% st_coordinates()
) 
nn_coords$nn.idx[,1] %>% hist

nn_coords %>% str
nn_coords$nn.idx[1,]
nn_coords$nn.idx %>% dim
dim(coords_vi)


coords_j <- inner_join(
  {coords_vi %>% 
  mutate(idx_nvis = nn_coords$nn.idx[,1]) %>% 
  mutate(lon=st_coordinates(.)[,1], 
         lat=st_coordinates(.)[,2]) %>% 
  as_tibble() %>% 
  select(-geometry)}, 
  {nvis %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  mutate(idx_nvis = as.integer(rowname)) %>% 
  select(-geometry)}, 
  by='idx_nvis')

dat <- inner_join(dat, 
                  coords_j %>% select(lon,lat,veg_class), 
                  by=c("lon","lat"))

dat <- dat %>%  
  inner_join(., codes, by='veg_class')

dat <- dat %>% 
  mutate(month=month(date), 
         ddate = decimal_date(date))

dat <- dat %>% filter(date >= ymd('1981-09-01') & 
                      date <= ymd('2019-09-01')) # SS to before fire
dat <- dat %>% filter(veg_class <= 13) # SS to only veg classes w/Euc or rainforest trees


nobs_mod <- 5e5
d_train <- dat %>% sample_n(nobs_mod)
d_test <- dat %>% sample_n(nobs_mod)


m0 <- bam(evi2 ~ 
            vc+
            ddate+
            s(month,vc, bs='fs')+
            s(ddate,by=vc), 
          data=d_train, 
          select=T, discrete=T)
m0 %>% summary
m0 %>% plot
getViz(m0) %>% plot(., allTerms=T)


m1 <- bam(evi2 ~ 
            vc+
            ddate+ # proxy for CO2
            s(vpd3pm_anom_sd, vc, k=5, bs='fs')+  # time varying 
            s(precip_anom_sd, vc, k=5, bs='fs')+  # time varying 
            s(month,vc, bs='fs'),
          data=d_train, 
          select=T, discrete=T)
summary(m1, re.test = T)
m1 %>% plot
getViz(m1) %>% plot(., allTerms=T)

viz1 <- getViz(m1)
plot(sm(viz1, 1)) + 
  # l_fitLine()+
  # l_points()+
  scale_x_continuous(limits=c(-5,5))





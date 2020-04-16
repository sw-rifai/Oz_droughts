"
Description: Extract the met variables for the AWAP datasets using the NVIS major vegetation 
classes that contain trees.
Start date: 2020-04-10
"
library(tidyverse); library(lubridate); library(stars)

#*******************************************************************************
# NVIS and NIRV (forest & woodlands) ------------------------------------------------
#*******************************************************************************
dat <- stars::read_stars("../data_general/AVHRR_CDRv5_VI/AVHRR_NIRV_monmean_EastOz_1982_2019.tif",
                         RasterIO = list(bands=1))
names(dat) <- "nirv"
nvis <- stars::read_stars("../data_general/NVIS/nvis51_majorVegClass_0p05.tif")
nvis2 <- st_warp(src=nvis, dest=dat[,,], use_gdal = T)
names(nvis2) <- "veg_class"
nvis <- nvis2 %>% as_tibble()
codes <- readr::read_fwf("../data_general/NVIS/nvis51_majorVegClass_codes.txt", 
                         fwf_widths(c(2,100)), skip = 1) %>% 
  set_names(c("veg_class","veg_class_descrip")) %>% 
  mutate(vc = as.factor(veg_class_descrip))
nvis <- inner_join(nvis, codes, by='veg_class')
nvis <- nvis %>% filter(veg_class <= 15) # !!! only forests and woodlands !!!
nvis <- nvis %>% select(-veg_class_descrip)
dat <- dat %>% as_tibble()
dat <- inner_join(nvis, dat, by=c('x','y'))
#*******************************************************************************
#*** END SECTION
#*******************************************************************************

########################################################################
# extract AWAP precip --------------------------------------------------
########################################################################
# part 1 1900/01-2019/03
coords <- dat %>% select(x,y) %>% 
  distinct() %>% 
  st_as_sf(coords = c("x","y"), crs=4326)
fp <- "../data_general/clim_grid/awap/AWAP/Monthly_data_1911_2019/Monthly_total_precipitation_AWAP_masked_1900_2019.nc"
vec_dates <- stars::read_ncdf(fp, var="time_bnds")
vec_dates <- vec_dates %>% as_tibble() %>% select(time) %>% distinct()
vec_dates <- vec_dates %>% mutate(date = ymd(paste(year(time), month(time),1)))
vec_dates <- vec_dates %>% select(-time) %>% rename(time=date)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$x <- st_coordinates(coords)[,"X"]
ex$y <- st_coordinates(coords)[,"Y"]
ex <- ex %>% 
  pivot_longer(c(-x,-y), 
               names_to="date", values_to="precip")
ex <- ex %>% 
  mutate(date=ymd(date))

fp <- "../data_general/clim_grid/awap/AWAP/Monthly_data_1911_2019/precip_total_monthly_0.05_2019.nc"
junk <- velox::velox(raster::stack(fp))
ex2 <- junk$extract_points(sp = coords)
ex2 <- as_tibble(ex2)
names(ex2) <- seq(ymd("2019-01-01"),ymd("2019-12-01"),length.out=12)
ex2$x <- st_coordinates(coords)[,"X"]
ex2$y <- st_coordinates(coords)[,"Y"]
ex2 <- ex2 %>% 
  pivot_longer(c(-x,-y), 
               names_to="date", values_to="precip")
ex2 <- ex2 %>% 
  mutate(date=ymd(date))

test <- inner_join(ex %>% filter(date>=ymd("2019-01-01")), 
                   ex2 %>% filter(date<=ymd("2019-04-01")), 
                   by=c("x","y","date"), suffix=c("_old","_new"))
test %>% remove_missing() %>% 
  select(precip_old, precip_new,x) %>% 
  mutate(x=round(x,1)) %>% 
  ggplot(data=., aes(precip_old, precip_new,color=as.factor(x)))+
  # geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  scale_color_viridis_d()+
  theme(legend.position = 'none')

test %>% remove_missing() %>% 
  select(precip_old, precip_new,x,y) %>% 
  # mutate(x=round(x,1)) %>% 
  group_by(x,y) %>% 
  summarize(old = sum(precip_old,na.rm=T), 
            new = sum(precip_new,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(x,y,fill=new-old))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(limits=c(-100,100),oob=scales::squish)

bind_rows(ex %>% filter(date<=ymd('2019-01-01')), 
          ex2) %>% 
  arrow::write_parquet(., 
                sink=paste0("../data_general/clim_grid/awap/parquet/awap_precip_monthly_1900_2019_proc",Sys.Date(),".parquet"),
                compression = "gzip",compression_level =9)
rm(ex,junk,vec_dates,fp)
gc()
################################################################
# *** END ***
################################################################

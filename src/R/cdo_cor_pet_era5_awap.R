library(stars);
library(tidyverse); library(lubridate); 

fp_era5 <- "../data_general/clim_grid/era5-land/Oz/Oz/Oz_era5-land_pet_1981_2019.nc"; 
fp_awap <- "../data_general/clim_grid/awap/AWAP/Monthly_data_1911_2019/AWAP_monthly_PriestleyTaylor_PET_1990_2019.nc"


s_awap <- stars::read_stars(fp_awap)
names(s_awap) <- "pet_pt"
st_crs(s_awap) <- st_crs(4326)
st_get_dimension_values(s_awap,3)
s_awap <- s_awap %>% filter(time < ymd("2019-12-01"))
s_awap <- s_awap %>% filter(time >= ymd("1990-01-01"))


s_e5 <- stars::read_stars(fp_era5)
names(s_e5) <- "pet_pm"
st_crs(s_e5) <- st_crs(4326)
s_e5 <- s_e5 %>% filter(time >= ymd("1990-01-01"))
s_e5 <- s_e5 %>% filter(time < ymd("2019-12-01"))

# test (1)
testthat::expect(dim(s_awap)["time"]==dim(s_e5)["time"], 
                 failure_message = "BREAK", ok = T)

# warp AWAP to match the ERA5 grid
s_awap2 <- stars::st_warp(s_awap, s_e5[,,,], use_gdal = T)
names(s_awap2) <- names(s_awap)
s_awap2 <- st_set_dimensions(s_awap2, 3, 
                  values=st_get_dimension_values(s_awap,'time'), 
                  names = 'time')

s_e5 <- units::drop_units(s_e5)
d_awap <- s_awap2 %>% as_tibble %>% filter(is.na(pet_pt)==F)
d_e5 <- s_e5 %>% as_tibble %>% filter(is.na(pet_pm)==F) %>% 
  mutate(days_in_month = days_in_month(time)) %>% 
  mutate(pet_pm = pet_pm*days_in_month*-1000)

dd <- inner_join(d_awap ,d_e5, by=c("x","y","time"))

dd %>% 
  filter(time == ymd('2000-01-01')) %>% 
  ggplot(data=., aes(x,y,fill=pet_pm))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()

dd %>% 
  filter(time == min(time)) %>% 
  mutate(diff = pet_pt - pet_pm) %>% 
  ggplot(data=., aes(x,y,fill=diff))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2()


# s_awap2[,,,1]/s_e5[,,,1] %>% plot
# 
# s_awap2[,,,1] %>% plot
# s_e5[,,,1] %>% units::drop_units() %>% plot
# 
# (s_awap2[,,,1] - units::drop_units(s_e5[,,,1])) %>% 
#   plot






cdo("--version",debug=F)


processx::run("cdo sinfo ",args = fp_era5)

dir.create("/home/sami/scratch")
temp_dir <- "/home/sami/scratch"

# resample awap to era5
system(
  paste0("cdo -f nc4c -z zip_6 remapbil,",fp_era5," ",fp_awap," regrid_fp_awap.nc")
)

system(
  paste("cdo timcor",fp_era5,fp_awap," test_pet_cor.nc")
)

system(
  paste("cdo timcor",fp_era5,fp_awap," test_pet_cor.nc")
)

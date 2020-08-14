library(stars); library(sf); 
library(tidyverse); library(data.table); library(lubridate)
library(mgcv); 

# Load VOD ----------------------------------------------------------------
vod_day <- stars::read_stars("../data_general/Oz_misc_data/LPDRv2_VODday_monmean_2002_2019.nc")
vod_day <- vod_day %>% as_tibble() %>% as.data.table() %>% 
  set_names(c("x","y","date","vod_day"))

vod_night <- stars::read_stars("../data_general/Oz_misc_data/LPDRv2_VODnight_monmean_2002_2019.nc")
vod_night <- vod_night %>% as_tibble() %>% as.data.table() %>% 
  set_names(c("x","y","date","vod_night"))

vod <- merge(vod_day, vod_night, by=c("x","y","date"))

vod_grid <- stars::read_stars("../data_general/Oz_misc_data/LPDRv2_VODday_monmean_2002_2019.nc", 
                              proxy=F, RasterIO = list(bands=1))


# Load Biomass ------------------------------------------------------------
carb <- stars::read_stars("../data_general/Oz_misc_data/WCMC_carbon_biomass_25km.tif", 
                          proxy=F)
carb <- stars::st_warp(carb, vod_grid, use_gdal = F)
carb <- carb %>% as_tibble %>% set_names(c("x","y","agb")) %>% filter(is.na(agb)==F)

st_crs(vod_grid) <- st_crs("EPSG:4326")
grid_area <- sf::st_area(vod_grid, ) %>% as_tibble() %>% 
  units::drop_units() %>% 
  mutate(ha = area/(100**2))


# Prep VOD and Biomass for model fitting ----------------------------------
vod[,`:=`(year=year(date),month=month(date))] %>% 
  .[,`:=`(season = case_when(month%in%c(3:5)~'MAM',
                             month%in%c(6:8)~'JJA',
                             month%in%c(9:11)~'SON',
                             month%in%c(12,1,2)~'DJF'))]

mod_dat <- vod[year==2011][,.(vod_day = mean(vod_day,na.rm=TRUE), 
                   vod_night = mean(vod_night,na.rm=TRUE)),keyby=.(x,y,season)]
mod_dat <- mod_dat[is.na(vod_day)==F]

mod_dat <- mod_dat %>% pivot_wider(., #id_cols=c("x","y"), 
                        names_from=season, 
                        values_from=c(vod_day,vod_night))

mod_dat <- inner_join(mod_dat, carb, by=c("x","y"))

mod_dat <- mod_dat %>% drop_na()
train_dat <- mod_dat %>% sample_frac(., 0.75)
test_dat <- mod_dat %>% anti_join(., train_dat)

fit <- gam(agb~s(vod_day_SON, bs='cs')+
               s(vod_night_SON, bs='cs')+
             s(vod_day_DJF, bs='cs')+
             s(vod_night_DJF, bs='cs')+
             s(vod_day_MAM, bs='cs')+
             s(vod_night_MAM, bs='cs')+
             s(vod_day_JJA, bs='cs')+
             s(vod_night_JJA, bs='cs'), 
           data=mod_dat,
           select=TRUE, method='REML')
summary(fit)
plot(fit)

fit2 <- bam(agb~
             te(vod_night_SON,x,y, bs='cs')+
             te(vod_night_JJA,x,y, bs='cs'),
           data=mod_dat %>% filter(agb>0),
           family=Gamma(link='log'), # Gamma distribution
           select=TRUE, method='fREML', discrete=TRUE)
summary(fit2)

yardstick::rsq_trad_vec(test_dat$agb, predict(fit2,newdata=test_dat,type='response'))
yardstick::rmse_vec(test_dat$agb, predict(fit2,newdata=test_dat,type='response'))



# Apply model prediction to VOD -------------------------------------------
vod_seasonal <- vod[,.(vod_day = mean(vod_day,na.rm=TRUE), 
                   vod_night = mean(vod_night,na.rm=TRUE)),keyby=.(x,y,year,season)]
vod_wide <- vod_seasonal %>% 
  select(x,y,year,season,vod_night,vod_day) %>% 
  drop_na() %>% 
  distinct() %>% 
  filter(season %in% c("SON","JJA")) %>% 
  pivot_wider(., #id_cols=c("x","y","year"), 
              names_from=season, 
              values_from=c(vod_night,vod_day))

max_agb <- mod_dat$agb %>% max(.,na.rm=TRUE)
vod_pred <- vod_wide %>% 
  mutate(agb_pred = predict(fit2, newdata=., type='response')) %>% 
  mutate(agb_pred = ifelse(agb_pred >= max_agb, max_agb, agb_pred))


vod_pred$agb_pred %>% hist

vod_pred %>% 
  inner_join(., grid_area, by=c('x','y')) %>% 
  mutate(agb_pred_mg = agb_pred*ha) %>% 
  group_by(year) %>% 
  summarize(agb_gt = sum(agb_pred_mg,na.rm=TRUE)/1e9) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(year,agb_gt))+
  geom_line()+
  scale_x_continuous(expand=c(0,0))+
  labs(x=NULL,y=expression(paste("Above Ground Carbon (Gt)")))+
  theme_linedraw()
ggsave("figures/upscale_abovegroundCarbonDensity_w_VOD.png",
       width=14, height=8, units='cm', type='cairo')

vod_pred %>% arrow::write_parquet(., 
                    sink=paste0('outputs/pred_agb_wVOD_',Sys.Date(),'.parquet'))


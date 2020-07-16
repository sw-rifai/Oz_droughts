library(RcppArmadillo); library(tidyverse); library(data.table); library(lubridate)
g <- stars::read_ncdf("../data_general/GRACE_reconstruction/GRACE_REC_v03_GSFC_ERA5_monthly_ensemble_mean.nc")
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly, dTolerance = 0.05)
g <- st_crop(g, oz_poly)
g <- g %>% as.data.table() %>% 
  units::drop_units() 
g[,`:=`(ddate = decimal_date(time))]
g[,`:=`(ddate_c = ddate - (g$ddate %>% mean))]
g %>% lazy_dt %>% rename(x=lon,y=lat) %>% show_query()
setnames(g, c("lon","lat"), c("x","y"))
gb <- g[,.(beta = list(unname(fastLm(X = cbind(1,ddate_c), 
                                     y=rec_ensemble_mean, 
                                     data=.SD)$coefficients))),by=.(x,y)] %>% 
  .[,`:=`(b0 = unlist(beta)[1], b1 = unlist(beta)[2]), by=.(x,y)]
gb %>% ggplot(data=., aes(x,y,fill=b1))+
  geom_tile()+
  scale_fill_gradient2()+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)




grace <- arrow::read_parquet("../data_general/GRACE_reconstruction/parquet/GRACE_ERA5_monthly_1979_2018.parquet")
grace %>% 
  group_by(date) %>% 
  summarize(val = mean(grace_rec_era5,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(date, val))+
  geom_line()+
  geom_smooth(method='gam', 
              formula=y~s(x,bs='cs'), 
              method.args=list(method='REML'), 
              color='darkgreen')+
 geom_smooth(method='gam', 
              formula=y~s(x,bs='cs',k=(2018-1978)), 
              method.args=list(method='REML'), 
              color='red')



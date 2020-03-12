library(arrow); library(tidyverse); library(lubridate); 
library(sf); library(lubridate)
# ascending ~ 1:30am 
# descending ~ 1:30pm

# Import ------------------------------------------------------------------
fp <- list.files("../data_general/lpdr_test/LPDR_VOD/", full.names = T, pattern='parquet')


library(foreach); library(doParallel)
n_cores <- 10
cl <- makeCluster(n_cores)
registerDoParallel(cl)


# Filter out daily values -------------------------------------------------------
out <- foreach(i = 1:length(fp), 
               .packages = c("tidyverse","lubridate","arrow"),
               .combine=rbind) %dopar% {
                 o <- arrow::read_parquet(fp[i])
                 test <- o %>% 
                   filter(band==5) %>% 
                   rename(vod_day = val_desc, 
                          vod_night = val_asc) %>% 
                   mutate(vod_ddiff = vod_night-vod_day) %>% 
                   filter(vod_ddiff >= 0)
                 
                 test          
               }


# Group by Hydro Year and Season ------------------------------------------
out <- out %>% 
  mutate(quarter = quarter(date, fiscal_start = 11))
out <- out %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON"))
out <- out %>% mutate(season = factor(q,
                                  levels=c("DJF","MAM","JJA","SON"), 
                                  ordered=T))
out <- out %>% 
  mutate(hydro_year = year(date+months(1)))
ls()


# Fit sigma by season and year --------------------------------------------
vod_s <- out %>% 
  filter(band==5) %>% 
  group_by(x,y,hydro_year, season) %>% 
  summarize(vod_day_avg = mean(vod_day, na.rm=T), 
            vod_night_avg = mean(vod_night, na.rm = T), 
            vod_ddiff = mean(vod_night-vod_day, na.rm=T), 
            vod_sigma = coef(lm(vod_day~vod_night))[2], 
            vod_lambda = coef(lm(vod_day~vod_night))[1], 
            vod_nobs = sum(is.na(vod_day)==F)) %>% 
  ungroup()

arrow::write_parquet(vod_s, 
                     sink=paste0("../data_general/lpdr_test/lpdr_vodSeasonal_Oz",Sys.Date(),".parquet"))

vod_cs <- out %>% 
  filter(band==5) %>% 
  mutate(x = round(x, digits = -5), 
         y= round(y, digits=-5)) %>% 
  group_by(x,y,hydro_year, season) %>% 
  summarize(vod_day_avg = mean(vod_day, na.rm=T), 
            vod_night_avg = mean(vod_night, na.rm = T), 
            vod_ddiff = mean(vod_night-vod_day, na.rm=T), 
            vod_sigma = coef(lm(vod_day~vod_night))[2], 
            vod_lambda = coef(lm(vod_day~vod_night))[1], 
            vod_nobs = sum(is.na(vod_day)==F)) %>% 
  ungroup()



vod_ss <- out %>% 
  filter(band==5) %>% 
  group_by(x,y,season) %>% 
  summarize(vod_day_avg = mean(vod_day, na.rm=T), 
            vod_night_avg = mean(vod_night, na.rm = T), 
            vod_ddiff = mean(vod_night-vod_day, na.rm=T), 
            vod_sigma = coef(lm(vod_day~vod_night))[2], 
            vod_lambda = coef(lm(vod_day~vod_night))[1], 
            vod_nobs = sum(is.na(vod_day)==F)) %>% 
  ungroup()

vod_ss %>% 
  filter(x>=1.3e07) %>% 
  inner_join(., 
             {.} %>% group_by(x,y) %>% summarize(u = mean(vod_sigma)), 
             by=c('x','y')) %>% 
  mutate(val = vod_sigma - u) %>% 
  ggplot(data=., aes(x,y,fill=val))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(
    expression(paste(Delta~sigma)),
    limits=c(-0.5,0.5),
                       oob=scales::squish, 
                       high=colorspace::darken('red',amount = 0.5), 
                       low=colorspace::darken('blue',amount = 0.5))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  facet_wrap(~season,nrow = 1)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())
ggsave("figures/map_vod_sigma_seasonal_difference.png", 
       width=12, height=6, units='cm', dpi='retina')

vod_ss %>% 
  filter(x>=1.3e07) %>% 
  # inner_join(., 
  #            {.} %>% group_by(x,y) %>% summarize(u = mean(vod_sigma)), 
  #            by=c('x','y')) %>% 
  # mutate(val = vod_sigma - u) %>% 
  ggplot(data=., aes(x,y,fill=vod_nobs))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c("Observations", 
                       option='B')+
  # scale_fill_gradient2(
  #   expression(paste(Delta~sigma)),
  #   limits=c(-0.5,0.5),
  #   oob=scales::squish, 
  #   high=colorspace::darken('red',amount = 0.5), 
  #   low=colorspace::darken('blue',amount = 0.5))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  facet_wrap(~season,nrow = 1)+
  theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())
ggsave("figures/map_vod_nobs_seasonal.png", 
       width=12, height=6, units='cm', dpi='retina')




out$y %>% unique %>% length
out$y %>% unique %>% sort %>% diff
out$y %>% unique %>% sort
out$y %>% unique %>% sort %>% round(., digits = -5) %>% 
  unique


junk <- out %>% 
  filter(band==5) %>% 
  filter(near(y, -4e06, tol=1e5)) %>% 
  filter(near(x, 1.4e07, tol=1e5)) %>% 
  group_by(x,y,hydro_year, season) %>%
  summarize(vod_day_ = mean(vod_day, na.rm=T),
            vod_night_ = mean(vod_night, na.rm = T),
            vod_ddiff = mean(vod_night-vod_day, na.rm=T),
            vod_sigma = coef(lm(vod_day~vod_night))[2],
            vod_lambda = coef(lm(vod_day~vod_night))[1],
            vod_nobs = sum(is.na(vod_day)==F)) %>%
  ungroup()
junk$vod_sigma %>% hist



vod_nobs <- out %>% 
  filter(band==5) %>% 
  group_by(x,y,hydro_year, season) %>% 
  summarize(nobs=n()) %>% 
  ungroup()
vod_nobs$nobs %>% hist  

vod_cs %>% 
  filter(hydro_year==2014) %>%
  # group_by(x,y,season) %>% 
  # summarize(vod_sigma = mean(vod_sigma, na.rm=T)) %>% 
  # ungroup() %>% 
  filter(vod_night_avg>=1) %>% 
  ggplot(data=., aes(x,y,fill=vod_sigma))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(
    limits=c(0,2)
                        )+
  facet_wrap(~season)


vod_cs %>% 
  # filter(hydro_year==2014) %>%
  filter(vod_night_avg>=1) %>% 
  group_by(hydro_year, season) %>% 
  summarize(val = mean(vod_night_avg,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(hydro_year,season,fill=val))+
  geom_tile()+
  # coord_equal()+
  scale_fill_viridis_c(
    # limits=c(0,2)
  )



out %>% 
  group_by(x,y) %>% 
  summarize(nobs = n()) %>% 
  ungroup() %>% 
  ggplot(data=., aes(x,y,fill=nobs))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()






o <- arrow::read_parquet(fp[i])

o %>% 
  rename(vod_day = val_desc, 
         vod_night = val_asc) %>% 
  mutate(vod_ddiff = vod_night-vod_day) %>% 
  filter(vod_ddiff >= 0)



test <- o %>% 
  filter(band==5) %>% 
  group_by(x,y,date) %>% 
  summarize(vod_asc = mean(val_asc, na.rm=T), 
            vod_desc = mean(val_desc, na.rm = T), 
            vod_ddiff = mean(val_asc-val_desc, na.rm=T), 
            vod_sigma = coef(lm(val_desc~val_asc))[2], 
            vod_lambda = coef(lm(val_desc~val_asc))[1], 
            vod_nobs = sum(is.na(val_asc)==F & is.na(val_desc)==F)) %>% 
  ungroup()




n_cores <- 10

library(foreach); library(doParallel)
cl <- makeCluster(n_cores)
registerDoParallel(cl)

out <- foreach(i = 1:length(fp), 
               .packages = c("tidyverse","lubridate","arrow"),
               .combine=rbind) %dopar% {
                 o <- arrow::read_parquet(fp[i])
                 test <- o %>% 
                   mutate(month=month(date), 
                          year=year(date)) %>% 
                   filter(band==5) %>% 
                   group_by(x,y,month,year) %>% 
                   summarize(vod_asc = mean(val_asc, na.rm=T), 
                             vod_desc = mean(val_desc, na.rm = T), 
                             vod_ddiff = mean(val_asc-val_desc, na.rm=T), 
                             vod_sigma = coef(lm(val_desc~val_asc))[2], 
                             vod_lambda = coef(lm(val_desc~val_asc))[1], 
                             vod_nobs = sum(is.na(val_asc)==F & is.na(val_desc)==F)) %>% 
                   ungroup()
                 test          
               }
arrow::write_parquet(out, 
                     sink=paste0("../data_general/lpdr_test/lpdr_vod_Oz_",Sys.Date(),".parquet"))


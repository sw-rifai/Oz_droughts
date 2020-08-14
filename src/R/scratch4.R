ref_grid <- stars::read_stars('../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif', 
                              RasterIO = list(bands=1))
srtm <- stars::read_stars("../data_general/Oz_misc_data/SRTM_elevation_500m_EastOz_.tif")
base <- stars::read_stars("../data_general/Oz_misc_data/MOD44BPercent_Tree_Cover_5000m_East_Oz_noMask_2000_2019.tif", 
                          RasterIO = list(bands=1))
srtm <- st_warp(src=srtm, dest=ref_grid[,,], use_gdal = F)
srtm <- srtm %>% as_tibble() %>% set_names('x','y','elev') %>% na.omit()

bom <- stars::read_stars("../data_general/Koppen_climate/BOM/kpngrp.txt")
st_crs(srtm)
bom <- st_warp(src=bom, dest=ref_grid[,,], use_gdal = F)
plot(bom)
bom <- set_names(bom, 'koppen') %>% as_tibble()
bom <- left_join(ref_grid %>% as_tibble() %>% select(x,y), 
                 bom)

bom %>% ggplot(data=., aes(x,y,fill=as_factor(koppen)))+geom_tile()+coord_equal()+
  scale_fill_viridis_d()

# K-means Climate Zones & P:PET Trend & NDVI & VCF distributions --------------------------------------------------
jj <- ldat %>% 
  # filter(season %in% c("DJF","JJA")) %>% 
  filter(date >= ymd("1982-01-01") & date<=ymd("2011-12-31")) %>% 
  group_by(x,y) %>% 
  summarize(ppet_u = mean(pe, na.rm=TRUE),
            ppet_sd = sd(pe,na.rm=TRUE), 
            tmax_u = mean(tmax,na.rm=TRUE),
            tmax_sd = sd(tmax,na.rm=TRUE), 
            tmin_u = mean(tmin,na.rm=TRUE), 
            tmin_sd = sd(tmin,na.rm=TRUE),
            vpd15_u = mean(vpd15,na.rm=TRUE),
            vpd15_sd = sd(vpd15, na.rm=TRUE),
            precip_u = mean(precip,na.rm=TRUE), 
            precip_sd = sd(precip,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  na.omit()
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
jj2 <- jj %>% 
  # group_by(season) %>% 
  mutate_at(c("ppet_u","ppet_sd",
              'tmax_u','tmax_sd',
              'tmin_u','tmin_sd',
              'vpd15_u','vpd15_sd',
              'precip_u','precip_sd'), scale2) %>% 
  ungroup()
srtm <- srtm %>% 
  mutate(x_s = x, 
         y_s = y) %>% 
  mutate_at(c('elev','x_s','y_s'),scale2)
# jj2 <- jj2 %>% pivot_wider(names_from=season, 
#                    values_from=c(ppet_u,tmax_u,tmin_u,vpd15_u,precip_u, 
#                                  ppet_sd,tmax_sd,tmin_sd,vpd15_sd,precip_sd))
jj2
jj2  <- inner_join(jj2,srtm,by=c('x','y'))
jj2
set.seed(999)
jj3 <- kmeans(jj2 %>% select(x_s,y_s#,elev#,starts_with(c("ppet","tmin"))
                             ) %>% 
                as.matrix(),centers=6,
              iter.max = 100, 
              nstart = 100)
jj2$cluster <- jj3$cluster
jj2 <- inner_join(jj2, 
                  jj3$centers %>% as_tibble() %>% 
                    mutate(cluster = 1:6) %>% 
                    mutate(cz=cluster) %>% 
                    # mutate(cz = rank(tmax_u_DJF+tmax_u_JJA+tmax_u_MAM+tmax_u_SON)) %>% 
                    select(cluster, cz))

jj2 <- jj2 %>% select(-cluster)
# jj2 %>% select(x,y,cz) %>% arrow::write_parquet(.,sink="data/EOz_clim_kmeans6_v2.parquet")

p_left <- jj2 %>% 
  ggplot(data=., aes(x,y,fill=as_factor(cz)))+
  geom_sf(inherit.aes = F, data=oz_poly,
          fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,155),
           ylim = c(-45,-10), 
           expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  scale_fill_viridis_d(option='B',direction = -1,end=0.95)+
  # scico::scale_fill_scico_d(end=0.9,direction = 1)+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  guides(fill=guide_legend(title='Climate Zone', 
                           title.position = 'top'))+
  theme(legend.position = c(0.64,0.925), 
        legend.direction = 'horizontal',
        panel.grid = element_blank()); p_left


left_join(jj2 %>% select(x,y), bom) %>% pull(koppen) %>% hist
left_join(jj2 %>% select(x,y), bom) %>% 
 ggplot(data=.,aes(x,y,fill=as_factor(koppen)))+
 geom_sf(inherit.aes = F, data=oz_poly,
          fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,155),
           ylim = c(-45,-10), 
           expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  scico::scale_fill_scico_d()

coords <- dat %>% select(x,y) %>% distinct()
left_join(coords, bom, by=c("x","y")) %>% 
  mutate(zone = case_when(between(koppen,0,11) ~ 'Temperate', 
                          (y <= -40) ~ 'Tasmania',
                          between(koppen, 12,21)~'Grassland & Desert', # Grassland
                          between(koppen, 22,30)~'Grassland & Desert', # Desert
                          between(koppen, 31,34)~'Subtropical',
                          between(koppen, 35,40)~'Tropical', 
                          koppen >= 41 ~ 'Equatorial')) %>% 
  mutate(zone = ifelse(y < -40, 'Temperate Tas.', zone)) %>% #pull(zone) %>% table
  mutate(zone = factor(zone, levels = c("Equatorial","Tropical",
                                        "Subtropical","Grassland & Desert",
                                        "Temperate","Temperate Tas."), ordered = T)) %>% 
  ggplot(data=.,aes(x,y,fill=zone))+
  geom_sf(inherit.aes = F, data=oz_poly,
          fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,155),
           # ylim=c(-23,-20),
           ylim = c(-45,-10),
           expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  scale_fill_viridis_d(option='B')
ggsave(filename = 'delete_me.png')

jj2 %>% ggplot(data=.,aes(x,y,fill=tmax_u))+geom_tile()+coord_equal()
(jj2$x %in% bom$x) %>% table
(jj2$y %in% bom$y) %>% table
(bom$y %in% jj2$y) %>% table
# 41 Equatorial
# 
# 35 Tropical # 35-40
# 
# 32 Subtropical # 32-34
# 
# 22 Desert   # 
# 
# 13 Grassland # 12-20
# 
# 3 Temperate # 1-11

lt_ndvi_hy %>% 
  ggplot(data=.,aes(hydro_year,b1))+
  geom_point()


dat[sample(.N,1e6)] %>% lm(ndvi_3mo~precip_12mo, data=.) %>% summary
dat[sample(.N,1e6)] %>% lm(ndvi_3mo~log(precip_12mo), data=.) %>% summary
dat[sample(.N,1e6)] %>% lm(ndvi_3mo~pet_12mo, data=.) %>% summary
dat[sample(.N,1e6)] %>% lm(ndvi_3mo~log(pet_12mo), data=.) %>% summary
dat[sample(.N,1e6)] %>% lm(ndvi_3mo~pe_12mo, data=.) %>% summary
dat[sample(.N,1e6)] %>% lm(ndvi_3mo~log(pe_12mo), data=.) %>% summary
dat[sample(.N,1e6)] %>% lm(ndvi_3mo~log(pe_12mo)*co2_trend, data=.) %>% summary

a1 <- dat[sample(.N,1e6)] %>% lm(ndvi_3mo~log(precip_12mo), data=.) %>% summary
a2 <- dat[sample(.N,1e6)] %>% lm(ndvi_3mo~log(pet_12mo), data=.) %>% summary
a3 <- dat[sample(.N,1e6)] %>% lm(ndvi_3mo~log(pe_12mo), data=.) %>% summary
a1$r.squared
a2$r.squared
a3$r.squared






#split test & train ------------------------------------------------------------
tmp[,`:=`(pe_anom_12mo = pe_12mo - mape)]
train_dat <- tmp[season=='DJF'][mape<2][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 100000)]
test_dat <- tmp[season=='DJF'][mape<2][is.na(ndvi_3mo)==F & is.na(pe_12mo)==F][sample(.N, 100000)]
#*******************************************************************************
l_log_dry <- lm(ndvi_3mo~co2_trend*pe_12mo + mape, 
             data=tmp[season=="SON"][mape<0.5][pe_anom_12mo < -0.1][sample(.N, 0.5e6)])
l_log_norm <- lm(ndvi_3mo~co2_trend*pe_12mo + mape, 
                data=tmp[season=="SON"][mape<0.5][pe_anom_12mo > -0.1&pe_anom_12mo < 0.1][sample(.N, 0.5e6)])
l_log_wet <- lm(ndvi_3mo~co2_trend*pe_12mo + mape, 
                data=tmp[season=="SON"][mape<0.5][pe_anom_12mo > 0.1][sample(.N, 0.75e6)])
summary(l_log_dry)
summary(l_log_norm)
summary(l_log_wet)

# DRY
expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10)) %>% 
  mutate(pe_12mo = mape) %>% 
  mutate(mape_d = cut_width(mape,0.05)) %>% 
  mutate(pred = predict(l_log_dry, newdata=.)) %>% 
  ggplot(data=.,aes(pe_12mo, pred,color=co2_trend,group=co2_trend))+
  # geom_point(data=test_dat[sample(.N,10000)], 
  #            aes(pe_12mo,ndvi_3mo,color=co2_trend),size=0.5,alpha=0.05)+
  geom_line()+
  scale_color_viridis_c(option='B')

# Normal 
expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10)) %>% 
  mutate(pe_12mo = mape) %>% 
  mutate(mape_d = cut_width(mape,0.05)) %>% 
  mutate(pred = predict(l_log_norm, newdata=.)) %>% 
  ggplot(data=.,aes(pe_12mo, pred,color=co2_trend,group=co2_trend))+
  # geom_point(data=test_dat[sample(.N,10000)], 
  #            aes(pe_12mo,ndvi_3mo,color=co2_trend),size=0.5,alpha=0.05)+
  geom_line()+
  scale_color_viridis_c(option='B')

# WET
expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10)) %>% 
  mutate(pe_12mo = mape) %>% 
  mutate(mape_d = cut_width(mape,0.05)) %>% 
  mutate(pred = predict(l_log_wet, newdata=.)) %>% 
  ggplot(data=.,aes(pe_12mo, pred,color=co2_trend,group=co2_trend))+
  # geom_point(data=test_dat[sample(.N,10000)], 
  #            aes(pe_12mo,ndvi_3mo,color=co2_trend),size=0.5,alpha=0.05)+
  geom_line()+
  scale_color_viridis_c(option='B')


l_log_anom <- lm(ndvi_3mo~co2_trend*pe_anom_12mo + mape, 
                data=tmp[season=="SON"][mape<0.5][sample(.N, 1e6)])
expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10), 
            pe_anom_12mo = c(-0.5,0,0.5)) %>% 
  mutate(pred = predict(l_log_anom, newdata=.)) %>% 
  mutate(pe_12mo = mape+pe_anom_12mo) %>% 
  filter(pe_12mo > 0) %>% 
  ggplot(data=.,aes(pe_12mo, pred,color=co2_trend,group=co2_trend))+
  # geom_point(data=test_dat[sample(.N,10000)], 
  #            aes(pe_12mo,ndvi_3mo,color=co2_trend),size=0.5,alpha=0.05)+
  geom_line()+
  scale_color_viridis_c(option='B')+
  facet_wrap(~pe_anom_12mo)

tmp[sample(.N,1e6)] %>% 
  filter(mape < 2) %>% 
  filter(is.na(season)==F) %>% 
  ggplot(data=., aes(pe_anom))+
  geom_histogram()+
  geom_vline(aes(xintercept=0),col='red')+
  facet_wrap(~season,ncol = 1)



tmp[season=="SON"][mape<=2][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5]
g_anom <- bam(ndvi_3mo~te(pe_anom_12mo, co2_trend,k=4,bs='cs')+s(mape,k=4,bs='cs'),
                 data=tmp[season=="SON"][mape<=2][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5][sample(.N, 0.5e6)], 
              select=TRUE)
summary(g_anom)
plot(g_anom, scale=0, scheme=2)
expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10), 
            xred = c(-0.5,0,0.5)) %>% 
  mutate(pe_anom_12mo = mape+mape*xred) %>% 
  mutate(pe_12mo = mape+pe_anom_12mo) %>% 
  filter(pe_12mo > 0) %>% 
  mutate(pred = predict(g_anom, newdata=.)) %>% 
  ggplot(data=.,aes(pe_12mo, pred,color=co2_trend,group=co2_trend))+
  # geom_point(data=test_dat[sample(.N,10000)], 
  #            aes(pe_12mo,ndvi_3mo,color=co2_trend),size=0.5,alpha=0.05)+
  geom_line()+
  scale_color_viridis_c(option='B')+
  facet_wrap(~xred)

# Log functions in linear mod w/CO2------------------------------------------------------------
l_log2 <- lm(ndvi_3mo~co2_trend*mape_d+co2_trend*pe_12mo, 
             data=train_dat %>% 
               mutate(mape_d = cut_width(mape,0.05)))
summary(l_log2)
yardstick::rsq_trad_vec(test_dat$ndvi_3mo, predict(l_log2, newdata=test_dat))

test_dat %>% as_tibble() %>%
  mutate(mape_d = cut_width(mape,0.05)) %>% 
  sample_n(10000) %>% 
  mutate(pred=predict(l_log2,newdata=.)) %>% 
  select(pred,ndvi_3mo) %>% 
  ggplot(data=.,aes(pred,ndvi_3mo))+
  geom_point()+
  geom_smooth(method='lm')+
  geom_abline(aes(intercept=0,slope=1),color='red')

test_dat %>% as_tibble() %>% 
  mutate(mape_d = cut_width(mape,0.05)) %>% 
  sample_n(10000) %>% 
  mutate(pred=predict(l_log2,newdata=.)) %>% 
  select(pe_12mo, pred,ndvi_3mo) %>% 
  ggplot(data=.,aes(pe_12mo,ndvi_3mo))+
  geom_point()+
  geom_point(aes(pe_12mo, pred), color='navy')

expand_grid(mape = seq(0.0837,2,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10)) %>% 
  mutate(pe_12mo = mape) %>% 
  mutate(mape_d = cut_width(mape,0.05)) %>% 
  mutate(pred = predict(l_log2, newdata=.)) %>% 
  ggplot(data=.,aes(pe_12mo, pred,color=co2_trend,group=co2_trend))+
  geom_point(data=test_dat[sample(.N,10000)], 
             aes(pe_12mo,ndvi_3mo,color=co2_trend),size=0.5,alpha=0.05)+
  geom_line()+
  scale_color_viridis_c(option='B')

train_dat[ndvi_3mo==max(ndvi_3mo)]$ndvi_3mo
train_dat[ndvi_3mo==max(ndvi_3mo)]$pe_12mo
train_dat[ndvi_3mo==max(ndvi_3mo)]$mape












# Four Param Logistic Function --------------------------------------------
n_fpl <- nls.multstart::nls_multstart(ndvi_3mo ~ 
                                        SSfpl(pe_12mo, A, B, xmid, scal),
                                      data=train_dat[mape<1], 
                                      iter = 3,
                                      supp_errors = 'Y',
                                      control=nls.control(maxiter=100),
                                      start_lower = c(A=0,    B=0.5, xmid=0.1, scal=0), 
                                      start_upper = c(A=0.25, B=1, xmid=1, scal=1)) 
summary(n_fpl)


fn <- function(input,cco2,pe_anom_12mo,A,B,xmid,scal){
  A+(B-A)/(1+exp((xmid-input)/scal) + cco2*pe_anom_12mo)
  }

n_fpl2 <- nls.multstart::nls_multstart(ndvi_3mo ~ 
         A+(B-A)/(1+exp((xmid-mape)/scal)) + C*cco2+D*(pe_anom_12mo/mape)+E*cco2*(pe_anom_12mo/mape),
         data=train_dat, 
        iter = 3,
        supp_errors = 'Y',
        control=nls.control(maxiter=100),
        start_lower = c(A=0,    B=0.5, xmid=0.1, scal=0, C=0, D=0,E=0), 
        start_upper = c(A=0.25, B=1, xmid=1, scal=1, C=0.01, D=0.01, E=0.01)) 
summary(n_fpl2)

yardstick::rsq_trad_vec(truth=train_dat$ndvi_3mo, predict(n_fpl, newdata=train_dat))
yardstick::rsq_trad_vec(truth=train_dat$ndvi_3mo, predict(n_fpl2, newdata=train_dat))
yardstick::rmse_vec(truth=train_dat$ndvi_3mo, predict(n_fpl, newdata=train_dat))
yardstick::rmse_vec(truth=train_dat$ndvi_3mo, predict(n_fpl2, newdata=train_dat))

expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10), 
            xred = c(-0.25,0,0.25)) %>% 
  mutate(cco2 = co2_trend - center_co2) %>% 
  mutate(pe_anom_12mo = mape+mape*xred) %>% 
  mutate(pe_12mo = mape+pe_anom_12mo) %>% 
  filter(pe_12mo > 0) %>% 
  mutate(pred = predict(n_fpl2, newdata=.)) %>% 
  ggplot(data=.,aes(pe_12mo, pred,color=co2_trend,group=co2_trend))+
  # geom_point(data=test_dat[sample(.N,10000)], 
  #            aes(pe_12mo,ndvi_3mo,color=co2_trend),size=0.5,alpha=0.05)+
  geom_line()+
  scale_color_viridis_c(option='B')+
  facet_wrap(~xred)

expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10), 
            xred = c(-0.5,0,0.5)) %>% 
  mutate(cco2 = co2_trend - center_co2) %>% 
  mutate(pe_anom_12mo = mape+mape*xred) %>% 
  mutate(pe_12mo = mape+pe_anom_12mo) %>% 
  filter(pe_12mo > 0) %>% 
  mutate(pred = predict(n_fpl2, newdata=.)) %>% 
  ggplot(data=.,aes(mape, pred,color=co2_trend,group=co2_trend))+
  geom_line()+
  scale_color_viridis_c(option='B')+
  facet_wrap(~xred)

test_dat %>% as_tibble() %>% 
  mutate(pred=predict(n_fpl2,newdata=.)) %>% 
  select(pred,ndvi_3mo) %>% 
  ggplot(data=.,aes(pred,ndvi_3mo))+
  geom_point()+
  geom_smooth(method='lm')+
  geom_abline(aes(intercept=0,slope=1),color='red')

test_dat %>% as_tibble() %>% 
  sample_n(10000) %>% 
  mutate(pred=predict(n_fpl2,newdata=.)) %>% 
  select(pe_12mo, pred,ndvi_3mo) %>% 
  ggplot(data=.,aes(pe_12mo,ndvi_3mo))+
  geom_point()+
  geom_point(aes(pe_12mo, pred), color='navy')

bind_rows((coef(n_fpl2)), coef(n_fpl2))

with(bind_rows(coef(n_fpl2)),C*50+D*(-0.25/0.5)+E*50*(-0.25/0.5))

with(coef(n_fpl2), C)
C*cco2+D*(pe_anom_12mo/mape)+E*cco2*(pe_anom_12mo/mape)


l_log_anom <- lm(evi2_hyb~cco2*I(pe_anom_12mo/mape) + mape, 
                 data=train_dat[season=="SON"][mape<1])
summary(l_log_anom)
l_log_anom <- lm(ndvi_hyb~log(co2_trend)*I(pe_anom_12mo/mape) + mape, 
                 data=train_dat[season=="SON"][mape<1])
expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10), 
            pe_anom_12mo = c(-0.15,0,0.15)) %>% 
  mutate(cco2 = co2_trend - center_co2) %>% 
  mutate(pred = predict(l_log_anom, newdata=.)) %>% 
  mutate(pe_12mo = mape+pe_anom_12mo) %>% 
  filter(pe_12mo > 0) %>%
  ggplot(data=.,aes(pe_12mo, pred,color=co2_trend,group=co2_trend))+
  # geom_point(data=test_dat[sample(.N,10000)], 
  #            aes(pe_12mo,ndvi_3mo,color=co2_trend),size=0.5,alpha=0.05)+
  geom_line()+
  scale_color_viridis_c(option='B')+
  facet_wrap(~pe_anom_12mo,scales='free')

expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 10), 
            # pe_anom_12mo = c(-0.15,0,0.15)
            xred = c(-0.5,0,0.5)
            ) %>% 
  mutate(cco2 = co2_trend - center_co2) %>% 
  mutate(pe_anom_12mo = xred*mape) %>% 
  mutate(pe_12mo = mape+pe_anom_12mo) %>% 
  filter(pe_12mo > 0) %>%
  mutate(pred = predict(l_log_anom, newdata=.)) %>% 
  ggplot(data=.,aes(mape, pred,color=co2_trend,group=co2_trend))+
  geom_line()+
  scale_color_viridis_c(option='B')+
  facet_wrap(~xred)

curve(predict(l_log_anom, newdata=data.frame(mape=0.3,pe_anom_12mo=-0.15,co2_trend=x)), 
      -50,50)
curve(predict(l_log_anom, newdata=data.frame(mape=0.3,pe_anom_12mo=x,co2_trend=0)), 
      -0.15,0.15)
curve(predict(l_log_anom, newdata=data.frame(mape=0.3,pe_anom_12mo=x,co2_trend=-50)), 
      -0.15,0.15,add=T,col='blue')
curve(predict(l_log_anom, newdata=data.frame(mape=0.3,pe_anom_12mo=x,co2_trend=50)), 
      -0.15,0.15,add=T,col='red')

train_dat %>% 
  filter(precip_anom_12mo < 0) %>% 
  filter(between(mape,0.2,0.3)) %>% 
  group_by(hydro_year) %>% 
  summarize(val = mean(ndvi_anom_sd,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(hydro_year,val))+
  geom_point()+
  geom_smooth(method='lm')

vec_ids <- unique(tmp$id)

tmp[id %in% sample(vec_ids,10000)] %>% 
  .[mape<1] %>% 
  .[season=='SON'] %>% 
  as_tibble() %>% 
  filter(precip_anom_12mo > -100 & precip_anom_12mo < 100) %>%
  # filter(p_anom_12mo_frac < -0.3) %>% 
  lme4::lmer(ndvi_3mo~co2_trend+(1|id), data=.) %>% summary






  
fit <- tmp[id %in% sample(vec_ids,1000)] %>%
     .[str_detect(vc,"Forests") | 
         str_detect(vc, "Eucalypt") |
          str_detect(vc, "Rainforests")] %>%
     .[mape<1] %>%
     .[date <= ymd("2019-09-01")] %>% 
     .[season=='SON'] %>% 
     as_tibble() %>% 
  mutate(id=as_factor(id)) %>% 
  bam(ndvi_3mo~
        s(pe_anom_12mo,by=co2_trend, k=3,bs='cs')+
        # s(pe_12mo,k=3,bs='cs')+
        s(mape,k=3,bs='cs')+
        s(vc,bs='re'),
        # ti(cco2,pe_anom_12mo,mape,k=c(3,3,3),bs='cs'),
        # s(cco2,k=3,bs='cs')+
        # s(pe_12mo,k=3,bs='cs')+
        # s(mape,k=3,bs='cs')+
        # s(vc,bs='re'), 
      data=.,
      discrete = F, 
      select=TRUE)
    # filter(precip_anom_12mo > -100 & precip_anom_12mo < 100) %>% 
    # filter(p_anom_12mo_frac < -0.3) %>% 
    # lme4::lmer(ndvi_3mo~cco2 + pe_12mo + 
    #              (1|id), data=.)
summary(fit)  
plot(fit,scale=0)
# predict(fit)
# predict(fit, exclude = 's(vc)')
expand_grid(mape = seq(0.0837,1,length.out = 100), 
            co2_trend = seq(340,412,length.out = 100),
            vc = tmp$vc[100000],
            # cco2 = seq(-35,41,length.out = 10),
            xred = c(-0.5,0,0.5)) %>% 
  mutate(cco2 = co2_trend - center_co2) %>%
  mutate(pe_anom_12mo = mape+mape*xred) %>% 
  mutate(pe_12mo = mape+pe_anom_12mo) %>% 
  filter(pe_12mo > 0) %>% 
  mutate(pred = predict(fit, newdata=.,  
                        exclude = "s(vc)",
                        type='response')) %>% 
  ggplot(data=.,aes(mape, pred,color=co2_trend,group=co2_trend))+
  geom_line()+
  scale_color_viridis_c(option='B')+
  facet_wrap(~xred)
yardstick::rsq_trad_vec(truth=train_dat$ndvi_3mo, predict(fit, newdata=train_dat,re.form=NA))
yardstick::rmse_vec(truth=train_dat$ndvi_3mo, predict(fit, newdata=train_dat, re.form=NA))



tmp[id %in% sample(vec_ids,10000)] %>%
  .[mape<1] %>%
  .[date <= ymd("2019-09-01")] %>% 
  .[season=='SON'] %>% 
  sample_n(10000) %>% 
  ggplot(data=.,aes(mape,ndvi_3mo))+
  geom_smooth()+
  geom_smooth(method='lm')











lm(ndvi_hyb~scale(log(co2_trend))+I(pe_anom_12mo/mape) + scale(mape) +vc, 
   data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5] %>% 
     .[(pe_anom_12mo/mape) <= -0.4] %>% .[date<=ymd("2000-01-01")]) %>% 
  summary


lm(ndvi_hyb~scale(log(co2_trend))+I(pe_anom_12mo/mape) + scale(mape) + vc, 
   data=train_dat[season=="SON"][mape<1][ndvi_anom_sd>-3.5&ndvi_anom_sd<3.5] %>% 
     .[(pe_anom_12mo/mape) >= 0.4] %>% .[date<=ymd("2000-01-01")]) %>% 
  summary



















# Load simplified BOM Koppen climate zones --------------------------------
ref_grid <- stars::read_stars('../data_general/AVHRR_CDRv5_VI/AVHRR_SR_median_EastOz_1982_2019.tif', 
                              RasterIO = list(bands=1))
bom <- stars::read_stars("../data_general/Koppen_climate/BOM/kpngrp.txt")
bom <- st_warp(src=bom, dest=ref_grid[,,], use_gdal = F)
bom <- set_names(bom, 'koppen') %>% as_tibble()
bom <- left_join(ref_grid %>% as_tibble() %>% select(x,y), 
                 bom)
coords <- dat %>% select(x,y) %>% distinct()

g_map <- ldat %>% 
  filter(date>=ymd("1982-01-01")&date<=ymd("2010-12-31")) %>% 
  group_by(x,y) %>% 
  summarize(map = mean(precip,na.rm=TRUE)*12) %>% 
  ungroup() %>% 
  as_tibble()

kop <- left_join(coords, bom, by=c("x","y")) %>% 
  inner_join(., g_map, by=c("x","y")) %>% 
  as_tibble() %>% 
  mutate(zone = case_when(between(koppen,0,11) ~ 'Temperate', 
                          (y <= -40) ~ 'Tasmania',
                          between(koppen, 12,21)~'GD_temp', # Grassland
                          between(koppen, 22,30)~'GD_temp', # Desert
                          between(koppen, 31,34)~'Subtropical',
                          between(koppen, 35,40)~'Tropical', 
                          koppen >= 41 ~ 'Equatorial')) %>% 
  mutate(zone = ifelse(y < -40, 'Temperate Tas.', zone)) %>% #pull(zone) %>% table
  mutate(zone = ifelse(zone == "GD_temp" & map < 500, 'Desert',zone)) %>%   
  mutate(zone = ifelse(zone == "GD_temp" & map >= 500, 'Grassland',zone)) %>%   
  mutate(zone = factor(zone, levels = c("Equatorial","Tropical",
                                        "Subtropical","Grassland","Desert",
                                        "Temperate","Temperate Tas."), ordered = T))
kop <- kop %>% mutate(cz=zone)
#*** End Kop zone load ********************************************************




# Koppen Climate Zones & P:PET Trend & NDVI & VCF distributions --------------------------------------------------
p_left <- kop %>% 
  filter(is.nan(map)==F) %>% 
  ggplot(data=., aes(x,y,fill=as_factor(zone)))+
  geom_sf(inherit.aes = F, data=oz_poly,
          fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,155),
           ylim = c(-45,-10), 
           expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  scale_fill_viridis_d(option='B',direction = 1,end=0.95)+
  # scico::scale_fill_scico_d(end=0.9,direction = 1)+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  guides(fill=guide_legend(title='Climate Zone', 
                           title.position = 'top'))+
  theme(legend.position = c(0.75,0.85), 
        legend.direction = 'vertical',
        panel.grid = element_blank(), 
        panel.background = element_rect(fill='lightblue')); p_left



kop <- arrow::read_parquet(file ='../data_general/Koppen_climate/BOM_Koppen_simplified7.parquet')

q_gv <- dat %>% 
  lazy_dt() %>%  
  filter(epoch=='modis') %>% 
  filter(is.na(gv)==F) %>% 
  select(x,y,hydro_year,gv) %>% 
  group_by(x,y,hydro_year) %>% 
  filter(gv == min(gv)) %>% 
  ungroup() %>% 
  as_tibble()

q_npv <- dat %>% 
  lazy_dt() %>%  
  filter(epoch=='modis') %>% 
  filter(is.na(npv)==F) %>% 
  select(x,y,hydro_year,npv) %>% 
  group_by(x,y,hydro_year) %>% 
  filter(npv == max(npv)) %>% 
  ungroup() %>% 
  as_tibble()

q_npv <- dat[epoch=='modis'][is.na(npv)==F] %>% 
  .[,.SD[npv==min(npv)],keyby=.(x,y,hydro_year)]

dat %>% lazy_dt() %>% group_by(x,y) %>% filter(npv == min(npv)) %>% show_query()

inner_join(q_npv,kop,by=c('x','y')) %>% 
  ggplot(data=., aes(hydro_year, npv, color=zone))+
  geom_smooth()+
  facet_wrap(~zone,scales = 'free')


o %>% filter(ndvi_u_1==min(ndvi_u_1))


o$delta_x %>% hist


o %>% 
  filter(delta_x > 0) %>% 
  filter(ndvi_u_2 < ndvi_u_1) %>% 
  # ggplot(aes(x,y,fill=ndvi_u_1 - ndvi_u_2))+
  ggplot(aes(x,y,fill=ppet_2 - ppet_1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_viridis_c(limits=c(0,0.1))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)



dat[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>%
    .[ndvi_hyb > 0] %>% 
    .[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(ndvi_hyb, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F]
dat$hydro_year %>% is.na %>% table
dat[is.na(hydro_year)==T]$date
# %>% 
#     .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
#       by=.(x,y,season)]
# )


year(ymd("1983-12-01")+months(1))

dat[date==ymd("1983-12-01")]$hydro_year



o %>%   
  # mutate(season=factor(o$season, levels=c("SON","DJF","MAM","JJA"),ordered = T)) %>% 
  mutate(date_d = cut_width(date, n=7)) %>% pull(date_d) %>% unique
  ggplot(data=., aes(date, ndvi_hyb, color=date_d))+
  geom_smooth(method='lm', se=F)+
  scale_x_date(expand=c(0,0))+
  scale_color_viridis_c(option='B',end=0.9)+
  labs(x=NULL, y="NDVI")+
  facet_grid(cz~season, scales = 'free_y', 
             # labeller = label_wrap_gen(width=10, multi_line = TRUE)
             labeller = labeller(cz = lut_kop)
  )+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        legend.position = 'bottom',
        strip.text = element_text(face='bold'))


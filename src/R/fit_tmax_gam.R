
test_dat <- tmp[hydro_year==2013][season=='DJF'][sample(.N,.N*0.1)]
fn_bam_tmax <- function(dat){
  b  <- mgcv::bam(ndvi_3mo~
              s(x,y)+
              s(mape, k=5)+
              s(precip_anom_12mo,bs='cr')+
              s(tmax_3mo, bs='cr'),
            # s(pe_12mo,fac_hy,bs=c('fs'),xt='gp',
            #   m=c(3,0.5), k=5), 
            data=dat[sample(.N, .N*0.2)], 
            select=TRUE, discrete = T)
  ev <- gratia::evaluate_smooth(object = b, smooth='s(tmax_3mo)') %>% 
    as.data.table()
  # ev$hydro_year <- (unique(test_dat[,.(hydro_year, season)]))$hydro_year
  # ev$season <- (unique(test_dat[,.(hydro_year, season)]))$season
  return(ev)
}
plot(b,select = 2, shade = T,scale=0)
summary(b)
evaluate_smooth(b, "s(tmax_3mo)")


topt_dat <- tmp[hydro_year %in% c(1983:2019)] %>% 
  .[season=="DJF"] %>%
  .[mape < 5] %>% 
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_hyb>0] %>%
  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  .[,fn_bam_tmax(.SD),by=.(hydro_year)]
topt_dat[,1:5] %>% 
  as_tibble() %>% 
  filter(tmax_3mo < 35) %>%
  group_by(hydro_year) %>% 
  filter(est==max(est)) %>% 
  ggplot(data=., aes(hydro_year, tmax_3mo))+geom_point()+
  geom_smooth(method='lm')

topt_dat %>% 
  # filter(mape < 5) %>% 
  ggplot(data=., aes(tmax_3mo, est,color=hydro_year, group=hydro_year))+
  geom_line()+
  scale_color_viridis_c()+
  facet_wrap(~hydro_year)


b  <- mgcv::bam(ndvi_3mo~
                  s(x,y)+
                  s(mape, k=5)+
                  s(precip_anom_12mo,bs='cr')+
                  s(tmax_3mo, bs='cr'),
                # s(pe_12mo,fac_hy,bs=c('fs'),xt='gp',
                #   m=c(3,0.5), k=5), 
                data=tmp[hydro_year==1985&season=="DJF"] %>% 
                  .[season=="DJF"] %>%
                  .[mape < 5] %>% 
                  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
                  .[ndvi_hyb>0] %>%
                  .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
                  .[is.na(veg_class)==F] %>% 
                  .[sample(.N, .N*0.2)],
                select=TRUE, discrete = T)
                # knots = list(tmax_3mo=c(10,15,25,30,35,40)),
                # knots = list(tmax_3mo=seq(10,45,length.out=3)),
                # select=TRUE, discrete = T)
plot(b,pages = 1, scale=0)
b$smooth[[3]] %>% str
b$smooth[[3]]$bs.dim

tmp[season=='DJF']$tmax_3mo %>% hist



vec_id <- unique(tmp$id)


tmp[,.SD[sample(.N, min(3,.N))],by = vc]

# sample 20 grid cells from each VC
vec_id <- tmp[, sample(id, 500), by=vc]$V1 

# subset to get full time series of selected grid cells
dat_t <- tmp[id %in% vec_id] %>% 
  .[season=="DJF"] %>%
  .[mape < 5] %>% 
  .[ndvi_anom_sd >= -3.5 & ndvi_anom_sd <= 3.5] %>% 
  .[ndvi_hyb>0] %>%
  .[veg_class %in% c(2,3,4,5,11)] %>% # just the dominant Euc classes
  # .[!veg_class %in% c(6,10,12,13,30,31,32)] %>%
  .[is.na(veg_class)==F] %>% 
  .[,`:=`(idf=as_factor(id))]

# Fit relatively simple tmax model conditioned on precip anom
bb  <- mgcv::bam(ndvi_3mo~
                  s(mape,bs='ts',k=4)+
                  s(precip_anom_12mo, by=vc, 
                    bs='ts',k=4, m=c(4,3,2,1,0))+
                  s(tmax_3mo, by=vc, 
                    bs='ts',k=4, m=c(4,3,2,1,0)),
                data=dat_t,
                select=TRUE, discrete = T)
summary(bb)
bb_tp  <- mgcv::bam(ndvi_3mo~
                   s(precip_anom_12mo, by=vc)+
                   s(tmax_3mo, by=vc),
                 data=dat_t,
                 select=F, discrete = F)

draw(bb, select = 1)

library(patchwork)
(evaluate_smooth(bb, "tmax_3mo") %>% 
    ggplot(., aes(x = tmax_3mo, y = est, colour = vc)) + geom_line()+
    scale_y_continuous(limits=c(-1.5,1.5))+
    theme(legend.position = 'none'))+
  (evaluate_smooth(bb_tp, "tmax_3mo") %>% 
     ggplot(., aes(x = tmax_3mo, y = est, colour = vc)) + geom_line()+
     scale_y_continuous(limits=c(-1.5,1.5))+
     theme(legend.position = 'none'))


(evaluate_smooth(bb, "precip_anom_12mo") %>% 
    ggplot(., aes(x = precip_anom_12mo, y = est, colour = vc)) + geom_line())+
  (evaluate_smooth(bb_tp, "precip_anom_12mo") %>% 
     ggplot(., aes(x = precip_anom_12mo, y = est, colour = vc)) + geom_line())


summary(bb)
plot(bb, pages=1)
getViz(bb) %>% plot

plot(getViz(bb))
draw(bb)
evaluate_smooth(bb, "tmax_3mo") %>% 
 ggplot(., aes(x = tmax_3mo, y = est, colour = vc)) + geom_line()



dat_t[,.(vc,veg_class)] %>% unique

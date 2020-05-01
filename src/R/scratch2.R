o <- tmp[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
    .("ape" = sum(pe)),
    by=.(x,y,year)][,.("mape"=mean(ape), 
                       "ape_sd"=sd(ape)),by=.(x,y)]

norms_mape$mape %>% hist
tmp$pe %>% hist

norms_mape %>% 
  ggplot(data=., aes(x,y,fill=mape))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(direction=-1)
norms_map %>% 
  ggplot(data=., aes(x,y,fill=map))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(direction=-1)
norms_mapet %>% 
  ggplot(data=., aes(x,y,fill=mapet))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(direction=-1)

norms[month==min(month)] %>% 
  .[,mape2:=map/mapet] %>% 
  ggplot(data=., aes(x,y,fill=mape))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(direction=-1)

tmp[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
    .("ape" = mean(pe)),
    by=.(x,y,year)][year %in% c(1983,1990,2000,2019)] %>% 
  ggplot(data=., aes(x,y,fill=ape))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(direction=-1, limits=c(0,5),na.value = 'red')+
  facet_wrap(~year)



tmp[,.(nobs=sum(is.na(nirv)==F)),by=.(date)] %>% 
  ggplot(data=., aes(date,nobs))+geom_line()


# Mean Annual NIR-V 
tmp[date>=ymd("1982-01-01") & date<= ymd("2011-12-31")] %>% 
  .[,.(nirv=mean(nirv,na.rm = TRUE)),by=.(x,y)] %>% 
  as_tibble() %>% 
  ggplot(data=., aes(x,y,fill=nirv))+
  geom_sf(data=oz_poly, inherit.aes = F)+
  geom_tile()+
  scale_fill_gradientn("",
                       colors = rev(c("#134940", "#175c42", "#19713e", "#1b8531", "#1e9a1d", "#4fa939", "#7cb856", "#a3c674", "#c3d492", "#dee1b0", "#ede9cf")),
                       # limits=c(0,0.25),
                       oob=scales::squish)+
  labs(x=NULL,y=NULL,title = expression(paste(NIR[V])))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  f1_theme

tmp[date>=ymd("2000-01-01") & date<= ymd("2000-12-31")] %>%
  .[is.na(nirv_u)==F] %>% 
  .[,.(nirv_max=max(nirv_u,na.rm = TRUE), 
       nirv_min=min(nirv_u,na.rm=TRUE)),by=.(x,y)] %>% 
  as_tibble() %>% 
  ggplot(data=., aes(x,y,fill=nirv_max-nirv_min))+
  geom_sf(data=oz_poly, inherit.aes = F)+
  geom_tile()+
  scale_fill_viridis_c(option='A',limits=c(0,0.15),oob=scales::squish)+
  # scale_fill_gradientn("",
  #                      colors = rev(c("#134940", "#175c42", "#19713e", "#1b8531", "#1e9a1d", "#4fa939", "#7cb856", "#a3c674", "#c3d492", "#dee1b0", "#ede9cf")),
  #                      # limits=c(0,0.25),
  #                      oob=scales::squish)+
  labs(x=NULL,y=NULL,title = expression(paste(NIR[V])))+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  f1_theme


tmp <- tmp %>% .[,.(id=paste0(x,"_",y))]
vec_id <- tmp[date==ymd('2000-01-01')] %>% select(id) %>% distinct()


tmp[,.(tmax=mean(tmax,na.rm=TRUE)),by=(year)] %>% arrange(tmax)
tmp[,.(precip_anom_12mo=mean(precip_anom_12mo,na.rm=TRUE)),by=(year)] %>% arrange(precip_anom_12mo)
tmp[,.(pet_anom_12mo=mean(pet_anom_12mo,na.rm=TRUE)),by=(year)] %>% arrange(pet_anom_12mo)

cut(-40:-10, breaks=seq(-40,-10,by=5),
    # labels = 'a', 
    include.lowest = T, ordered_result = T)


tmp[year %in% c(1983, 1992, 2003, 2018,2019)] %>% 
   # .[month == 12] %>% 
   .[id %in% sample.int(60000,1e3)] %>% 
   .[,`:=`(lat = cut(y, breaks=seq(-40,-10,by=5), 
             # labels = 'a',  
             include.lowest = T, ordered_result = F), 
           lon = cut(x, breaks=seq(135,155,by=5),
                     include.lowest = T, ordered_result = F))] %>% 
  .[,.(precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE), 
       pet_anom_12mo = mean(pet_anom_12mo,na.rm=TRUE)), by=.(id,lat,lon,year)] %>% 
  as_tibble() %>% 
  filter(is.na(lat)==F) %>% 
  ggplot(data=., aes(precip_anom_12mo, pet_anom_12mo,
                     color=as.factor(year),
                     group=as.factor(id)))+
  geom_point(alpha=0.4,fill=NA)+
  geom_path(inherit.aes = F, 
            aes(precip_anom_12mo, 
                pet_anom_12mo,group=id),
            alpha=0.1,lty=1,lwd=0.3)+
  scale_color_viridis_d(end=0.8)+
  facet_grid(as.factor(lat)~as.factor(lon), drop = TRUE)+
  theme_linedraw()+
  theme(panel.grid = element_blank())
  # ggpointdensity::geom_pointdensity()+
  # scale_color_viridis_c()



library(tidyverse); library(sf)
oz_poly <- sf::read_sf("../../../data_general/GADM/gadm36_AUS.gpkg", 
                                             layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)

oz_outline <- ggplot()+
  geom_sf(data=oz_poly)+
  coord_sf(xlim=c(135,153),ylim=c(-42,-12))+
  theme_void()



p <- tmp[year %in% c(1990:2019)] %>% 
  .[id %in% sample.int(60000,1e4)] %>% 
  .[,`:=`(lat = cut(y, breaks=seq(-40,-10,by=5), 
                    include.lowest = T, ordered_result = F), 
          lon = cut(x, breaks=seq(135,155,by=5),
                    include.lowest = T, ordered_result = F))] %>% 
  .[,.(precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE), 
       pet_anom_12mo = mean(pet_anom_12mo,na.rm=TRUE)), by=.(id,lat,lon,year)] %>% 
  as_tibble() %>% 
  filter(is.na(lat)==F) %>% 
  ggplot(data=., aes(year,precip_anom_12mo  # precip_anom_12mo, pet_anom_12mo,
                     # color=as.factor(year),
                     # group=as.factor(id))
         ))+
  # geom_point(alpha=0.1,fill=NA)+
  geom_violin(aes(group=factor(year)))+
  # geom_path(inherit.aes = F, 
  #           aes(precip_anom_12mo, 
  #               pet_anom_12mo,group=id),
  #           alpha=0.1,lty=1,lwd=0.3)+
  geom_hline(aes(yintercept=0),color='red')+
  geom_smooth(method='lm',se=F 
              # aes(color='regression')
              )+
  scale_color_viridis_d(end=0.8)+
  scale_x_continuous(breaks = seq(1990,2020,by=5))+
  facet_grid(lat~lon,scales = 'free')+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        rect = element_rect(fill = "transparent") # all rectangles
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

library(cowplot)
ggsave(oz_outline,filename = "figures/oz_outline.png",width = 4, height = 8)
ggsave(p, filename = "figures/test_ts.png",width = 4,height = 8,bg='transparent')
i1 <- magick::image_read("figures/oz_outline.png")
image_info(i1)
i1 <- image_scale(i1,1300)
i2 <- magick::image_read("figures/test_ts.png")
image_info(i2)
library(magick)
image_mosaic(c(i1,i2))


ggplot(mtcars, aes(cyl, mpg)) +
  geom_violin(aes(colour = factor(cyl)), size = 2) +
  geom_jitter(width = 0.5) +
  geom_smooth(method = "lm", aes(color = "Regression"), size = 2) +
  scale_x_continuous(breaks = c(4,6,8))




qdat %>% sample_n(1e3) %>% ggplot(data=., aes(ap_sd, apet_sd))+geom_point()

qdat <- tmp %>% sample_n(100000) %>% 
  filter(is.na(vc)==F) %>% 
  filter(is.na(nirv_anom_sd)==F & 
           between(nirv_anom_sd, -3.5, 3.5)) %>% 
  filter(between(precip_anom_sd, -5,5))
# f0 <- bam(nirv~ 
#             s(x,y,by=precip_12mo, fx = TRUE)+
#             s(vc, bs='re') +
#             s(map, mapet, ap_sd, k=5)+
#             s(pet_anom_12mo, by=vc, k=5)+
#             s(precip_anom_12mo, by=vc, k=5), 
#             # s(vc, bs='re'), 
#           select=TRUE, discrete = TRUE, method='fREML', 
#           nthreads = 6, 
#           data=qdat)

f0 <- bam(nirv~ 
            s(x,y,by=pet_12mo, fx = TRUE)+
            s(x,y,by=precip_12mo, fx = TRUE)+
            s(vc, bs='re') +
            s(mape, ape_sd, k=5), 
          # s(vc, bs='re'), 
          select=TRUE, discrete = TRUE, method='fREML', 
          nthreads = 6, 
          data=qdat)

f0 %>% summary
print(plot(getViz(f0), allTerms = TRUE), pages=5)
plot(getViz(f0), allTerms = TRUE,select = 3)
  # coord_equal()+
  # scale_fill_gradient2()
  # geom_vline(aes(xintercept=0))
plot.gam(f0, select=1, rug=F)
plot.gam(f0, select=2, rug=F)



qdat <- tmp %>% sample_n(100000) %>% 
  filter(is.na(vc)==F) %>% 
  filter(is.na(nirv_anom_sd)==F & 
           between(nirv_anom_sd, -3.5, 3.5)) %>% 
  filter(between(precip_anom_sd, -5,5))
test <- tmp %>% sample_n(100000) %>% 
  filter(is.na(vc)==F) %>% 
  filter(is.na(nirv_anom_sd)==F & 
           between(nirv_anom_sd, -3.5, 3.5)) %>% 
  filter(between(precip_anom_sd, -5,5))
f_climatology <- bam(nirv_u~ 
            te(x,y,month, bs=c("tp","tp","cc"))+
            s(vc, bs='re') +
            s(map,mapet,k=5)+
            s(ap_sd, apet_sd,k=5)+
            s(precip_u, precip_sd, k=5)+
            s(pet_u, pet_sd, k=5)
            # s(mape, ape_sd, k=5)
            # s(map,mapet,ap_sd,k=5)
            , 
          # s(vc, bs='re'), 
          select=TRUE, discrete = TRUE, method='fREML', 
          nthreads = 6, 
          data=qdat)
f_climatology %>% summary
test %>% 
  mutate(pred = predict(f_climatology, newdata=.)) %>% 
  drop_na(c("pred","nirv_u")) %>% 
  # select(pred,nirv) %>% 
  as_tibble() %>% 
  # group_by(vc, year) %>% 
  summarize(r2 = cor(pred, nirv_u)**2, 
            rmse = sqrt(mean((pred-nirv_u)**2))) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year, rmse,color=vc))+
  geom_point()+
  geom_smooth(method='lm',se=F)
# b <- getViz(f_climatology)
# plotSlice(sm(b,1), 
#           fix=list("x"=c(140,145)))+
#   l_fitRaster()+
#   l_fitContour()




test %>% 
  mutate(pred = predict(f_climatology, newdata=.)) %>% 
  drop_na(c("pred","nirv_u")) %>% 
  # select(pred,nirv) %>% 
  as_tibble() %>% 
  group_by(vc) %>%
  summarize(manirv = mean(nirv_u), 
            range_nirv = diff(range(nirv_u)),
            r2 = cor(pred, nirv_u)**2, 
            rmse = sqrt(mean((pred-nirv_u)**2))) %>% 
  ungroup() %>% 
  knitr::kable(col.names = 
                 c("NVIS Vegetation Class","Mean NIRV","Range NIRV","R2","RMSE"), 
               format = "markdown", digits = c(2), 
               caption = "Predictability of seasonal NIR-V")





tmp[,.(tmax=mean(tmax,na.rm=TRUE)),by=(year)] %>% arrange(tmax)
tmp[month==12][,.(precip=mean(precip_12mo,na.rm=TRUE)),by=(year)] %>% arrange(precip)

tmp[,.(precip_anom_12mo=mean(precip_anom_12mo,na.rm=TRUE)),by=(year)] %>% arrange(precip_anom_12mo)
tmp[,.(pet_anom_12mo=mean(pet_anom_12mo,na.rm=TRUE)),by=(year)] %>% arrange(pet_anom_12mo)


tmp[year %in% c(1983, 1992, 2002,2003, 2018,2019)] %>% 
  .[month == 12] %>%
  .[id %in% sample.int(60000,1e3)] %>% 
  .[,`:=`(lat = cut(y, breaks=seq(-40,-10,by=5), 
                    # labels = 'a',  
                    include.lowest = T, ordered_result = F), 
          lon = cut(x, breaks=seq(135,155,by=5),
                    include.lowest = T, ordered_result = F))] %>% 
  .[,.(precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE), 
       pet_anom_12mo = mean(pet_anom_12mo,na.rm=TRUE)), by=.(id,lat,lon,year)] %>% 
  as_tibble() %>% 
  filter(is.na(lat)==F) %>% 
  ggplot(data=., aes(precip_anom_12mo, pet_anom_12mo,
                     color=as.factor(year),
                     group=as.factor(id)))+
  geom_hline(aes(yintercept=0),color='gray')+
  geom_vline(aes(xintercept=0),color='gray')+
  geom_point(alpha=0.4,fill=NA)+
  geom_path(inherit.aes = F, 
            aes(precip_anom_12mo, 
                pet_anom_12mo,group=id),
            alpha=0.1,lty=1,lwd=0.3)+
  scale_color_viridis_d(end=0.8)+
  facet_grid(as.factor(lat)~as.factor(lon), drop = TRUE)+
  theme_linedraw()+
  theme(panel.grid = element_blank())


tmp[year %in% c(1983, 1992, 2002,2003, 2018,2019)] %>% 
  .[month == 12] %>%
  .[id %in% sample.int(60000,1e3)] %>% 
  # .[,`:=`(lat = cut(y, breaks=seq(-40,-10,by=5), 
  #                   # labels = 'a',  
  #                   include.lowest = T, ordered_result = F), 
  #         lon = cut(x, breaks=seq(135,155,by=5),
  #                   include.lowest = T, ordered_result = F))] %>% 
  .[,.(precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE), 
       pet_anom_12mo = mean(pet_anom_12mo,na.rm=TRUE)), by=.(id,vc,year)] %>% 
  as_tibble() %>% 
  # filter(is.na(lat)==F) %>% 
  ggplot(data=., aes(precip_anom_12mo, pet_anom_12mo,
                     color=as.factor(year),
                     group=as.factor(id)))+
  geom_hline(aes(yintercept=0),color='gray')+
  geom_vline(aes(xintercept=0),color='gray')+
  geom_point(alpha=0.4,fill=NA)+
  geom_path(inherit.aes = F, 
            aes(precip_anom_12mo, 
                pet_anom_12mo,group=id),
            alpha=0.1,lty=1,lwd=0.3)+
  scale_color_viridis_d(end=0.8)+
  facet_wrap(~vc, drop = TRUE)+
  theme_linedraw()+
  theme(panel.grid = element_blank())




tmp[year %in% c(1983, 1992, 2002,2003, 2018,2019)] %>% 
  .[vc == "Eucalypt Tall Open Forests"] %>% 
  .[month == 12] %>%
  # .[id %in% sample.int(100000,1e3)] %>% 
  .[,.(precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE), 
       pet_anom_12mo = mean(pet_anom_12mo,na.rm=TRUE), 
       p_05 = quantile(precip_anom_12mo ,0.05,na.rm=TRUE), 
       p_95 = quantile(precip_anom_12mo, 0.95,na.rm=TRUE),
       tmax_anom_12mo = mean(tmax_anom_12mo,na.rm=TRUE), 
       # tmax_anom_12mo = mean(tmax_anom_12mo,na.rm=TRUE), 
       tmax_05 = quantile(tmax_anom_12mo ,0.05,na.rm=TRUE), 
       tmax_95 = quantile(tmax_anom_12mo, 0.95,na.rm=TRUE),
       pet_05 = quantile(pet_anom_12mo, 0.05,na.rm=TRUE), 
       pet_95 = quantile(pet_anom_12mo, 0.95,na.rm=TRUE),
       p_sd = sd(precip_anom_12mo,na.rm=TRUE), 
       pet_sd = sd(pet_anom_12mo,na.rm=TRUE)), by=.(vc,year)] %>% 
  as_tibble() %>% 
  ggplot(data=., aes(precip_anom_12mo, tmax_anom_12mo,
                     color=as.factor(year)))+
  geom_hline(aes(yintercept=0),color='gray')+
  geom_vline(aes(xintercept=0),color='gray')+
  geom_point(alpha=1,fill=NA)+
  geom_errorbar(aes(xmin=p_05, 
                    xmax=p_95))+
  geom_errorbar(aes( 
    ymin=tmax_05, 
    ymax=tmax_95))+
  # geom_errorbar(aes(xmin=precip_anom_12mo - p_sd, 
  #                   xmax=precip_anom_12mo + p_sd))+
  # geom_errorbar(aes( 
  #                   ymin=pet_anom_12mo - pet_sd, 
  #                   ymax=pet_anom_12mo + pet_sd))+
  geom_path(inherit.aes = F, 
            aes(precip_anom_12mo, 
                tmax_anom_12mo,group=vc),
            alpha=0.4,lty=1,lwd=0.3)+
  scale_color_viridis_d(end=0.8)+
  facet_wrap(~vc, drop = TRUE)+
  theme_linedraw()+
  theme(panel.grid = element_blank())




tmp %>% #[year %in% c(1983, 1992, 2002,2003, 2018,2019)] %>% 
  .[vc == "Eucalypt Tall Open Forests"] %>% 
  .[month == 12] %>%
  # .[id %in% sample.int(100000,1e3)] %>% 
  .[,.(precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE), 
       pet_anom_12mo = mean(pet_anom_12mo,na.rm=TRUE), 
       p_05 = quantile(precip_anom_12mo ,0.05,na.rm=TRUE), 
       p_95 = quantile(precip_anom_12mo, 0.95,na.rm=TRUE),
       tmax_anom_12mo = mean(tmax_anom_12mo,na.rm=TRUE), 
       # tmax_anom_12mo = mean(tmax_anom_12mo,na.rm=TRUE), 
       tmax_05 = quantile(tmax_anom_12mo ,0.05,na.rm=TRUE), 
       tmax_95 = quantile(tmax_anom_12mo, 0.95,na.rm=TRUE),
       pet_05 = quantile(pet_anom_12mo, 0.05,na.rm=TRUE), 
       pet_95 = quantile(pet_anom_12mo, 0.95,na.rm=TRUE),
       p_sd = sd(precip_anom_12mo,na.rm=TRUE), 
       pet_sd = sd(pet_anom_12mo,na.rm=TRUE)), by=.(vc,year)] %>% 
  as_tibble() %>% 
  ggplot(data=., aes(precip_anom_12mo, tmax_anom_12mo,
                     color=year))+
  geom_hline(aes(yintercept=0),color='gray')+
  geom_vline(aes(xintercept=0),color='gray')+
  geom_point(alpha=1,fill=NA)+
  geom_errorbar(aes(xmin=p_05, 
                    xmax=p_95),lwd=0.25)+
  geom_errorbar(aes( 
    ymin=tmax_05, 
    ymax=tmax_95),lwd=0.25)+
  # geom_errorbar(aes(xmin=precip_anom_12mo - p_sd, 
  #                   xmax=precip_anom_12mo + p_sd))+
  # geom_errorbar(aes( 
  #                   ymin=pet_anom_12mo - pet_sd, 
  #                   ymax=pet_anom_12mo + pet_sd))+
  geom_path(inherit.aes = F,
            aes(precip_anom_12mo,
                tmax_anom_12mo),
            alpha=0.4,lty=1,lwd=0.3)+
  scale_color_viridis_c(end=0.9)+
  facet_wrap(~vc, drop = TRUE)+
  theme_linedraw()+
  theme(panel.grid = element_blank())



tmp[year %in% c(1983, 1992, 2002,2003, 2018,2019)] %>% 
  .[vc %in% c(
    "Acacia Forests and Woodlands",
    "Callitris Forests and Woodlands",
    "Casuarina Forests and Woodlands",
    "Eucalypt Open Forests",
    "Eucalypt Woodlands",
    "Eucalypt Open Woodlands",
    "Eucalypt Tall Open Forests")] %>%
  # .[month == 8] %>%
  .[,.(
       precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE), 
       pet_anom_12mo = mean(pet_anom_12mo,na.rm=TRUE), 
       p_05 = quantile(precip_anom_12mo ,0.05,na.rm=TRUE), 
       p_95 = quantile(precip_anom_12mo, 0.95,na.rm=TRUE),
       tmax_anom_12mo = mean(tmax_anom_12mo,na.rm=TRUE), 
       # tmax_anom_12mo = mean(tmax_anom_12mo,na.rm=TRUE), 
       tmax_05 = quantile(tmax_anom_12mo ,0.05,na.rm=TRUE), 
       tmax_95 = quantile(tmax_anom_12mo, 0.95,na.rm=TRUE),
       pet_05 = quantile(pet_anom_12mo, 0.05,na.rm=TRUE), 
       pet_95 = quantile(pet_anom_12mo, 0.95,na.rm=TRUE),
       p_sd = sd(precip_anom_12mo,na.rm=TRUE), 
       pet_sd = sd(pet_anom_12mo,na.rm=TRUE)), by=.(vc,month,year)] %>% 
  as_tibble() %>% 
  ggplot(data=., aes(year, tmax_anom_12mo,color=as.factor(month)))+
  geom_line()+
  scale_color_viridis_d()+
  facet_wrap(~vc)
  

tmp[id==1000 & year %in% 2010:2019] %>% 
  as_tibble() %>% 
  arrange(date) %>% 
  mutate(alt_p12mo = RcppRoll::roll_sumr(precip,n = 12,fill=NA)) %>% 
  ggplot(data=., aes(date, precip_12mo))+
  geom_line(aes(date,alt_p12mo),color='blue',lwd=2)+
  geom_line()+
  geom_line(aes(date, precip_anom_12mo+tmax_u),color='red')

tmp[id==1000 & year %in% 2010:2019] %>% 
  as_tibble() %>% 
  arrange(date) %>% 
  mutate(alt_tmax12mo = RcppRoll::roll_maxr(tmax,n = 12,fill=NA)) %>% 
  ggplot(data=., aes(date, tmax))+
  geom_line()+
  geom_line(aes(date, alt_tmax12mo),color='red')+
  geom_line(aes(date, tmax_12mo), color='blue')+
  geom_line(aes(date, tmax_u+tmax_anom),col='green')+
  geom_line(aes(date, tmax_anom_12mo+37),col='green')




tmp[year %in% c(1992, 2003, 2018,2019)] %>% 
  .[month == 11] %>%
  # .[id %in% sample.int(60000,1e3)] %>% 
  .[,`:=`(lat = cut(y, breaks=seq(-40,-10,by=5), 
                    # labels = 'a',  
                    include.lowest = T, ordered_result = T), 
          lon = cut(x, breaks=seq(135,155,by=5),
                    include.lowest = T, ordered_result = T))] %>% 
  .[,.(precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE), 
       pet_anom_12mo = mean(pet_anom_12mo,na.rm=TRUE), 
       tmax_anom_12mo = mean(tmax_anom_12mo, na.rm=TRUE), 
       p_05 = quantile(precip_anom_12mo ,0.05,na.rm=TRUE), 
       p_95 = quantile(precip_anom_12mo, 0.95,na.rm=TRUE),
       tmax_05 = quantile(tmax_anom_12mo ,0.05,na.rm=TRUE), 
       tmax_95 = quantile(tmax_anom_12mo, 0.95,na.rm=TRUE)),
    by=.(year)] %>% 
  as_tibble() %>% 
  # filter(is.na(lat)==F) %>% 
  ggplot(data=., aes(precip_anom_12mo, tmax_anom_12mo,
                     color=as.factor(year)))+
  geom_hline(aes(yintercept=0),color='gray')+
  geom_vline(aes(xintercept=0),color='gray')+
  geom_point(alpha=0.4,fill=NA)+
  geom_errorbar(aes(xmin=p_05, 
                    xmax=p_95))+
  geom_errorbar(aes( 
    ymin=tmax_05, 
    ymax=tmax_95))+
  geom_path(inherit.aes = F,
            aes(precip_anom_12mo,
                tmax_anom_12mo),
            alpha=0.5,lty=1,lwd=0.3)+
  scale_color_viridis_d("", 
                        end=0.8, option='A', direction = -1)+
  # facet_grid(as.factor(lat)~as.factor(lon), drop = TRUE)+
  labs(x=expression(paste("Precip. Anomaly"["12-month"]~(mm~yr**-1))), 
       y=expression(paste("Max. Monthly Temp. Anom."~(degree*C))))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        legend.position = c(0.1,0.8), 
        legend.key = element_rect(fill='transparent'),
        legend.background = element_rect(fill='transparent')
        # panel.background = element_rect(fill='#EEEEFF')
        )

cut_interval(rnorm(100), n = 3, center=0, ordered_factor=T)
"[-0.05,0.05]  (0.05,0.15]  (0.15,0.25]  (0.25,0.35]  (0.35,0.45]  (0.45,0.55]"

gsub(",+)","\\1-",levels(cut(140:155,breaks=5)))



vec_vc <- unique(tmp$vc) %>% sort
library(ggridges)
tmp[nirv_anom_sd >= -5 & nirv_anom_sd <= 5] %>%
  .[id %in% sample.int(60000, 10000)] %>% 
  ggplot(., aes(x = nirv_anom_sd, 
                y = as.factor(year), 
                fill=stat(x))) +
  geom_density_ridges_gradient(
    scale=3,
    rel_min_height=0.05,
    quantile_lines = TRUE, 
    quantiles = c(0.025, 0.975), alpha = 0.7)+
  geom_vline(aes(xintercept=0),color='grey')+
  scale_fill_viridis_c(expression(paste(sigma)), 
                       option='B')+
  scale_x_continuous(limits=c(-5,5))+
  labs(x=expression(paste(NIR[V]~Anomaly~(sigma))), 
       y=NULL)+
  theme_linedraw()+
  facet_wrap(~vc)


tmp[nirv_anom_sd >= -5 & nirv_anom_sd <= 5] %>% 
  .[(nirv_anom_sd==min(nirv_anom_sd,na.rm=TRUE)), by=.(x,y)]


tmp[,date==max(date), by=.(x,y)]


library(tidyverse); library(sf)
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
f1_theme <- theme(panel.background = element_rect(fill = '#99A3C4'), 
                  panel.grid = element_blank(), 
                  legend.position = 'bottom', 
                  axis.text = element_blank(), 
                  axis.ticks = element_blank())
tmp %>% 
  group_by(x,y) %>% 
  filter(is.na(nirv_anom_sd)==F) %>% 
  filter(nirv_anom_sd == min(nirv_anom_sd, na.rm = TRUE)) %>% 
  mutate(ddate=decimal_date(date)) %>% 
  ggplot(data=., aes(x,y,fill=ddate))+
  geom_sf(data=oz_poly, inherit.aes = F, 
          fill='gray',color='#828387')+
  geom_tile()+
  scale_fill_viridis_c("",
                       option='B', end=0.95)+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  f1_theme+
  theme(legend.position = 'right')



format(summary(f_climatology)$r.sq, digits = 2)



dim(tmp)


f_cwd_v5 <- function(cwd_et,precip,et,map){
  # No reset during the wettest month of the year
  for(i in seq(2,length(precip))){
    
    cwd_et[i] <-  min(0, cwd_et[i-1] + (precip[i]) - max(et[i],1, na.rm=T), na.rm=T)
    cwd_et[i] <- ifelse(cwd_et[i] < -3000, -3000, cwd_et[i])
    cwd_et[i] <- ifelse(precip[i] > 0.333*map[i], 0, cwd_et[i])
    
  }
  cwd_et
}



fn1 <- function(vi, d_threshold, r_threshold){
  # d_threshold 
  tsr <- rep(0,length(vi)) # time since recovery array
  vec_d <- vi<d_threshold
  vec_r <- vi>r_threshold
  for(i in seq(2,length(vi))){
   tsr[i] <- tsr[i-1] + vec_d[i]
   # tsr[i] <- ifelse(vi[i]>=r_threshold, 0, tsr[i]+1)
   tsr[i] <- base::ifelse(tsr[i] >=1 & vec_r[i]==F, tsr[i]+1, 0)
  }
  tsr
}
fn2 <- function(vi, d_threshold, r_threshold){
  # d_threshold 
  tsr <- rep(0,length(vi)) # time since recovery array
  vec_d <- vi<d_threshold
  vec_r <- vi>r_threshold
  vec_d[is.na(vec_d)==T] <- FALSE
  vec_r[is.na(vec_r)==T] <- FALSE
  for(i in seq(2,length(vi))){
    tsr[i] <- tsr[i-1] + vec_d[i]
    # tsr[i] <- ifelse(vi[i]>=r_threshold, 0, tsr[i]+1)
    tsr[i] <- ifelse(tsr[i] >=1 & vec_r[i]==F, tsr[i]+1, 0)
  }
  tsr
}
microbenchmark::microbenchmark(
fn1(vec, -2,2),
fn2(vec,-2,2),
times=1000
)
vec <- (rnorm(100,2.5*sin(seq(0,5*pi,length.out = 100))))
vec[sample.int(length(vec), 10)] <- NA
plot(vec,type='l',ylim=c(-5,35));abline(h=c(-2,2))
points(fn1(vec, -2,2),pch=20,col='blue')




fn_tsr <- function(vi, d_threshold, r_threshold){
  # vi: vegetation index
  # d_threshold: level of vegetation index to signal a disturbance
  # r_threshold: level of vegetation index to recover from a disturbance
  # Assumptions: Continuous time record of vi (no gaps)
  tsr <- rep(0,length(vi)) # time since recovery array
  vec_d <- vi<d_threshold
  vec_r <- vi>r_threshold
  vec_d[is.na(vec_d)==T] <- FALSE
  vec_r[is.na(vec_r)==T] <- FALSE
  for(i in seq(2,length(vi))){
    tsr[i] <- tsr[i-1] + vec_d[i]
    tsr[i] <- ifelse(tsr[i] >=1 & vec_r[i]==F, tsr[i]+1, 0)
  }
  tsr
}
plot(vec,type='l',ylim=c(-5,35));abline(h=c(-2,2))
points(fn_tsr(vec, -2,2),pch=20,col='blue')


vec_vc <- unique(tmp$vc)
vec_col <- RColorBrewer::brewer.pal(n=11, name='BrBG')
tmp[vc=="Eucalypt Tall Open Forests"] %>% 
  .[between(x,151,153.5) & between(y,-33.5,-30)] %>% 
  # .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>%
  as_tibble() %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(nirv_anom_sd,na.rm=TRUE)) %>% 
  ungroup() %>% # pull(val) %>% hist(100)
  # mutate(val = ifelse(year==2011,2.5,val)) %>%
  group_by(x,y) %>% 
  arrange(year) %>% 
  mutate(tsr = fn1(val, -2, 0)) %>% 
  ungroup() %>% #pull(tsr) %>% summary
  # filter(is.na(tsr)==T)
  # mutate(decade=case_when(between(year,1980,1989)~"1980s",
  #                         between(year,1990,1999)~"1990s",
  #                         between(year,2000,2009)~"2000s",
  #                         between(year,2010,2019)~"2010s")) %>% 
  # mutate(decade = fct_inorder(decade)) %>% 
  # mutate(year = fct_inorder(as.character(year))) %>% 
  ggplot(data=., aes(x,y,fill=val))+
  # geom_sf(data=oz_poly, inherit.aes = F, 
  #         fill='gray',color='#828387')+
  geom_tile()+
  coord_sf()+
  scale_fill_viridis_c("",
                       option='B', end=0.95,
                       limits=c(-0.001,3),na.value = 'blue'
  )+
  # scale_fill_gradient2(high=vec_col[11],mid = vec_col[6],low=vec_col[1],
  #                      limits=c(-3,3),
  #                      oob=scales::squish)+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(151,153.5),
           ylim = c(-33.5,-30), expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 4)+
  # f1_theme+
  theme(legend.position = 'right')



tmp[vc=="Eucalypt Tall Open Forests"] %>% 
  .[between(x,151,153.5) & between(y,-33.5,-30)] %>% 
  # .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>%
  as_tibble() %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(nirv_anom_sd,na.rm=TRUE), 
            nobs= sum(is.na(nirv_anom_sd)==F)) %>% 
  ungroup() %>% pull(nobs) %>% hist
  ggplot(data=., aes(x,y,fill=nobs))+
  geom_tile()+
  coord_sf()+
  scale_fill_viridis_c("",
                       option='B', end=0.95,
                       # limits=c(-0.001,3),
                       na.value = 'blue'
  )+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(151,153.5),
           ylim = c(-33.5,-30), expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 4)+
  f1_theme+
  theme(legend.position = 'right')
  

  
#################################################  
tmp[vc=="Eucalypt Tall Open Forests"] %>% 
    .[between(x,151,153.5) & between(y,-33.5,-30)] %>% 
    # .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>%
    as_tibble() %>% 
    mutate(nirv_anom_sd = ifelse(nirv_anom_sd >= 3.5 |
                                   nirv_anom_sd <= -3.5, NA, nirv_anom_sd)) %>% 
    group_by(id,date) %>% 
    summarize(val = mean(nirv_anom_sd,na.rm=TRUE), 
              nobs= sum(is.na(nirv_anom_sd)==F)) %>% 
    ungroup() %>% 
  ggplot(data=., aes(date,val,group=id))+
  geom_line(lwd=0.2,alpha=0.5)+
  geom_smooth(inherit.aes = F, aes(date,val))+
  geom_hline(aes(yintercept=c(-2)),color='red')+
  geom_hline(aes(yintercept=c(2)),color='red')

  
df_coords <- unique(tmp[,.(x,y)])  
write_csv(df_coords,paste0("data/East_Australia_ForestAVHRR_coords_",Sys.Date(),".csv"))






vec_vc <- unique(tmp$vc)
vec_col <- RColorBrewer::brewer.pal(n=11, name='BrBG')
tmp[vc=="Eucalypt Tall Open Forests"] %>% 
  .[between(x,151,153.5) & between(y,-33.5,-30)] %>% 
  # .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>%
  as_tibble() %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(nirv_anom_sd,na.rm=TRUE)) %>% 
  ungroup() %>% # pull(val) %>% hist(100)
  # mutate(val = ifelse(year==2011,2.5,val)) %>%
  group_by(x,y) %>% 
  arrange(year) %>% 
  mutate(tsr = fn_tsr(val, d_threshold = -2, r_threshold = 0)) %>% 
  ungroup() %>% #pull(tsr) %>% summary
  ggplot(data=., aes(x,y,fill=val))+
  geom_tile()+
  coord_sf()+
  # scale_fill_viridis_c("",
  #                      option='B', end=0.95,
  #                      limits=c(-0.001,3),na.value = 'blue'
  # )+
  scale_fill_gradient2(high=vec_col[11],mid = vec_col[6],low=vec_col[1],
                       limits=c(-3,3),
                       oob=scales::squish, na.value = 'red')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(151,153.5),
           ylim = c(-33.5,-30), expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 4)+
  blah_theme+
  theme(legend.position = 'right')





vec_vc <- unique(tmp$vc)
vec_col <- RColorBrewer::brewer.pal(n=11, name='BrBG')
tmp[vc=="Eucalypt Tall Open Forests"] %>% 
  .[between(x,151,153.5) & between(y,-33.5,-30)] %>% 
  .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>%
  as_tibble() %>% 
  group_by(x,y,year) %>% 
  summarize(val = diff(range(nirv_anom_sd,na.rm=TRUE))) %>% 
  ungroup() %>% # pull(val) %>% hist(100)
  ggplot(data=., aes(x,y,fill=val))+
  geom_tile()+
  coord_sf()+
  scale_fill_viridis_c("",
                       option='B', end=0.95,
                       # limits=c(-0.001,3),
                       na.value = 'blue'
  )+
  # scale_fill_gradient2(high=vec_col[11],mid = vec_col[6],low=vec_col[1],
  #                      limits=c(-3,3),
  #                      oob=scales::squish, na.value = 'red')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(151,153.5),
           ylim = c(-33.5,-30), expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 4)+
  blah_theme+
  theme(legend.position = 'right')


tmp[vc=="Eucalypt Tall Open Forests"] %>% 
  .[between(x,151,153.5) & between(y,-33.5,-30)] %>%
  .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>%
  as_tibble() %>% 
  group_by(x,y,year) %>% 
  summarize(hi_lo = diff(range(nirv_anom_sd,na.rm=TRUE)), 
            avg = mean(nirv_anom_sd,na.rm = TRUE), 
            vi_sd = sd(nirv_anom_sd,na.rm=TRUE)) %>% 
  ungroup() %>% # pull(val) %>% hist(100)
  group_by(x,y) %>% 
  summarize(beta = coef(lm(hi_lo~year))[2]) %>% 
  ungroup() %>% 
  ggplot(data=., aes(x,y,fill=beta))+
  geom_tile()+
  coord_sf()+
  # scale_fill_viridis_c("",
  #                      option='B', end=0.95,
  #                      # limits=c(-0.001,3),
  #                      na.value = 'blue'
  # )+
  scale_fill_gradient2(high=vec_col[11],mid = vec_col[6],low=vec_col[1],
                       # limits=c(-3,3),
                       oob=scales::squish, na.value = 'red')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(151,153.5),
           ylim = c(-33.5,-30), expand = FALSE)+
  blah_theme+
  theme(legend.position = 'right')


library(RcppArmadillo)
system.time(
tmp[vc=="Eucalypt Tall Open Forests"] %>% 
  # .[between(x,151,153.5) & between(y,-33.5,-30)] %>%
  .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,c("b0","b1") := as.list(
    unname(fastLm(nirv_anom_sd ~ year, data=.SD)$coefficients)),
    by=id] %>% 
  dim()
)

system.time(
  tmp[vc=="Eucalypt Tall Open Forests"] %>% 
    # .[between(x,151,153.5) & between(y,-33.5,-30)] %>%
    .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
    .[,c("b0","b1") := as.list(
      unname(fastLm(X = year, y = nirv_anom_sd, data=.SD)$coefficients)),
      by=id] %>% 
    dim()
)



tmp[vc=="Eucalypt Tall Open Forests"] %>% 
  .[between(x,151,153.5) & between(y,-33.5,-30)] %>%
  .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv_anom_sd, na.rm=TRUE)), by=.(x,y,year)] %>% 
  .[,c("b0","b1") := as.list(
  unname(fastLm(X = year, y = val, data=.SD)$coefficients)),
  by=.(x,y)] %>% 
  as_tibble() %>% 
  ggplot(data=., aes(x,y,fill=b1))+
  geom_tile()+
  coord_sf()+
  # scale_fill_viridis_c("",
  #                      option='B', end=0.95,
  #                      # limits=c(-0.001,3),
  #                      na.value = 'blue'
  # )+
  scale_fill_gradient2(high=vec_col[11],mid = vec_col[6],low=vec_col[1],
                       # limits=c(-3,3),
                       oob=scales::squish, na.value = 'red')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(151,153.5),
           ylim = c(-33.5,-30), expand = FALSE)+
  facet_wrap(~year)+
  blah_theme+
  theme(legend.position = 'right')



norms_tmax <- tmp[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
                  .(tmax_u = mean(tmax,na.rm=TRUE), 
                    tmax_sd = sd(tmax,na.rm=TRUE)),
                  by=.(x,y,month)]

tmp[vc=="Eucalypt Tall Open Forests"] %>% 
  .[between(x,151,153.5) & between(y,-33.5,-30)] %>%
  .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv_anom_sd, na.rm=TRUE)), by=.(x,y,year)] %>% 
  .[,c("b0","b1") := as.list(
    unname(fastLm(X = year, y = val, data=.SD)$coefficients)),
    by=.(x,y)] %>% 
  as_tibble()

system.time(
tmp[vc=="Eucalypt Tall Open Forests"] %>% 
  # .[between(x,151,153.5) & between(y,-33.5,-30)] %>%
  .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv_anom_sd, na.rm=TRUE)), by=.(x,y,year)] %>% 
  .[,.(b1 = fastLm(X = year, y = val, data=.SD)$coefficients), 
    by=.(x,y)]
)

jy <- rnorm(100); jx <- rnorm(100)
coef(fastLm(jy~jx))["jx"]
fastLm(X=jx,y=jy)$coefficients
fastLm(X=jx,y=jy)


system.time(
    tmp[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
    .[,.(val = mean(nirv_anom_sd, na.rm=TRUE)), by=.(x,y,year)] %>% 
    .[,.(b1 = fastLm(X = year, y = val, data=.SD)$coefficients), 
      by=.(x,y)]
)



vec_dates <- data.table(date=sort(unique(tmp$date))) %>% 
  .[,quarter:=quarter(date)] %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON")) %>% 
  mutate(season = factor(q,
                         levels=c("DJF","MAM","JJA","SON"), 
                         ordered=T)) %>% 
  select(date,season) %>% 
  mutate(hydro_year = year(date+months(1)))

tmp <- vec_dates[tmp,by=date]


vec_dates <- vec_dates %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON"))
vec_dates <- vec_dates %>% mutate(season = factor(q,
                                                  levels=c("DJF","MAM","JJA","SON"), 
                                                  ordered=T))
vec_dates <- vec_dates %>% 
  mutate(hydro_year = year(date+months(1)))
vec_dates <- vec_dates %>% select(date,season, hydro_year)

tmp[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(q = quarter(date))]
  

#*******************************************************************************
library(RcppArmadillo)
system.time(
  lt_season <- tmp[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
    .[,.(val = mean(nirv, na.rm=TRUE)), by=.(x,y,season,year)] %>% 
    .[,.(b1 = fastLm(X = year, y = val, data=.SD)$coefficients), 
      by=.(x,y,season)]
)

source("src/R/helper_funs_Oz_droughts.R")

library(sf)
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly,dTolerance = 0.05)


map_theme <- theme(panel.background = element_rect(fill = '#99A3C4'), 
                  panel.grid = element_blank(), 
                  legend.position = 'bottom', 
                  axis.text = element_blank(), 
                  axis.ticks = element_blank())

vec_col <- RColorBrewer::brewer.pal(n=11, name='BrBG')
lt_season %>% 
 ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_viridis_c(expression(paste(Delta*NIR[V]~yr^-1)),
                       option='D',direction = 1,
                       limits=c(-0.00001,0.00015),
                       oob=scales::squish, na.value = 'red')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  facet_wrap(~season, nrow = 1)+
  map_theme+
  theme(legend.position = 'bottom')
#*******************************************************************************
#*
#*
#*

lt_vc <- tmp[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv, na.rm=TRUE)), by=.(x,y,vc,year)] %>% 
  .[,.(trend = fastLm(X = year, y = val, data=.SD)$coefficients), 
    by=.(vc)]

lt_vc <- tmp[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv, na.rm=TRUE)), by=.(x,y,vc,year)] %>% 
  .[,.(trend = fastLm(val~year)$coefficients["year"]), 
    by=.(vc)]

lt_vc2 <- tmp[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv, na.rm=TRUE)), by=.(x,y,vc,year)] %>% 
  group_by(vc) %>% 
  summarize(trend = coef(lm(val~year))[2]) %>% 
  ungroup()

inner_join(lt_vc, lt_vc2, by='vc') %>% plot(trend.x~trend.y, data=.)

lt_season <- tmp[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv, na.rm=TRUE)), by=.(vc,year)] %>% 
  as_tibble() %>% 
  ggplot(data=., aes(year,val,color=vc))+
  geom_point()+
  geom_smooth(method='lm',se=F)


dt[,c("lm_b0","lm_b1") := as.list(
  unname(fastLm(Y ~ x, data=.SD)$coefficients))
  ,by = "user_id"]

sens_season <- tmp[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv, na.rm=TRUE),
       p = mean(precip_anom_12mo, na.rm=TRUE), 
       pet = mean(pet_anom_12mo, na.rm=TRUE)), by=.(x,y,hydro_year,season)] %>% 
  .[,c("b_0","b_p","b_pet") := as.list(
    unname(fastLm(val~p+pet)$coefficients)
  ),by=.(x,y,season)]
sens_season[hydro_year %in% c(1983,2019)] %>% 
  ggplot(data=., aes(x,y,fill=b_p))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(limits=c(-0.0001,0.0001))+
  facet_wrap(~season+hydro_year)


 
o <- tmp[between(x,151,153.5) & between(y,-33.5,-30)] %>%
  .[hydro_year>=1983] %>% 
  .[,.(val = mean(nirv, na.rm=TRUE),
       p = mean(precip_anom_12mo, na.rm=TRUE), 
       pet = mean(pet_anom_12mo, na.rm=TRUE)), by=.(x,y,hydro_year,season)]

o <- o[, c("b_0","b_p","b_pet") := 
    fastLm(val~p+pet)$coefficients[c("(Intercept)","p","pet")],by=c('x','y','season')]
o

o <- o[,c("b_0","b_p","b_pet") := as.list(
  unname(fastLm(val~p+pet)$coefficients)
  ),by=.(x,y,season)]

  
val <- rnorm(100); p <- rnorm(100); pet <- rnorm(100)
d <- data.table(val,p,pet)
fastLm(val~p+pet,data=d)$coefficients[c("(Intercept)","p")]
unname(fastLm(val~p+pet,data=d)$coefficients)

library(dtplyr)
jj <- tmp[between(x,151,153.5) & between(y,-33.5,-30)] %>%
  .[hydro_year>=1983] %>% 
  .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv, na.rm=TRUE),
       p = mean(precip_anom_12mo, na.rm=TRUE), 
       pet = mean(pet_anom_12mo, na.rm=TRUE)), by=.(x,y,hydro_year,season)] 
jj <- lazy_dt(jj)

jj %>% 
  group_by(x,y,season) %>% 
  summarize(beta = list(coef(lm(val~p)))) %>% 
  ungroup() %>% 
  show_query()





o <- tmp[between(x,151,153.5) & between(y,-33.5,-30)] %>%
  .[hydro_year>=1983] %>% 
  .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv, na.rm=TRUE),
       p = mean(precip_anom_12mo, na.rm=TRUE), 
       pet = mean(pet_anom_12mo, na.rm=TRUE)), by=.(x,y,hydro_year,season)] 
# oo <- o[,c("b_0","b_p","b_pet") := as.list(
#     unname(fastLm(val~p+pet)$coefficients)
#   ),keyby=.(x,y)]
oo <- o[, .(beta = list(unname(fastLm(val~p+pet)$coefficients))), keyby=.(x,y,season)]
ooo <- oo[1:3,][,c("b0","b1","b2") := unlist(beta,use.names = F), keyby=.(x,y,season)]
ooo

# mynames = c("Name1", "Longer%")
# x[ , (mynames) := list(mean(b) * 4, sum(b) * 3), by = a]
oo <- o[, (c("b0","b1","b2")) := list(unname(fastLm(val~p+pet)$coefficients)), 
        by=.(x,y,season)]
beta_names <- c("b0", "b1","b2")
oo <- o[ , .(betas := list(unname(fastLm(val~p+pet)$coefficients))), keyby=.(x,y,season)]
oo <- o[ , (beta_names) := list(fastLm(val~p+pet)$coefficients), keyby=.(x,y,season)]


oo <- o[, .(beta = list(unname(fastLm(val~p+pet)$coefficients))), keyby=.(x,y,season)]
ooo <- oo[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2], b2=unlist(beta)[3]), by=.(x,y,season)]
ooo


jj %>% 
  group_by(x,y,season) %>% 
  summarize(beta = coef(lm(val~p))[2]) %>% 
  ungroup() %>% 
  show_query()



# THIS SEEMS TO WORK!!! #######
o <- tmp[hydro_year>=1983] %>%   #[between(x,151,153.5) & between(y,-33.5,-30)] %>%
  .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv, na.rm=TRUE),
       p = mean(precip_anom_12mo, na.rm=TRUE), 
       pet = mean(pet_anom_12mo, na.rm=TRUE)), by=.(x,y,hydro_year,season)] 

system.time({
  oo <- o[, .(beta = list(unname(fastLm(val~p+pet)$coefficients))), keyby=.(x,y,season)]
  ooo <- oo[,`:=`(b0=unlist(beta)[1], b1=unlist(beta)[2], b2=unlist(beta)[3]), by=.(x,y,season)]
}
)



ooo %>% as_tibble() %>% 
  select(-beta) %>% 
  filter(between(b0,-1,1)) %>% 
  select(-b0) %>% 
  gather(-season, -x,-y, key='coef',value='estimate') %>% 
  ggplot(data=.,aes(x,y,fill=estimate))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(
    limits=c(-0.0001,0.0001), 
    na.value = '#faf3dc'
    )+
  facet_grid(rows = vars(coef), cols = vars(season))+
  theme(panel.background = element_rect(fill ='#565759'  #'#99A3C4'
                                        ), 
                     panel.grid = element_blank(), 
                     legend.position = 'bottom', 
                     axis.text = element_blank(), 
                     axis.ticks = element_blank())



ss <- tmp[hydro_year>=1983] %>%   #[between(x,151,153.5) & between(y,-33.5,-30)] %>%
  .[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>% 
  .[,.(val = mean(nirv_anom_sd, na.rm=TRUE),
       p = mean(precip_anom_12mo/ap_sd, na.rm=TRUE), 
       pet = mean(pet_anom_12mo/apet_sd, na.rm=TRUE)), 
    by=.(x,y,hydro_year,season)] 
system.time({
  sss <- ss[, .(beta = list(unname(fastLm(val~p+pet)$coefficients))), keyby=.(x,y,season)]
})
sss <- sss[,`:=`(Intercept=unlist(beta)[1], P=unlist(beta)[2], PET=unlist(beta)[3]), by=.(x,y,season)]

library(sf)
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- st_as_sf(oz_poly)
oz_poly <- st_simplify(oz_poly,dTolerance = 0.05)

sss %>% as_tibble() %>% 
  select(-beta) %>% 
  filter(between(Intercept,-1,1)) %>% 
  select(-Intercept) %>% 
  gather(-season, -x,-y, key='coef',value='estimate') %>% 
  ggplot(data=.,aes(x,y,fill=estimate))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_gradient2(expression(paste(beta)),
    limits=c(-1,1),oob=scales::squish,
    na.value = '#black'
  )+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  facet_grid(rows = vars(coef), cols = vars(season))+
  theme(panel.background = element_rect(fill ='gray50'  #'#99A3C4'
  ), 
  panel.grid = element_blank(), 
  legend.position = 'right', 
  axis.text = element_blank(), 
  axis.ticks = element_blank())







################################################################################
#!!! The following is for for a form of lagged variable GAM where the lagged
# covariates are organized into matrices. 
# This can/will def blow up the memory... even 64 GB...  
#*******************************************************************************
# Cast the variables to lagged matrices -----------------------------------
#*******************************************************************************
# Because this is so memory intensive I'm subsetting in time
tmp <- tmp[date >= ymd("1978-01-01")]

# precip anom lags
gc(reset = TRUE,full=T)
mat_p <- tmp[,.(x,y,date,precip_anom_sd)][order(x,y,date), c(paste0("precip_anom_sd_",1:13)) := shift(precip_anom_sd, 1:13) , .(x,y)][order(date)]
mat_p <- mat_p %>% rename(precip_anom_sd_0 = precip_anom_sd) %>% select(-x,-y,-date)
gc(verbose = T, reset = T, full = T)

# pet anom lags
gc(reset = TRUE,full=T)
mat_pet <- tmp[,.(x,y,date,pet_anom_sd)][order(x,y,date), c(paste0("pet_anom_sd_",1:13)) := shift(pet_anom_sd, 1:13) , .(x,y)][order(date)]
mat_pet <- mat_pet %>% rename(pet_anom_0 = pet_anom_sd) %>% select(-x,-y,-date)
gc(verbose = T, reset = T, full = T)

# P:PET anom lags
mat_pe <- tmp[,.(x,y,date,pe_anom_sd)][order(x,y,date), c(paste0("pe_anom_",1:13)) := shift(pe_anom, 1:13) , .(x,y)][order(date)]
mat_pe <- mat_pe %>% rename(pe_anom_0 = pe_anom_sd) %>% select(-x,-y,-date)
gc(verbose = T, reset = T, full = T)


lag_n <- 0:13 ## create time lag matrix...

tmp_mat <- t(matrix(lag_n,length(lag_n),length(tmp$x)))
tmp <- as_tibble(tmp)
tmp$lag_month <- tmp_mat # lag index is needed for GAM
# tmp <- tmp %>% rename(precip_0 = precip, 
#                     pet_0 = pet, 
#                     pe_0 = pe)
# tmp$lag_precip <- tmp %>% select(paste0('precip_',0:13))
# tmp$lag_pet <- tmp %>% select(paste0('pet_',0:13))
# tmp$lag_pe <- tmp %>% select(paste0('pe_',0:13))

# tmp <- tmp %>% mutate(precip_anom_0 = precip_anom, 
#                     pet_anom_0 = pet_anom, 
#                     pe_anom_0 = pe_anom)
# tmp$lag_precip_anom <- tmp %>% select(paste0('precip_anom_',0:13))
# tmp$lag_pet_anom <- tmp %>% select(paste0('pet_anom_',0:13))
# tmp$lag_pe_anom <- tmp %>% select(paste0('pe_anom_',0:13))

tmp$lag_precip_anom <- as.matrix(mat_p)
rm(mat_p); gc()
tmp$lag_pet_anom <- as.matrix(mat_pet)
rm(mat_pet); gc()
tmp$lag_pe_anom <- as.matrix(mat_pe)
rm(mat_pe); gc()


library(mgcv); library(mgcViz)
fit_son <- bam(nirv_anom_sd ~ 
             s(vc,bs='re')+
             s(lag_month,by=lag_precip_anom, bs='gp',k=5)+
             s(lag_month,by=lag_pet_anom, bs='gp',k=5),
           data=tmp %>% #filter(season=='SON') %>% 
             sample_n(50000) %>% 
             filter(between(nirv_anom_sd,-3.5,3.5)), 
           select=T, method='fREML', discrete = T, nthreads = 6)
fit_djf <- bam(nirv_anom_sd ~ 
                 s(vc,bs='re')+
                 s(lag_month,by=lag_precip_anom, bs='gp',k=5)+
                 s(lag_month,by=lag_pet_anom, bs='gp',k=5),
               data=tmp %>% filter(season=='DJF') %>% sample_n(50000) %>% 
                 filter(between(nirv_anom_sd,-3.5,3.5)), 
               select=T, method='fREML', discrete = T, nthreads = 6)
fit_mam <- bam(nirv_anom_sd ~ 
                 s(vc,bs='re')+
                 s(lag_month,by=lag_precip_anom, bs='gp',k=5)+
                 s(lag_month,by=lag_pet_anom, bs='gp',k=5),
               data=tmp %>% filter(season=='MAM') %>% sample_n(50000) %>% 
                 filter(between(nirv_anom_sd,-3.5,3.5)), 
               select=T, method='fREML', discrete = T, nthreads = 6)
fit_jja <- bam(nirv_anom_sd ~ 
                 s(vc,bs='re')+
                 s(lag_month,by=lag_precip_anom, bs='gp',k=5)+
                 s(lag_month,by=lag_pet_anom, bs='gp',k=5),
               data=tmp %>% filter(season=='JJA') %>% sample_n(50000) %>% 
                 filter(between(nirv_anom_sd,-3.5,3.5)), 
               select=T, method='fREML', discrete = T, nthreads = 6)
summary(fit_son); summary(fit_djf); summary(fit_mam); summary(fit_jja)

par(mfrow=c(2,2))
plot(fit_son,rug=F,shade=T, select = 2, scale=0, 
     xlab="lag month",ylab=expression(paste(beta~Precip)), 
     main="SON");abline(h=0,col='red',lty=3)
plot(fit_djf,rug=F,shade=T, select = 2, scale=0, 
     xlab="lag month",ylab=expression(paste(beta~Precip)), 
     main="DJF");abline(h=0,col='red',lty=3)
plot(fit_mam,rug=F,shade=T, select = 2, scale=0, 
     xlab="lag month",ylab=expression(paste(beta~Precip)), 
     main="MAM");abline(h=0,col='red',lty=3)
plot(fit_jja,rug=F,shade=T, select = 2, scale=0, 
     xlab="lag month",ylab=expression(paste(beta~Precip)), 
     main="JJA");abline(h=0,col='red',lty=3)
sm(getViz(fit_son),2) %>% plot

p1 <- plot(sm(getViz(fit_son),2))+
  l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)+
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(limits=c(-0.003,0.0035))+
  labs(x="Lag Month",y=expression(paste(beta~Precip)),title="SON")
p2 <- plot(sm(getViz(fit_djf),2))+
  l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)+
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(limits=c(-0.003,0.0035))+  labs(x="Lag Month",y=expression(paste(beta~Precip)),title="DJF")
p3 <- plot(sm(getViz(fit_mam),2))+
  l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)+
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(limits=c(-0.003,0.0035))+  labs(x="Lag Month",y=expression(paste(beta~Precip)),title="MAM")
p4 <- plot(sm(getViz(fit_jja),2))+
  l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)+
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(limits=c(-0.003,0.0035))+  labs(x="Lag Month",y=expression(paste(beta~Precip)),title="JJA")
pet1 <- plot(sm(getViz(fit_son),3))+
  l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)+
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(limits=c(-0.003,0.0035))+  labs(x="Lag Month",y=expression(paste(beta~PET)),title="SON")
pet2 <- plot(sm(getViz(fit_djf),3))+
  l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)+
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(limits=c(-0.003,0.0035))+  labs(x="Lag Month",y=expression(paste(beta~PET)),title="DJF")
pet3 <- plot(sm(getViz(fit_mam),3))+
  l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)+
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(limits=c(-0.003,0.0035))+  labs(x="Lag Month",y=expression(paste(beta~PET)),title="MAM")
pet4 <- plot(sm(getViz(fit_jja),3))+
  l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)+
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(limits=c(-0.003,0.0035))+
  labs(x="Lag Month",y=expression(paste(beta~PET)),title="JJA")
mgcViz::gridPrint(p1,p2,p3,p4, 
                  pet1,pet2,pet3,pet4,ncol=4)


plot(fit_son,rug=F,shade=T, select = 3, scale=0);abline(h=0,col='red',lty=3)

getViz(fit_son) %>% plot


fit <- bam(nirv_anom_sd ~ 
             s(vc,bs='re')+
             # s(month,vc,bs='fs')+
             # s(x,y,month)+
             # s(x,y,by=ddate)+
             s(lag_month,by=lag_precip_anom, bs='gp',k=5)+
             s(lag_month,by=lag_pet_anom, bs='gp',k=5)
           ,
           data=tmp %>% 
             filter(month %in% c(9,10)) %>%
             sample_n(40000) %>% 
             filter(between(nirv_anom_sd,-3.5,3.5)) %>% 
             mutate(ddate = decimal_date(date)) 
           , 
           select=T, method='fREML', discrete = T, 
           # gamma=50, 
           nthreads = 6
)
summary(fit)

getViz(fit) %>% plot(allTerms=T)
plot(fit,rug=F,shade=T, select = 1, scale=0);abline(h=0,col='red',lty=3)
plot(fit,rug=F,shade=T, select = 2, scale=0);abline(h=0,col='red',lty=3)
plot(fit,rug=F,shade=T, select = 3, scale=0);abline(h=0,col='red',lty=3)
plot(fit,rug=F,shade=T, select = 4, scale=0);abline(h=0,col='red',lty=3)

#*******************************************************************************
#* END SECTION
#*******************************************************************************

sdat <- tmp %>% 
  filter(month==12) %>% 
  sample_frac(0.2) %>% 
  mutate(ddate=decimal_date(date)) %>% 
  filter(between(nirv_anom_sd,-3.5,3.5)) %>% 
  filter(between(pe_anom_12mo,-3,4)) %>% 
  filter(is.na(precip_anom_12mo)==F) %>% 
  filter(is.na(pet_anom_12mo)==F) %>% 
  filter(is.na(nirv_anom_sd)==F)


fit_st1 <- sdat %>% 
  bam(nirv_anom_sd ~ te(x,y,ddate,k=5)+
        scale(precip_anom_12mo)*scale(pet_anom_12mo),
      data=.,
      discrete=T,select=T)
fit_st2 <- sdat %>% 
  bam(nirv_anom_sd ~ 
        s(x,y,by=ddate)+
        s(vc,bs='re')+
        te(pet_anom, pet_anom_12mo, mapet)+
        te(precip_anom, precip_anom_12mo,map)
      # vc*ddate +
      # s(pe_anom_12mo,k=5)
      # s(pet_anom, precip_anom_12mo, pet_anom_12mo,mape)
      ,
      data=.,
      discrete=T,select=T)

fit_st1 %>% summary
fit_st2 %>% summary
getViz(fit_st1) %>% plot(allTerms=T)
b <- getViz(fit_st2)
b %>% plot()
plot(sm(b,1))+l_fitRaster()+coord_equal()
plot(sm(b,2))+
  l_fitRaster()+
  l_rug()+
  scale_fill_gradient2(high='blue',low='red')+
  geom_hline(aes(yintercept=0))+
  geom_vline(aes(xintercept=0))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))


tibble(x=152,y=-30,
       vc="Eucalypt Tall Open Forests",
       ddate=1982:2019, 
       pet_anom_12mo=-100,
       precip_anom_12mo=500, 
       pe_anom_12mo=5) %>% 
  mutate(pred1 = predict(fit_st1, newdata=.)) %>% 
  mutate(pred2 = predict(fit_st2, newdata=.)) %>% 
  gather(-ddate,-x,-y,-vc, 
         -precip_anom_12mo,-pet_anom_12mo, 
         -pe_anom_12mo, 
         key = 'model',value='pred') %>% 
  ggplot(data=., aes(ddate,pred,color=model))+
  geom_point()

tmp %>% 
  filter(near(x,152,tol = 0.1)) %>% 
  filter(near(y,-30,tol=0.1)) %>% 
  mutate(ddate= decimal_date(date)) %>% 
  # mutate(pred1 = predict(fit_st1, newdata=.)) %>% 
  mutate(pred2 = predict(fit_st2, newdata=.)) %>% 
  select(ddate,starts_with('pred'),nirv_anom_sd) %>% 
  gather(-ddate,
         key = 'model',value='pred') %>% 
  ggplot(data=., aes(ddate,pred,color=model))+
  geom_point(alpha=0.2)
# geom_smooth()+
# facet_wrap(~model,ncol=1)





tmp %>% 
  filter(between(nirv_anom_sd, -3.5,3.5)) %>% 
  filter(year>=2018) %>% 
  filter(nirv_anom_sd < -1) %>% 
  sample_n(1e4) %>% 
  ggplot(data=., aes(precip_anom_12mo, pet_anom_sd,color=nirv_anom_sd))+
  geom_point(alpha=0.2)+
  geom_smooth(method='gam',
              formula=y~s(x,k=50),
              method.args=list(method='GCV.Cp'))+
  scale_color_viridis_c(option='B',direction = -1)+
  theme_linedraw()



tmp[is.na(pet)==F]
"/home/sami/srifai@gmail.com/work/research/Oz_droughts/figures/nirv_anom_sigma_lagModel_P_PET.png"




load("data/gridCell_lm_nirv_clim.Rdata") # grid cell linear regressions



map_theme <- theme(panel.background = element_rect(fill = '#99A3C4'), 
                   panel.grid = element_blank(), 
                   legend.position = 'bottom', 
                   axis.text = element_blank(), 
                   axis.ticks = element_blank())

vec_col <- RColorBrewer::brewer.pal(n=11, name='BrBG')
lt_nirv_season %>% 
  ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_viridis_c(expression(paste(Delta*NIR[V]~yr^-1)),
                       option='D',direction = 1,
                       limits=c(-0.0001,0.0015),
                       oob=scales::squish, na.value = 'red')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  facet_wrap(~season, nrow = 1)+
  map_theme+
  theme(legend.position = 'bottom')



map_theme <- theme(panel.background = element_rect(fill = '#99A3C4'), 
                   panel.grid = element_blank(), 
                   legend.position = 'bottom', 
                   axis.text = element_blank(), 
                   axis.ticks = element_blank())

vec_col <- RColorBrewer::brewer.pal(n=11, name='BrBG')
lt_tmax_season %>% 
  ggplot(data=., aes(x,y,fill=b1))+
  geom_sf(inherit.aes = F, data=oz_poly,fill='gray70',color='gray10')+
  geom_tile()+
  scale_fill_viridis_c(expression(paste(T[max]~(degree*C~yr^-1))),
                       option='B',direction = 1,
                       limits=c(-0.025,0.1),
                       oob=scales::squish, 
                       na.value = 'gray')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(140,154),
           ylim = c(-45,-10), expand = FALSE)+
  facet_wrap(~season, nrow = 1)+
  map_theme+
  theme(legend.position = 'bottom')





#*******************************************************
library(mgcv); library(mgcViz)
qdat <- tmp %>% sample_n(100000) %>% 
  filter(is.na(vc)==F) %>% 
  filter(is.na(nirv_anom_sd)==F & 
           between(nirv_anom_sd, -3.5, 3.5)) %>% 
  filter(between(precip_anom_sd, -5,5))
test <- tmp %>% sample_n(100000) %>% 
  filter(is.na(vc)==F) %>% 
  filter(is.na(nirv_anom_sd)==F & 
           between(nirv_anom_sd, -3.5, 3.5)) %>% 
  filter(between(precip_anom_sd, -5,5))
f_climatology <- bam(nirv_u~ 
                       te(x,y,month, bs=c("tp","tp","cc"))+
                       s(vc, bs='re') +
                       s(map,mapet,k=5)+
                       s(ap_sd, apet_sd,k=5)+
                       s(precip_u, precip_sd, k=5)+
                       s(pet_u, pet_sd, k=5)
                     , 
                     select=TRUE, discrete = TRUE, method='fREML', 
                     nthreads = 6, 
                     data=qdat)

# NIRV trend (working!) by vegetation class
lt_vc <- tmp[nirv_anom_sd >= -3.5 & nirv_anom_sd <= 3.5] %>%
    .[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(nirv, na.rm=TRUE)), by=.(x,y,vc,hydro_year)] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year), y=val, data=.SD)$coefficients[2]), 
      by=.(vc)]


test %>% 
  mutate(pred = predict(f_climatology, newdata=.)) %>% 
  drop_na(c("pred","nirv_u")) %>% 
  # select(pred,nirv) %>% 
  as_tibble() %>% 
  group_by(vc) %>%
  summarize(manirv = mean(nirv_u), 
            range_nirv = diff(range(nirv_u)),
            r2 = cor(pred, nirv_u)**2, 
            rmse = sqrt(mean((pred-nirv_u)**2))) %>% 
  ungroup() %>% 
  left_join(., as_tibble(lt_vc), by='vc') %>% 
  knitr::kable(col.names = 
                 c("NVIS Vegetation Class","Mean NIRV","Range NIRV","R2","RMSE","Long-term Trend"), 
               format = "markdown", digits = c(2,2,2,2,2,5), 
               caption = "Predictability of seasonal NIR-V")





###########################################################33
sdat <- tmp %>% 
  # filter(season=="DJF") %>% 
  sample_frac(0.025) %>% 
  mutate(ddate=decimal_date(date)) %>% 
  filter(between(nirv_anom_sd,-5,3.5)) %>% 
  filter(between(pe_anom_12mo,-4,4)) %>% 
  filter(is.na(precip_anom_12mo)==F) %>% 
  filter(is.na(pet_anom_12mo)==F) %>% 
  filter(is.na(nirv_anom_sd)==F)


fit_st0 <- sdat %>% 
  bam(nirv_anom_sd ~ 
        s(x,y,month,bs=c("tp","tp","cc"))+
        s(vc,bs='re')+
        te(precip_u, pet_u, tmax_u)+
        te(map, mapet, matmax),
      data=.,
      discrete=T,select=T)
fit_st1 <- sdat %>% 
  bam(nirv_anom_sd ~ 
        s(x,y,month,bs=c("tp","tp","cc"))+
        s(vc,bs='re')+
        te(precip_u, pet_u, tmax_u)+
        te(precip_anom, pet_anom, tmax_anom)+
        te(map, mapet, matmax),
      data=.,
      discrete=T,select=T)
fit_st2 <- sdat %>% 
  bam(nirv_anom_sd ~ 
        s(x,y,month,bs=c("tp","tp","cc"))+
        s(vc,bs='re')+
        te(precip_u, pet_u, tmax_u)+
        te(precip_anom, pet_anom, tmax_anom)+
        te(precip_anom_3mo, pet_anom_3mo, tmax_anom_3mo)+
        te(map, mapet, matmax),
      data=.,
      discrete=T,select=T)
fit_st3 <- sdat %>% 
  bam(nirv_anom_sd ~ 
        s(x,y,month,bs=c("tp","tp","cc"))+
        s(vc,bs='re')+
        te(precip_u, pet_u, tmax_u)+
        te(precip_anom, pet_anom, tmax_anom)+
        te(tmax_anom_3mo,pet_anom_3mo,precip_anom_3mo)+
        te(tmax_anom_12mo,pet_anom_12mo,precip_anom_12mo)+
        te(map, mapet, matmax),
      data=.,
      discrete=T,select=T)
fit_st4 <- sdat %>% 
  bam(nirv_anom_sd ~ 
        s(x,y,month,bs=c("tp","tp","cc"))+
        s(vc,bs='re')+
        te(precip_u, pet_u, tmax_u)+
        te(tmax_anom,pet_anom,precip_anom)+
        te(tmax_anom_3mo,pet_anom_3mo,precip_anom_3mo)+
        te(tmax_anom_12mo,pet_anom_12mo,precip_anom_12mo)+
        te(tmax_anom_36mo,pet_anom_36mo,precip_anom_36mo)+
        te(map, mapet, matmax),
      data=.,
      discrete=T,select=T)

fit_st1 %>% summary
fit_st2 %>% summary
fit_st3 %>% summary
fit_st4 %>% summary

getViz(fit_st3) %>% plot(allTerms=T) %>% print(pages=2)
b <- getViz(fit_st2)
b %>% plot()
plot(sm(b,1))+l_fitRaster()+coord_equal()
plot(sm(b,2))+
  l_fitRaster()+
  l_rug()+
  scale_fill_gradient2(high='blue',low='red')+
  geom_hline(aes(yintercept=0))+
  geom_vline(aes(xintercept=0))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))


tibble(x=152,y=-30,
       vc="Eucalypt Tall Open Forests",
       ddate=1982:2019, 
       pet_anom_12mo=-100,
       precip_anom_12mo=500, 
       pe_anom_12mo=5) %>% 
  mutate(pred1 = predict(fit_st1, newdata=.)) %>% 
  mutate(pred2 = predict(fit_st2, newdata=.)) %>% 
  gather(-ddate,-x,-y,-vc, 
         -precip_anom_12mo,-pet_anom_12mo, 
         -pe_anom_12mo, 
         key = 'model',value='pred') %>% 
  ggplot(data=., aes(ddate,pred,color=model))+
  geom_point()

tmp %>% 
  filter(near(x,152,tol = 0.1)) %>% 
  filter(near(y,-30,tol=0.1)) %>% 
  mutate(ddate= decimal_date(date)) %>% 
  filter(ddate>=2015) %>% 
  mutate(pred0 = predict(fit_st0, newdata=.)) %>%
  mutate(pred1 = predict(fit_st1, newdata=.)) %>%
  mutate(pred2 = predict(fit_st2, newdata=.)) %>% 
  mutate(pred3 = predict(fit_st3, newdata=.)) %>% 
  mutate(pred4 = predict(fit_st4, newdata=.)) %>% 
  select(ddate,starts_with('pred'),nirv_anom_sd) %>% 
  gather(-ddate,
         key = 'model',value='pred') %>% 
  ggplot(data=., aes(ddate,pred,color=model))+
  geom_hline(aes(yintercept=-2),col='red')+
  geom_point(alpha=0.1)+
  geom_smooth()


tmp %>% 
  filter(near(x,148,tol = 0.1)) %>% 
  filter(near(y,-36.5,tol=0.1)) %>% 
  mutate(ddate= decimal_date(date)) %>% 
  filter(ddate>=2015) %>% 
  mutate(pred0 = predict(fit_st0, newdata=.)) %>%
  mutate(pred1 = predict(fit_st1, newdata=.)) %>%
  mutate(pred2 = predict(fit_st2, newdata=.)) %>% 
  mutate(pred3 = predict(fit_st3, newdata=.)) %>% 
  mutate(pred4 = predict(fit_st4, newdata=.)) %>% 
  select(ddate,starts_with('pred'),nirv_anom_sd) %>% 
  gather(-ddate,
         key = 'model',value='pred') %>% 
  ggplot(data=., aes(ddate,pred,color=model))+
  geom_hline(aes(yintercept=-2),col='red')+
  geom_point(alpha=0.1)+
  geom_smooth()+
  scale_y_continuous(limits = c(-5,2))


tmp[date==min(date)] %>% ggplot(data=.,aes(x,y,tmax))+geom_tile()+
  geom_rect(aes(xmin = 147.5, 
                xmax = 148.5,
                ymin = -36,
                ymax = -37),color='red',fill=NA)


#*******************************************************************************
source("src/R/helper_funs_Oz_droughts.R")
fn_tsr <- function(vi, d_threshold, r_threshold){
  # vi: vegetation index
  # d_threshold: level of vegetation index to signal a disturbance
  # r_threshold: level of vegetation index to recover from a disturbance
  # Assumptions: Continuous time record of vi (no gaps)
  tsr <- rep(0,length(vi)) # time since recovery array
  vec_d <- vi<d_threshold
  vec_r <- vi>r_threshold
  vec_d[is.na(vec_d)==T] <- FALSE
  vec_r[is.na(vec_r)==T] <- FALSE
  for(i in seq(2,length(vi))){
    tsr[i] <- tsr[i-1] + vec_d[i]
    tsr[i] <- ifelse(tsr[i] >=1 & vec_r[i]==F, tsr[i]+1, 0)
   }
  return(tsr)
}


# tmp[vc=="Eucalypt Tall Open Forests"] %>% 
#   .[between(x,151,153.5) & between(y,-33.5,-30)] %>% 
tmp[year==2019 & nirv_anom_sd >= -5 & nirv_anom_sd <= -2 & 
      y > -35 & y < -30 & x > 147] %>%
    as_tibble() %>% 
    group_by(x,y) %>% 
    summarize(val = mean(nirv_anom_sd,na.rm=TRUE)) %>% 
    ungroup() %>% 
    raster::rasterFromXYZ() %>% 
    rasterVis::levelplot(,margin=F)


vec_vc <- unique(tmp$vc)
vec_col <- RColorBrewer::brewer.pal(n=11, name='BrBG')
tmp[y > -35 & y < -30 & x > 147 & nirv_anom_sd <= 3.5] %>%
  as_tibble() %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(nirv_anom_sd,na.rm=TRUE)) %>% 
  ungroup() %>% #pull(val) %>% summary
  # mutate(val = ifelse(year==2011,2.5,val)) %>%
  group_by(x,y) %>% 
  arrange(year) %>% 
  mutate(tsr = fn_tsr(val, d_threshold = -2.5, r_threshold = 1)) %>% 
  ungroup() %>% #pull(tsr) %>% is.na() %>% table()
  ggplot(data=., aes(x,y,fill=tsr))+
  geom_tile()+
  coord_sf()+
  scale_fill_viridis_c("",
                       option='B', end=0.95,
                       limits=c(0,5),
                       oob=scales::squish,
                       na.value = 'blue'
  )+
  # scale_fill_gradient2(high=vec_col[11],mid = vec_col[6],low=vec_col[1],
  #                      limits=c(-3,3),
  #                      oob=scales::squish, na.value = 'red')+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(151,153.5),
           ylim = c(-33.5,-30), expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 4)+
  blah_theme+
  theme(legend.position = 'right')


#-------------------------------------------------------------------------------
library(raster); library(rasterVis); library(tidyverse); library(lubridate); 
library(stars); library(data.table);
mod <- stars::read_stars("../data_general/MOD13A2/MOD13A2_NIRV_1km_EastOz_NVIStreeClassMask_2003_2019-0000001792-0000000000.tif")
levelplot(mod[[1]], margin=F)
mod <- stars::st_set_dimensions(mod,which = 3, 
                         values = seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"), 
                         names='date')
tmp <- mod %>% as.data.frame(.) %>% as.data.table()
names(tmp) <- c("x","y","date","nirv")
tmp <- tmp[y> -39 & y< -30 & x> 145]
tmp <- tmp[, `:=`(month = month(date))] # create month
tmp <- tmp[, `:=`(year = year(date))]   # create year
norms_nirv <- tmp[date>=ymd('2003-01-01')&date<=ymd("2018-12-31"), # filter to ref period
                  .("nirv_u" = mean(nirv,na.rm=TRUE), 
                    "nirv_sd" = sd(nirv,na.rm=TRUE)),
                  by=.(x,y,month)] # joining on x,y,month

norms <- tmp[,.(nirv_u=mean(nirv,na.rm=TRUE)),by=.(x,y)]
rasterFromXYZ(norms[y> -39 & y< -30 & x> 145]) %>% levelplot(., margin=F)


levelplot(mod[[1]], margin=F)



library(dtplyr, warn.conflicts = F)
tmp2 <- lazy_dt(tmp)
tmp2 %>% group_by(x,y) %>% filter(nirv_anom_sd==min(nirv_anom_sd)) %>% show_query()
tmp[, .SD[nirv_anom_sd==min(nirv_anom_sd,na.rm=TRUE)], keyby=.(x,y)]

tmp2 %>% filter(is.na(nirv_anom_sd)==F) %>% 
  group_by(x,y) %>% filter(nirv_anom_sd==min(nirv_anom_sd)) %>% 
  ungroup()


mod[,400:800,400:800,190:204] %>% plot
o <- aggregate(mod[,400:800,400:800,190:204], 
          by = seq(ymd("2019-01-01"),ymd("2019-12-01"),by="1 month"), 
          FUN=mean)
o %>% plot


# aggregate time dimension in format Date
tif = system.file("tif/L7_ETMs.tif", package = "stars")
t1 = as.Date("2018-07-31")
x = read_stars(c(tif, tif, tif, tif), along = list(time = c(t1, t1+1, t1+2, t1+3)))[,1:30,1:30]
st_get_dimension_values(x, "time")
x_agg_time = aggregate(x, by = t1 + c(0, 2, 4), FUN = max) 
aggregate(x, FUN = max) 

# aggregate time dimension in format Date - interval
by_t = "2 days"
x_agg_time2 = aggregate(x, by = by_t, FUN = max) 
st_get_dimension_values(x_agg_time2, "time")
x_agg_time - x_agg_time2

# aggregate time dimension in format POSIXct
x = st_set_dimensions(x, 4, values = as.POSIXct(c("2018-07-31", 
                                                  "2018-08-01", 
                                                  "2018-08-02", 
                                                  "2018-08-03")), 
                      names = "time")
by_t = as.POSIXct(c("2018-07-31", "2018-08-02"))
x_agg_posix = aggregate(x, by = by_t, FUN = max)
st_get_dimension_values(x_agg_posix, "time")
x_agg_time - x_agg_posix
aggregate(x, "2 days", mean)

plot(st_apply(mod[,,,192:204], 1:2, mean, na.rm=TRUE), 
     col=viridis::inferno(50,end=0.9))

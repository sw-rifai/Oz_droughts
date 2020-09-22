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

hyb %>% 
  lazy_dt() %>% 
  filter(veg_class %in% c(1:6)) %>% 
  filter(date >= ymd("2014-01-01")) %>% 
  filter(is.na(ndvi_mcd)==F & is.na(ndvi_myd)==F) %>% 
  # sample_n(100000) %>% 
  as_tibble() %>% 
  ggplot(data=., aes(date, ndvi_myd))+
  # geom_point()+
  # geom_abline(aes(intercept=0,slope=1))+
  geom_smooth(span=0.1)+
  geom_smooth(aes(date,ndvi_mcd),col='red',span=0.1)+
  facet_wrap(~vc)

hyb %>% 
  lazy_dt() %>% 
  filter(veg_class %in% c(1:6)) %>% 
  filter(date >= ymd("2001-01-01")) %>% 
  filter(is.na(ndvi_mcd)==F & is.na(ndvi_myd)==F) %>% 
  sample_n(10000) %>% 
  as_tibble() %>% 
  ggplot(data=., aes(ndvi_mcd, ndvi_myd))+
  geom_point(alpha=0.1)+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')+
  facet_wrap(~vc) 


unique(base$x) %in% unique(tmp_clim$x_vi) %>% table
unique(base$y) %in% unique(tmp_clim$y_vi) %>% table

v_x1 <- sort(unique(base$x)) %>% round(., 2)
v_x2 <- sort(unique(tmp_clim$x_vi)) %>% round(., 2)
table(v_x1 %in% v_x2)

dc1 <- base %>% select(x,y) %>% distinct()
dc2 <- tmp_clim %>% select(x,y) %>% distinct()

sort(unique(base$x)) %>% length # 403
sort(unique(tmp_clim$x)) %>% length

sort(unique(base$x)) %>% round(.,2) %>% length



library(data.table); library(tidyverse)
tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet") %>% 
  as.data.table()

tmpc <- tmp[y> -39 & y< -30 & x> 145]
empty <- expand_grid(x=unique(tmpc$x),y=unique(tmpc$y),date=unique(tmpc$date)) %>% 
  arrange(x,y,date) %>% as.data.table()
tmpc <- empty[tmpc,on=.(x,y,date)]
o <- tmpc %>% 
  as_tibble() %>% 
  mutate(year=year(date), 
         val = ndvi_anom_sd) %>% 
  mutate(val = ifelse(val > 4.5 | val < -6, NA, val)) %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(val,na.rm=TRUE)) %>% 
  ungroup() %>% #pull(val) %>% summary
  # mutate(val = ifelse(year==2011,2.5,val)) %>%
  group_by(x,y) %>% 
  arrange(year) %>% 
  mutate(tsr = fn_tsr(val, d_threshold = -1.5, r_threshold = 1.5)) %>% 
  ungroup()

o %>% 
  # mutate(year=year(date)) %>% 
  # filter(year==2018) %>% pull(tsr) %>% summary
  ggplot(data=., aes(round(x,2),round(y,2),fill=tsr))+
  geom_sf(data=oz_poly, inherit.aes = F, fill='white')+
  geom_tile()+
  scale_fill_viridis_c(expression(paste("years since ",NIR[V]," recovery")),
                       option='B', end=0.95,
                       limits=c(0,5),
                       oob=scales::squish,
                       na.value = 'blue'
  )+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(151,153.5),
           ylim = c(-33.5,-30), expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 4)+
  guides(fill = guide_colorbar(title.position = "top"))+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(fill='#99A3C4'), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        strip.placement = 'inside', 
        strip.text = element_text(family='AvantGarde'), 
        legend.direction = 'horizontal', 
        legend.key.width = unit(1,units = 'cm'),
        legend.key.height = unit(0.5,units='cm')
  )

library(dtplyr)
tmp %>% 
  lazy_dt() %>% 
  filter(date == ymd("1983-01-01")) %>% 
  as.data.table() %>% 
  ggplot(data=., aes(x_vi,y_vi,fill=tmax))+
  geom_tile()+
  scale_fill_viridis_c()+
  coord_equal()

p1 <- tmp_clim %>% 
  lazy_dt() %>% 
  filter(date <= ymd("1983-12-01")) %>% 
  group_by(x_vi,y_vi) %>% 
  summarize(tmax=mean(tmax,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as.data.table() #%>% 
  ggplot(data=., aes(x_vi,y_vi,fill=tmax))+
  geom_tile()+
  scale_fill_viridis_c()+
  coord_equal()
p2 <- base %>% 
  lazy_dt() %>% 
  filter(date <= ymd("1983-12-01")) %>% 
  group_by(x_vi,y_vi) %>% 
  summarize(ndvi_mcd=mean(ndvi_mcd,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as.data.table() #%>% 
  ggplot(data=., aes(x_vi,y_vi,fill=ndvi_mcd))+
  geom_tile()+
  scale_fill_viridis_c()+
  coord_equal()
library(patchwork)
p1+p2


p3 <- merge(p1, 
      p2,
      by=c("x_vi","y_vi"), 
      all=TRUE,allow.cartesian=TRUE)
tmp %>% 
  filter(date == max(date)) %>% 
  ggplot(data=., aes(x_vi,y_vi,fill=pet))+
  geom_tile()+
  scale_fill_viridis_c()+
  coord_equal()
tmp %>% 
  filter(date == max(date)) %>% 
  ggplot(data=., aes(x,y,fill=pet))+
  geom_tile()+
  scale_fill_viridis_c()+
  coord_equal()





tmpc <- tmp[y> -39 & y< -30 & x> 145]
empty <- expand_grid(x=unique(tmpc$x),y=unique(tmpc$y),date=unique(tmpc$date)) %>% 
  arrange(x,y,date) %>% as.data.table()
tmpc <- empty[tmpc,on=.(x,y,date)]
o <- tmpc %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(ndvi_anom,na.rm=TRUE)) %>% 
  ungroup() %>% 
  
  as_tibble() %>% 
  mutate(val = ifelse(val > 4.5 | val < -6, NA, val)) %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(val,na.rm=TRUE)) %>% 
  ungroup() %>% #pull(val) %>% summary
  # mutate(val = ifelse(year==2011,2.5,val)) %>%
  group_by(x,y) %>% 
  arrange(year) %>% 
  mutate(tsr = fn_tsr(val, d_threshold = -1.5, r_threshold = 1.75)) %>% 
  ungroup()

o %>% 
  # mutate(year=year(date)) %>% 
  # filter(year==2018) %>% pull(tsr) %>% summary
  ggplot(data=., aes(x,y,fill=tsr))+
  geom_sf(data=oz_poly, inherit.aes = F, fill='white')+
  geom_tile()+
  scale_fill_viridis_c(expression(paste("years since ",NIR[V]," recovery")),
                       option='B', end=0.9,
                       limits=c(0,30),
                       oob=scales::squish,
                       na.value = 'blue'
  )+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(151,153.5),
           ylim = c(-33.5,-30), expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 4)+
  guides(fill = guide_colorbar(title.position = "top"))+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(fill='#99A3C4'), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        strip.placement = 'inside', 
        strip.text = element_text(family='AvantGarde'), 
        legend.direction = 'horizontal', 
        legend.key.width = unit(1,units = 'cm'),
        legend.key.height = unit(0.5,units='cm')
  )






tmpc <- tmpc %>% as_tibble()
tmpc <- tmp[y> -39 & y< -30 & x> 145]
empty <- expand_grid(x=unique(tmpc$x),y=unique(tmpc$y),date=unique(tmpc$date)) %>% 
  arrange(x,y,date) %>% as.data.table()
tmpc <- empty[tmpc,on=.(x,y,date)]

tmpc %>% 
  as_tibble() %>% 
  group_by(x,y,year) %>% 
  summarize(val = mean(ndvi_mcd,na.rm=TRUE)) %>% 
  ungroup() %>% 
  inner_join(., {.} %>% 
      group_by(x,y) %>% 
      summarize(val_sd = sd(val,na.rm=TRUE), 
                val_u = mean(val,na.rm=TRUE)) %>% 
      ungroup(),by=c('x','y')) %>% 
  mutate(val_anom = val - val_u) %>% 
  mutate(val_anom_sd = val_anom/val_sd) %>% 
  group_by(x,y) %>% 
  arrange(year) %>% 
  mutate(tsr = fn_tsr(val, d_threshold = -1.5, r_threshold = 1.75)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(x,y,fill=tsr))+
  geom_sf(data=oz_poly, inherit.aes = F, fill='white')+
  geom_tile()+
  scale_fill_viridis_c(expression(paste("years since ",NIR[V]," recovery")),
                       option='B', end=0.9,
                       limits=c(0,30),
                       oob=scales::squish,
                       na.value = 'blue'
  )+
  labs(x=NULL,y=NULL)+
  coord_sf(xlim = c(151,153.5),
           ylim = c(-33.5,-30), expand = FALSE)+
  facet_wrap(~year, drop = T, nrow = 4)+
  guides(fill = guide_colorbar(title.position = "top"))+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(fill='#99A3C4'), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        strip.placement = 'inside', 
        strip.text = element_text(family='AvantGarde'), 
        legend.direction = 'horizontal', 
        legend.key.width = unit(1,units = 'cm'),
        legend.key.height = unit(0.5,units='cm')
  )

? lme4::nlmer

tmp_clim
tmp_clim[date==min(date)] %>% 
  ggplot(data=., aes(x_vi,y_vi,fill=vpd15))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(option='B')


tmp_clim <- tmp_clim %>% 
  group_by(x_vi,y_vi) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup()

tmp_clim %>% 
  filter(id %in% sample(64000,10)) %>% 
  filter(between(date, ymd("1990-01-01"), ymd("2010-12-01"))) %>% 
  ggplot(data=., aes(date, ndvi_mcd,color=as.factor(id)))+
  geom_line()+
  geom_smooth(se=F,span=0.5)+
  facet_wrap(~id, scales = 'free')

library(dtplyr)
train <- tmp %>% lazy_dt() %>% 
  filter(is.na(ndvi_anom_sd)==F) %>%
  filter(str_detect(vc,"Eucalypt")) %>% 
  filter(str_detect(vc,"Forest")) %>% 
  sample_n(1e5) %>% 
  as_tibble()
test <- tmp %>% lazy_dt() %>% sample_n(1e5) %>% as_tibble()


tmp[vc=="Eucalypt Tall Open Forests"] %>% 
  .[ndvi_mcd > 0] %>% 
  .[date >= ymd("2000-01-01")] %>% dim
sdat <- tmp %>% 
  lazy_dt() %>% 
  # filter(season=="DJF") %>% 
  filter(ndvi_mcd > 0) %>% 
  mutate(ddate=decimal_date(date)) %>% 
  filter(between(ndvi_anom_sd,-5,5)) %>% 
  filter(between(pe_anom_12mo,-4,4)) %>% 
  filter(is.na(precip_anom_12mo)==F) %>% 
  filter(is.na(pet_anom_12mo)==F) %>% 
  filter(date >= ymd("2000-01-01")) %>% 
  filter(vc=="Eucalypt Tall Open Forests") %>% 
  as_tibble()

stest <- tmp %>% 
  lazy_dt() %>% 
  # filter(season=="DJF") %>% 
  filter(ndvi_mcd > 0) %>% 
  mutate(ddate=decimal_date(date)) %>% 
  filter(between(ndvi_anom_sd,-5,5)) %>% 
  filter(between(pe_anom_12mo,-4,4)) %>% 
  filter(is.na(precip_anom_12mo)==F) %>% 
  filter(is.na(pet_anom_12mo)==F) %>% 
  sample_n(1e5) %>% 
  as_tibble()

sdat <- tmp[vc=="Eucalypt Tall Open Forests"] %>% 
  .[ndvi_mcd > 0] %>% 
  .[date >= ymd("2000-01-01")] %>% 
  as_tibble()

sdat$t_threshold <- sdat %>% select(paste0("t",35:45)) %>% as.matrix()
tdegs <- 35:45 ## 
sdat$t_degs <- t(matrix(tdegs,length(tdegs),length(sdat$t35)))

m <- bam(ndvi_anom_sd ~
           # s(precip_anom_12mo,k=5)+
           # s(precip_anom_36mo,k=5)+
           #                   s(pet_anom_3mo,k=5) +
           #                   s(precip_anom_6mo,k=5)+
           # s(tmax_u)+
           #                    s(vpd15_anom_3mo,k=5),
           # s(tmax_anom_3mo),
           s(tmax_u, t42),
           # s(tmax_anom, by= tmax_anom),
           # s(t35,by=tmax_anom),
           # s(t_threshold, by=t_degs),
                 # s(t_degs,by=t_threshold),
           # s(vpd15_anom_sd),
         data=sdat %>%
           filter(date >= ymd("2000-01-01")) %>% 
           filter(vc=="Eucalypt Tall Open Forests"), 
         select=TRUE,method='fREML')
summary(m)
plot(m,select = 1,scale = 0)
plot(m,select = 2,scale = 0); abline(h=0)
plot(m,select = 3,scale = 0)
getViz(m) %>% plot

m2 <- bam(ndvi_anom_sd ~ s(vpd15_anom_sd, 
                           tmax_anom_3mo,
                           precip_anom_12mo), 
         data=sdat %>%
           filter(date >= ymd("2000-01-01")) %>% 
           filter(vc=="Eucalypt Tall Open Forests"), 
         select=TRUE,method='fREML',discrete=TRUE)
summary(m2)
plotSlice(sm(getViz(m2),1),fix = list("tmax_anom_3mo"=c(-2,0,2)))+
  l_fitRaster()+l_fitContour()+l_rug()


sdat %>% 
  select(tmax_anom_sd, vpd15_anom_sd, pet_anom_sd) %>% 
  drop_na() %>% cor


tmp[date==ymd("1999-01-01")] %>% 
  ggplot(data=.,aes(x,y,fill=precip_u))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(limits=c(0,300),na.value ='red')

tmp[date>=ymd('1982-01-01')&date<=ymd("2011-12-31"), # filter to ref period
    .("ap" = sum(precip,na.rm=TRUE)),
    by=.(x,y,year)][,.("map"=mean(ap,na.rm=TRUE), 
                       "ap_sd"=sd(ap,na.rm=TRUE)),by=.(x,y)] %>% 
  ggplot(data=.,aes(x,y,fill=map))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(limits=c(0,3000),na.value ='red')

tmp[date>=ymd("1983-01-01") & date <= ymd("1983-12-31")] %>% 
  .[,.("val" = sum(is.na(precip)==F)), by=.(x,y)] %>% 
  pull(val) %>% table
  ggplot(data=.,aes(x,y,fill=val))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(#limits=c(0,300),
                       na.value ='red')

aprecip[time>=ymd("1983-01-01") & time <= ymd("1983-01-31")] %>% 
    .[,.("val" = sum(is.na(precip)==F)), by=.(x_vi,y_vi)] %>% 
    pull(val) %>% table
  ggplot(data=.,aes(x_vi,y_vi,fill=val))+
    geom_tile()+
    coord_equal()+
    scale_fill_viridis_c(#limits=c(0,300),
      na.value ='red')

coords_dict %>% ggplot(data=., aes(x_clim,y_clim,fill=x_clim))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()
  
coords_dict %>% ggplot(data=., aes(x_vi,y_vi,fill=x_clim))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()

coords_dict %>% dim
coords_dict %>% distinct() %>% dim
coords_dict[,c("x","y")] %>% distinct() %>% dim


aprecip[is.na(x_vi)==F &
          is.na(y_vi)==F][time>=ymd('2000-01-01',tz='UTC')&
          time<=ymd('2000-12-31',tz='UTC')] %>% 
  .[,.(val=sum(precip,na.rm=TRUE)),by=.(x_vi,y_vi)] %>% 
 ggplot(data=.,aes(x_vi,y_vi,fill=val))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c(#limits=c(0,300),
    na.value ='red')

tmp[date==ymd("2019-01-01")] %>% 
  ggplot(data=., aes(x,y,fill=precip_anom_36mo))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2()

m4_evi2 <- bam(evi2_mcd~
                 s(sz)+s(nir_c)+s(red_c)+s(ndvi_c,m = 1)+
                 s(x,y)+s(vc,bs='re'),
               family=betar(link='logit'),
               select = TRUE, discrete=TRUE, method='fREML', nthreads = 8,
               data=train)
m4_nirv <- bam(nirv_mcd~
                 s(sz)+s(red_c)+s(nir_c,ndvi_c,m = 1)+
                 s(x,y)+s(vc,bs='re'),
               family=betar(link='logit'),
               select = TRUE, 
               discrete=TRUE, method='fREML', nthreads = 8,
               data=train)

coalesce(c(1,2,NA),
         c(NA,1,2))


hyb <- hyb %>% lazy_dt() %>% 
  mutate(ndvi_m = coalesce(ndvi_mcd, ndvi_hyb), 
         evi2_m = coalesce(evi2_mcd, evi2_hyb), 
         nirv_m = coalesce(nirv_mcd, nirv_hyb)) %>% 
  as.data.table() %>% 
  group_by(x,y) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup()

tmp %>% 
  filter(id==100) %>% 
  ggplot(data=., aes(ndvi_hyb, ndvi_c))+
  geom_point()+
  geom_abline(aes(intercept=0,slope=1),col='red')
tmp %>% 
  filter(id==1000) %>% 
  ggplot(data=., aes(date, ndvi_m))+
  geom_point()+geom_smooth()+
  geom_point(aes(date,ndvi_mcd),col='red',size=0.2)


tmp_clim %>% 
  filter(id==30000) %>% 
  filter(date>=ymd('2000-01-01')) %>% 
  ggplot(data=., aes(date, ndvi_m))+
  geom_smooth(color='black',method='lm')+
  geom_smooth(aes(date,evi2_m),col='red',method='lm')+
  geom_smooth(aes(date,nirv_m),col='blue',method='lm')

tmp[ndvi_anom_sd >= -5 & ndvi_anom_sd <= 5] %>%
  .[date>= ymd("1982-01-01") & date<= ymd("1982-01-01")] %>% pull(ndvi_mcd) %>% hist


tmp$ndvi_mcd


lt_t35_season %>% 
  ggplot(data=.,aes(x,y,fill=b1))+
  geom_tile()+
  coord_equal()+
  scale_fill_gradient2(high='orange',low='blue',limits=c(-0.25,0.25))+
  # scale_fill_viridis_c(option='B')+
  facet_wrap(~season,ncol=4)


tmp1
summary(fit_son)
plot(fit_son, scale = 0)

tmp1 %>% #filter(season=='SON') %>% 
  sample_n(50000) %>% 
  filter(between(ndvi_anom_sd,-3.5,3.5)) %>% 
  select(vpd15_anom, pet_anom, precip_anom, tmax_anom) %>% 
  cor


alt <- bam(ndvi_anom_sd ~ 
                 s(mandvi,vc,bs='fs')+
                 s(vc,bs='re')+
                 s(lag_month,by=lag_precip_anom, bs='gp',k=5)+
                 s(lag_month,by=lag_pet_anom, bs='gp',k=5)+
                 s(lag_month,by=lag_vpd15_anom, bs='gp',k=5)+
                 s(x,y,fx = TRUE),
               data=tmp1 %>% #filter(season=='SON') %>% 
                 sample_n(50000) %>% 
                 filter(between(ndvi_anom_sd,-3.5,3.5)), 
               select=T, method='fREML', discrete = T, nthreads = 6)
bbmle::AICtab(alt,fit_son)



fit_son <- bam(ndvi_anom_sd ~ 
                 # s(ndvi_u)+
                 # te(map,mavpd15,mapet)+
                 s(lag_month,by=lag_precip_anom, bs='gp',k=5)+
                 s(lag_month,by=lag_pet_anom, bs='gp',k=5)+
                 s(lag_month,by=lag_vpd15_anom, bs='gp',k=5)+
                 s(x,y,fx = TRUE),
               data=tmp1 %>% filter(season=='SON') %>% 
                 filter(between(ndvi_anom_sd,-3.5,3.5)) %>% 
                 filter(vc == "Eucalypt Tall Open Forests") %>% 
                 sample_n(50000), 
               select=T, method='fREML', discrete = T, nthreads = 6)
summary(fit_son)
plot(fit_son, scale = 0)



p1 <- plot(sm(getViz(fit_son),1))+
  l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)+
  scale_x_continuous(expand=c(0,0), breaks = seq(0,12,length.out = 7))+
  scale_y_continuous(limits=c(-0.004,0.008))+
  labs(x="Lag Month",y=expression(paste(beta~Precip)),title="SON")
p1


alt <- bam(ndvi_anom_sd ~ 
                 # s(ndvi_u)+
                 # te(map,mavpd15,mapet)+
                 s(lag_month,by=lag_precip_anom, bs='gp',k=5)+
                 s(lag_month,by=lag_pet_anom, bs='gp',k=5)+
                 s(lag_month,by=lag_vpd15_anom, bs='gp',k=5)+
                 s(x,y,fx = TRUE),
               data=tmp1 %>% filter(season=='SON') %>% 
                 filter(between(ndvi_anom_sd,-3.5,3.5)) %>% 
                 filter(vc == "Eucalypt Woodlands") %>% 
                 sample_n(50000), 
               select=T, method='fREML', discrete = T, nthreads = 6)
p2 <- plot(sm(getViz(alt),1))+
  l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)+
  scale_x_continuous(expand=c(0,0), breaks = seq(0,12,length.out = 7))+
  scale_y_continuous(limits=c(-0.004,0.008))+
  labs(x="Lag Month",y=expression(paste(beta~Precip)),title="SON")
p2
library(patchwork)
p1+p2

tmp1$vc %>% table



mgcViz::gridPrint(p1,p2,p3,p4, 
                  ncol=4)

library(gratia)
bb1 <- evaluate_smooth(fit1_djf,smooth = "s(lag_month):lag_precip_anom")
bb2 <- evaluate_smooth(fit2_djf,smooth = "s(lag_month):lag_precip_anom")
bind_rows(bb1 %>% mutate(vc="Eucalypt Tall Open Forests"),
          bb2 %>% mutate(vc="Eucalypt Woodlands")) %>% 
  group_by(vc) %>% 
  mutate(est_s = scale(est)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(lag_month,est_s,color=vc))+geom_line()

bb1 <- evaluate_smooth(fit1_djf,smooth = "s(lag_month):lag_pet_anom")
bb2 <- evaluate_smooth(fit2_djf,smooth = "s(lag_month):lag_pet_anom")
bind_rows(bb1 %>% mutate(vc="Eucalypt Tall Open Forests"),
          bb2 %>% mutate(vc="Eucalypt Woodlands")) %>% 
  group_by(vc) %>% 
  mutate(est_s = scale(est)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(lag_month,est_s,color=vc))+geom_line()

bb1 <- evaluate_smooth(fit1_djf,smooth = "s(lag_month):lag_vpd15_anom")
bb2 <- evaluate_smooth(fit2_djf,smooth = "s(lag_month):lag_vpd15_anom")
bind_rows(bb1 %>% mutate(vc="Eucalypt Tall Open Forests"),
          bb2 %>% mutate(vc="Eucalypt Woodlands")) %>% 
  group_by(vc) %>% 
  mutate(est_s = scale(est)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(lag_month,est_s,color=vc))+geom_line()

tmp %>% 
  group_by(vc,season) %>% 
  summarize(val = mean(ndvi_u,na.rm=TRUE), 
            val2 = mean(ndvi_sd, na.rm=TRUE))


library(dtplyr)

tmp %>% lazy_dt() %>% 
  group_by(vc,hydro_year) %>% 
  summarize(val = mean(ndvi_mcd,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  ggplot(data=., aes(hydro_year,val,color=vc))+
  geom_line()

vi[date<=ymd("1999-01-01")]


gc()
clim_p_anom <- tmp[date>=ymd("2001-01-01")] %>% 
  lazy_dt() %>% 
  mutate(zone = case_when(y > -23.5 ~ 'Tropical', 
                          y <= -23.5 & y > -35 ~ 'Subtropical',
                          y <= -35 ~ 'Temperate')) %>%
  mutate(zone = factor(zone, levels =      c("Tropical","Subtropical","Temperate"),ordered = T)) %>%
  group_by(date,zone) %>% 
               summarize(precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE)) %>% 
               ungroup() %>% 
  as_tibble()
  
tmp[date>=ymd("2001-01-01")] %>% 
  lazy_dt() %>% 
  mutate(zone = case_when(y > -23.5 ~ 'Tropical', 
                          y <= -23.5 & y > -35 ~ 'Subtropical',
                          y <= -35 ~ 'Temperate')) %>%
  mutate(zone = factor(zone, levels =      c("Tropical","Subtropical","Temperate"),ordered = T)) %>%
  inner_join(., {.} %>% group_by(date,zone) %>% 
               summarize(precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE)) %>% 
               ungroup(), by=c("date","zone"))
  group_by(vc,zone,date) %>% 
  summarize(val = mean(ndvi_mcd,na.rm=TRUE), 
            nobs = sum(is.na(ndvi_mcd)==F)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  inner_join(., {.} %>% 
               mutate(year=year(date)) %>% 
               filter(year >=2001 & year<= 2015) %>% 
               group_by(vc,zone) %>% 
               summarize(mandvi=mean(val,na.rm=TRUE)) %>% 
               ungroup(), 
             by=c("vc","zone")) %>% 
  group_by(vc,zone) %>% 
  arrange(date) %>% 
  mutate(val_12mo = RcppRoll::roll_meanr(val,n=12,fill=NA)) %>% 
  ungroup() %>% 
  mutate(val_anom_12mo = val_12mo-mandvi) %>% 
  filter(nobs > 500) %>% 
  filter(date <= ymd("2019-10-01")) %>% 
    inner_join(., clim_p_anom, by=c("date","zone")) %>% 
  ggplot(data=., aes(date,precip_anom_12mo,color=vc))+
  geom_hline(aes(yintercept=0))+
  geom_line()+
  scale_color_paletteer_d("awtools::bpalette")+
  facet_wrap(~zone,nrow=3)+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        legend.position = 'right')

  
  
  
#########################3
names(tmp)

c(
  # "ap_sd", "ape_sd", "apet_sd", "atmax_sd", "avpd15_sd", 
  "date", "hydro_year", "id", 
  # "map", "mape", "mapet", "matmax", "mavpd15", 
  # "month", "ndvi_12mo", 
  "ndvi_anom", "ndvi_anom_12mo","ndvi_anom_sd", 
  "ndvi_mcd", 
  # "ndvi_sd", "ndvi_u", "pe", "pe_12mo", "pe_36mo", 
  # "pe_anom", "pe_anom_12mo", "pe_anom_36mo", "pe_anom_3mo", "pe_anom_6mo", 
  # "pe_anom_sd", "pe_sd", "pe_u", "pet", "pet_12mo", "pet_36mo", 
  # "pet_anom", "pet_anom_12mo", "pet_anom_36mo", "pet_anom_3mo", 
  # "pet_anom_6mo", "pet_anom_sd", "pet_sd", "pet_u", "precip", "precip_12mo", 
  # "precip_36mo", "precip_anom", "precip_anom_12mo", "precip_anom_36mo", 
  # "precip_anom_3mo", "precip_anom_6mo", "precip_anom_sd", "precip_sd", 
  # "precip_u", "season", "t35", "t36", "t37", "t38", "t39", "t40", 
  # "t41", "t42", "t43", "t44", "t45", "tmax", "tmax_anom", "tmax_anom_12mo", 
  # "tmax_anom_36mo", "tmax_anom_3mo", "tmax_anom_6mo", "tmax_anom_sd", 
  # "tmax_sd", "tmax_u", "vc", "veg_class", "vp15", "vp9", "vpd15", 
  # "vpd15_12mo", "vpd15_36mo", "vpd15_anom", "vpd15_anom_12mo", 
  # "vpd15_anom_36mo", "vpd15_anom_3mo", "vpd15_anom_6mo", "vpd15_anom_sd", 
  # "vpd15_sd", "vpd15_u", 
  "x", "y", "year")



tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             # "ap_sd", "ape_sd", "apet_sd", "atmax_sd", "avpd15_sd", 
                             "date", "hydro_year", "id", 
                             # "map", "mape", "mapet", "matmax", "mavpd15", 
                             # "month", "ndvi_12mo", 
                             "ndvi_anom", "ndvi_anom_12mo","ndvi_anom_sd", 
                             "ndvi_mcd", 
                             # "ndvi_sd", "ndvi_u", "pe", "pe_12mo", "pe_36mo", 
                             # "pe_anom", "pe_anom_12mo", "pe_anom_36mo", "pe_anom_3mo", "pe_anom_6mo", 
                             # "pe_anom_sd", "pe_sd", "pe_u", "pet", "pet_12mo", "pet_36mo", 
                             # "pet_anom", "pet_anom_12mo", "pet_anom_36mo", "pet_anom_3mo", 
                             # "pet_anom_6mo", "pet_anom_sd", "pet_sd", "pet_u", "precip", "precip_12mo", 
                             # "precip_36mo", "precip_anom", "precip_anom_12mo", "precip_anom_36mo", 
                             # "precip_anom_3mo", "precip_anom_6mo", "precip_anom_sd", "precip_sd", 
                             # "precip_u", "season", "t35", "t36", "t37", "t38", "t39", "t40", 
                             # "t41", "t42", "t43", "t44", "t45", "tmax", "tmax_anom", "tmax_anom_12mo", 
                             # "tmax_anom_36mo", "tmax_anom_3mo", "tmax_anom_6mo", "tmax_anom_sd", 
                             # "tmax_sd", "tmax_u", "vc", "veg_class", "vp15", "vp9", "vpd15", 
                             # "vpd15_12mo", "vpd15_36mo", "vpd15_anom", "vpd15_anom_12mo", 
                             # "vpd15_anom_36mo", "vpd15_anom_3mo", "vpd15_anom_6mo", "vpd15_anom_sd", 
                             # "vpd15_sd", "vpd15_u", 
                             "x", "y", "year"))


library(tidyverse); library(data.table); library(lubridate); library(dtplyr)
tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id","season","precip_anom", 
                             "precip_anom_12mo","map",
                             # "ndvi_anom", "ndvi_anom_12mo","ndvi_anom_sd", 
                             # "ndvi_mcd", 
                             "x", "y", "year")) %>% as.data.table()
tmp %>% lazy_dt() %>% 
  filter(hydro_year >= 1982 & hydro_year < 2020) %>% 
  filter(y < -23.5 & y > -35) %>% 
  group_by(hydro_year,season) %>% 
  summarize(val = mean(precip_anom,na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  mutate(val_pr = percent_rank(val)*100) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  ggplot(data=., aes(hydro_year, val_pr, color=season))+geom_line()+theme_dark()+
  geom_vline(aes(xintercept=2017),col='red')+
  geom_vline(aes(xintercept=2003),col='red')





tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id","season","precip_anom", 
                             "precip_anom_3mo","precip_anom_36mo",
                             "precip_anom_12mo","map", 
                             "tmax","tmax_anom","tmax_anom_sd", "matmax",
                             "vpd15","vpd15_anom","vpd15_anom_sd","mavpd15",
                             "pet","mapet","pet_anom","pet_anom_3mo",
                             "pe","mape",
                             "ndvi_anom", "ndvi_anom_12mo","ndvi_anom_sd",
                             "ndvi_mcd",
                             'vc','veg_class',
                             "x", "y", "year")) %>% as.data.table()
unique(tmp[,.(vc,veg_class)]) %>% View

library(paletteer)
vec_ids <- tmp[veg_class %in% 2:3][,.(id)] %>% unique %>% pull(id)
# sample randomly
tmp %>% lazy_dt() %>% 
  filter(veg_class %in% 1:4) %>% 
  sample_n(10000) %>% 
  as_tibble() %>% 
  ggplot(data=., aes(tmax, ndvi_mcd,
                     color=hydro_year, 
                     group=vc))+
  geom_point()+
  scale_color_viridis_c()+
  geom_smooth(se=F,color='red')+
  facet_wrap(~vc, scales='free')

set.seed(1)
tmp[veg_class==3] %>% 
  # .[id %in% c(44208,41405,42710,40976,3926,41028,1205,1897,13756)] %>%
  .[id %in% sample(vec_ids, 500)] %>%
  ggplot(data=., aes(tmax, ndvi_mcd,
                     # color=as.factor(id),
                     color=matmax,
                     # color=hydro_year, 
                     group=vc))+
  geom_point(alpha=0.5)+
  geom_vline(aes(xintercept=32.5),col='red')+
  scale_color_viridis_c(option='B',end=0.9)+
  geom_smooth(se=F, aes(tmax, ndvi_mcd,group=as.factor(id)), 
              span=0.5)

tmp[veg_class %in% c(1:4)] %>% 
  .[y> -35 & y < -23.5] %>% 
  .[id %in% sample(vec_ids, 100)] %>%
  lazy_dt() %>% 
  filter(ndvi_mcd > 0) %>% 
  mutate(matmax = cut_interval(matmax,4), 
         map = cut_interval(map,4), 
         mapet = cut_interval(mapet,4), 
         mape = cut_interval(mape,4)) %>% 
  as_tibble() %>% 
  ggplot(data=., aes(tmax_anom, ndvi_anom_sd,
                     color=as.factor(id),
                     # color=matmax,
                     ))+
  geom_point(alpha=0.25,color='black')+
  geom_hline(aes(yintercept=0),col='yellow')+
  geom_vline(aes(xintercept=0),col='blue')+
  geom_vline(aes(xintercept=2),col='red')+
  # scale_color_viridis_c(option='B',end=0.9)+
  geom_smooth(se=F,inherit.aes = T, 
              # method='lm',
              # aes(tmax_anom, ndvi_anom,color=vc),
              span=0.5)+
  facet_grid(~mape, labeller = label_both)+
  theme(legend.position = 'bottom')


library(mgcv); library(mgcViz)
# pe: precip/pet

test <- tmp[veg_class %in% c(2:3)] %>% 
  .[y> -35 & y < -23.5] %>% 
  .[id %in% sample(vec_ids, 1000)] %>% 
  .[,`:=`(id=as.factor(id))]
m1 <- bam(ndvi_anom_sd~s(mape,by=tmax_anom,k=5)+s(id,bs='re'),
         data=test,
         select=T, discrete = T, method='fREML')
summary(m1)

m2 <- bam(ndvi_anom_sd~s(mape,by=vpd15_anom,k=5,bs='ts')+s(id,bs='re'),
         data=test,
         select=T, discrete = T, method='fREML')
summary(m2)

m3 <- bam(ndvi_anom_sd~s(mape,by=vpd15_anom,k=5)+
            s(map,by=precip_anom,k=5)+
            s(id,bs='re'), 
          data=test, 
          select=T, discrete = T, method='fREML')
m4 <- bam(ndvi_anom_sd~s(mape,by=vpd15_anom,k=5)+
            s(map,by=precip_anom,k=5)+
            s(mapet,by=pet_anom,k=5)+
            s(id,bs='re'), 
          data=test, 
          select=T, discrete = T, method='fREML')
m5 <- bam(ndvi_anom_sd~
            s(mape,by=vpd15_anom,k=5)+
            s(map,by=precip_anom,k=5)+
            s(mapet,by=pet_anom,k=5)+
            s(map,by=precip_anom_3mo,k=5)+
            s(id,bs='re'), 
          data=test, 
          select=T, discrete = T, method='fREML')

m6 <- bam(ndvi_anom_sd~
            s(mape,vpd15_anom,precip_anom_3mo,k=5)+
            s(id,bs='re'), 
          data=test, 
          select=T, discrete = T, method='fREML', nthreads = 8)



bbmle::AICtab(m1,m2,m3,m4,m5,m6)

plot(m2,select = 1)
plot(m3,select = 2,scale = 0)
plot(m5,scale=0,pages=1)
summary(m2)
summary(m5)


plot(sm(getViz(m1),1))+
  l_fitRaster()+
  l_fitContour()+
  l_rug()+
  scale_fill_gradient2()






# MCD43  ------------------------------------------------------------
o1 <- stars::read_stars("../data_general/MCD15/MCD15A3_LAI_5000m_SE_Oz_mmean_maskFireDefor_2003_2019.tif") %>% 
  # slice('band', seq(1,by=2,to = 456)) %>% 
  st_set_dimensions(., 3, 
                    values=seq(ymd("2003-01-01"),ymd("2019-12-01"),by="1 month"), 
                    names = 'date') %>%  
  as_tibble() %>% 
  as.data.table() %>% 
  set_names(c("x","y","date","lai"))
o1


o1 %>% lazy_dt() %>% 
  # filter(hydro_year >= 1982 & hydro_year < 2020) %>% 
  filter(y < -23.5 & y > -35) %>% 
  mutate(year=year(date)) %>% 
  group_by(year) %>% 
  summarize(val = mean(lai,na.rm=TRUE)) %>% 
  ungroup() %>% 
  # group_by(season) %>% 
  mutate(val_pr = percent_rank(val)*100) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  ggplot(data=., aes(year, val))+geom_line()+theme_dark()+
  geom_vline(aes(xintercept=2017),col='red')+
  geom_vline(aes(xintercept=2003),col='red')


o1 %>% filter(date==ymd("2019-09-01")) %>% ggplot(aes(x,y,fill=lai))+geom_tile()










###########################################################################
tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet",
                           col_select = c(
                             "date", "hydro_year", "id","season","precip_anom", 
                             "precip_anom_3mo","precip_anom_36mo",
                             "precip_anom_12mo","map", 
                             "tmax","tmax_anom","tmax_anom_sd", "matmax",
                             "vpd15","vpd15_anom","vpd15_anom_sd","mavpd15",
                             "vpd15_u",
                             "pet","mapet","pet_anom","pet_anom_3mo","pet_u",
                             "pe","mape",
                             "ndvi_anom", "ndvi_anom_12mo","ndvi_anom_sd",
                             "ndvi_mcd",
                             'vc','veg_class',
                             'month',
                             "x", "y", "year")) %>% as.data.table()
unique(tmp[,.(vc,veg_class)]) %>% View

train <- tmp[veg_class%in%c(2:3)]  %>% 
  .[y> -35 & y < -23.5] %>% 
  .[ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5] %>% 
  # .[hydro_year >= min_year & hydro_year <= max_year] %>% 
  .[,`:=`(id=as.factor(id))] %>% 
  .[sample(.N,50000)]

m <- bam(ndvi_anom_sd~
            s(mape,by=vpd15_anom,k=5)+
            s(map,by=precip_anom,k=5)+
            s(mapet,by=pet_anom,k=5)+
            s(map,by=precip_anom_3mo,k=5)+
            s(map,by=precip_anom_12mo,k=5)+
            # s(id,bs='re'), 
          data=train, 
          select=T, discrete = T, method='fREML')
plot(m,scale=0,pages=1)

m1 <- bam(ndvi_anom_sd~
           s(mape,by=vpd15_anom,k=5)+
           # s(map,by=precip_anom,k=5)+
           s(mapet,by=pet_anom,k=5)+
           s(map,by=precip_anom_3mo,k=5)+
           s(map,by=precip_anom_12mo,k=5),
           data=train, 
         select=T, discrete = T, method='fREML')
summary(m1)
print(plot(getViz(m2),allTerms=T),pages=1)


train <- train %>% lazy_dt() %>% 
  mutate(x_p12 = (precip_anom_12mo)/(precip_anom_12mo+map), 
         # x_v = (vpd15_anom - vpd15_u)/(vpd15_anom+vpd15_u), 
         x_v = (vpd15-vpd15_u)/(vpd15+vpd15_u),
         x_p3 = (precip_anom_3mo-map)/(precip_anom_3mo+map), 
         x_pet3 = -(pet_anom_3mo - pet_u)/(pet_u) - 1) %>% 
  as_tibble()

m2 <- bam(ndvi_anom_sd~ 
            x_v*season + x_p12*season + x_p3*season + 
            s(x_pet3,by=season, k=5),
          data=train, 
          select=T, discrete = T, method='fREML')
summary(m2)
print(plot(getViz(m2),allTerms=T),pages=1)

m3 <- bam(ndvi_anom_sd~ 
            s(month,by=x_v, bs='cc', k=5)+
            s(month,by=x_p12, bs='cc', k=5)+
            s(month,by=vc,bs='fs', xt=list(bs='cc'))+
            s(mandvi,ndvi_u)+
            s(x,y,fx = TRUE, k=60),
            # s(x_v,by=season,bs='fs') + 
            # x_p12*season + x_p3*season + 
            # t2(x_pet3,season, k=5, bs='fs', xt=list(bs='cr'), full=TRUE),
          data=train, 
          select=T, discrete = T, method='fREML')
summary(m3)
print(plot(getViz(m3),allTerms=T),pages=1)
plot(m3, scale=0, pages=1)
appraise(m3)
curve(-(x-100)/(x+100),-50,150)

ggplot(data=train, aes(pet_anom_3mo,x_pet3,color=mapet))+
  geom_point()+scale_color_viridis_c()

for(i in 1986:2014){
  min_year <- i-5
  max_year <- i+5
  train <- tmp[veg_class%in%c(2:3)]  %>% 
  .[y> -35 & y < -23.5] %>% 
  .[hydro_year >= min_year & hydro_year <= max_year] %>% 
  .[,`:=`(id=as.factor(id))] %>% 
  .[sample(.N,50000)]
  m <- bam(ndvi_anom_sd~
        s(mape,vpd15_anom,precip_anom_3mo,k=5)+
        s(id,bs='re'), 
      data=train, 
      select=T, discrete = T, method='fREML', nthreads = 8)
}
test <- tmp[veg_class %in% c(2:3)] %>% 
  .[y> -35 & y < -23.5] %>% 
  .[id %in% sample(vec_ids, 1000)] %>% 
  .[,`:=`(id=as.factor(id))]

tmp %>% lazy_dt() %>% mutate(id=as.factor(id)) %>% show_query()

m6 <- bam(ndvi_anom_sd~
            s(mape,vpd15_anom,precip_anom_3mo,k=5)+
            s(id,bs='re'), 
          data=test, 
          select=T, discrete = T, method='fREML', nthreads = 8)


25*(1000*1000)


# nice
tmp %>% 
  sample_n(1e4) %>% 
  filter(mape <10) %>% 
  ggplot(data=., aes(mape, ndvi_mcd))+geom_point()+
  geom_smooth(color='red')+
  geom_vline(aes(xintercept=1),col='red')+
  geom_smooth(method='gam',se=F,
              formula=y~x,
              method.args=list(family=betar(link='probit')))
  # geom_smooth(method='nls',se=F,
  #             formula=y~SSlogis(mape,Asym,xmid,scal),
  #             method.args=list(start("Asym"=1, 
  #                                    "xmin"=0.5,
  #                                    "scal"=0.25)))

nls(ndvi_3mo~log(beta*mape/(1+mape)), 
    data=dat1 %>% sample_n(1e4), 
    start=list("beta"=1))




# Blah
tmp %>% 
  sample_n(1e5) %>% 
  filter(mape <10) %>% 
  ggplot(data=., aes(mapet, ndvi_3mo))+geom_point()+
  geom_smooth()

#
tmp %>% 
  sample_n(1e5) %>% 
  filter(mape <10) %>% 
  ggplot(data=., aes(map, ndvi_3mo))+geom_point()+
  geom_smooth()

curve(SSlogis(x, Asym = 1, xmid = 0.5,scal = 0.2), 0,3,ylim=c(0,1))

m <- nls(ndvi_3mo~SSlogis(mape,Asym, xmid,scal), 
         start=list(Asym= 1, 
                    xmid = 0.5, 
                    scal = 0.2), 
         data=dat1 %>% 
           sample_n(1e4))
summary(m)
plot(ndvi_3mo~mape,data=dat1 %>% sample_n(1e4))
curve(SSlogis(x, Asym = coef(m)["Asym"], xmid = coef(m)["xmid"],scal = coef(m)["scal"]), 0,3,ylim=c(0,1),add=T,col='red')
abline(v=coef(m)["xmid"],col='red')
abline(h=coef(m)["Asym"],col='red')
abline(0,coef(m)["scal"],col='red')


md <- tmp  %>% 
  .[y> -35 & y < -23.5] %>% 
  .[ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5] %>% 
  .[ndvi_mcd > 0] %>% 
  # .[year < 1990] %>%
  .[is.infinite(mape)==F] %>% 
  sample_n(4e5) %>% select(mape,ndvi_3mo,year)

m1 <- nls(ndvi_3mo~SSlogis(mape,Asym, xmid,scal), 
         start=list(Asym= 1, 
                    xmid = 0.5, 
                    scal = 0.2), 
         data=md %>% filter(year<1990))
m2 <- nls(ndvi_3mo~SSlogis(mape,Asym, xmid,scal), 
          start=list(Asym= 1, 
                     xmid = 0.5, 
                     scal = 0.2), 
          data=md %>% filter(between(year,1990,1999)))
m3 <- nls(ndvi_3mo~SSlogis(mape,Asym, xmid,scal), 
          start=list(Asym= 1, 
                     xmid = 0.5, 
                     scal = 0.2), 
          data=md %>% filter(between(year,2000,2009)))
m4 <- nls(ndvi_3mo~SSlogis(mape,Asym, xmid,scal), 
          start=list(Asym= 1, 
                     xmid = 0.5, 
                     scal = 0.2), 
          data=md %>% filter(between(year,2010,2019)))

vec_cols <- viridis::inferno(5)
curve(SSlogis(x, Asym = coef(m1)["Asym"], xmid = coef(m1)["xmid"],scal = coef(m1)["scal"]), 0.01,3,ylim=c(0,0.8),add=F,col=vec_cols[1])
curve(SSlogis(x, Asym = coef(m2)["Asym"], xmid = coef(m2)["xmid"],scal = coef(m2)["scal"]), 0.01,3,add=T,col=vec_cols[2])
curve(SSlogis(x, Asym = coef(m3)["Asym"], xmid = coef(m3)["xmid"],scal = coef(m3)["scal"]), 0.01,3,add=T,col=vec_cols[3])
curve(SSlogis(x, Asym = coef(m4)["Asym"], xmid = coef(m4)["xmid"],scal = coef(m4)["scal"]), 0.01,3,add=T,col=vec_cols[4])

j1 <- tmp  %>% 
  .[y> -35 & y < -23.5] %>% 
  # .[year < 1990] %>% 
  .[,.(ep = sum(pet,na.rm=TRUE)/sum(precip,na.rm=TRUE), 
       ndvi = mean(ndvi_mcd,na.rm=TRUE)),by=.(x,y,year,vc)] %>% 
  .[,.(maep = mean(ep,na.rm=TRUE), 
       maep_sd = sd(ep,na.rm=TRUE), 
       mandvi = mean(ndvi,na.rm=TRUE), 
       mandvi_sd = sd(ndvi,na.rm=TRUE)),
    by=.(x,y,vc)]

j1 %>% ggplot(data=., aes(maep, mandvi,color=vc))+
  geom_point()+geom_smooth(se=F,method='loess')+
  facet_wrap(~vc)



tmp <- arrow::read_parquet("/home/sami/scratch/ARD_ndvi_aclim_anoms.parquet")
sort(names(tmp))

vi <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet") %>% 
  as.data.table() %>% 
  .[is.infinite(ndvi_mcd)==F]

vi[,.(x,y,ndvi_c,date)][,j1,by=.(x,y)]


j1 <- vi[date<=ymd("1999-12-31")][,.(max_ndvi_c = max(ndvi_c,na.rm=TRUE)),by=.(x,y)]
j2 <- vi[date>ymd("1999-12-31")][,.(max_ndvi_mcd = max(ndvi_mcd,na.rm=TRUE)),by=.(x,y)]
j1[j2,on=.(x,y)] %>% 
  filter(max_ndvi_c > 0 & max_ndvi_mcd >0) %>% 
  ggplot(data=., aes(max_ndvi_c, max_ndvi_mcd))+
  ggpointdensity::geom_pointdensity()+
  geom_smooth(method='lm')+
  geom_abline(aes(intercept=0,slope=1),col='red')

 

j1 <- vi[date>=ymd("1981-01-01")&date<=ymd("1989-12-31")][,.(ndvi_c = mean(ndvi_c,na.rm=TRUE)),by=.(x,y)]
j2 <- vi[date>=ymd("1990-01-01")&date<=ymd("1999-12-31")][,.(ndvi_c = mean(ndvi_c,na.rm=TRUE)),by=.(x,y)]
j3 <- vi[date>=ymd("2000-01-01")&date<=ymd("2009-12-31")][,.(ndvi_c = mean(ndvi_c,na.rm=TRUE)),by=.(x,y)]
j4 <- vi[date>=ymd("2010-01-01")&date<=ymd("2017-12-31")][,.(ndvi_c = mean(ndvi_c,na.rm=TRUE)),by=.(x,y)]

d1 <- tmp[date>=ymd("1981-01-01")&date<=ymd("1989-12-31")][,.(mape = mean(pe,na.rm=TRUE)),by=.(x,y)]
d2 <- tmp[date>=ymd("1990-01-01")&date<=ymd("1999-12-31")][,.(mape = mean(pe,na.rm=TRUE)),by=.(x,y)]
d3 <- tmp[date>=ymd("2000-01-01")&date<=ymd("2009-12-31")][,.(mape = mean(pe,na.rm=TRUE)),by=.(x,y)]
d4 <- tmp[date>=ymd("2010-01-01")&date<=ymd("2017-12-31")][,.(mape = mean(pe,na.rm=TRUE)),by=.(x,y)]


m1 <- nls(ndvi_c~SSlogis(mape,Asym, xmid,scal), 
          start=list(Asym= 1, 
                     xmid = 0.5, 
                     scal = 0.2), 
          data=j1[d1,on=.(x,y)])
m2 <- nls(ndvi_c~SSlogis(mape,Asym, xmid,scal), 
          start=list(Asym= 1, 
                     xmid = 0.5, 
                     scal = 0.2), 
          data=j2[d2,on=.(x,y)])
m3 <- nls(ndvi_c~SSlogis(mape,Asym, xmid,scal), 
          start=list(Asym= 1, 
                     xmid = 0.5, 
                     scal = 0.2), 
          data=j3[d3,on=.(x,y)])
m4 <- nls(ndvi_c~SSlogis(mape,Asym, xmid,scal), 
          start=list(Asym= 1, 
                     xmid = 0.5, 
                     scal = 0.2), 
          data=j4[d4,on=.(x,y)])

vec_cols <- viridis::inferno(5)
curve(SSlogis(x, Asym = coef(m1)["Asym"], xmid = coef(m1)["xmid"],scal = coef(m1)["scal"]), 0.01,3,ylim=c(0,0.8),add=F,col=vec_cols[1])
curve(SSlogis(x, Asym = coef(m2)["Asym"], xmid = coef(m2)["xmid"],scal = coef(m2)["scal"]), 0.01,3,add=T,col=vec_cols[2])
curve(SSlogis(x, Asym = coef(m3)["Asym"], xmid = coef(m3)["xmid"],scal = coef(m3)["scal"]), 0.01,3,add=T,col=vec_cols[3])
curve(SSlogis(x, Asym = coef(m4)["Asym"], xmid = coef(m4)["xmid"],scal = coef(m4)["scal"]), 0.01,3,add=T,col=vec_cols[4])


library(nlraa)
# Tmax
d1 <- vi[date>=ymd("1981-01-01")&date<=ymd("1989-12-31")] %>% 
  .[,.(ndvi_c,x,y,veg_class,date)] %>% 
  .[tmp[date>=ymd("1981-01-01")&date<=ymd("1989-12-31")][,.(tmax,id,mape,matmax,map,x,y,veg_class,date)],on=.(x,y,veg_class,date)] %>% 
  .[is.na(ndvi_c)==F] %>% 
  .[veg_class == 3]
d2 <- vi[date>=ymd("1990-01-01")&date<=ymd("1999-12-31")] %>% 
  .[,.(ndvi_c,x,y,veg_class,date)] %>% 
  .[tmp[date>=ymd("1990-01-01")&date<=ymd("1999-12-31")][,.(tmax,id,mape,matmax,map,x,y,veg_class,date)],on=.(x,y,veg_class,date)] %>% 
  .[is.na(ndvi_c)==F] %>% 
  .[veg_class == 3]
d3 <- vi[date>=ymd("2000-01-01")&date<=ymd("2009-12-31")] %>% 
  .[,.(ndvi_c,x,y,veg_class,date)] %>% 
  .[tmp[date>=ymd("2000-01-01")&date<=ymd("2009-12-31")][,.(tmax,id,mape,matmax,map,x,y,veg_class,date)],on=.(x,y,veg_class,date)] %>% 
  .[is.na(ndvi_c)==F] %>% 
  .[veg_class == 3]
d4 <- vi[date>=ymd("2010-01-01")&date<=ymd("2017-12-31")] %>% 
  .[,.(ndvi_c,x,y,veg_class,date)] %>% 
  .[tmp[date>=ymd("2010-01-01")&date<=ymd("2017-12-31")][,.(tmax,id,mape,matmax,map,x,y,veg_class,date)],on=.(x,y,veg_class,date)] %>% 
  .[is.na(ndvi_c)==F] %>% 
  .[veg_class == 3]

d1 <- d1 %>% filter(veg_class %in% 3) %>% filter(ndvi_c > 0) %>% filter(matmax >= 25) 
d1 %>% filter(veg_class %in% 2:3) %>%
  filter(matmax >= 25) %>% 
  sample_n(1000) %>% ggplot(data=., aes(tmax,ndvi_c))+geom_point()+geom_smooth()

fit1 <- nls_multstart(
  ndvi_c ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                       (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = d1 %>% sample_n(10000) %>% mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0.5, Hd = 1, Ha = 1, Topt = 273+25),
  start_upper = c(kopt = 0.7, Hd = 500, Ha = 2.5, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0.4, Hd = 200, Ha = 0.1, Topt = 285), 
  upper = c(kopt = 0.9, Hd=200, Ha=100, Topt=310))
fit2 <- nls_multstart(
  ndvi_c ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                     (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = d2 %>% sample_n(10000) %>% mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0.5, Hd = 1, Ha = 1, Topt = 273+25),
  start_upper = c(kopt = 0.7, Hd = 500, Ha = 2.5, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0.4, Hd = 200, Ha = 0.1, Topt = 285), 
  upper = c(kopt = 0.9, Hd=200, Ha=100, Topt=310))
fit3 <- nls_multstart(
  ndvi_c ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                     (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = d3 %>% sample_n(10000) %>% mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0.5, Hd = 1, Ha = 1, Topt = 273+25),
  start_upper = c(kopt = 0.7, Hd = 500, Ha = 2.5, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0.4, Hd = 200, Ha = 0.1, Topt = 285), 
  upper = c(kopt = 0.9, Hd=200, Ha=100, Topt=310))
fit4 <- nls_multstart(
  ndvi_c ~ kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
                     (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt))))))),
  data = d4 %>% sample_n(10000) %>% mutate(Tk=tmax+273.15),
  iter = 1000,
  start_lower = c(kopt = 0.5, Hd = 1, Ha = 1, Topt = 273+25),
  start_upper = c(kopt = 0.7, Hd = 500, Ha = 2.5, Topt = 320),
  # supp_errors = 'Y',
  na.action = na.omit,
  #convergence_count = 500,
  lower = c(kopt = 0.4, Hd = 200, Ha = 0.1, Topt = 285), 
  upper = c(kopt = 0.9, Hd=200, Ha=100, Topt=310))
coef(fit1)
coef(fit2)
coef(fit3)
coef(fit4)

fn <- function(Tk,kopt,Topt,Hd=200,Ha){kopt * ((Hd * (2.718282^((Ha*(Tk-Topt))/(Tk*0.008314*Topt)))) / 
          (Hd - (Ha*(1-(2.718282^((Hd*(Tk-Topt))/(Tk*0.008314*Topt)))))))}
curve(fn(Tk=x+273.15,kopt=coef(fit1)["kopt"],Topt=coef(fit1)["Topt"], Hd=200,Ha=coef(fit1)["Ha"]),10,40, 
      ylim=c(0.2,0.8),xlab=expression(paste(Tmax~(degree*C))),ylab="NDVI")
curve(fn(Tk=x+273.15,kopt=coef(fit2)["kopt"],Topt=coef(fit2)["Topt"], Hd=200,Ha=coef(fit2)["Ha"]),10,40,add=T,col='blue')
curve(fn(Tk=x+273.15,kopt=coef(fit3)["kopt"],Topt=coef(fit3)["Topt"], Hd=200,Ha=coef(fit3)["Ha"]),10,40,add=T,col='purple')
curve(fn(Tk=x+273.15,kopt=coef(fit4)["kopt"],Topt=coef(fit4)["Topt"], Hd=200,Ha=coef(fit4)["Ha"]),10,40,add=T,col='red')
abline(v=coef(fit1)["Topt"]-273.15,col='black');
abline(v=coef(fit2)["Topt"]-273.15,col='blue');
abline(v=coef(fit3)["Topt"]-273.15,col='purple');
abline(v=coef(fit4)["Topt"]-273.15,col='red')
points(ndvi_c~tmax, data=d1 %>% sample_n(100),cex=0.5,pch=20,col='black')
points(ndvi_c~tmax, data=d1 %>% sample_n(100),cex=0.5,pch=20,col='blue')
points(ndvi_c~tmax, data=d1 %>% sample_n(100),cex=0.5,pch=20,col='purple')
points(ndvi_c~tmax, data=d1 %>% sample_n(100),cex=0.5,pch=20,col='red')


d1 %>% sample_n(5000) %>% 
  ggplot(data=., aes(tmax,ndvi_c,color=matmax))+
  # ggpointdensity::geom_pointdensity()+scale_color_viridis_c()+
  geom_point(alpha=0.5)+
  geom_smooth(se=F)+
  geom_vline(aes(xintercept=coef(fit)["Topt"]-273.15),col=vec_cols[3])+
  geom_hline(aes(yintercept=coef(fit)["kopt"]),col=vec_cols[3])+
  scale_color_viridis_c()









curve(SSbgf(x,w.max = 1,t.e = 10,t.m = 100), 5,35)

nls(ndvi_c~SSbgf(tmax, w.max, t.e, t.m), 
    start=list(w.max= 1, 
               t.e = 100, 
               t.m = 0), 
    data=d1)

require(ggplot2)
set.seed(1234)
x <- 1:20
y <- bell(x, 8, -0.0314, 0.000317, 13) + rnorm(length(x), 0, 0.5)
dat <- data.frame(x = x, y = y)
fit <- nls(y ~ SSbell(x, ymax, a, b, xc), data = dat)
## plot
ggplot(data = dat, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = fitted(fit)))


plot(mg)


library(gratia)
bb <- evaluate_smooth(mg, "te(pe,precip_anom_12mo,map)")
plotSlice(sm(getViz(mg),1), fix=list("pe"))
plot(getViz(mg))


sdat <- tmp[date>= ymd("2004-01-01") & date <= "2004-03-01"][is.na(ndvi_mcd)==F] %>% 
  .[ndvi_anom_sd > -3.5 & ndvi_anom_sd < 3.5]
ms_4_3 <- nls(ndvi_mcd~Asym-Drop*exp(-exp(lrc)*pe^pwr)+
      beta*(precip_anom_12mo/map)+
      alpha*(tmax), 
    start=list(Asym=1, 
               Drop=1, 
               lrc=1,
               pwr=1, 
               beta=-0.2,
               alpha=0), 
    lower=c(0,0,0,0,-1,-0.1),
    upper=c(1.5,1,1,1,1,0.1),
    algorithm = 'port',
    data=sdat)
summary(ms_4_3)
cor(predict(ms_4_3),sdat$ndvi_mcd)**2
curve(SSweibull(x, 
                coef(ms_4_3)["Asym"], 
                coef(ms_4_3)["Drop"], 
                coef(ms_4_3)["lrc"], 
                coef(ms_4_3)["pwr"]), 0.1, 3)

msg <- bam(ndvi_mcd~s(pe)+s(I(precip_anom_12mo/map))+s(tmax), 
          data=sdat, 
          select=TRUE, method='fREML',discrete = T)
summary(msg)
plot(msg)


ms_4_4 <- nls(ndvi_mcd~Asym-Drop*exp(-exp(lrc)*tmax^pwr), 
              start=list(Asym=1, 
                         Drop=1, 
                         lrc=1,
                         pwr=1), 
              lower=c(0,0,0,0),
              upper=c(1.5,1,1,1),
              algorithm = 'port',
              data=sdat)


schoolfield_high <- function(lnc, E, Eh, Th, temp, Tc) {
  Tc <- 273.15 + Tc
  k <- 8.62e-5
  boltzmann.term <- lnc + log(exp(E/k*(1/Tc - 1/temp)))
  inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))
  return(boltzmann.term + inactivation.term)
}
curve(schoolfield_high(lnc = 0.86,E=0.1128,Eh = 1.0359,Th=307.558, temp=x+273,Tc = 20),10,43, ylim=c(0,1))
points(ndvi_mcd~tmax, data=sdat %>% sample_n(1000))

fit <- nls_multstart(ndvi_mcd ~ schoolfield_high(lnc, E, Eh, Th, temp = tmax+273.15, Tc = 25),
                     data = sdat %>% sample_n(1000),
                     iter = 50,
                     start_lower = c(lnc = -1, E = 0.01, Eh = 0.2, Th = 285),
                     start_upper = c(lnc = 2, E = 1, Eh = 3, Th = 320),
                     # supp_errors = 'Y',
                     na.action = na.omit,
                     lower = c(lnc = -10, E = 0, Eh = 0, Th = 0))
summary(fit)
cor(predict(fit, newdata=sdat), sdat$ndvi_mcd)**2
curve(schoolfield_high(lnc = coef(fit)["lnc"],E=coef(fit)["E"],
                       Eh = coef(fit)["Eh"],Th=coef(fit)["Th"], 
                       temp=x+273,Tc = 20),10,43, ylim=c(0,1))
points(ndvi_mcd~tmax, data=sdat %>% sample_n(1000))

sdat %>% sample_n(1000) %>% ggplot(data=., aes(tmax,ndvi_mcd))+geom_point()+geom_smooth()


fit <- nls_multstart(ndvi_mcd ~ schoolfield_high(lnc, E, Eh, Th, temp = tmax+273.15, Tc = 25),
                     data = sdat %>% sample_n(1000),
                     iter = 50,
                     start_lower = c(lnc = -1, E = 0.01, Eh = 0.2, Th = 285),
                     start_upper = c(lnc = 2, E = 1, Eh = 3, Th = 320),
                     # supp_errors = 'Y',
                     na.action = na.omit,
                     lower = c(lnc = -10, E = 0, Eh = 0, Th = 0))


tmp[is.na(ndvi_mcd)==F][date==ymd("1994-01-01")]

df_fit <- tibble(date=unique(tmp[is.na(ndvi_mcd)==F][date!=ymd("1994-01-01")]$date), 
       lnc=NA_real_, 
       E=NA_real_, 
       Eh=NA_real_, 
       Th=NA_real_, 
       R2 = NA_real_)
for(i in 1:dim(df_fit)[1]){
  sdat <- tmp[date==df_fit$date[i]] %>% 
    .[is.na(ndvi_mcd)==F] %>% 
    .[ndvi_anom_sd>-3.5 & ndvi_anom_sd < 3.5] %>% 
    .[sample(.N,3000)]
  fit <- nls_multstart(ndvi_mcd ~ schoolfield_high(lnc, E, Eh, Th, temp = tmax+273.15, Tc = 25),
                       data = sdat,
                       iter = 10,
                       start_lower = c(lnc = 0.5, E = 0.01, Eh = 0.2, Th = 285),
                       start_upper = c(lnc = 5, E = 1, Eh = 3, Th = 310),
                       supp_errors = 'Y',
                       na.action = na.omit,
                       lower = c(lnc = 0.5, E = 0, Eh = 0, Th = 273))
  df_fit[i,2:5] <- t(coef(fit))
  df_fit[i,6] <- cor(sdat$ndvi_mcd, predict(fit))**2
  print(df_fit[i,])
}

sdat <- tmp[date==ymd('1994-05-01')] %>% 
  .[is.na(ndvi_mcd)==F] %>% 
  .[ndvi_anom_sd>-3.5 & ndvi_anom_sd < 3.5] %>% 
  .[sample(.N,3000)]
fit <- nls_multstart(ndvi_mcd ~ schoolfield_high(lnc, E, Eh, Th, temp = tmax+273.15, Tc = 25),
                     data = sdat,
                     iter = 100,
                     start_lower = c(lnc = 0.5, E = 0.25, Eh = 0.2, Th = 273.15+15),
                     start_upper = c(lnc = 5, E = 1, Eh = 3, Th = 310),
                     supp_errors = 'Y',
                     na.action = na.omit,
                     lower = c(lnc = 0.5, E = 0.25, Eh = 0, Th = 283))
summary(fit)
curve(schoolfield_high(lnc = coef(fit)["lnc"],E=coef(fit)["E"],
                       Eh = coef(fit)["Eh"],Th=coef(fit)["Th"], 
                       temp=x+273,Tc = 20),10,43, ylim=c(0,1))
points(ndvi_mcd~tmax, data=sdat %>% sample_n(1000))


tmp[date==ymd('1994-07-01')] %>% 
  .[is.na(ndvi_mcd)==F] %>% 
  .[ndvi_anom_sd>-3.5 & ndvi_anom_sd < 3.5] %>% 
  .[sample(.N,3000)] %>% 
  ggplot(data=., aes(tmax, ndvi_mcd))+geom_point()

df_fit %>% 
  # filter(R2 > 0.4) %>% 
  ggplot(data=.,aes(date,Th-273.15))+geom_point()+geom_smooth(method='loess')
df_fit %>% 
  filter(R2 > 0.4) %>% ggplot(data=.,aes(date,lnc))+geom_point()+geom_smooth(method='loess')
df_fit %>% ggplot(data=.,aes(date,E))+geom_point()+geom_smooth(method='loess')
df_fit %>% 
  filter(R2 > 0.4) %>% ggplot(data=.,aes(date,Eh))+geom_point()+geom_smooth(method='loess')
df_fit %>% ggplot(data=.,aes(date,R2))+geom_point()+geom_smooth(method='loess')

df_fit %>% 
  filter(month(date)==1) %>% 
  ggplot(data=., aes(Eh,Th-273.15,color=R2))+
  geom_point()+
  scale_color_viridis_c()

df_fit %>% filter(R2 == min(R2))


df_fit[1,2:5] <- t(coef(fit))

df_fit[1,2:5] <- t(c(0,0,0,0))

sdat %>% lazy_dt() %>% sample_n(100) %>% show_query()
tmp[date==df_fit$date[1]][sample(.N,1000)]
tmp[is.na(ndvi_mcd)==F]$date %>% min
df_fit$date[1]





# Simple non-linear gaussian model
x <- rnorm(100)
y <- rnorm(100, mean = 2 - 1.5^x, sd = 1)
data5 <- data.frame(x, y)
bprior5 <- prior(normal(0, 2), nlpar = a1) +
  prior(normal(0, 2), nlpar = a2)
fn <- function(x,a1,a2) a1 - a2**x
fit5 <- brm(bf(y ~ fn(x,a1,a2), a1 + a2 ~ 1, nl = TRUE),
            data = data5, prior = bprior5)
summary(fit5)
plot(conditional_effects(fit5), ask = FALSE)




schoolfield_high <- function(lnc, E, Eh, Th, temp, Tc) {
  Tc <- 273.15 + Tc
  k <- 8.62e-5
  boltzmann.term <- lnc + log(exp(E/k*(1/Tc - 1/temp)))
  inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))
  return(boltzmann.term)
}
schoolfield_high(1,1,1,300,30,25)

fn2 <- function(lnc,E,Eh,Th,temp,Tc){
  return(lnc + log(exp(E/8.62e-5*(1/(Tc+273.15) - 1/(temp)))) )}# +log(1/(1 + exp(Eh/8.62e-5*(1/Th - 1/(temp+273.15)))))}
fn2(lnc=1,E=1,Eh=1,Th=300,temp=30,Tc=25)
lnc <- 1; E <- 1; Eh <- 1; Tc <- 25; temp <- 30
fn(2)




x <- ts(dat$ndvi_hyb, 
        start=c(1982,1), 
        end=c(2019,12),
        frequency=12)
s <- ssa(x, L=13) # or 1d-ssa?
g <- gapfill(s, groups = list(c(1,2,3)), method='simultaneous')
plot(x,lwd=3,ylim=c(0,1));lines(g,col='red')
xx <- ts(coalesce(x,g),
         start=c(1982,1), 
         end=c(2019,12),
         frequency=12)
s <- ssa(xx, L=37) # or 1d-ssa?
r <- reconstruct(s,groups = list(c(1),c(2),c(3),c(4)))
plot(r)

plot(s, type = 'series')
f1 <- ts(reconstruct(s, groups=list(c(1)))$F1, 
           start=c(1982,1), 
           end=c(2019,12),
           frequency=12)
f2 <- ts(reconstruct(s, groups=list(c(2)))$F1, 
         start=c(1982,1), 
         end=c(2019,12),
         frequency=12)
f3 <- ts(reconstruct(s, groups=list(c(3)))$F1, 
         start=c(1982,1), 
         end=c(2019,12),
         frequency=12)
f4 <- ts(reconstruct(s, groups=list(c(4)))$F1, 
         start=c(1982,1), 
         end=c(2019,12),
         frequency=12)
f5 <- ts(reconstruct(s, groups=list(c(5)))$F1, 
         start=c(1982,1), 
         end=c(2019,12),
         frequency=12)

f12 <- ts(reconstruct(s, groups=list(c(1:2)))$F1, 
         start=c(1982,1), 
         end=c(2019,12),
         frequency=12)
n_c <- ts(reconstruct(s, groups=list(c(2,3,4)))$F1, 
          start=c(1982,1), 
          end=c(2019,12),
          frequency=12)
f34 <- ts(reconstruct(s, groups=list(c(3,4)))$F1, 
           start=c(1982,1), 
           end=c(2019,12),
           frequency=12)
f1234 <- ts(reconstruct(s, groups=list(c(1,2,3,4)))$F1, 
          start=c(1982,1), 
          end=c(2019,12),
          frequency=12)
f_c <- ts(reconstruct(s, groups=list(c(2:14)))$F1, 
            start=c(1982,1), 
            end=c(2019,12),
            frequency=12)

signal::filtfilt(n_c)

cols <- hcl.colors('Viridis',n=3)
plot(xx) # original
lines(f1,ylim=c(0,0.8),col='darkgreen')
lines(f1+f2,ylim=c(0,0.8),col='darkgreen')
lines(f1+f3,ylim=c(0,0.8),col='darkgreen')
lines(f1+f4,ylim=c(0,0.8),col='darkgreen')
lines(f1+f5,ylim=c(0,0.8),col='darkgreen')
lines(f1+f6,ylim=c(0,0.8),col='darkgreen')

plot(xx,col='red')
loess(xx~time(xx), span=5/38) %>% 
  predict %>% 
  ts(., start=c(1982,1), 
        end=c(2019,12),
        frequency=12) %>% 
  lines(type='l')
microbenchmark::microbenchmark(
  roll::roll_min(xx,width=12,complete_obs = T, min_obs = 1),
  RcppRoll::roll_minr(xx, n=12, fill=NA)
)
lines(xx*0+roll::roll_min(xx,width=12,complete_obs = T, min_obs = 1))

lines(f1-f34,col='blue')
lines(f34)
plot(f1234,lwd=3,col='black')
lines(f1234-f34,col=cols[2],lwd=2)
lines(grass+mean(tree,na.rm = TRUE),col=cols[2],lwd=2)
dat %>% select(x,y)

plot(f1234-f34)

out[coverFraction=='npv']$coverFraction.1 %>% hist

dcast(out[x>=143.5782 & x<=144], 
      x+y+yearSeason~coverFraction, 
      value.var = 'coverFraction.1',
      fun.aggregate = first)$soil %>% summary

out[x>=143.5782 & x<=144 & coverFraction=='soil']$coverFraction.1 %>% summary
out$soil %>% hist


dat <- merge(dat, kop, by=c("x","y"))

dat %>% lazy_dt() %>% 
  group_by(zone, season) %>% 
  summarize(val = mean(precip,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  ggplot(data=.,aes(season,val,color=zone))+
  geom_point()+
  facet_wrap(~zone)


o <- dat %>% 
  lazy_dt() %>% 
  group_by(x,y) %>% 
  summarize(val=mean(mape,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble()
sum(o$val<0.333,na.rm=TRUE)/length(o$val)



curve(SSweibull(x, 1,0.9,-2,pwr = 5), 0,2)



?mvnfast::rmvn

d <- 5
mu <- 1:d

# Creating covariance matrix
tmp <- matrix(rnorm(d^2), d, d)
mcov <- tcrossprod(tmp, tmp)
mcov %>% image

set.seed(414)
o1 <- rmvn(4, 1:d, mcov)
o2 <- rmvn(4, 1:d, mcov)
cov(o1)-cov(o2) %>% image
cov(o1) %>% image

cor(o1)-cor(o2) %>% image
cor(o1) %>% image
cor(o2) %>% image


ldat %>% 
  filter(date>=ymd("2009-01-01")) %>% 
  group_by(date,hydro_year) %>% 
  summarize(val=mean(precip_anom_12mo,na.rm=TRUE)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  ggplot(data=.,aes(date,val))+
  geom_label(aes(label=hydro_year))


x <- rnorm(100)
y <- 0.5 + 0.5*x + 0.25*x**2 + 0.125*x**3 + rnorm(100, mean=0,sd=sqrt(abs(x)))
plot(y~x)
tibble(x,y) %>% 
  ggplot(data=.,aes(x,y))+
  geom_point()+
  geom_smooth(fill='navy')+ # LOESS default
  geom_smooth(method='gam', # generalized additive model
              formula=y~s(x,bs='cs'), # cubic regression spline; good at not getting too wiggly
              method.args=list(method='REML', # restricted maximum likelihoo 
                               select=TRUE),  # penalizes the wiggliness of the spline
              color='orange',fill='orange')

library(tidyverse); library(lubridate)
dat <- read_csv("../data_general/Oz_misc_data/MCD43_OzFlux_subset_CCI_test.csv")
dat %>% 
   ggplot(data=.,aes(date, cci, color=site))+
  geom_line()+
  facet_wrap(~site)













library(tidyverse)
x <- rnorm(100)
y <- 0.5 + 0.5*x + 0.25*x**2 + 0.125*x**3 + rnorm(100, mean=0,sd=1)
plot(y~x)
tibble(x,y) %>%
  ggplot(data=.,aes(x,y))+
  geom_point()+
  geom_smooth(fill='navy')+ # LOESS default
  geom_smooth(method='gam', # generalized additive model
              formula=y~s(x,bs='cs'), # cubic regression spline; good at not getting too wiggly
              method.args=list(method='REML', # restricted maximum likelihoo
                               select=TRUE),  # penalizes the wiggliness of the spline
              color='orange',fill='orange')


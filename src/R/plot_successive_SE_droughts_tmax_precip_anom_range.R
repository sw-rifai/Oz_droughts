tmp %>% 
  .[x >= 145 & x<= 154] %>% 
  .[y >= -39.5 & y<= -28] %>% 
  .[,.(p_anom = mean(precip_anom_12mo,na.rm=TRUE)), by=.(year)] %>% 
  .[year %in% c(1983,2003,2009,2019)] %>% 
  ggplot(data=.,aes(year,p_anom)) +
  geom_line()+
  geom_point()



tmp[year %in% c(1992, 2003, 2018,2019)] %>% 
  .[month == 11] %>%
  # .[id %in% sample.int(60000,1e3)] %>% 
  .[,`:=`(lat = cut(y, breaks=seq(-40,-10,by=5), 
                    # labels = 'a',  
                    include.lowest = T, ordered_result = T), 
          lon = cut(x, breaks=seq(135,155,by=5),
                    include.lowest = T, ordered_result = T))] %>% 
  # xlim = c(145,154),
  # ylim = c(-39.5,-28)
  .[x >= 145 & x<= 154] %>% 
  .[y >= -39.5 & y<= -28] %>% 
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



tmp %>% 
  .[x >= 145 & x<= 154] %>% 
  .[y >= -39.5 & y<= -28] %>% 
  .[year %in% c(1983,2003,2009,2019)] %>%
  .[month %in% c(9,10,11)] %>% 
  .[,.(precip_anom_12mo = mean(precip_anom_12mo,na.rm=TRUE), 
       pet_anom_12mo = mean(pet_anom_12mo,na.rm=TRUE), 
       tmax_anom_12mo = mean(tmax_anom_12mo, na.rm=TRUE), 
       p_05 = quantile(precip_anom_12mo ,0.1,na.rm=TRUE), 
       p_95 = quantile(precip_anom_12mo, 0.9,na.rm=TRUE),
       tmax_05 = quantile(tmax_anom_12mo ,0.1,na.rm=TRUE), 
       tmax_95 = quantile(tmax_anom_12mo, 0.9,na.rm=TRUE)),
    by=.(year)] %>% 
  as_tibble() %>% 
  # filter(is.na(lat)==F) %>% 
  ggplot(data=., aes(precip_anom_12mo, tmax_anom_12mo,
                     color=as.factor(year)))+
  geom_hline(aes(yintercept=0),color='gray')+
  geom_vline(aes(xintercept=0),color='gray')+
  geom_point(alpha=0.4,fill=NA)+
  geom_errorbar(aes(xmin=p_05, 
                    xmax=p_95), width=1)+
  geom_errorbar(aes( 
    ymin=tmax_05, 
    ymax=tmax_95))+
  geom_path(inherit.aes = F,
            aes(precip_anom_12mo,
                tmax_anom_12mo),
            alpha=0.5,lty=1,lwd=0.3)+
  geom_label(aes(label=year))+
  scale_color_viridis_d("", 
                        end=0.75, option='B', direction = 1)+
  # facet_grid(as.factor(lat)~as.factor(lon), drop = TRUE)+
  labs(x=expression(paste("Precip. Anomaly"["12-month"]~(mm~yr**-1))), 
       y=expression(paste("Max. Monthly Temp. Anom."~(degree*C))))+
  theme_linedraw()+
  theme(panel.grid = element_blank(), 
        legend.position = 'none', #c(0.1,0.1), 
        legend.justification = c(0,0),
        legend.key = element_rect(fill='transparent'),
        legend.background = element_rect(fill='transparent')
        # panel.background = element_rect(fill='#EEEEFF')
  )

ggsave(filename = "figures/SE_SON_droughts_tmax_precip_anom.png", 
       width=12, height=10, units='cm')

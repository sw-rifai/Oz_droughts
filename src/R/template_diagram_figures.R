library(tidyverse); library(viridis)
library(ggforce); 
params_base <- c(list(input=0.5, Asym=0.65, xmid=0.35, scal=0.15))
params_elev <- c(list(input=0.5, Asym=0.75, xmid=0.35-0.05, scal=0.175))
do.call(SSlogis, params_base)
do.call(SSlogis, params_elev)

tibble(x=seq(0.01,1,length.out=100)) %>% 
  ggplot(data=., aes(x))+
  geom_function(fun = function(x,Asym,xmid,scal) Asym/(1+exp((xmid-x)/scal)), 
                args=params_base[2:4], 
                col=inferno(5)[1])+
  geom_function(fun = function(x,Asym,xmid,scal) Asym/(1+exp((xmid-x)/scal)), 
                args=params_elev[2:4],
                col=inferno(5)[3])+
  geom_curve(aes(x = 0.5, 
                 xend=0.5-0.075, 
                 y=do.call(SSlogis, params_base), 
                 yend=SSlogis(input=0.5-0.075, 
                              Asym = params_base$Asym, 
                              xmid = params_base$xmid, 
                              scal = params_base$scal)
  ),
  curvature = -0.2, lwd=0.5, 
  color=scales::muted('navy'), 
  arrow = arrow(length = unit(0.03, "npc")))+
  geom_curve(aes(x = 0.5, 
                 xend=0.425, 
                 y=do.call(SSlogis, params_base), 
                 yend=SSlogis(input=0.425, 
                              Asym = params_elev$Asym, 
                              xmid = params_elev$xmid, 
                              scal = params_elev$scal)
  ),
  curvature = -0.2, lwd=0.5, 
  color=scales::muted('red'), 
  arrow = arrow(length = unit(0.03, "npc")))+
  geom_segment(aes(x = 0.425, 
                   xend=0.425, 
                   y=SSlogis(input=0.425, 
                             Asym = params_base$Asym, 
                             xmid = params_base$xmid, 
                             scal = params_base$scal), 
                   yend=SSlogis(input=0.425, 
                                Asym = params_elev$Asym, 
                                xmid = params_elev$xmid, 
                                scal = params_elev$scal)
  ),
  lwd=0.5,lty=3, 
  color='black', 
  arrow = arrow(length = unit(0.1, "npc")))+
  geom_hline(aes(yintercept=0.475),lwd=0.2)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="NDVI",
       x="P:PET")+
  facet_zoom(xlim=c(0.41,0.505),
             ylim=c(0.36,0.55), 
             # zoom.data = T, split = F, 
             show.area = F,
             horizontal = T, zoom.size = 1)+
  theme_linedraw()+
  theme(panel.grid = element_blank())
ggsave("figures/diagram_PPET_CO2_response.png", 
       width=16, height = 10, units='cm')






tibble(x=seq(0.01,2,length.out=100)) %>% 
  ggplot(data=., aes(x))+
  geom_function(fun = function(x,Asym,xmid,scal) Asym/(1+exp((xmid-x)/scal)), 
                     args=params_base[2:4], 
                col=inferno(5)[1])+
  geom_function(fun = function(x,Asym,xmid,scal) Asym/(1+exp((xmid-x)/scal)), 
                args=params_elev[2:4],
                col=inferno(5)[3])+
  geom_curve(aes(x = 0.5, 
                 xend=0.5-0.075, 
                 y=do.call(SSlogis, params_base), 
                 yend=SSlogis(input=0.5-0.075, 
                              Asym = params_base$Asym, 
                              xmid = params_base$xmid, 
                              scal = params_base$scal)
                ),
             curvature = -0.2, lwd=0.5, 
             color=scales::muted('navy'), 
             arrow = arrow(length = unit(0.03, "npc")))+
  geom_curve(aes(x = 0.5, 
                 xend=0.425, 
                 y=do.call(SSlogis, params_base), 
                 yend=SSlogis(input=0.425, 
                              Asym = params_elev$Asym, 
                              xmid = params_elev$xmid, 
                              scal = params_elev$scal)
  ),
  curvature = -0.2, lwd=0.5, 
  color=scales::muted('red'), 
  arrow = arrow(length = unit(0.03, "npc")))+
  geom_hline(aes(yintercept=0.475),lwd=0.2)+
  scale_x_continuous(expand=c(0,0), limits=c(0,1.25))+
  scale_y_continuous(expand=c(0,0), limits=c(0,0.8))+
  labs(y="NDVI",
       x="P:PET")+
  theme_linedraw()+
  theme(panel.grid = element_blank())




# Diagram with P:PET anomaly ----------------------------------------------

logis_model <- function(x,xu, 
                        Asym, Asym2=0, 
                        xmid,xmid2=0, 
                        scal, scal2=0,
                        chydro_year=0){
  (Asym+Asym2*chydro_year) / 
    (1+exp(((xmid+xmid2*chydro_year)- (x))/
             (scal+scal2*chydro_year)))
}




# To plot functions without data, specify range of x-axis


ggplot(data.frame(x = c(-5, 5)), aes(x))+
 geom_function(fun = dnorm, args = list(mean = 2, sd = .5))

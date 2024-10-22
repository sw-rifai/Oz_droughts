library(Rssa)
library(tidyverse)
library(data.table); setDTthreads(threads = 8)
library(lubridate); 
library(dtplyr, warn.conflicts = F);

# load
vi <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet", 
                          col_select = c("x","y","date",
                                         "ndvi_c","ndvi_m","ndvi_hyb", 
                                         "evi2_hyb","evi2_m")) %>% 
  as_tibble() 

# add ids
vi <- vi %>% 
  group_by(x,y) %>% 
  arrange(date) %>% 
  mutate(vid = cur_group_id()) %>% 
  ungroup() %>% 
  as.data.table()

vec_vids <- unique(vi$vid)

# fortify with full list of dates ------
d_dates <- expand_grid(date=seq(min(vi$date), max(vi$date), by='1 month'), 
                       vid=vec_vids) %>% as.data.table()
d_xy <- unique(vi[,.(x,y,vid)])
d_dates <- inner_join(d_dates, d_xy, by='vid')

# vi <- merge(vi, d_dates,
#             by.x = c('date','vid'), by.y=c('date','vid'),
#             allow.cartesian = TRUE)
vi %>% lazy_dt() %>% select(-x,-y) %>% show_query()
vi <- left_join(d_dates,
                vi %>% select(-x,-y),
                by=c("date","vid"))

d_na <- vi %>% group_by(vid) %>% 
  summarize(n_na = sum(is.na(ndvi_hyb))) %>% 
  ungroup()
  
# d_na$n_na %>% hist
# d_na$n_na %>% min
d_na <- d_na %>% filter(n_na < 50)

vi <- vi[vid%in%d_na$vid]

# vi <- vi %>% 
#   lazy_dt() %>% 
#   group_by(vid) %>% 
#   arrange(date) %>%
#   mutate(ndvi_hyb = nafill(ndvi_hyb,'locf')) %>% 
#   ungroup() %>% 
#   as.data.table()

vi$ndvi_hyb %>% is.na %>% table

# # Approach 1 using Equations From Lu et al 2003 RSE ---------------------------------------------
# Y <- Yw + Yh # eq3, Y is a proxy for fraction projected cover
# Y <- a*X + b # eq4
# X <- c*Y + d # eq4, d is the baseline of the veg index 
# Yw <- (1+lambda*S)*ywB # eq5 
# Yh <- S*yhA # eq5 
# X <- c*ywB + d + c*S*(yhA + lambda*ywB) # eq6, forward model 
# # xT = sum(X)/length(Tslow)
# xT <- c*ywB + d + c*s*(yhA + lambda*ywB) # eq7
# xA <- c*(yhA + lambda*ywB) # eq7
# s <- mean(S) # ~0.5
# ywB <- a*(x - s*xa)+b # eq8
# yhA <- a*(1+lambda*s)*xA - lambda*(a*xT + b) # eq8
# Yw <- (1+lambda*S)*(a(x-s*xa)+b) # eq9 
# Yh <- S*(a*(1+lambda*s)*xA - lambda*(axT + b)) # eq9
# X <- Xw + Xh + d # eq 10
# # where Xw <- Xw + Xs
# Xw <- (1+lambda*S)(xT - s*xA - d) # eq11 
# Xws <- (1+lambda*S)*(xT - s*xA) - lambda*S*d # eq11
# Xh <- S*((1+lambda*s)*xA - lambda*xT) + lambda*S*d # eq11
# X <- xT + xC + xI # eq12

# Approach 2 from Ma et al 2020 RSE --------------------------------------------
# Modified from Lu (2003), and adapted to use SSA
# Y <- Ytree + Ygrass # eq2 # Y == EVI(t) in Ma
# Ytree <- (1+lambda*S)*YtreeB # eq3 
# Ygrass <- S*YgrassA # eq3
# YtreeB <- YT - s*YA # eq4, YT is the trend component of SSA
# YgrassA <- (1+lambda*s)*YA - lambda*Y #
# Ytree <- (1+lambda*S)*(Y - s*YA - Ysoil) # eq5
# Ygrass <- S*((1+lambda*s)*YA - lambda*Y) + lambda*S*Ysoil # eq5
# YCmin <- min(c(YC, (p/(2-p) * YCmin*(t-dt)+(1-p)*YC/(2-p)))) # eq6, YC is cyclic component of SSA 
# YCmin <- max(c(YC, (p/(2-p) * YCmax*(t-dt)+(1-p)*YC/(2-p)))) # eq6
# S <- YC/YA + s # <- (YC - YCmin)/YA # eq7


# function to apply SSA over VI data.table --------------------------------
dat <- vi[vid==18088]
fn_ssa <- function(dat){
  lambda <- 0.6 # from Ma
  Ysoil <- 0.137 # 0.5% percentile of NDVI 
  s <- 0.5 # guess!
  # cast to ts
  x <- ts(dat$ndvi_hyb, 
          start=c(1982,1), 
          end=c(2019,12),
          frequency=12)
  # short window SSA to gapfill ts
  s1 <- ssa(x, L=13) # optimal L?
  # g <- reconstruct(s1, groups=list(c(1,2,3)))
  g <- gapfill(s1, groups = list(c(1,2,3))) # gapfill with trend and 1st seasonal component
  
  g1 <- raster::clamp(as.numeric(g), min(x,na.rm=TRUE), max(x,na.rm=TRUE))
  g2 <- g*0+g1
  
  xx <- ts(coalesce(x,g2), # apply g to holes in x
           start=c(1982,1), 
           end=c(2019,12),
           frequency=12)
  # plot(x,ylim=c(0,1),lwd=3);lines(g,col='red');lines(xx,col='blue')
  
  s1 <- ssa(xx, L=13) # optimal L to separate grass/tree? 
  Y <- xx
  YT <- reconstruct(s1, groups=list(1))$F1 # Trend component
  YC <- reconstruct(s1,groups = list(c(2:13)))$F1
  YA <- RcppRoll::roll_meanr(YT, n=12, fill=mean(YT,na.rm=TRUE))
  S <- (YC/YA)+s
  # S <- (YC - min(YC))/(YA) # other form of S which is actually different
  YtreeB <- YT - s*YA
  YgrassA <- (1+lambda*s)*YA - lambda*Y
  Ytree <- (1+lambda*S)*(Y - s*YA - Ysoil)
  Ygrass <- S*((1+lambda*s)*YA - lambda*Y) + lambda*S*Ysoil
  
  dat$ndvi_tree <- as.numeric(Ytree)
  dat$ndvi_grass <- as.numeric(Ygrass)
  return(dat)
}

# apply SSA separation of tree & grass contribution to NDVI
vi <- vi[,fn_ssa(.SD), by=vid]

#*******************************************************************************
#* Add season and hydro year -----
#*******************************************************************************
vec_dates <- data.table(date=sort(unique(vi$date))) %>% 
  .[,quarter:=quarter(date)] %>% 
  mutate(q = case_when(quarter==1~"DJF",
                       quarter==2~"MAM",
                       quarter==3~"JJA",
                       quarter==4~"SON")) %>% 
  mutate(season = factor(q,
                         levels=c("SON","DJF","MAM","JJA"), 
                         ordered=T)) %>% 
  select(date,season) %>% 
  mutate(hydro_year = year(date+months(1)))

vi2 <- merge(x = vi, y = vec_dates, by = 'date')
gc(verbose = T, reset = T, full = T)



# Linear change in VCF  ---------------------------------------------------
library(RcppArmadillo)
system.time(
  lt_ndviTree_season <- vi2[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(ndvi_tree, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)

system.time(
  lt_ndviGrass_season <- vi2[date>= ymd("1982-01-01") & date<= ymd("2019-12-31")] %>% 
    .[,.(val = mean(ndvi_grass, na.rm=TRUE)), by=.(x,y,season,hydro_year)] %>% 
    .[is.na(val)==F] %>% 
    .[,.(b1 = fastLm(X = cbind(1,hydro_year-2000.5), y=val, data=.SD)$coefficients[2]), 
      by=.(x,y,season)]
)

czones <- arrow::read_parquet("data/EOz_clim_kmeans6.parquet") %>% 
  as.data.table()

lt_ndviFrac <- merge(lt_ndviTree_season %>% rename(b1_tree=b1), 
                     lt_ndviGrass_season %>% rename(b1_grass=b1), 
                     by=c("x","y","season"))
lt_ndviFrac <- merge(lt_ndviFrac, czones, by=c("x","y"))


# load("data/gridCell_lm_ndvi_clim.Rdata") # grid cell linear regressions
oz_poly <- sf::read_sf("../data_general/GADM/gadm36_AUS.gpkg", 
                       layer="gadm36_AUS_1")
oz_poly <- sf::st_as_sf(oz_poly)
oz_poly <- sf::st_simplify(oz_poly, dTolerance = 0.05)

p_tree <- lt_ndviFrac %>% 
  ggplot(data=., aes(x,y,fill=b1_tree))+
  geom_sf(inherit.aes = F, data=oz_poly,
          fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,155),
           ylim = c(-45,-10), 
           expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  guides(fill=guide_legend(expression(paste(Delta~NDVI[Tree])), 
                           title.position = 'top'))+
  scale_fill_gradient2(limits=c(-0.003,0.003),oob=scales::squish)+
  # scale_fill_viridis_c(option='B',direction = -1,end=0.95)+
  # scico::scale_fill_scico_d(end=0.9,direction = 1)+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  facet_wrap(~season,nrow = 1)+
  guides(fill=guide_legend(expression(paste(Delta~NDVI[Tree])), 
                           title.position = 'top'))+
  theme(
        #legend.position = c(0.64,0.925),
        #legend.direction = 'horizontal',
        panel.grid = element_blank());p_tree

p_grass <- lt_ndviFrac %>% 
  ggplot(data=., aes(x,y,fill=b1_grass))+
  geom_sf(inherit.aes = F, data=oz_poly,
          fill='gray70',color='gray10')+
  geom_tile()+
  coord_sf(xlim = c(140,155),
           ylim = c(-45,-10), 
           expand = FALSE)+
  scale_x_continuous(breaks=seq(140,155,by=5))+
  scale_fill_gradient2(limits=c(-0.002,0.002),oob=scales::squish)+
  # scale_fill_viridis_c(option='B',direction = -1,end=0.95)+
  # scico::scale_fill_scico_d(end=0.9,direction = 1)+
  labs(x=NULL,y=NULL)+
  theme_linedraw()+
  facet_wrap(~season,nrow = 1)+
  guides(fill=guide_legend(expression(paste(Delta~NDVI[Grass])), 
                           title.position = 'top'))+
  theme(
    #legend.position = c(0.64,0.925),
    #legend.direction = 'horizontal',
    panel.grid = element_blank()); p_grass

library(patchwork)
p_tree+p_grass+plot_layout(nrow=2)
ggsave("figures/prototype_NDVI_fracContrib_ltTrend.png",
       width=16,height=16,units='cm',dpi=350,type='cairo')





vi2[date<=ymd("2015-01-01")][ndvi_tree==max(ndvi_tree)]
vi2[vid==34924] %>%
  mutate(ndvi_test = ndvi_tree+ndvi_grass) %>% 
  select(date,ndvi_hyb, ndvi_tree, ndvi_grass, ndvi_test) %>% 
  gather(-date, key='measure',value='estimate') %>% 
  ggplot(data=.,aes(date,estimate,color=measure))+
  geom_line()+
  geom_smooth(method='lm',se=F)




lt_ndviFrac %>% 
  sample_n(1000) %>% 
  ggplot(data=.,aes(b1_tree,b1_grass,color=cz))+
  geom_point()+
  scale_color_viridis_c()+
  geom_smooth(method='lm')+
  geom_abline(aes(intercept=0,slope=1),col='red')

vi$ndvi_tree %>% hist
vi$ndvi_grass %>% hist


tmp <- vi[vid%in%vec_vids[1]]
tmp <- tmp[,fn_ssa(.SD),by=vid]

tmp %>% 
  ggplot(data=., aes(date, ndvi_grass,color=as_factor(vid)))+
  geom_line()+
  geom_smooth(method='lm')

dat <- tmp[vid==19578] %>% 
  ggplot(data=.,aes(date,ndvi_hyb))+
  geom_line()+
  geom_line(aes(date, ndvi_F2),color='red')

tmp %>% 
  ggplot(data=.,aes(date,ndvi_hyb))+
  geom_line()+
  geom_line(aes(date, ndvi_F2),color='red')+
  facet_wrap(~vid, scales = 'free')



x <- ts(tmp[vid==18088]$ndvi_hyb, 
      start=c(1982,1), 
      end=c(2019,12),
      frequency=12)
s <- ssa(x, L=13) # or 1d-ssa?
summary(s)
plot(s)
plot(s, type = "vectors", idx=1:9)
plot(s, type = "series", groups = as.list(1:3))
plot(s, type = "paired", groups = as.list(1:4))
g <- gapfill(s, groups = list(c(1,2,3,4,5)))
yardstick::rmse_vec(as.numeric(x),as.numeric(g))

xx <- coalesce(x,g)

plot(g); lines(x,col='red'); lines(xx,col='blue')
s <- ssa(xx, L=37) # or 1d-ssa?
s
lines(x,col='red')
r <- reconstruct(s,groups = list(c(1)))
r2 <- reconstruct(s,groups = list(c(1),c(2)))
r3 <- reconstruct(s,groups = list(c(1,3:10)))

r2$F1
plot(x, ylim=c(0,1))
lines(ts(r$F1,start=c(1982,1), 
         end=c(2019,12),
         frequency=12),col='red')
lines(ts(r2$F1+r2$F2,start=c(1982,1), 
         end=c(2019,12),
         frequency=12),col='blue')
lines(ts(r3$F1,start=c(1982,1), 
         end=c(2019,12),
         frequency=12),col='purple')



# Produce series with gaps
F <- co2; F[100:200] <- NA
# Perform shaped SSA
s <- ssa(F, L = 72)
# Fill in gaps using the trend and 2 periodicty components
g <- gapfill(s, groups = list(1:6))
# Compare the result
plot(g)
lines(co2, col = "red")

vi[vid==vec_vids[100]]
vi %>% filter(vid==14562) %>% pull(ndvi_hyb) %>% is.na %>% table

bad <- vi[,.(bad = is.na(ndvi_hyb)),by=.(x,y)]
bad$x %>% is.na %>% table
vi$x %>% is.na %>% table

vi
sum(is.na(NA))

tmp <- vi[is.na(x)==F][,.(n_na=sum(is.na(ndvi_hyb))),by=.(vid)]
tmp[vid==14562]
tmp$n_na %>% table



dtmp <- vi %>% as_tibble() %>% 
  mutate(missing = case_when(ndvi_hyb==NA~1,
                             ndvi_hyb>0~0))
  group_by(x,y) %>% 
  summarize(n_na = sum(is.na(ndvi_hyb))) %>% 
  ungroup()
dtmp %>% filter(n_na>0) %>% pull(x)
dtmp$n_na %>% summary
dtmp$n_na %>% table
tmp %>% #filter(is.na(x)==F) %>% pull(n_na) %>% summary
  filter(n_na>0) %>%  
  ggplot(data=.,aes(x,y,fill=n_na))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()



vi %>% group_by(vid) %>% 
  summarize(n_na = sum(is.na(ndvi_hyb)),na.rm=TRUE) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  pull(n_na) %>% summary


lvi <- lazy_dt(vi)

vi %>% group_by(x,y) %>% 
  summarize(n_na = sum(is.na(ndvi_hyb)),na.rm=TRUE) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  # filter(n_na < 100) %>% pull(n_na) %>% summary
  ggplot(data=.,aes(x,y,fill=n_na))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()

lvi %>% mutate(month=month(date))

vi <- full_join(lazy_dt(d_dates),lazy_dt(vi), by=c('date','vid')) %>% show_query()

vi %>% filter(vid==vec_ids[10]) %>% pull(ndvi_hyb) %>% is.na %>% table




unique(vi$date) %>% length
dim(d_dates)

vi <- d_dates[vi, on=.(date), allow.cartesian=TRUE]

vi <- left_join(lazy_dt(d_dates),lazy_dt(vi), by='date') %>% show_query()

left_join(lazy_dt(d_dates), lazy_dt(vi), by='date') %>% show_query()

vi[vid==vec_ids[10]] %>% pull(date) %>% plot
  
vi[vid==vec_ids[10]] %>% pull(ndvi_hyb) %>% is.na %>% table
 ggplot(data=., aes(date, ndvi_hyb))+geom_line()+
  geom_point()
  
  
  
gg <- g[,14,50,] %>% units::drop_units() %>% as_tibble()
as.ts(rnorm(100))
s <- ssa(as.ts(vi[id==vec_ids[10]]$ndvi_hyb))
s
plot(s)
r <- reconstruct(s, groups = list(c(1,4), c(2,3), c(5,6)))
plot(r,add.residuals = T)


  ggplot(data=., aes(time,rec_ensemble_mean))+
  geom_line()


library(Rssa)
# Decompose 'co2' series with default parameters
s <- ssa(co2)
# Show the summary
summary(s)
plot(s)
# Reconstruct the series, with suitable grouping
r1 <- reconstruct(s, groups = list(c(1, 4), c(2, 3), c(5, 6)))
plot(r1)

# 'groups' argument might contain duplicate entries as well
r2 <- reconstruct(s, groups = list(1, 1:4, 1:6))
plot(r2)

curve(sin(x),0,10*pi)

x <- sin(seq(0,32*pi,length.out = 400))+rnorm(400, mean=0, sd=0.5)
x[sample.int(length(x),5)] <- NA
plot(x,type='l')
x <- ts(x, frequency = 16)
stats::decompose(x) %>% plot
s <- ssa(x, L=37, kind = '1d-ssa')
summary(s)
r <- reconstruct(s,groups = list(1))
plot(r$F1)
r$F1
r %>% attributes


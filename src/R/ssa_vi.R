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
  
d_na$n_na %>% hist
d_na$n_na %>% min
d_na %>% filter(n_na < 50)


# function to apply SSA over VI data.table --------------------------------
fn_mssa <- function(dat){
  s <- ssa(as.ts(dat$ndvi_hyb, 
                 start=c(1982,1), 
                 end=c(2019,12),
                 frequency=12), L=37, kind='1d-ssa') # or 1d-ssa?
  r <- reconstruct(s,groups = list(1))
  dat$ndvi_F1 <- r$F1
  return(dat)
}


tmp <- vi[vid%in%vec_vids[1000:1003]]
tmp <- tmp[,fn_mssa(.SD),by=vid]

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

# Real example: Mars photo
data(Mars)
# Decompose only Mars image (without backgroud)
s <- ssa(Mars, mask = Mars != 0, wmask = circle(50), kind = "2d-ssa")
# Reconstruct and plot trend
plot(reconstruct(s, 1), fill.uncovered = "original")
# Reconstruct and plot texture pattern
plot(reconstruct(s, groups = list(c(13, 14, 17, 18))))

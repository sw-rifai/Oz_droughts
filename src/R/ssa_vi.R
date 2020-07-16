library(Rssa)
library(tidyverse)
library(data.table); setDTthreads(threads = 8)
library(lubridate); 
library(dtplyr);

vi <- arrow::read_parquet("../data_general/MCD43/MCD64_AVHRR_NDVI_hybrid_2020-05-26.parquet", 
                          col_select = c("x","y","date",
                                         "ndvi_c","ndvi_m","ndvi_hyb", 
                                         "evi2_hyb","evi2_m")) %>% 
  as.data.table()



gg <- g[,14,50,] %>% units::drop_units() %>% as_tibble()
as.ts(rnorm(100))
s <- ssa(as.ts(gg$rec_ensemble_mean))
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

x <- sin(seq(0,8*pi,length.out = 100))+rnorm(100, mean=0, sd=0.5)
plot(x,type='l')
s <- ssa(x)
summary(s)
r <- reconstruct(s)
plot(r)



# Real example: Mars photo
data(Mars)
# Decompose only Mars image (without backgroud)
s <- ssa(Mars, mask = Mars != 0, wmask = circle(50), kind = "2d-ssa")
# Reconstruct and plot trend
plot(reconstruct(s, 1), fill.uncovered = "original")
# Reconstruct and plot texture pattern
plot(reconstruct(s, groups = list(c(13, 14, 17, 18))))

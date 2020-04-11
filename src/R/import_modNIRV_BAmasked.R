library(arrow); library(tidyverse)
library(stars); library(lubridate)
src_list <- list.files("../data_general/MOD13A2/","NIRV_1km_EastOz",full.names = T)
vec_dates <- seq(ymd("2003-01-01"),ymd("2019-12-01"),by='1 month')

for(f in 1:length(src_list)){
  # read big file
  mod <- read_stars(src_list[f])
  names(mod) <- 'nirv'
  mod <- st_set_dimensions(mod,3,
                           values=vec_dates,
                           names='time')
  
  #splice filter and convert to dataframe 
  for(y in 2003:2019){
    gc(reset = T, full = T)
    tmp <- mod[,,,str_which(vec_dates,as.character(y))] %>% 
      as_tibble() %>% 
      filter(is.na(nirv)==F) 
    if(y==2003 & f==1){out <- tmp}
    if(exists('out')==T){
      out <- bind_rows(out,tmp)
    }
    rm(tmp); gc()
    print(paste(y,"done"))
  }
  rm(mod); gc()
}

out %>% 
  write_parquet(., sink = "../data_general/MOD13A2/MOD13A2_NIRV_1km_EastOz_NVIStreeClassMask_2003_2019.parquet", 
                compression='gzip', 
                compression_level = 9)

# tmp <- out %>% 
#   filter(time==min(time))
# library(rasterVis)
# tmp %>% dplyr::select(-time) %>% rasterFromXYZ(.) %>% levelplot(margin=F)
# out %>% 
#   group_by(time) %>% 
#   summarize(val = mean(nirv,na.rm=T)) %>% 
#   ungroup() %>% 
#   mutate(year=year(time)) %>% 
#   group_by(year) %>% 
#   summarize(val2 =mean(val)) %>% 
#   ungroup() %>% 
#   ggplot(data=., aes(year, val2))+
#   geom_point()
# 
# vec_x <- out$x %>% unique
# round(vec_x, 1)
# 
# out %>% 
#   filter(time==min(time)) %>% 
#   mutate(x = round(x,1), 
#        y=round(y,1)) %>% 
#   ggplot(data=., aes(x,y,fill=nirv))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c()

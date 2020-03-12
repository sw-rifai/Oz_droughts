# debugging
arrow::write_parquet(data.frame(x=1:10), sink='delete_me.parquet')

library(stars); library(tidyverse); library(lubridate)
# Change this for Storm server ------------------------------------------------------------
base_path <- "../data_general/lpdr_test/LPDR_Oz/"
out_dir <- "../data_general/lpdr_test/LPDR_VOD/"

yr <- 2002
n_cores <- 8 # number of cores to use in the parallel processing

# debugging
arrow::write_parquet(data.frame(x=1:10), sink='delete_me.parquet')


# Hopefully don't change this ---------------------------------------------


# Parallelized processing -------------------------------------------------
library(foreach); library(doParallel)
cl <- makeCluster(n_cores)
registerDoParallel(cl)


for(yr in 2006:2019){
  d_path_yr <- file.path(base_path,yr)
  
  fp_asc <- list.files(d_path_yr, pattern = "[0-9]A.tif", full.names = T)
  fp_desc <- list.files(d_path_yr, pattern = "[0-9]D.tif", full.names = T)
  fp_qa_asc <- list.files(d_path_yr, pattern = "A_QA.tif", full.names = T)
  fp_qa_desc <- list.files(d_path_yr, pattern = "D_QA.tif", full.names = T)
  
  vec_dates <- str_extract(list.files(d_path_yr), pattern = "[0-9]{1,8}") %>% 
    unique() %>% 
    sort()
  
  
data_out <- foreach(i = 1:length(vec_dates), 
                        .packages = c("stars","tidyverse","lubridate"),
                        .combine=rbind) %dopar% {
  
  i_asc <- fp_asc[str_detect(fp_asc, pattern = vec_dates[i])]
  i_desc <- fp_desc[str_detect(fp_desc, pattern = vec_dates[i])]
  i_asc_qa <- fp_qa_asc[str_detect(fp_qa_asc, pattern = vec_dates[i])]
  i_desc_qa <- fp_qa_desc[str_detect(fp_qa_desc, pattern = vec_dates[i])]
  
  try({
  d_asc <- stars::read_stars(i_asc) %>% as_tibble()
  names(d_asc) <- c("x","y","band","val")
  d_desc <- stars::read_stars(i_desc) %>% as_tibble()
  names(d_desc) <- c("x","y","band","val")
  d_asc_qa <- stars::read_stars(i_asc_qa) %>% as_tibble()
  names(d_asc_qa) <- c("x","y","qa_asc")
  d_desc_qa <- stars::read_stars(i_desc_qa) %>% as_tibble()
  names(d_desc_qa) <- c("x","y","qa_desc")
  }, silent=T)

  if(FALSE %in% c(exists("d_asc"),exists("d_desc"),exists("d_asc_qa"),exists("d_desc_qa"))){
    next
  }
  vec_bad_qa <- c(64)
  fn_vod <- function(d_asc, d_asc_qa, 
                     d_desc, d_desc_qa, 
                     date_string){
    out <- inner_join(d_asc, 
                      d_desc, 
                      by=c('x','y','band'), 
                      suffix=c("_asc","_desc"))
    out <- left_join(out, d_asc_qa, by=c('x','y'))
    out <- left_join(out, d_desc_qa, by=c('x','y'))
    out <- out %>% 
      mutate(date= parse_date_time(date_string, orders = "%y%j")) %>% 
      filter(val_asc > -999 & val_desc > -999) %>%
      filter(!qa_asc %in% vec_bad_qa) %>% 
      filter(!qa_desc %in% vec_bad_qa)
    
    return(out)
  }
  
  p <- fn_vod(d_asc, d_asc_qa, 
                 d_desc, d_desc_qa, 
                 vec_dates[i])
  p
}

feather::write_feather(data_out, 
                      path=file.path(out_dir,paste0("LPDRv2_",yr,".feather")))
}
stopCluster(cl)

# Debugging ---------------------------------------------------------------
arrow::write_parquet(data.frame(x=1:10), sink='delete_me.parquet')






arrow::write_parquet(., 
                     sink=paste0("../data_general/clim_grid/awap/parquet/delete_me_",Sys.Date(),".parquet"))


lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE, force=TRUE))




parse_date_time("2003001", orders = "%y%j")

i_asc <- fp_asc[str_detect(fp_asc, pattern = vec_dates[i])]
i_desc <- fp_desc[str_detect(fp_desc, pattern = vec_dates[i])]
i_asc_qa <- fp_qa_asc[str_detect(fp_qa_asc, pattern = vec_dates[i])]
i_desc_qa <- fp_qa_desc[str_detect(fp_qa_desc, pattern = vec_dates[i])]

d_asc <- stars::read_stars(i_asc) %>% as_tibble()
names(d_asc) <- c("x","y","band","val")
d_desc <- stars::read_stars(i_desc) %>% as_tibble()
names(d_desc) <- c("x","y","band","val")
d_asc_qa <- stars::read_stars(i_asc_qa) %>% as_tibble()
names(d_asc_qa) <- c("x","y","qa_asc")
d_desc_qa <- stars::read_stars(i_desc_qa) %>% as_tibble()
names(d_desc_qa) <- c("x","y","qa_desc")

intToBits(32)[1:8]
d_desc_qa
d_asc[1,]

vec_bad_qa <- c(64)
fn_vod <- function(d_asc, d_asc_qa, 
                   d_desc, d_desc_qa){
  out <- inner_join(d_asc %>% filter(band==5) %>% select(-band), 
                   d_desc %>% filter(band==5) %>% select(-band), 
                   by=c('x','y'), 
                   suffix=c("_asc","_desc"))
  out <- left_join(out, d_asc_qa, by=c('x','y'))
  out <- left_join(out, d_desc_qa, by=c('x','y'))
  out <- out %>% 
    filter(val_asc > -999 & val_desc > -999) %>% 
    filter(!qa_asc %in% vec_bad_qa) %>% 
    filter(!qa_desc %in% vec_bad_qa)
  
  return(out)
}
stopCluster(cl)
data_out %>% 
  arrow::write_parquet(., 
                sink=paste0("../data_general/clim_grid/awap/parquet/delete_me_",Sys.Date(),".parquet"))




# detach("package:doParallel", unload = TRUE)
# detach("package:gdalUtils", unload = TRUE)
# detach("package:arrow", unload = TRUE)
# 
# 
# data_out %>% arrow::write_parquet(., 
#                                   sink="deleteme_2003.parquet")
# 
# 
# arrow::write_parquet(data.frame(x=1:10), sink="delete_me.parquet")
# arrow::write_feather(data.frame(x=1:10), sink="delete_me.feather")
# feather::write_feather(data.frame(x=1:10), path="delete_me.feather")
# 
# 
# test <- fn_vod(d_asc, d_asc_qa, 
#                d_desc, d_desc_qa)
# test
# 
# test %>% 
#  ggplot(data=.,aes(x,y,fill=val_desc - val_asc))+
#   geom_tile()+
#   scale_fill_gradient2()
# 
# d_asc %>% 
#   filter(band==5) %>% 
#   filter(val > -999) %>% 
#   ggplot(data=.,aes(x,y,fill=val))+
#   geom_tile()+
#   scale_fill_viridis_c()
# 
# 
# fp_asc[i]
# fp_desc[i]
# fp_qa[i]
# 
# 
# i <- 1
# gdalUtils::gdalinfo(fp_asc[i])
# 
# stars::read_stars(fp_asc[i])
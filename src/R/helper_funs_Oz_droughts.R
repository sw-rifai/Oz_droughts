library(tidyverse); library(lubridate); 

blah_theme <- theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank())


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



# function to cast time series var to multiple lagged columns -------------------------------------------
jetlag2 <- function(data, variable, n=10){
  
  # https://purrple.cat/blog/2018/03/02/multiple-lags-with-tidy-evaluation/
  variable <- enquo(variable)
  
  indices <- seq(from=1, to=n, by=1)
  quosures <- map( indices, ~quo(lag(!!variable, !!.x)) ) %>% 
    set_names(sprintf("lag_%02d", indices))
  out_lag_var <- mutate( data, !!!quosures ) %>% as.matrix()
  
  out_lag_var <- out_lag_var[,1:n]
  out_lag_n <- t(matrix(rep(0:(n-1),dim(data)[1]),n)) %>% as.matrix()
  colnames(out_lag_n) <- vctrs::vec_cast(paste0("_",c(1:n)), character())
  
  out <- data
  out$lag_var <- out_lag_var
  out$lag_n <- out_lag_n
  return(out)
}

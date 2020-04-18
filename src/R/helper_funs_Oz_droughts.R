library(tidyverse); library(lubridate); 

blah_theme <- theme_linedraw()+
  theme(panel.background = element_rect(fill='grey'), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank())



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

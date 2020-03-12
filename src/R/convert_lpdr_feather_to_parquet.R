library(tidyverse); library(lubridate); library(arrow); 
flist <- list.files("../data_general/lpdr_test/LPDR_VOD/", pattern='.feather',full.names = T)
str_extract(flist, pattern = "^.feather")
str_replace(flist, pattern = "feather",replacement = 'parquet')

for(i in 1:length(flist)){
 tmp <- arrow::read_feather(flist[i])  
 arrow::write_parquet(tmp, sink=str_replace(flist[i], pattern = "feather",replacement = 'parquet')) 
}

file.remove(flist)



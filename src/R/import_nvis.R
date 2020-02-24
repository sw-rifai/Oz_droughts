library(tidyverse); library(stars);
se_nvis <- stars::read_ncdf(list.files("../data_general/NVIS/NVIS_tmp/",full.names = T)[1])
dim(se_nvis)
se_nvis[,500:5000,500:5000] %>% plot
se_nvis

list.files("../data_general/NVIS/GRID_NVIS5_1_AUST_EXT_MVG/info/")

list.files("../data_general/NVIS/GRID_NVIS5_1_AUST_EXT_MVG/aus5_1e_mvg/")

r <- raster::raster(
  list.files("../data_general/NVIS/GRID_NVIS5_1_AUST_EXT_MVG/aus5_1e_mvg/",full.names = T)[7])

raster::plot(r)

arrow::install_arrow()

?arrow::read_parquet

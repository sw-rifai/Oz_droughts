library(stars); library(sf); library(tidyverse); 
gdal_utils(util='warp', 
           source="../data_general/NVIS/aus5_1e_mvg.tif",
           destination ="../data_general/NVIS/AVHRR_REGRID_aus5_1e_mvg.tif", 
           options=c("-r", "mode", 
                     "-t_srs","EPSG:4326"
                     # "-tr","0.05","0.05"
                     # "SRC_METHOD=NO_GEOTRANSFORM",
                     # "-to ../data_general/AVHRR_NDVI_CDR_V5/AVHRR_CDRv5_NIRV_seasonalMedian_Australia_1982_2019.tif")
))

nvis <- stars::read_stars("../data_general/NVIS/aus5_1e_mvg.tif")
avhrr <- stars::read_stars("../data_general/AVHRR_NDVI_CDR_V5/AVHRR_CDRv5_NIRV_seasonalMedian_Australia_1982_2019.tif", 
                           vars='band1')
stars::st_warp(nvis, )

gdal_utils(util='info', 
           source="../data_general/NVIS/aus5_1e_mvg.tif")
gdal_utils(util='info', 
           source="../data_general/AVHRR_NDVI_CDR_V5/AVHRR_CDRv5_NIRV_seasonalMedian_Australia_1982_2019.tif")

           # destination ="../data_general/NVIS/AVHRR_REGRID_aus5_1e_mvg.tif", 
           # options="-r mode")



# Martin's SE NVIS ncdf ---------------------------------------------------
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

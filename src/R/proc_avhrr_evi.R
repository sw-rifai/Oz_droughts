library(tidyverse); library(stars); library(lubridate)
fp <- list.files("../data_general/AVHRR_EVI2_CDR_V5/", pattern=".tif", full.names = T)


# part 1 ------------------------------------------------------------------
o <- stars::read_stars(fp[1])
dim(o)
vec_dates1 <- seq(ymd("1981-06-01"),ymd("1990-12-01"),by='1 month')
tmp_dates <- tibble(band=1:115, date=vec_dates1)
tmp <- o %>% as.data.frame(xy=T)
tmp <- tmp %>% inner_join(., tmp_dates, by='band')
names(tmp) <- c('lon','lat','band','evi2','date')
tmp <- tmp %>% filter(is.na(evi2)==F)
arrow::write_parquet(tmp, sink = "../data_general/AVHRR_EVI2_CDR_V5/tmp_part_1.parquet", 
                     compression = "snappy")

# part 2 ------------------------------------------------------------------
fp[2]
o <- stars::read_stars(fp[2])
dim(o)
vec_dates1 <- seq(ymd("1991-01-01"),ymd("2000-12-01"),by='1 month')
tmp_dates <- tibble(band=1:dim(o)[3], date=vec_dates1)
tmp <- o %>% as.data.frame(xy=T)
tmp <- tmp %>% inner_join(., tmp_dates, by='band')
names(tmp) <- c('lon','lat','band','evi2','date')
tmp <- tmp %>% filter(is.na(evi2)==F)
arrow::write_parquet(tmp, sink = "../data_general/AVHRR_EVI2_CDR_V5/tmp_part_2.parquet", 
                     compression = "snappy")

# part 3 ------------------------------------------------------------------
fp[3]
o <- stars::read_stars(fp[3])
dim(o)
vec_dates1 <- seq(ymd("2001-01-01"),ymd("2010-12-01"),by='1 month')
tmp_dates <- tibble(band=1:dim(o)[3], date=vec_dates1)
tmp <- o %>% as.data.frame(xy=T)
tmp <- tmp %>% inner_join(., tmp_dates, by='band')
names(tmp) <- c('lon','lat','band','evi2','date')
tmp <- tmp %>% filter(is.na(evi2)==F)
arrow::write_parquet(tmp, sink = "../data_general/AVHRR_EVI2_CDR_V5/tmp_part_3.parquet", 
                     compression = "snappy")

# part 4 ------------------------------------------------------------------
fp[4]
o <- stars::read_stars(fp[4])
dim(o)
vec_dates1 <- seq(ymd("2011-01-01"),ymd("2019-12-01"),by='1 month')
tmp_dates <- tibble(band=1:dim(o)[3], date=vec_dates1)
tmp <- o %>% as.data.frame(xy=T)
tmp <- tmp %>% inner_join(., tmp_dates, by='band')
names(tmp) <- c('lon','lat','band','evi2','date')
tmp <- tmp %>% filter(is.na(evi2)==F)
arrow::write_parquet(tmp, sink = "../data_general/AVHRR_EVI2_CDR_V5/tmp_part_4.parquet", 
                     compression = "snappy")


# join the pieces ---------------------------------------------------------
fparts <- list.files("../data_general/AVHRR_EVI2_CDR_V5/",pattern = ".parquet",full.names = T)
rm(tmp); 
tmp <- bind_rows(arrow::read_parquet(fparts[1]), 
                 arrow::read_parquet(fparts[2]), 
                 arrow::read_parquet(fparts[3]), 
                 arrow::read_parquet(fparts[4]))
arrow::write_parquet(tmp, 
                     sink="../data_general/AVHRR_EVI2_CDR_V5/Oz_AVHRR_EVI2_CDR_1981_2019.parquet",
                     compression="snappy")


# Cleanup -----------------------------------------------------------------
rm(tmp); 
rm(o);
file.remove(
 list.files("../data_general/AVHRR_EVI2_CDR_V5/", pattern = "tmp_part_", full.names = T)
)

# tmp %>% 
#   filter(date==min(date)) %>% 
#   ggplot(data=., aes(lon,lat,fill=evi2))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c()
# 
# 
# 
# o[,,,1] %>% dim
# o[,,,1] %>% plot
# 
# 
# tmp$band %>% table

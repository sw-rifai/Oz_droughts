library(rvest)
library(xml2)
# library(data.table)
library(dplyr)
# library(raster)
library(tidyr)
library(sf); library(stars);

#### Download rain files ####
# list of urls with file urls 
webs <- list("http://opendap.bom.gov.au:8080/thredds/catalog/agcd/precip/total/r005/01month/2019/catalog.html")

# preamble required for file url
# pre <- "http://opendap.bom.gov.au:8080/thredds/fileServer/daily_rain_5km/"
pre <- "http://opendap.bom.gov.au:8080/thredds/dodsC/agcd/precip/total/r005/01month/2019/"


# function that gets file urls from websites 
get_html_text <- function(url, css_or_xpath="*"){
  html_text(
    html_nodes(
      read_html(url), css_or_xpath
    )
  )
}
# lists file urls 
flist <- lapply(webs, get_html_text, css_or_xpath="tr~ tr+ tr a tt")
flist <- unlist(flist)

# # subsets to only include fire season
# sub <- which(substring(ls, 41, 42) %in% c("01", "02", "03", "10", "11", "12"))
# ls <- ls[sub]

fileurl <- paste0(pre, 
                  # substring(ls, 37, 40), 
                  "/", flist)
# filename <- paste0("./data_raw/rain/", substring(ls, 37, 47))

o <- stars::read_ncdf("http://opendap.bom.gov.au:8080/thredds/dodsC/agcd/precip/total/r005/01month/2019/precip_total_r005_20190101_20190131.nc")
o %>% plot

fileurl[1]
o <- stars::read_ncdf(fileurl[1])
o2 <- stars::read_ncdf(fileurl[2])

for(i in 1:12){
  if(i==1){
    o <- stars::read_ncdf(fileurl[1])
  }else{
    o2 <- stars::read_ncdf(fileurl[i])
    o <- c(o, o2)
  }
}

ot <- o %>% as_tibble()
ot <- ot %>% mutate(date=as.Date(time))
ot <- ot %>% filter(date==ymd("2019-03-01"))

p <- stars::read_ncdf("../data_general/clim_grid/awap/AWAP/Monthly_data_1911_2019/Monthly_total_precipitation_AWAP_masked_1900_2019.nc")
p <- p %>% filter(time==ymd('2019-3-01'))
pt <- p %>% as_tibble()

sort(unique(ot$lat)) %in% sort(unique(pt$lat))
sort(unique(ot$lat)) %in% sort(unique(pt$lat))

j <- inner_join(ot %>% select(-time), 
           pt %>% select(-time), 
           by=c('lon','lat'))
j <- j %>% units::drop_units()
j %>% 
  filter(is.na(pre)==F) %>% 
  ggplot(data=.,aes(lon,lat,color=precip-pre))+
  geom_point(size=0.1)+
  coord_equal()+
  scale_color_gradient2()

j %>% 
  ggplot(data=., aes(pre, precip))+
  geom_point()+
  geom_abline(aes(intercept=0,slope=1),col='red')



p <- inner_join(read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv") %>% 
             select(id,longitude,latitude), 
           p, by='id') %>% 
  rename(lon=longitude, lat = latitude)

inner_join(ot, p, by=c("lon","lat"))




v <- ncdf4::nc_open(fileurl[1])
ncdf4::nc_create(filename = "junk.nc", v)

v$dim




o[,,,1] %>% 
  as_tibble() %>% # pull(lat) %>% unique() %>% sort() %>% diff
  select(lon,lat,precip) %>% 
  raster::rasterFromXYZ(res = 0.05)


library(tidyverse)
o[,,,2] %>%
  as_tibble() %>% 
  units::drop_units() %>% 
  ggplot(data=., aes(lon,lat,fill=log1p(precip)))+
  geom_tile()+
  scale_fill_viridis_c()


dir.create("tmp")
for(i in 1:12){
  stars::write_stars(o[1,,,i], dsn=file.path("tmp",flist[i]),
                     layer='precip', 
                     driver="netCDF")
}
list.files("tmp")

system("cdo cat tmp/*.nc tmp/joined.nc")
system("cdo settunits,months -settaxis,2019-01-01,00:00,1month tmp/joined.nc tmp/joined2.nc")

#Set correct time units
var <- "pwd"
system(paste(var))
"cdo settunits,days -settaxis,${year}-01-01,00:00,1day $out_path/temp.nc $out_path/temp1.nc"






gdal_crs(file.path("tmp",flist[1]))
gdal_crs("../data_general/clim_grid/awap/AWAP/Monthly_data_1911_2019/Monthly_total_precipitation_AWAP_masked_1900_2019.nc")
gdal_crs("../data_general/clim_grid/era5-land/Oz/Oz/Oz_era5-land_monmean_d2m_1979_2019.nc", options=character(0))

st_dim_to_attr(o)
st_dimensions(o)$time$values$start
st_redimension(o)

sf::st_drivers()
o[1] %>% dim
stars::write_stars(o[1], dsn="awap_precip_2019.nc",
                   layer='precip', 
                   driver="netCDF")
system("cdo sinfo awap_precip_2019.nc")
system("ncdump -h awap_precip_2019.nc")

v <- c(o[,,,1], c[,,,2])


o_brick <- st_as_raster(o)

o %>% 
  as_tibble() %>% 
  pull(time) %>% 
  unique()


sf::st_crs(o)

c(o,o2) %>% dim
o %>% dim
o
crs(o)
download.file(fileurl[1], destfile = "test.nc")
o %>% select(time)
o[,1,1,4]

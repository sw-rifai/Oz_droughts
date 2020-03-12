library(tidyverse); 
library(sf); library(lubridate)
library(arrow); 

list.files("../data_general/clim_grid/era5-land/",recursive = T,full.names = T)

coords <- read_csv("data/coords_set_EA_lai_amp0p5_min0p5.csv")
coords <- sf::st_as_sf(coords, coords = c("longitude","latitude"), crs=4326)


# evaporation ------------------------------------------------
fp <- "../data_general/clim_grid/era5-land/Oz/Oz/Oz_era5-land_monmean_evaporation_1979_2019.nc"
vec_dates <- stars::read_ncdf(fp, var="time")
vec_dates <- vec_dates %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="evap")
ex <- ex %>% 
  mutate(date=ymd(date)) %>% 
  mutate(days_in_month = days_in_month(date)) 
ex <- ex %>% 
  mutate(evap = evap*days_in_month*-1000)

ex %>% 
  write_parquet(., sink="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_evaporation_1979_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)
gc()


# potential evaporation ------------------------------------------------
fp <- "../data_general/clim_grid/era5-land/Oz/Oz/Oz_era5-land_pet_1981_2019.nc"
vec_dates <- stars::read_ncdf(fp, var="time")
vec_dates <- vec_dates %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="pet")
ex <- ex %>% 
  mutate(date=ymd(date)) %>% 
  mutate(days_in_month = days_in_month(date)) 
ex <- ex %>% 
  mutate(pet = pet*days_in_month*-1000)

ex %>% 
  write_parquet(., sink="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_PET_1979_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)
gc()



# precip ------------------------------------------------
fp <- "../data_general/clim_grid/era5-land//Oz/Oz/Oz_era5-land_monmean_precip_1979_2019.nc"
vec_dates <- stars::read_ncdf(fp, var="time")
vec_dates <- vec_dates %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="precip")
ex <- ex %>% 
  mutate(date=ymd(date)) %>% 
  mutate(days_in_month = days_in_month(date)) 
ex <- ex %>% 
  mutate(precip = precip*days_in_month*1000)

ex %>% 
  write_parquet(., sink="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_precip_1979_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)
gc()




# d2m ------------------------------------------------
fp <- "../data_general/clim_grid/era5-land//Oz/Oz/Oz_era5-land_monmean_d2m_1979_2019.nc"
vec_dates <- stars::read_ncdf(fp, var="time")
vec_dates <- vec_dates %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="d2m")
ex <- ex %>% 
  mutate(date=ymd(date)) %>% 
  mutate(days_in_month = days_in_month(date)) 

ex %>% 
  write_parquet(., sink="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_d2m_1979_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)
gc()

# t2m ------------------------------------------------
fp <- "../data_general/clim_grid/era5-land//Oz/Oz/Oz_era5-land_monmean_t2m_1979_2019.nc"
vec_dates <- stars::read_ncdf(fp, var="time")
vec_dates <- vec_dates %>% as_tibble() %>% select(time)
junk <- velox::velox(raster::stack(fp))
ex <- junk$extract_points(sp = coords)
ex <- as_tibble(ex)
names(ex) <- vec_dates$time
ex$id <- coords$id
ex <- ex %>% 
  pivot_longer(-id, names_to="date", values_to="t2m")
ex <- ex %>% 
  mutate(date=ymd(date)) %>% 
  mutate(days_in_month = days_in_month(date)) 

ex %>% 
  write_parquet(., sink="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_t2m_1979_2019.parquet", 
                compression = "snappy")  
rm(ex,junk,vec_dates,fp)
gc()



# VPD ---------------------------------------------------------------------
#' Calculates saturation vapour pressure
#' @return saturation vapour pressure
calc_esat <- function(airtemp){
  #Tair in degrees C
  #From Jones (1992), Plants and microclimate: A quantitative approach 
  #to environmental plant physiology, p110
  esat <- 613.75 * exp(17.502 * airtemp / (240.97+airtemp))
  
  return(esat)
}

get.rh <- function(Tk, Td) {
  if(Td >= Tk){
    rh <- 100
  } else {
    Rw <- 461.5 # gas constant for water vapor, J K-1 kg-1
    L <- 2.501e6 + (T-273.15) * (-2430) 
    arg <- -L / (Rw * Tk * Td) * (Tk - Td)
    rh <- 100 * exp(arg)
  }
  return(rh)
}

get.vpd <- function(temp_Dk, temp_K) {
  # temp_DK is dewpoint in K
  # temp_K is airtemp in K
  Td <- temp_Dk
  TK <- temp_K
  
  get.rh <- function(Tk, Td) {
    if(Td >= Tk){
      rh <- 100
    } else {
      Rw <- 461.5 # gas constant for water vapor, J K-1 kg-1
      L <- 2.501e6 + (T-273.15) * (-2430) 
      arg <- -L / (Rw * Tk * Td) * (Tk - Td)
      rh <- 100 * exp(arg)
    }
    return(rh)
  }
  
  
  rh <- get.rh(TK, Td)
  temp <- temp_K-273.15
  ## calculate saturation vapor pressure
  es <- get.es(temp)
  ## calculate vapor pressure deficit
  out <- (((100 - rh)/100) * es) 
  return(out/10)
} # get.vpd

get.es <- function(temp_C) {
  return(6.11 * exp((2500000/461) * (1/273 - 1/(273 + temp_C))))
} # get.es

calc_esat(300-273.15)-calc_esat(299-273.15)
plantecophys::DewtoVPD

curve(get.vpd(x, 315),273,315)
315-273.15

ex_d2m <- read_parquet("../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_d2m_1979_2019.parquet")
ex_t2m <- read_parquet("../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_t2m_1979_2019.parquet")
ex_vpd <- inner_join(ex_d2m %>% select(-days_in_month), 
           ex_t2m %>% select(-days_in_month), 
           by=c("id","date")) %>% 
  mutate(vpd = get.vpd(d2m, t2m))

ex_vpd %>% 
  write_parquet(., sink="../data_general/clim_grid/era5-land/Oz/Oz/parquet/Oz_era5-land_monmean_vpd_1979_2019.parquet", 
                compression = "snappy")  


tmp <- tmp %>% filter(is.na(hydro_year)==F)
d <- st_dimensions(lon=sort(unique(tmp$lon)), 
              lat=sort(unique(tmp$lat)), 
              hydro_year=units::set_units(sort(unique(tmp$hydro_year)), 'years'), 
              season = unique(tmp$season), 
              .raster=c("lon","lat"), 
              cell_midpoints = T)
st_as_stars(tmp, dimensions=d)





data(air, package = 'spacetime')
air %>% class
air %>% dim

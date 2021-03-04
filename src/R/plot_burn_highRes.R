library(tidyverse); library(stars); library(patchwork)

p1 <- stars::read_stars("/home/sami/Downloads/MCD13Q1_x150p41027_y-32p94863_median_NDVI_2019-10-15_2019-10-26.tif", 
                        proxy=F) %>% 
  set_names('NDVI')
p2 <- stars::read_stars("/home/sami/Downloads/MCD13Q1_x150p41027_y-32p94863_median_NDVI_2020-01-12_2020-01-23.tif", 
                        proxy=F) %>% 
  set_names('NDVI')
p3 <- stars::read_stars("/home/sami/Downloads/FIRMS_max_T21_x150p41027_y-32p94863_2019-10-26_2020-01-12.tif", 
                        proxy=F) %>% 
  set_names('T21')


i1 <- ggplot()+
  geom_stars(data=p1)+
  coord_equal()+
  scale_fill_viridis_c(limits=c(0,1))+
  labs(title='NDVI',subtitle='2019-10-15 to 2019-10-26')

i2 <- ggplot()+
  geom_stars(data=p2)+
  coord_equal()+
  scale_fill_viridis_c(limits=c(0,1))+
  labs(title='NDVI', subtitle='2020-01-12 to 2020-01-23')

i3 <- ggplot()+
  geom_stars(data=p2-p1)+
  coord_equal()+
  scale_fill_gradient2(expression(paste(Delta~NDVI)),
                       limits=c(-0.5,0.5))+
  labs(title='NDVI Difference')


i4 <- ggplot()+
  geom_stars(data=p3)+
  coord_equal()+
  scale_fill_viridis_c('K', option='B')+
  labs(title='Max thermal Anom.',subtitle='2019-10-15 to 2020-01-12')

(i1+i2)/(i3+i4)
ggsave(filename = "NDVI_pre_post_fire_x150p41027_y-32p94863.png", 
       width = 20, height=15, units='cm')

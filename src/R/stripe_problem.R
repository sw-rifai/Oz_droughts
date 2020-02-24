library(reprex)
library(ggplot2); library(dplyr); library(tidyr)
N <- 1500
expand_grid(x=c(1:N), 
            y=c(1:N)) %>% 
  as_tibble() %>% 
  mutate(z=sin(5*x/N)+cos(15*y/N)) %>% 
  ggplot(data=., aes(x,y,fill=z))+
  geom_tile()+
  coord_equal()+
  scale_fill_viridis_c()
ggsave("why_stripey.png")

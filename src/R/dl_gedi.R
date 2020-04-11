library(tidyverse); library(rGEDI)
# Study area boundary box coordinates
ul_lat<- -32.9701+1
lr_lat<- -32.9701-1
ul_lon<- 150.5394-1
lr_lon<- 150.5394+1


# Specifying the date range
daterange=c("2019-07-01","2020-05-22")

# Get path to GEDI data
gLevel1B<-gedifinder(product="GEDI01_B",ul_lat, ul_lon, lr_lat, lr_lon,version="001",daterange=daterange)
gLevel2A<-gedifinder(product="GEDI02_A",ul_lat, ul_lon, lr_lat, lr_lon,version="001",daterange=daterange)
gLevel2B<-gedifinder(product="GEDI02_B",ul_lat, ul_lon, lr_lat, lr_lon,version="001",daterange=daterange)





# Set output dir for downloading the files
outdir="/home/sami/Downloads/tmp/"

# Downloading GEDI data
system.time(gediDownload(filepath=gLevel1B,outdir=outdir))
gediDownload(filepath=gLevel2A,outdir=outdir)
gediDownload(filepath=gLevel2B,outdir=outdir)

#** Herein, we are using only a GEDI sample dataset for this tutorial.
# downloading zip file
download.file("https://github.com/carlos-alberto-silva/rGEDI/releases/download/datasets/examples.zip",destfile=file.path(outdir, "examples.zip"))

# unzip file 
unzip(file.path(outdir,"examples.zip"))





















# out$lst %>% summary
# out$lst %>% is.na %>% table
# 
# out %>% 
#   group_by(time) %>% 
#   summarize(val = mean(lst,na.rm=T)) %>% 
#   ungroup() %>% 
#   mutate(year=year(time), 
#          month=month(time)) %>% 
#   ggplot(data=., aes(month, val,color=as.factor(year)))+
#   geom_point()+
#   geom_line()+
#   scale_color_viridis_d()+
#   theme_dark()
# 
# out %>% 
#   mutate(year=year(time)) %>% 
#   filter(year==2019) %>% 
#   group_by(x,y) %>% 
#   summarize(val = max(lst,na.rm=T)) %>% 
#   ungroup() %>% 
#   ggplot(data=., aes(x,y,fill=val))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c(option='B')
# 
# norms <- out %>% 
#   mutate(month=month(time)) %>% 
#   group_by(x,y,month) %>% 
#   summarize(lst_u = mean(lst,na.rm=T)) %>% 
#   ungroup()
# out <- out %>% 
#   mutate(month=month(time)) %>% 
#   inner_join(out,norms,by=c('x','y','month'))
# 
# 
# norms %>% 
# ggplot(data=., aes(x,y,fill=lst_u))+
#   geom_tile()+
#   coord_equal()+
#   scale_fill_viridis_c(option='B')+
#   facet_wrap(~month)
# 

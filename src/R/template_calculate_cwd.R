library(tidyverse); library(RcppRoll)
#*******************************************************************************
#--- FUNCTIONS ---------------------------------------------------------------------------------#
#*******************************************************************************mode <- function(x) {
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

f_cwd <- function(cwd_et,precip,et){
  # No reset during the wettest month of the year
  for(i in seq(2,length(precip))){
    
    cwd_et[i] <-  min(0, cwd_et[i-1] + (precip[i]) - max(et[i],1, na.rm=T), na.rm=T)
    cwd_et[i] <- ifelse(cwd_et[i] < -3000, -3000, cwd_et[i])
  }
  cwd_et
}

f_cwd_wmy <- function(cwd,precip,et,month, wmy){
  # Resets to 0 on the wettest month of the year
  for(i in seq(2,length(precip))){
    
    cwd[i] <- ifelse((precip[i]) < (et[i]*2), 
                     min(0, cwd[i-1] + (precip[i]) - max(et[i],40, na.rm=T), na.rm=T), 
                     0)
    cwd[i] <- ifelse(month[i]==wmy[i], 0, cwd[i])
    cwd[i] <- ifelse(cwd[i] < -1000, -1000, cwd[i])
  }
  cwd
}

#*******************************************************************************
# END SECTION
#*******************************************************************************

# Simulate data --------------------------
vec_p <- (100*rgamma(120, shape = sin((1:120)/(0.5*pi))+1, scale = 0.5))
vec_et <- rnorm(120, mean=50*sin((1:120)/(0.5*pi))+100, sd=20); 
plot(vec_et, type='l')

d <- expand_grid(id=1:10,
                 date=1:120)
d <- d %>% 
  rowwise() %>% 
  mutate(dn = date) %>% 
  mutate(precip=(100*rgamma(1, shape = sin((dn)/(0.5*pi))+1, scale = 0.5)), 
         et=rnorm(1, mean=50*sin((dn)/(0.5*pi))+50, sd=20)) %>% 
  mutate(precip=abs(precip), 
         et=abs(et)) %>% 
  mutate(month=1+date%%12, 
         year=1+round(date/12))

# Calculate the most frequent month that is the wettest --------------- 
d_wmy <- d %>% 
  group_by(id,year) %>% 
  filter(precip==max(precip,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(wm = month) %>% 
  group_by(id) %>% 
  summarize(wmy = mode(wm)) %>% 
  ungroup()
d <- left_join(d, d_wmy, by="id")

# Calc CWD ---------------------------------------
d <- d %>% 
  filter(is.na(precip)==F & is.na(et)==F) %>% 
  mutate(cwd = NA, 
         cwd_wmy=NA) %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(cwd = f_cwd(cwd_et = cwd, precip = precip, et = et), 
         cwd_wmy = f_cwd_wmy(cwd = cwd_wmy, precip = precip, 
                            et = et, month = month, wmy = wmy)) %>% 
  ungroup()

# Calc MCWD -------------------------------------
d <- d %>% 
  group_by(id) %>% 
  arrange(date) %>% 
  mutate(mcwd = roll_minr(cwd, n = 12, fill=NA)) %>% 
  mutate(mcwd_wmy = roll_minr(cwd_wmy, n = 12, fill=NA)) %>% 
  ungroup()


# No reset
d %>% 
  ggplot(data=.,aes(date, cwd,color=as_factor(id)))+
  geom_line()+
  geom_line(aes(date, mcwd),lty=3)

# With annual wet month reset
d %>% 
  ggplot(data=.,aes(date, cwd_wmy,color=as_factor(id)))+
  geom_line()+
  geom_line(aes(date, mcwd_wmy),lty=3)


library(tidyverse)
library(ggplot2)

load(file="new_data_cleaning1.Rdata") ## loads script from last assignment 

g1 <- ggplot(data_pref,aes(x=Time,y=Tobj))+
  labs(x="Time (s)",y="Object Temperature (°C)")+
  geom_line()

g2 <- ggplot(data_pref,aes(x=Time,y=Tpref))+
  labs(x="Time (s)",y="Preferred Temperature (°C)")+
  geom_point()

g3 <- ggplot(data_pref,aes(Tobj))+
  labs(x="Object Temperature (°C)", y="Time (s)")+
  geom_histogram(binwidth=0.5) ## cant figure out how to make the bins more conspicuous.. blend together 



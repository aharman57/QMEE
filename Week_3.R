library(tidyverse)
library(ggplot2)

## JD: Sorry to move (from "Week 3 Assignment.R" to "Week3.R"; spaces in filenames make me crazy

L <- load(file="new_data_cleaning1.Rdata") ## loads script from last assignment 

## BMB: this doesn't actually load a data frame, it is just three separate
## objects.  You should go upstream and fix this, but in the meantime:

data_pref <- tibble(Time,Tobj,Tpref)

g1 <- ggplot(data_pref,aes(x=Time,y=Tobj))+
  labs(x="Time (s)",y="Object Temperature (°C)")+
  geom_line()

g2 <- ggplot(data_pref,aes(x=Time,y=Tpref))+
  labs(x="Time (s)",y="Preferred Temperature (°C)")+
  geom_point()
## BMB: what's the decision whether to use geom_line or geom_point ?

g3 <- ggplot(data_pref,aes(Tobj))+
  labs(x="Object Temperature (°C)", y="Time (s)")+
    geom_histogram(binwidth=0.5,colour="black") ## cant figure out how to make the bins more conspicuous.. blend together
## BMB: see colour="black"


g4 <- ggplot(data_pref,aes(Tobj))+
  labs(x="Object Temperature (°C)", y="Time (s)")+
    geom_histogram(binwidth=0.5,colour="black",aes(y=..density..))+
    geom_histogram(binwidth=0.1,colour="blue",fill="blue",alpha=0.2,aes(y=..density..))+
    geom_density(colour="red")
## some alternatives (not necessarily useful)
## BMB: your text in README was good, but you said very little about your *graphical* choices ...
##  score=2

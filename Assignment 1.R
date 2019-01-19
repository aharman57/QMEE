## I forgot to save my R script halfway through wednesdays class and lost most of my notes... whoops ##
## BMB: oh well.
## Tried to add a pipe, but struggled without my notes ##
## BMB: did the lecture notes help? Also, there are *lots* of references about the tidyverse, introductions etc etc, on the web ...

library(tidyverse)
library(readr)

## BMB: special characters in file names (e.g. #, &, ...)
##  are generally a bad idea
## don't name 
data <- read_csv(file="pilot4_test_exp#3.csv")

## BMB: don't leave head(), str(), etc. in your code ...
head(data)
str(data)

## BMB: it's a good idea to rename unwieldy variables


Tobj <- data$`Object temperature [?C]`

Tpref <- median(Tobj)
sd <- sd(Tobj)
Tpref
sd

## BMB: I can see that this is taking just the first hour of data
## (assuming observations are sampled every second). Why?

data_1hour <- data[1:3600,]

Tobj_1hour <- data_1hour$`Object temperature [?C]`

Tpref_1hour <- median(Tobj_1hour)
sd_1hour <- sd(Tobj_1hour)
Tpref_1hour
sd_1hour

## BMB: this plots a single (x,y) point.  Is that what you intended?
plot(Tpref, Tpref_1hour)

## BMB: maybe you want something like this ...
## 1. get just (renamed) temperature, add index
data2 <- (data
    %>% rename(obj_temp="Object temperature [?C]")
    %>% select(obj_temp)
    %>% mutate(index=seq(nrow(data)))
    %>% mutate(hour=seq(nrow(data)) %% 3600)
)
library(ggplot2)
ggplot(data2,aes(hour,obj_temp))+geom_line()

## BMB: score 2 (1=poor, 2=fine, 3=excellent)


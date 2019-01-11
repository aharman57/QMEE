## I forgot to save my R script halfway through wednesdays class and lost most of my notes... whoops ##
## Tried to add a pipe, but struggled without my notes ##

library(tidyverse)
library(readr)

data <- read_csv(file="pilot4_test_exp#3.csv")
head(data)
str(data)

Tobj <- data$`Object temperature [?C]`

Tpref <- median(Tobj)
sd <- sd(Tobj)
Tpref
sd

data_1hour <- data[1:3600,]

Tobj_1hour <- data_1hour$`Object temperature [?C]`

Tpref_1hour <- median(Tobj_1hour)
sd_1hour <- sd(Tobj_1hour)
Tpref_1hour
sd_1hour

plot(Tpref, Tpref_1hour)

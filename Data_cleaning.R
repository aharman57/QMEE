library(tidyverse)

data <- read.delim(file="pilot4_test_exp#3.txt") 

## JD: Rename using new tools (dplyr rename)
## This allows you to use words, so that your code is clear
## and you won't trick yourself if input format changes
#column names by default include square brackets, so re-naming helps#
colnames(data) <- c("Zone", "Tobj", "Tpref", "INCR", "DECR", "X pos", "Y pos", "Velocity", "Distance", "Time INCR", "Time DECR", "Delta T", "Hysteresis", "Max", "Min", "Max Rate")

data_pref <- select(data, Tobj, Tpref) %>%
  na.omit() %>%
  mutate(Time = row_number()) 

## JD: Usually this should not be necessary, just pass the object to ggplot
Tobj <- data_pref$Tobj        
Tpref <- data_pref$Tpref
Time <- data_pref$Time

plot(Time, Tobj) ## not very useful, no pattern in this data 
plot(Time, Tpref) ## more or less what the plot should look like

## JD: This is the right idea, but …
## it would be cooler to do this with select, and save just one tibble
save(Tobj,Tpref,Time,file="new_data_cleaning1.Rdata")

## JD: Not really clear that much cleaning or validation has gone on here
## JD: score 2. (1=poor, 2=fine, 3=excellent)

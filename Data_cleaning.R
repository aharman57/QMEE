library(tidyverse)

data <- read.delim(file="pilot4_test_exp#3.txt") 

#column names by default include square brackets, so re-naming helps#
colnames(data) <- c("Zone", "Tobj", "Tpref", "INCR", "DECR", "X pos", "Y pos", "Velocity", "Distance", "Time INCR", "Time DECR", "Delta T", "Hysteresis", "Max", "Min", "Max Rate")

data_pref <- select(data, Tobj, Tpref) %>%
  na.omit() %>%
  mutate(Time = row_number()) 

Tobj <- data_pref$Tobj        
Tpref <- data_pref$Tpref
Time <- data_pref$Time

plot(Time, Tobj) ## not very useful, no pattern in this data 
plot(Time, Tpref) ## more or less what the plot should look like

save(Tobj,Tpref,Time,file="new_data_cleaning1.Rdata")


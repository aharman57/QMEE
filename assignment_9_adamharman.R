library(tidyverse)
library(lme4)

Morph <- (read_csv(file="Morph_Data.csv")
          %>% rename(Age="Age (dph)",
                     Eye="Eye diameter",
                     Fin="Fin indent",
                     Yolk="Yolk weight",
                     Jaw="Jaw Gape")
)

lmm_data <- select(Morph,Length,Age,Treatment)
## lmm_data$Fish_ID <- 1:nrow(lmm_data)
lmm_data2 <- tibble::rowid_to_column(lmm_data, "Fish_ID") ## tried a different way of creating an ID column, didn't help
## BMB ?? this command seems to work for me??
## or do you mean that the FishID variable you're constructing doesn't
## work in the mixed model?
## if the fish measurements are terminal -- you measure each fish only
## once -- this isn't a good context for a mixed model (which are closely
## related to *repeated measures* models ... if you don't have repeated
## measurements of some sort it doesn't make sense to use a MM)

### Not sure if the variables I am using make sense for random effects, since I care about which treatments and which age classes influence length ###

## tried adding a "Fish_ID" column to use as a grouping variable, but couldn't get it to work
## keep getting this error "Error: number of levels of each grouping factor must be < number of observations"
## the columns are the same length, so I don't know how to make the Fish_ID column smaller than the total observations??.. subject # was used as a grouping factor in the example as well as several examples I looked at online... is it because of the way I am adding the column??

o
lmm1 <- lmList(Length~Age|Fish_ID, data=lmm_data2) ### not sure what this is showing ### BMB: it's showing that you can't fit a linear regression to a single point per fish!
lmm1_2 <- lmer(Length~Age+(Treatment|Fish_ID),data=lmm_data2)

lmm2 <- lmer(Length~Age+(1|Treatment),data=lmm_data) ### Age is fixed effect, and Intercept varies between treatment groups (random effect) ###
## BMB: doesn't make sense -- Treatment levels aren't exchangeable

lmm3 <- lmer(Length~Age+(Age|Treatment),data=lmm_data) ### Age is fixed effect, and intercept and slope are random effects - Age varies between treatment groups?? is that the right interpretation? ###
### singular fit, too complicated of a model for the amount of data? ###
### This is the maximum model?? ### 
lmm3_2 <- lmer(Length~Treatment+(Treatment|Age),data=lmm_data)
### singular fit... ### This makes the most sense, as I am primarily looking for the effect of temperature treatment on size
## BMB: also doesn't make sense to use Age as a RE -- not exchangeable.
### If I interpret it like: treatment is a random effect within age groups, it doesn't make sense... All treatment groups had the same age and grew over the course of the study
lmm3_3 <- lmer(Length~Age+Treatment+(Age|Treatment)+(Treatment|Age),data=lmm_data)


lmm4 <- lmer(Length~Age+(1|Age),data=lmm_data) ### Age is fixed effect, and the intercept varies between age classes ###

lmm5 <- lmer(Length~Treatment+(1|Age),data=lmm_data) ### Treatment is fixed effect, intercept varies between age classes ###


plot(lmm2, sqrt(abs(resid(.))) ~ fitted(.),
     type=c("p","smooth"), col.line="red")

plot(lmm3, sqrt(abs(resid(.))) ~ fitted(.),
     type=c("p","smooth"), col.line="red")

plot(lmm3_2, sqrt(abs(resid(.))) ~ fitted(.),
     type=c("p","smooth"), col.line="red") ### second best fitted vs residuals plot -- maximal model?? 

plot(lmm4, sqrt(abs(resid(.))) ~ fitted(.),
     type=c("p","smooth"), col.line="red") ### produced the best fitted vs residuals plot -- but using the fixed effect as a random effect?? does that make sense
### This lmm uses Age as a fixed effect and a random effect (intercept) -- similar to lmm3 but does not produce a singluar fit
### In this case the random effect is the intercept, rather than the treatment group -- making it a simpler model
### I would change my maximal model into this one by changing the random effect from treatment group to intercept, that way it can be modelled with the amount of data I currently have 

plot(lmm5, sqrt(abs(resid(.))) ~ fitted(.),
     type=c("p","smooth"), col.line="red")


### maximum model = one where all variables have fixed and random effects - can't have any more
### therefore the maximal model would be lmm3_2 : Length~Treatment+(Treatment|Age)
## OR it could be Length~Age+(Age|Treatment), but it makes more sense to use Age as grouping variable, as treatment is the primary variable that I am concerned with
### in this model treatment has both a fixed and a random effect (and is the only predictor variable)

### Below is the maximal model that I wanted to use, but I can't get Fish_ID column to work as a grouping variable... this makes way more sense to me to use as a grouping variable
### Each fish has its own growth rate, which I care about, but I don't care exactly WHICH fish is growing the fastest
### I am probably also interested in the question: does the treatment affect ages classes of fish differently?? So I included the interaction term, and a random effect for the interaction

#Length ~ Age*Treatment + (1|Fish_ID) + (0+Age|Fish_ID) + (0+Treatment|Fish_ID) + (0+Age:Treatment|Fish_ID)

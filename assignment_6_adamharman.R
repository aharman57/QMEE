library(tidyverse)

data <- read_csv(file="Morph_Data.csv")

## JD: Dangerous code; what if your data file structure changes
## Better to rename by name (use dplyr::rename, I think)
## Or to edit the input file to make names that are complete but succinct
colnames(data)[1] <- "Age"
colnames(data)[4] <- "Eye"
colnames(data)[6] <- "Fin"
colnames(data)[8] <- "Yolk"
colnames(data)[11] <- "Jaw"

lm_data <- select(data,Length,Age,Treatment)

lm1 <- lm(Length~Age*Treatment,data=lm_data)
summary(lm1)

## JD: Why are you messing with margins?
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(lm1, which=1:4)

## The diagnostic plots are used to check whether the linear model fits the major assumptions - make something that is hard to visualize into a linear relationship that is easier to assess visually
## Residual vs fitted diagnostic plot is testing whether a non-linear relationship is present. If the line is linear then it means we have met our assumptions
## Q-Q plot assesses normality of the data by plotting "quantiles" against each other -- the more linear the relationship the better the "fit"
## Scale location tests if the residuals are distributed evenly between predictors - which essentially tests for Heteroscedasticity -- the flatter the line the more similar the variance
## Cooks distance looks at the relative influence of a single data point on the regression -- cooks distance < 0.05 is acceptable 

## I'm not sure why my cooks distance is a histogram, but it communicates the same information
## JD: Yeah. WTH??


####################### More variables

lm2 <- lm(Length~Age*Treatment*Eye*Yolk,data=data)
summary(lm2)

## Not necessary (nor good) to repeat this.
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(lm2, which=1:4)

## Including more variables improved the residuals vs fitted plot, which means that this relationship is more linear now
## Also improved the scale-location plot, meaniing it reduced heteroscedasticity 
## Cooks distance increased though, with several points being over .05 -- means that certain data points now have more influence than previously

######################## Without interactions... to compare

lm3 <- lm(Length~Age+Treatment,data=lm_data)
summary(lm3)

par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(lm3, which=1:4)


## removing the interaction terms strongly affected several of the diagnostic plots 
## the relationship is less linear than it was, but it has better normality (qq plot)
## heteroscedaciticity increased, and cooks distance also increased (relative to lm1)

########################

lm4 <- lm(Length~Age+Treatment+Eye+Yolk,data=data)
summary(lm4)

par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(lm4, which=1:4)

#### Fits more of the assumptions of the lm when I include the interaction terms ####

lm5 <- lm(Length~Age+Treatment+Eye+Yolk+Fin+Jaw,data=data)
summary(lm5)

par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(lm5, which=1:4)


### Adding more variables without including interaction terms didnt improve much
## cooks distance was reduced slightly, but normality was reduced and heteroscedaciticity is similar 


################

lm6 <- lm(Length~Age*Treatment*Eye*Yolk*Fin*Jaw,data=data)
summary(lm6)

par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(lm6, which=1:4)

### Eye diameter and Yolk weight are consistently the most explanatory variable (having the largest estimated coefficient)
######## Adding the fin indent and jaw gape variables increased the cooks distance substantically (almost 2x) for several points
### I'm not sure exactly what this means, but I think that it means that there were a few "outliers" in either fin indent or jaw gape that influence the linear model more heavily when included in the analysis
## All the other diagnostic plots look good, but not necessarily that much better than lm2

### Would it make sense to pick and chose specific interaction terms that I think should be relevant instead of looking at all of them? LOTS of terms for 6 predictors 
### Does it make more sense to include all of the measured variables in the model??? You mentioned before that we shouldn't remove variables because of a low p-value (relationship is unclear)
### JD: We recommend that you should figure out how many predictors you think your data can support, then decide scientifically which interaction terms are important to include and try to stick with the decision
### … journal as you go and explain what changes if any you decide to make.

lm7 <- lm(Length~Age,data=data)
summary(lm7)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(lm7, which=1:4)

lm8 <- lm(Length~Treatment,data=data)
summary(lm8)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(lm8, which=1:4)

## JD: No inferential plot nor discussion about inference ☹

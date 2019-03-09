## BMB: I won't move it now, to avoid confusion, but best *not* to use
## special characters like '#' in file names when coding ...

## BMB: do you actually use all of these?
library("arm")
library("R2jags")
library("coda")
library("emdbook")    
library("arm")    
library("lattice")
library("rstanarm")
library("dotwhisker")
library("broom.mixed")
library("ggplot2"); theme_set(theme_bw())
library("tidyverse")

## BMB: please don't call data 'data'
data <- (read_csv(file="Morph_Data.csv")
    %>% rename(Age="Age (dph)",
               Eye="Eye diameter",
               Fin="Fin indent",
               Yolk="Yolk weight",
               Jaw="Jaw Gape")
)
## BMB: use rename().  renaming by column is unsafe. What if data format
## changes in the future? 
## colnames(data)[1] <- "Age"
## colnames(data)[4] <- "Eye"
## colnames(data)[6] <- "Fin"
## colnames(data)[8] <- "Yolk"
## colnames(data)[11] <- "Jaw"

jags.data <- select(data,Age,Treatment,Length,Eye)
jags.params <- c("ma","mb","mc","int")

N <- 210
y <- jags.data$Length
a <- jags.data$Age
b <- jags.data$Treatment
c <- jags.data$Eye

### I tried to change the distribution of my Age coefficient to gamma (only positive) as I am confident that the fish aren't shrinking as they age but the model wouldn't run..
### Does that make sense? Or should I have put in a positive mean? Still could have the distribution going beyond 0

## BMB: good thoughts.  Maybe the simplest thing is to model on the log scale. log(Length) means length will never go negative.  Modeling log(increment) would mean you never had negative increments ... (if you make log(increment) Normal, that's equivalent to assuming log-Normal increments ...)

jags1 <- jags(model.file='assign7.bug',
              parameters=c("ma","mb","mc","int"),
              data = list('a'=a, 'b'=b, 'c'=c, 'N'=N, 'y'=y),
              n.chains=4,
              inits=NULL)
plot(jags1)

#### Ran lm to get an estimate of parameters for priors... seemed like a good idea
## Ideally, should I have used an independent dataset / different study to generate these????

## BMB: yeah, this seems like a good idea, but it's not -- it's double-dipping.


#### Made a new .bug file with a new set of prior assumptions 
summary(lm(y~a+b+c,data=jags.data))

## BMB: don't need to make a new .bug file - you can pass prior information
## in with the data

#### plugged in coefficient values, reduced the coefficient variation
#### converted standard error values given in the lm estimates summary -- not quite sure if this makes sense, but couldn't think of another way to get variance

## BMB: nice try, that doesn't make sense.  It's double-dipping
## You need to use *independent* prior information/data, if you're going to try
## to specify an informative prior
jags2 <- jags(model.file='bayes.bug',
              parameters=c("ma","mb","mc","int"),
              data = list('a'=a, 'b'=b, 'c'=c, 'N'=N, 'y'=y),
              n.chains=4,
              inits=NULL)


plot(jags2)

######### Almost giving the same coefficients as the standard linear model before changing any of the default priors
## changing the priors changed the 80% CI for the coefficients, but the median was pretty much the same 

bb <- jags1$BUGSoutput  
mm <- as.mcmc.bugs(bb)

cc <- jags2$BUGSoutput
nn <- as.mcmc.bugs(cc)

xyplot(mm,layout=c(2,3))
densityplot(mm,layout=c(2,3))

xyplot(nn,layout=c(2,3))
densityplot(nn,layout=c(2,3))

#### I can't see any difference in the density or x/y plots... hard to visualize??

mmL1 <- emdbook::lump.mcmc.list(mm)
mmL2 <- emdbook::lump.mcmc.list(nn)

hist(mmL1[,"ma"])
hist(mmL1[,"mb"])
hist(mmL1[,"mc"])
hist(mmL2[,"ma"])
hist(mmL2[,"mb"])
hist(mmL2[,"mc"])
hist(mmL1[,"int"])
hist(mmL2[,"int"])

### overall changing the priors had a much smaller effect on the output than I thought it would
### the changes I made only seemed to have an impact on the tails of the coefficient distribution, not really changing the median 

mean(mmL1[,"ma"]>0.12) 
mean(mmL2[,"ma"]>0.12)

mean(mmL1[,"mb"]>-0.1)
mean(mmL2[,"mb"]>-0.1)

mean(mmL1[,"mc"]<3.5)
mean(mmL2[,"mc"]<3.5)

mean(mmL1[,"int"]<6)
mean(mmL2[,"int"]<6)

### mmL1 consistently had a higher probability for more extreme values, meaning it was more likely to have a wider distribution of the coefficient
### adding priors seemed to condense the distribution of the coefficient, which would provide a more accurate estimate ASSUMING that my priors make sense (I think they do?)
### this was hard to visualize or pick out before doing this last step... it seemed like the priors didn't do much by just looking at the histograms

## BMB: score 2.5

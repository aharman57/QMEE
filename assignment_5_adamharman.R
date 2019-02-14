library(tidyverse)

data <- read_csv(file="Morph_Data.csv")

colnames(data)[1] <- "Age"

set.seed(101)
nsim <- 1000

res <- numeric(nsim)
for (i in 1:nsim) {
  perm <- sample(nrow(data))
  bdat <- transform(data,Age=Age[perm])
  res[i] <- mean(bdat$Length)-mean(bdat[bdat$Age==0,"Length"])
}
obs <- mean(bdat$Length)-mean(bdat[bdat$Age==0,"Length"])
res <- c(res,obs)

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")

### hypothesis: Age has a significant impact on size
### permutation: scrambled the ages, tried to see if it affected anything
### test statistic: total mean - group mean (can only get 1 treatment at a time)

## I am scrambling all of the ages, so technically I am still testing for the effect of age... but the test statistic only looks at the difference between the most "extreme" treatment group and the total mean

### Does it even make sense to do a permutation with multiple treatment groups? 

set.seed(101)
nsim <- 1000

res <- numeric(nsim)
for (i in 1:nsim) {
  perm <- sample(nrow(data))
  bdat <- transform(data,Treatment=Treatment[perm])
  res[i] <- mean(bdat[bdat$Treatment==2,"Length"])-mean(bdat$Length)/sd(bdat$Length)/sqrt(210)
}
obs <- mean(bdat[bdat$Treatment==2,"Length"])-mean(bdat$Length)/sd(bdat$Length)/sqrt(210)
res <- c(res,obs)

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")

### Hypothesis (main): Treatment group has significant affect on length 
### Permutation: scrambled the treatment groups
### Test Statistic: tried to use the T-test statistic 


### Attempting to find a better "test statistic" to use for the permutation, and it is very confusing for me.
### Tried using the T-test statistic, but running into the same problem with the code...
### the part where I have to specify which treatment mean I am using is confusing to me, how can I include multiple treatment means?
### the permutation works, but I am pretty confident it is not testing what I want it to, because I am not including the other groups

## I tried a few different ways, but this is definitely a roadblock since the examples in class were very simple compared to this
## I think I would need the permutation to spit out the means of all three treatments into a separate table, and analyze them in a separate step
## That way instead of getting a distribution of "test statistics" I would get 3 different mean distributions

library(tidyverse)

Morph <- (read_csv(file="Morph_Data.csv")
         %>% rename(Age="Age (dph)",
                    Eye="Eye diameter",
                    Fin="Fin indent",
                    Yolk="Yolk weight",
                    Jaw="Jaw Gape")
)

glm_data <- select(Morph,Length,Age,Treatment,Eye,Yolk)

### simplest glm
glm_1 <- glm(family=gaussian(),Length~Age+Treatment,data=glm_data)
summary(glm_1)

par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(glm_1, which=1:4)


glm1 <- glm(family=poisson(),Length~Age+Treatment,data=glm_data)
### getting a lot of warning messages, doesn't like non-integers... but it still works

glm2 <- glm(family=poisson(),Length~Age*Treatment,data=glm_data)
### added interactions

par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(glm1, which=1:4)
### residuals vs fitted plot is non-linear

par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(glm2, which=1:4)
### residuals vs fitted looks better - look at other diagnostic plots 
### some heteroscadacitiy 
glm3 <- glm(family=quasipoisson(),Length~Age+Treatment,data=glm_data)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(glm3, which=1:4)


######
glm4 <- glm(family=quasipoisson(),Length~Age*Treatment*Eye*Yolk,data=glm_data)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(glm4, which=1:4)

### quasi is to help with overdispersion?? I don't think I have that problem as it doesn't seem to help with the fit
glm5 <- glm(family=poisson(),Length~Age*Treatment*Eye*Yolk,data=glm_data)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(glm5, which=1:4)
### best looking model, not sure if adding more variables is the right way to meet model assumptions


summary(glm5)
summary(glm4)
summary(glm3)
summary(glm2)
summary(glm1)
### residual deviance / residual df = 0.022 - 0.038  <-- said it was supposed to be close to 1?
### but also said only values >1 were bad, didn't mention what <1 meant 
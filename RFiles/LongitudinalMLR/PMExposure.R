library(tidyverse)
library(multcomp)
library(nlme)
library(car)

## Read in the data
pm <- read.delim("https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/3%20-%20Project/Data/BreathingZonePM.txt",
                 header = TRUE, sep = "")
ggplot(pm, aes(x=Stationary, y=Aerosol)) + geom_point() + geom_smooth()
ggplot(pm, aes(x=Activity, y=Aerosol)) + geom_boxplot()
ggplot(pm, aes(x=log(Stationary), y=log(Aerosol), col = ID)) + geom_point() + geom_smooth()
## Exploratory plots of relationship between stationary and aerosol
ggplot(pm,aes(x=log(Stationary),y=log(Aerosol)))+geom_point() +
  facet_wrap(~ID)#Use log transform

## Fit a regular lm and show there is correlation
pm.lm <- lm(log(Aerosol)~(log(Stationary)+Activity)*as.factor(ID),data=pm)
summary(pm.lm)
ar1.coefs <- sapply(split(resid(pm.lm), f=pm$ID),
                    function(x){acf(x,plot=FALSE)$acf[2]})
ggplot() + geom_histogram(aes(x=ar1.coefs))

## Fit a gls model with ar correlation
pm.gls <- gls(log(Aerosol)~(log(Stationary)+Activity)*as.factor(ID),data=pm,correlation=corAR1(form=~Minute|ID),
              method="ML")
summary(pm.gls)
pm.gls.no.int <-
  gls(log(Aerosol)~log(Stationary)+Activity+as.factor(ID),data=pm,
      correlation=corAR1(form=~Minute|ID), method="ML")
anova(pm.gls.no.int, pm.gls)

## Justify assumptions
V <- getVarCov(pm.gls)
decor.resids <- c(solve(t(chol(V)))%*%matrix(resid(pm.gls),ncol=60))
correlationMatrix <- cor(matrix(decor.resids, ncol = 60, byrow=TRUE))

qplot(fitted(pm.gls), decor.resids, geom="point")
ggplot() + geom_density(aes(x=decor.resids)) + xlab("Resids")
ks.test(decor.resids, "pnorm")


pm.gls.raw <- gls(Aerosol~(Stationary+Activity)*as.factor(ID),data=pm,correlation=corAR1(form=~Minute|ID),
                  method="ML")
decor.resids <- c(solve(t(chol(V)))%*%matrix(resid(pm.gls.raw),ncol=60))
qplot(fitted(pm.gls.raw), decor.resids, geom="point")
ar1.coefs <- sapply(split(decor.resids,
                          f=pm$ID),function(x){acf(x,plot=FALSE)$acf[2]})
ggplot() + geom_histogram(aes(x=ar1.coefs))

## Does stationary alone do OK at explaining PM exposure?
pm.gls.just.stationary <-
  gls(log(Aerosol)~log(Stationary),data=pm,correlation=corAR1(form=~Minute|ID),
      method="ML")
(pm$Aerosol - exp(fitted(pm.gls)))^2 %>% mean() %>% sqrt()
(pm$Aerosol - exp(fitted(pm.gls.just.stationary)))^2 %>% mean() %>% sqrt()

cor(pm$Aerosol, exp(fitted(pm.gls)))^2
cor(pm$Aerosol, exp(fitted(pm.gls.just.stationary)))^2
plot(pm$Aerosol~exp(fitted(pm.gls.just.stationary)))
plot(pm$Aerosol~exp(fitted(pm.gls)))
anova(pm.gls.just.stationary, pm.gls)
## Do activities, in addition to the stationary measurement, explain PM exposure?
act <- "OnPhone"
a <- matrix(0,nrow=length(coef(pm.gls)), ncol=1)
a[str_detect(names(coef(pm.gls)), act),] <- c(1,rep(1/60,59))
summary(glht(pm.gls, linfct=t(a)))

## Is there an interaction effect
anova(pm.gls.no.int, pm.gls)


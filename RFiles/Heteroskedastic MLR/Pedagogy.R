library(readr)
library(GGally)
library(lubridate)
library(MASS)
library(ggplot2)
library(lmtest)
library(car)
library(tidyverse)
library(magrittr)
library(multcomp)
library(nlme)
source("predictgls.R")
Pedagogy <- read.delim("https://mheaton.byu.edu/Courses/Stat469/Topics/1%20-%20Independence/3%20-%20Project/Data/ClassAssessment.txt", header = TRUE, sep = "")
head(Pedagogy)
tail(Pedagogy)

ggpairs(Pedagogy)
pairs(Pedagogy)

ggplot(data=Pedagogy, mapping=aes(x=Quiz,y=Final)) + geom_point() + geom_smooth()
ggplot(data = Pedagogy, aes(x=Exam1, y=Final)) + geom_point() + geom_smooth(color = "red")
ggplot(data = Pedagogy, aes(x=Exam2, y=Final)) + geom_point() + geom_smooth(color = "yellow")
ggplot(data = Pedagogy, aes(x=Exam3, y=Final)) + geom_point() + geom_smooth(color = "green")
ggplot(data = Pedagogy, aes(x=HW, y=Final)) + geom_point() + geom_smooth(color = "purple")

scatter.smooth(Pedagogy$Quiz,Pedagogy$Final)

pedagogy.lm <- lm(Final~., data = Pedagogy)
summary(pedagogy.lm)


avPlots(pedagogy.lm)

ggplot()+geom_histogram(mapping = aes(x=stdres(pedagogy.lm)))

ks.test(stdres(pedagogy.lm), "pnorm")

plot(fitted(pedagogy.lm),resid(pedagogy.lm))

bptest(pedagogy.lm)

D <- 1/Pedagogy$NStudents
D

pedagogy.gls <- gls(Final~.-Quiz, data = Pedagogy, weights = varFixed(value = ~(1/NStudents)), method = "ML")
pedagogy.gls

pedagogy.gls$coefficients
coef(pedagogy.gls$modelStruct, unconstrained = FALSE)
pedagogy.gls$sigma
summary(pedagogy.gls)

rpmse <- rep(NA, nrow(Pedagogy))
cvg <- rep(NA, nrow(Pedagogy))
wid <- rep(NA, nrow(Pedagogy))
bias <- rep(NA, nrow(Pedagogy))
ci.level <- 0.95
alpha <- 1-ci.level
for(obs in 1:nrow(Pedagogy)){
  ## Split Test and Training
  test.set <- Pedagogy[obs,]
  train.set <- Pedagogy[-obs,]
  
  ## Fit gls to training set
  train.gls <- gls(Final~.-Quiz, data=train.set, weights=varFixed(~1/NStudents), method = "ML")
  
  ## Generate prediction
  pred <- predictgls(train.gls, newdframe=test.set, ci.level)
  
  ## Get prediction interval
  pred.low <- pred[['Prediction']] - qt(1-alpha/2, df=nrow(Pedagogy)-length(coef(train.gls)))*pred[['SE.pred']]
  pred.high <- pred[['Prediction']] + qt(1-alpha/2, df=nrow(Pedagogy)-length(coef(train.gls)))*pred[['SE.pred']]
  
  ## RPMSE
  rpmse[obs] <- (pred[['Prediction']]-test.set[['Final']])^2
  
  ## CVG
  cvg[obs] <- (test.set[['Final']] > pred.low) & (test.set[['Final']] < pred.high)
  
  ## Wid
  wid[obs] <- (pred.high - pred.low) %>% mean()
  
  ## bias
  bias[obs] <- mean(pred[,'Prediction']-test.set[,'Final'])
}
data.frame(RPMSE=sqrt(mean(rpmse)), CVG=mean(cvg), WID=mean(wid), BIAS=mean(bias))
ggplot() + geom_point(aes(x=fitted(pedagogy.gls), y=resid(pedagogy.gls, type="pearson"))) +
  xlab("Fitted Values") + ylab("Std. Residuals")
ggplot() + geom_histogram(aes(x=resid(pedagogy.gls, type="pearson"))) + xlab("Std. Resid")
pedagogy.gls$coefficients
mean(rpmse)


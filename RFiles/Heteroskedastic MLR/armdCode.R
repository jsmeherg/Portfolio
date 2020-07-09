library(ggplot2)
library(nlme)
library(mvtnorm)
library(car)
library(multcomp)

armdData <- read.table("ARMD.txt", header = TRUE)

##Exploratory Data Analysis

#1
armdData$Trt <- as.factor(armdData$Trt)

#2
ggplot(data=armdData, mapping = aes(x= Baseline, y=Vision)) + geom_point()

#3
ggplot(data=armdData, mapping = aes(x=Time, y=Vision, color = Trt)) + geom_point() + geom_smooth()

##Analysis with an MLR

#1
armdLM <- lm(Vision ~ Baseline + Time:Trt, data = armdData)
residsLM <- data.frame(matrix(residuals(armdLM), nrow = 50, ncol = 4))
corMatrix <- cor(residsLM)

##Longitudinal MLR Model Fitting and Iterative Optimization

#1
armdGLS <- gls(model=Vision~Baseline + Time + Trt + Time:Trt, data=armdData, correlation=corSymm(form=~1|Subject), method="ML")
constEst <- coef(armdGLS$modelStruct$corStruct, unconstrained=FALSE)
betahat <- armdGLS$coefficients 
sigmasquared <- (armdGLS$sigma)^2

#2

logdensFun <- function(u) {
  n.cv <- 100
  yi <- rnorm(n.cv, mean=17, sd=sqrt(5))
  logdens <- rep(x=NA, times=n.cv)
  for(i in 1:100) {
    logdens[i] <- dnorm(yi[i],mean=u,sd=sqrt(5),log=TRUE)
  }
  sum(logdens)
}
logdensFun(17)
optim(par = 13, fn = logdensFun, method = "BFGS", control = list(fnscale = -1))
#It does return a value near y-bar.

#3
X <- data.frame(c(1,armdData[-5]))
X$Trt <- as.double(X$Trt)
n <- nrow(armdData)
betaFunc <- function(betaVec){
    dmvnorm(x=armdData$Vision, mean=X%*%t(t(betaVec)), sigma=(6.807^2)*diag(n), log=TRUE)
}
ybar <- mean(armdData$Vision)
optim(par =data.frame(c(ybar,0,0,0,0)), fn = betaFunc, method = "BFGS", control = list(fnscale = -1))

#4

myFunction <- function(x){
  0.25*dnorm(x,-2,1)+0.75*dnorm(x,2,1)
}
optim(par = -4, fn = myFunction, method = "BFGS", control = list(fnscale=-1))
optim(par = 4, fn = myFunction, method = "BFGS", control = list(fnscale=-1))
#Although -4 comes to a maximum, as it is approaching from the left sid it finds the maximum at the given value, whereas the other approaches from the right
#and thus discovers a larger maximum value.

##Validating Longitudinal MLR Model Assumptions
#stdres.gls()
source("stdres.gls.R")
#1
armdNewLM <- lm(Vision~Baseline + Time*Trt, data=armdData)
avPlots(armdNewLM)
#2
sres <- stdres.gls(armdGLS)
residsGLS <- data.frame(matrix(sres, nrow = 50, ncol = 4))
corMatrix2 <- cor(residsGLS)
#3
hist(sres)
#4
plot(x=sres, y=fitted(armdGLS))
##Statistical Inference

#1
a <- matrix(c(1,0,1,1,0), nrow = 1)
test1 <-glht(armdGLS, linfct=a, alternative ="greater")
summary(test1)
#2
b <- matrix(c(1,0,1,0,0), nrow = 1)
test2 <- glht(armdGLS, linfct=b, alternative = "greater")
summary(test2)
#3
c <- matrix(c(1,29,1,1,1), nrow =1)
test3 <- glht(armdGLS, linfct =c, alternative = "greater")
summary(test3)

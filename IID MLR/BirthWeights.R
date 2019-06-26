library("ggplot2")
library("GGally")
library("MASS")
library("car")
library("lmtest")
library("tidyverse")
library("magrittr")
library("multcomp")

birthweightdata <- read.table("BirthWeights.txt", header = TRUE)

###Summary of Data###

#1
ggplot(data=birthweightdata, mapping=aes(x=Mage,y=BirthWeight)) + geom_point()
#2
ggplot(data=birthweightdata, mapping=aes(x=Race,y=BirthWeight)) + geom_boxplot()
#3
ggplot(data=birthweightdata, mapping=aes(x=Gage,y=BirthWeight, color=Gen)) + geom_point()
#4
cor(x=birthweightdata$Mage, y=birthweightdata$BirthWeight)
#5
ggpairs(data=birthweightdata)

###Fitting a Linear Model###

birthweight.lm <- lm(BirthWeight~., data=birthweightdata)

X <- model.matrix(BirthWeight~., data=birthweightdata)
Y <- birthweightdata[,1]

#1

#(X'X)inverse X'y
betahats <- (solve(t(X)%*%X))%*%(t(X)%*%Y)
betahats
coef(birthweight.lm)

#(y???Xb^)'(y???xb^)/(n???P???1)
n <- nrow(X)
p <- ncol(X) - 1
sigmasquared <- ((t(Y-(X%*%betahats)))%*%(Y-(X%*%betahats))) / (n - p - 1)
sigmasquared
(summary(birthweight.lm)$sigma)^2

#2
fittedbirthweight <- X%*%betahats
fittedbirthweight
fitted.values(birthweight.lm)
head(fittedbirthweight)
head(fitted.values(birthweight.lm))

#3
residbirthweight <- Y-(X%*%betahats)
residbirthweight
resid(birthweight.lm)

#4
summary(birthweight.lm)$r.squared


###Checking Assumptions###

#1
avPlots(birthweight.lm)
#All of the plots appear to be linear although a better look at residuals and testing is necessary.

#2
standardized.residuals <- stdres(birthweight.lm)
ggplot()+geom_histogram(mapping=aes(x=standardized.residuals))

ks.test(standardized.residuals, "pnorm")
#The results of the test fail to reject the null hypotethis.
#3
ggplot(data=birthweightdata, mapping=aes(x=fittedbirthweight,y=standardized.residuals)) + geom_point()

bptest(birthweight.lm)
#The results of the test fail to reject the null hypotethis.


###Prediction###

#1
newx <- as.matrix(c(1, 26, 37, 1, 0, 0, 0))
betahats<- as.matrix(betahats)
t(newx)%*%betahats

new.x <- data.frame(Mage=26, Gage=37, Race="hisp", Gen="Female")
predict.lm(birthweight.lm, newdata=new.x, interval="prediction", level=0.97)

#2
predict.lm(birthweight.lm, newdata=new.x, interval="prediction", level=0.99)

###Cross Validation###

n.cv <- 100 #Number of CV studies to run
n.test <- nrow(birthweightdata) / 2  #Number of observations in a test set
n <- nrow(birthweightdata)
rpmse <- rep(x=NA, times=n.cv)
bias <- rep(x=NA, times=n.cv)
wid <- rep(x=NA, times=n.cv)
cvg <- rep(x=NA, times=n.cv)
for(cv in 1:n.cv){
  ## Select test observations
  
  test.obs <- sample(x=1:n, size=n.test)
  
  ## Split into test and training sets
  test.set <- birthweightdata[test.obs,]
  train.set <- birthweightdata[-test.obs,]
  
  ## Fit a lm() using the training data
  train.lm <- lm(BirthWeight~., data=train.set)
  
  ## Generate predictions for the test set
  my.preds <- predict.lm(train.lm, newdata=test.set, interval="prediction")
  
  ## Calculate bias
  bias[cv] <- mean(test.set[['BirthWeight']]-my.preds[,'fit'])
  
  ## Calculate RPMSE
  rpmse[cv] <- (test.set[['BirthWeight']]-my.preds[,'fit'])^2 %>% mean() %>% sqrt()
  
  ## Calculate Coverage
  cvg[cv] <- ((test.set[['BirthWeight']] > my.preds[,'lwr']) & (test.set[['BirthWeight']] < my.preds[,'upr'])) %>% mean()
  
  ## Calculate Width
  wid[cv] <- (my.preds[,'upr'] - my.preds[,'lwr']) %>% mean()
  
}

qplot(bias, geom="histogram")
qplot(rpmse, geom="histogram")
qplot(cvg, geom="histogram")
qplot(wid, geom="histogram")

#Hypothesis Testing and Confidence Intervals

#1
summary(birthweight.lm)
#Mage has a t value of -2.259 and a P-value of 0.024171 according to the summary of the full linear model

#2
confint(birthweight.lm, 0.90)

#3
norace.lm <- lm(BirthWeight~.-Race, data=birthweightdata)
anova(birthweight.lm, norace.lm)

a <- c(1,24,40,0,0,1,1) - c(1,34,33,0,0,1,1)
a.transpose <- t(as.matrix(a))
my.test <- glht(birthweight.lm, linfct=a.transpose, alternative="two.sided")
confint(my.test, 0.94)

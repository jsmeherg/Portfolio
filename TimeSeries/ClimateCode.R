library(ggplot2)
library(splines)
library(astsa)
library(magrittr)

##Exploratory Data Analysis

weatherData <- read.table("AnnAvgGlobalClimate.txt", header = TRUE)
#1
weatherData$YrMon <- weatherData$Year + (weatherData$Month - 0.5)/12

#2
ggplot(data=weatherData, mapping=aes(x=YrMon, y=AnnAnom)) + geom_line() + geom_smooth()


#3 Using 3 seasonal cycles, or 3 years.
anom.ACF <- acf(weatherData$AnnAnom, lag.max=36)
ACF.dframe <- data.frame(Lag=anom.ACF$lag, ACF=anom.ACF$acf)
ggplot(data=ACF.dframe, aes(x=Lag, y=ACF)) + geom_col()

##Fitting Splines and Time Series Models

#1
X <- bs(x=weatherData$YrMon, knots=1975, degree=1, Boundary.knots= c(min(weatherData$YrMon), max(weatherData$YrMon)+60*(1/12)))
spline.lm <- lm(weatherData$AnnAnom~ X)
#2
ggplot(data=weatherData, mapping=aes(x=YrMon, y=AnnAnom)) + geom_line() + geom_smooth(method=lm, formula=y~bs(x, knots=1975, degree=1), se=FALSE)
#3
resid.ACF <- acf(resid(spline.lm), lag.max=36)
resid.ACF.dframe <- data.frame(Lag=resid.ACF$lag, ACF=resid.ACF$acf)
ggplot(data=resid.ACF.dframe, aes(x=Lag, y=ACF)) + geom_col()

#4
#p,d,q,P,D,Q
key.sarima <- data.frame(c(0,0,0,0,0,0),
                         c(1,0,0,0,0,0),
                         c(0,0,1,0,0,0),
                         c(0,0,0,1,0,0),
                         c(0,0,0,0,0,1),
                         c(1,0,0,1,0,0),
                         c(0,0,1,0,0,1),
                         c(1,0,1,1,0,1),
                         c(0,1,0,0,0,0),
                         c(1,1,0,0,0,0),
                         c(0,1,1,0,0,0),
                         c(0,1,0,1,0,0),
                         c(0,1,0,0,0,1),
                         c(1,1,0,1,0,0),
                         c(0,1,1,0,0,1),
                         c(1,1,1,1,0,1),
                         c(0,0,0,0,1,0),
                         c(1,0,0,0,1,0),
                         c(0,0,1,0,1,0),
                         c(0,0,0,1,1,0),
                         c(0,0,0,0,1,1),
                         c(1,0,0,1,1,0),
                         c(0,0,1,0,1,1),
                         c(1,0,1,1,1,1))


s = 12
aics <- rep(x=NA, times = ncol(key.sarima))
bics <- rep(x=NA, times = ncol(key.sarima))
for(i in 1:ncol(key.sarima)) {
  my.ts.model <- sarima(weatherData$AnnAnom, p=key.sarima[1,i], d=key.sarima[2,i], q=key.sarima[3,i], P=key.sarima[4,i], 
                        D=key.sarima[5,i], Q=key.sarima[6,i], S=s, xreg=X, details=FALSE)
  aics[i] <- my.ts.model$AIC
  bics[i] <- my.ts.model$BIC
 
}                
which(aics == min(aics))
which(bics == min(bics))

#Under both AIC and BIC it is best to use the 16th model in the for loop, or the model with p,d,q,P,D,Q = 1,1,1,1,0,1 respectively.

#5

weather.model <- sarima(weatherData$AnnAnom, p=1, d=1, q=1, P=1, D=0, Q=1, S=12, xreg=X, details=FALSE)
weather.model$ttable


#Model Validation

decorResid <- resid(weather.model$fit)
decorResid.ACF <- acf(decorResid, lag.max=36)
decorResid.ACF.dframe <- data.frame(Lag=decorResid.ACF$lag, ACF=decorResid.ACF$acf)
#1
#ACF of Decorrelated Residuals
ggplot(data=decorResid.ACF.dframe, aes(x=Lag, y=ACF)) + geom_col()
#Fitted Values vs. 
fitSarima <- weatherData$AnnAnom - decorResid
plot(x=fitSarima, y=decorResid)

#Histogram of Decorrelated Residuals
hist(decorResid)


#2 Cross Validation

n <- 6
rpmse <- rep(x=NA, )
coverage <- rep(x=NA, )

newX <- bs(weatherData$YrMon, knots = 1975, degree = 1)
train.set <- weatherData[1:(nrow(weatherData)-60),]
test.set <- weatherData[-(1:(nrow(weatherData)-60)),]
predicts <- max(weatherData$YrMon) + seq(1/12, 60*(1/12), by=1/12)
x.train <- X[1:(nrow(weatherData)-60),]
x.test <- X[-(1:(nrow(weatherData)-60)),]


weather.for <- sarima.for(train.set$AnnAnom, p=1, d=1, q=1, P=1, D=0, Q=1, S=12, xreg = x.train, n.ahead=60, newxreg=x.test)

#RPMSE
(weather.for$pred - test.set$AnnAnom)^2 %>% mean() %>% sqrt()
#Coverage
low <- weather.for$pred - qt(0.975, df=nrow(train.set)-n)*weather.for$se
upr <- weather.for$pred + qt(0.975, df=nrow(train.set)-n)*weather.for$se
cvg <- mean((test.set$AnnAnom > low) & (test.set$AnnAnom < upr))
cvg
##Statistical Inference

#1
weather.model$ttable["2", "p.value"]

weather.model$ttable["2", "Estimate"] + c(-1,1)*qt(.975, df=nrow(weatherData) - n)*weather.model$ttable["2", "SE"]

#2

pred.YrMon <- max(weatherData$YrMon) + seq(1/12, 60*(1/12), by=1/12)
Xpred <- predict(X, newx=pred.YrMon)
my.for <- sarima.for(weatherData$AnnAnom, p=1, d=1, q=1, P=1, D=0, Q=1, S=12, n.ahead=60, newxreg=Xpred)

library(ggplot2)
library(geoR)
library(tidyverse)
library(nlme)
library(car)
library(gstat)
library(multcomp)

surfaceData <- read.table("SurfaceTemps.txt", header = TRUE)
#Exploratory Data Analysis

#1
ggplot(data=surfaceData, mapping=aes(x=Surface, y=Temp)) + geom_boxplot()

#2
ggplot(data=surfaceData, mapping=aes(x=Lon, y=Lat, fill=Temp)) + geom_raster() + scale_fill_distiller(palette="Spectral",na.value=NA)

#3
surfacepredict <- surfaceData %>% filter(is.na(Temp))#fix NAs dropping
surfaceDataNoNA <- surfaceData %>% filter(!is.na(Temp))

surface.lm <- lm(Temp~Surface, data=surfaceDataNoNA)
lmResids <- as.numeric(resid(surface.lm))
ggplot(data=surfaceDataNoNA, mapping=aes(x=Lon, y=Lat, fill=lmResids)) + geom_raster() + scale_fill_distiller(palette="Spectral",na.value=NA)

coordinates <- cbind.data.frame(surfaceDataNoNA$Lon, surfaceDataNoNA$Lat)
plot(variog(coords= coordinates, data=lmResids))

#Spatial MLR Model Fitting
#1
model1 <- gls(model=Temp~Surface, data=surfaceDataNoNA, correlation=corExp(form=~Lon+Lat, nugget=TRUE), method="ML")
model2 <- gls(model=Temp~Surface, data=surfaceDataNoNA, correlation=corSpher(form=~Lon+Lat, nugget=TRUE), method="ML")
model3 <- gls(model=Temp~Surface, data=surfaceDataNoNA, correlation=corGaus(form=~Lon+Lat, nugget=TRUE), method="ML")

summary(model1) #AIC 1191.253
summary(model2) #AIC 1189.086
summary(model3) #AIC 1188.856

coef(model3$modelStruct$corStruct, unconstrained=FALSE) #constrained estimates of the correlation structure
model3$coefficients #Beta
model3$sigma^2 #sigma^2
#Validating Spatial MLR Model Assumptions and Predictions
#1
avModel <- lm(Temp~Surface, data=surfaceDataNoNA)
avPlots(avModel)
#2
source("stdres.gls.R")
decorResid <- stdres.gls(model3)
residDF <- data.frame(Lon=surfaceDataNoNA$Lon, Lat=surfaceDataNoNA$Lat, decorrResid=decorResid)
residVariogram <- variogram(object=decorrResid~1, locations=~Lon+Lat, data=residDF)
plot(residVariogram)
#3

hist(decorResid)
#4
ggplot() + geom_point(mapping=aes(x=fitted.values(model3)), y=decorResid)
#5
system.time({
  land.gls <- gls(model=Temp~Surface, data=surfaceDataNoNA, correlation=corGaus(form=~Lon+Lat, nugget=TRUE), method="ML")
})
#6

n.cv <- 50
n.test <- nrow(surfaceDataNoNA) / 2  #Number of observations in a test set
n <- nrow(surfaceDataNoNA)
rpmse <- rep(x=NA, times=n.cv)
bias <- rep(x=NA, times=n.cv)
wid <- rep(x=NA, times=n.cv)
cvg <- rep(x=NA, times=n.cv)

pb <- txtProgressBar(min = 0, max = n.cv, style = 3)
source("predictgls.R")
for(cv in 1:n.cv){
  ## Run the CV code
  test.obs <- sample(x=1:n, size=n.test)
  
  ## Split into test and training sets
  test.set <- surfaceDataNoNA[test.obs,]
  train.set <- surfaceDataNoNA[-test.obs,]
  
  ## Fit a lm() using the training data
  train.gls <- gls(model=Temp~Surface, data=train.set, correlation=corGaus(form=~Lon+Lat, nugget=TRUE), method="ML")
  
  ## Generate predictions for the test set
  my.preds <- predictgls(train.gls, newdframe=test.set)
  
  ## Calculate bias
  bias[cv] <- mean(test.set[['Temp']]-my.preds[,'Prediction'])
  
  ## Calculate RPMSE
  rpmse[cv] <- (test.set[['Temp']]-my.preds[,'Prediction'])^2 %>% mean() %>% sqrt()
  
  ## Calculate Coverage
  cvg[cv] <- ((test.set[['Temp']] > my.preds[,'lwr']) & (test.set[['Temp']] < my.preds[,'upr'])) %>% mean()
  
  ## Calculate Width
  wid[cv] <- (my.preds[,'upr'] - my.preds[,'lwr']) %>% mean()
  
  ## Update the progress bar
  setTxtProgressBar(pb, cv)
}
close(pb)

summary(surface.lm)
fitted.values(surface.lm)
#Statistical Inference
#1
anova(model3)
#There is a difference across surface types.
#2
confint(model3)
#Urban results in increased temperatures.

#3
a <- c(1, 0, 0, 1, 0) - c(1, 0, 0, 0, 1)
a.transpose <- t(as.matrix(a))
my.test <- glht(model3, linfct=a.transpose, alternative="two.sided")
confint(my.test, 0.95)
#4
tempPreds <- predictgls(model3, newdframe = surfacepredict)
surfacepredict$Temp <- tempPreds$Prediction
ggplot(data=surfacepredict, mapping=aes(x=Lon, y=Lat, fill=Temp)) + geom_raster() + scale_fill_distiller(palette="Spectral") #Predictions only.
surfaceFinal <- rbind.data.frame(surfacepredict, surfaceDataNoNA)
ggplot(data=surfaceFinal, mapping=aes(x=Lon, y=Lat, fill=Temp)) + geom_raster() + scale_fill_distiller(palette="Spectral") #Predictions with given values.

library(ggplot2)
library(geoR)
library(tidyverse)
library(nlme)
library(car)
library(gstat)
library(multcomp)


waterData <- read.table("WaterHoldingCapacity.txt", header=TRUE)

#1
#Exploratory Graphs
ggplot(data=waterData, mapping=aes(x=Yield, y=WHC)) + geom_point()
ggplot(data=waterData, mapping=aes(x=EC, y=WHC)) + geom_point()

#There appears to be some form of a linear relationship between WHC and Yield and EC.

#2
waterpredict <- waterData %>% filter(is.na(WHC))#fix NAs dropping
waterDataNoNA <- waterData %>% filter(!is.na(WHC))
water.lm <- lm(WHC~Yield + EC, data=waterDataNoNA)
lmResids <- as.numeric(resid(water.lm))

ggplot(data=waterDataNoNA, mapping=aes(x=Lon, y=Lat, fill=lmResids)) + geom_raster() + scale_fill_distiller(palette="Spectral",na.value=NA)

coordinates <- cbind.data.frame(waterDataNoNA$Lon, waterDataNoNA$Lat)
plot(variog(coords= coordinates, data=lmResids))

#There appears to be some spatial correlation that is seen through a non-linear variogram.

#3
model1 <- gls(model=WHC~Yield + EC, data=waterDataNoNA, correlation=corExp(form=~Lon+Lat, nugget=TRUE), method="ML")
model2 <- gls(model=WHC~Yield + EC, data=waterDataNoNA, correlation=corSpher(form=~Lon+Lat, nugget=TRUE), method="ML")
model3 <- gls(model=WHC~Yield + EC, data=waterDataNoNA, correlation=corGaus(form=~Lon+Lat, nugget=TRUE), method="ML")

summary(model1) #AIC 272.3653
summary(model2) #AIC 272.9623
summary(model3) #AIC 273.4355

#Best coorelation model to use is an exponential. 

#4 Write out model.

#5
water.gls <- gls(model=WHC~Yield + EC, data=waterDataNoNA, correlation=corExp(form=~Lon+Lat, nugget=TRUE), method="ML")

avPlots(water.lm)

source("stdres.gls.R")
decorResid <- stdres.gls(water.gls)
residDF <- data.frame(Lon=waterDataNoNA$Lon, Lat=waterDataNoNA$Lat, decorrResid=decorResid)
residVariogram <- variogram(object=decorrResid~1, locations=~Lon+Lat, data=residDF)
plot(residVariogram)
ggplot(data=waterDataNoNA, mapping=aes(x=Lon, y=Lat, fill=decorResid)) + geom_raster() + scale_fill_distiller(palette="Spectral",na.value=NA)
hist(decorResid)
ggplot() + geom_point(mapping=aes(x=fitted.values(water.gls)), y=decorResid)


#6
a <- c(1, 1, 0) - c(1, 0, 0)
a.transpose <- t(as.matrix(a))
my.test <- glht(water.gls, linfct=a.transpose, alternative="two.sided")
confint(my.test, 0.95)
#A 95% confidence interval between 0.00724 and 0.04432 with an estimate of 0.02578. This shows that locations with higher yield had higher WHC.

#7
source("predictgls.R")
tempPreds <- predictgls(water.gls, newdframe = waterpredict)
waterpredict$WHC <- tempPreds$Prediction
ggplot(data=waterpredict, mapping=aes(x=Lon, y=Lat, fill=WHC)) + geom_raster() + scale_fill_distiller(palette="Spectral") #Predictions only.
waterFinal <- rbind.data.frame(waterpredict, waterDataNoNA)
ggplot(data=waterFinal, mapping=aes(x=Lon, y=Lat, fill=WHC)) + geom_raster() + scale_fill_distiller(palette="Spectral") #Predictions with given values.

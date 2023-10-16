install.packages("tidyverse")
install.packages("datarium")
library(datarium)
library(tidyverse)
data("marketing", package = "datarium")
marketing #sales is the response, marketing is the data frame
#full model with all three predictors
MLR<-lm(sales~youtube+facebook+newspaper,data=marketing)
summary(MLR)
#predict PI w/ specific predictor values
fullMarket<-data.frame(youtube=50.9,facebook=42.1,newspaper=87.6)
fullMarket
PI=predict(MLR,fullMarket,se.fit=TRUE,interval="prediction",level=.95)
PI
CI=predict(MLR,marketing,se.fit=TRUE,interval="confidence",level=.95)
CI
PI=predict(MLR,marketing,se.fit=TRUE,interval="prediction",level=.95)
PI
#reduced model for youtube and facebook predictors
redMLRyouface<-lm(sales~youtube+facebook,data=marketing)
summary(redMLRyouface)
#reduced model CI
youAvg<-mean(marketing$youtube)
youAvg
faceAvg<-mean(marketing$facebook)
faceAvg
newMark<-data.frame(youAvg,faceAvg)
newMark
newMarket<-data.frame(youtube=176.451,facebook=27.9168)
newMarket
CI=predict(redMLRyouface,newMarket,se.fit=TRUE,interval="confidence",level=.95)
CI
#reduced model PI
PI=predict(redMLRyouface,newMarket,se.fit=TRUE,interval="prediction",level=.95)
PI
#reduced model for facebook and newspaper predictors
redMLRfacenews<-lm(sales~facebook+newspaper,data=marketing)
summary(redMLRfacenews)
#reduced model for newspaper and youtube predictors
redMLRnewsyou<-lm(sales~newspaper+youtube,data=marketing)
summary(redMLRnewsyou)
#single predictor model for youtube
SLRyou<-lm(sales~youtube,data=marketing)
summary(SLRyou)
#single predictor model for facebook
SLRface<-lm(sales~facebook,data=marketing)
summary(SLRface)
#single predictor model for newspaper
SLRnews<-lm(sales~newspaper,data=marketing)
summary(SLRnews)
#par(mfrow=c(2,3))
#single predictor youtube plot
plot(marketing$youtube,marketing$sales)
residualsModel<-residuals(SLRyou)
fittedModel<-fitted.values(SLRyou)
#residual vs fitted values
plot(fittedModel,residualsModel,xlab="fitted response value",
     ylab="model residual",col="black")
#residual vs youtube
plot(marketing$youtube,residualsModel,xlab="youtube",
     ylab="model residual",col="black")
#histogram
hist(residualsModel) #hist(residuals_model1,breaks=10)
box()
#qq plot
qqnorm(residualsModel,main="normal QQ plot",xlab="theoretical quantiles",
       ylab="residual quantiles")
qqline(residualsModel)
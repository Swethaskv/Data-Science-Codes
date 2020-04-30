com <- read.csv(file.choose())
pairs(com)
attach(com)
View(com)
str(com)
com$cd <- as.numeric(com$cd)
com
com$multi <- as.numeric(com$multi)
com$premium <- as.numeric(com$premium)


View(com)
is.na(com)


summary(com)

plot(price,speed)
plot(com)
head(com)

cor(com)
pairs(com)

##partial corelation matrix

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(com))

##Creating model
m1 <- lm(price~.,data=com)
summary(m1)


m2<- lm(log(price)~.,data=com)
summary(m2)


m3 <- lm(sqrt(price)~.,data=com)
summary(m3)

m4 <- lm(price~X+log(speed)+hd+ram+screen+cd+multi+premium+ads+trend)
summary(m4)

m6 <-lm(price~X+sqrt(speed)+hd+ram+screen+cd+multi+premium+ads)
summary(m6)
library(car)

##influencial observartions
influence.measures(m2)
influencePlot(m2,id.n=3)
influenceIndexPlot(m2,id.n=3)

vif(m2)

modelD <- lm(log(price)~.,data=com[-4478,])
summary(modelD)
?av.plots

avPlots(m2,id.n=2,id.cex=0.7)




library(MASS)
stepAIC(m2,direction = "both")



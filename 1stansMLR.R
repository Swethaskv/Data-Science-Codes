data <- read.csv(file.choose())
pairs(data)
attach(data)
View(data)

##ignoring state variable

data$State <- NULL
View(data)
is.na(data)
qqnorm(Profit)
qqline(Profit)

summary(data)

plot(Profit,R.D.Spend)
plot(Profit,Marketing.Spend)
plot(Profit,Administration)


cor(data)
pairs(data)

##partial corelation matrix

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(data))

##Creating model
m1 <- lm(Profit~.,data=data)
summary(m1)


m2 <- lm(Profit~Administration)
summary(m2)

m3 <- lm(Profit~Marketing.Spend)
summary(m3)

m4 <- lm(Profit~Administration+Marketing.Spend)
summary(m4)
library(car)
##influencial observartions
influence.measures(m1)
influencePlot(m1,id.n=3)
influenceIndexPlot(m1,id.n=3)

vif(m1)

modelD <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=data[-50,])
summary(modelD)
?av.plots

avPlots(m1,id.n=2,id.cex=0.7)

modelR <- lm(Profit~R.D.Spend+Marketing.Spend)
summary(modelR)
library(MASS)
stepAIC(modelR,direction = "both")
stepAIC(m1,direction = "both")
stepAIC(modelD,direction = "both")

model_l <- lm(log(Profit)~R.D.Spend+Marketing.Spend)
summary(model_l)
stepAIC(model_l,direction = "both")


model_p <- lm(Profit~(R.D.Spend*R.D.Spend)+Marketing.Spend)
summary(model_p)


model_q <- lm(Profit~sqrt(R.D.Spend)+Marketing.Spend)
summary(model_q)


modelk <- lm(Profit~sqrt(R.D.Spend)+sqrt(Marketing.Spend))
summary(modelk)

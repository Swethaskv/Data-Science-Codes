cor <- read.csv(file.choose())
pairs(cor)
attach(cor)
View(cor)
str(cor)
##ignoring the unwanted variables
cor <- cor[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
cor

View(cor)
is.na(cor)

summary(cor)

plot(cor)


cor(cor)
pairs(cor)

##partial corelation matrix

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(cor))

##Creating model
mod1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=cor)
summary(mod1)


mod2 <- lm(Price~cc,data=cor)
summary(mod2)

m3 <- lm(Price~Doors,data = cor)
summary(m3)

m4 <- lm(Price~cc+Doors,data=cor)
summary(m4)

library(car)
##influencial observartions
influence.measures(mod1)
influencePlot(mod1,id.n=3)
influenceIndexPlot(mod1,id.n=3)

vif(m1)

modelD <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=cor[-81,])
summary(modelD)
?av.plots

avPlots(mod1,id.n=2,id.cex=0.7)

model_del <-lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data=cor[-81,])
summary(model_del)


modT <- lm(log(Price)~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data=cor[-81,])
summary(modT)

modS <- lm(sqrt(Price)~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data=cor[-81,])
summary(modS)

m2 <- lm(Price~sqrt(Age_08_04)+KM+HP+cc+Gears+Quarterly_Tax+Weight,data=cor[-81,])
summary(m2)

m3 <- lm(Price~sqrt(Age_08_04)+KM+HP+sqrt(cc)+Gears+Quarterly_Tax+Weight,data=cor[-81,])
summary(m3)

modelR <- lm(Profit~R.D.Spend+Marketing.Spend)
summary(modelR)


library(MASS)
stepAIC(modelR,direction = "both")
stepAIC(mod1,direction = "both")
stepAIC(modelD,direction = "both")




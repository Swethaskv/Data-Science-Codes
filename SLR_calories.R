data <- read.csv(file.choose())
attach(data)
View(data)
summary(data)
x <- data$Calories.Consumed
y <- data$Weight.gained..grams.

qqnorm(x)
qqline(x)
shapiro.test(x)

##calories is normalised data

qqnorm(y)
qqline(y)
shapiro.test(y)
## weight gained is not normalised

y1 <- log(y)
qqnorm(y1)
qqline(y1)
shapiro.test(y1)
##now both the x and y are normalised data.

##create the model


m1 <- lm(y1~x)
summary(m1)
 
p1 <- predict(m1)
p1

e1 <- exp(p1)
e1
##rmse value of model1

r1 <- sqrt(sum(m1$residuals^2)/nrow(data))
r1
##model2
m2 <- lm(log(y1)~x)
summary(m2)

p2 <- predict(m2)
p2

##since we are taking log(y) we should do the  exp to compare it with the actual value.

e2 <- exp(p2)
e2


r2 <- sqrt(sum(m2$residuals^2)/nrow(data))
r2
##model3

m3 <- lm((y1^2)~x)
summary(m3)

p3 <- predict(m3)
p3

e3 <- exp(p3)
e3

r3 <- sqrt(sum(m3$residuals^2)/nrow(data))
r3

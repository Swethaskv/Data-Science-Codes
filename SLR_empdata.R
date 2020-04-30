data <- read.csv(file.choose())
View(data)
attach(data)
x <- data$Salary_hike
y <- data$Churn_out_rate
summary(data)
##corelation

c <- cor(data)
c
##build the 1st model

m1 <- lm(y~x)
summary(m1)

p1 <- predict(m1)
p1

r1 <- sqrt(sum(m1$residuals^2)/nrow(data))
r1

##error
e1 <- y-p1
e1
##model2


m2 <- lm(log(y)~x)
summary(m2)

p2 <- predict(m2)
p2

exp2 <- exp(p2) 
exp2

r2 <- sqrt(sum(m2$residuals^2)/nrow(data))
r2

##model3

m3 <- lm(log(y)~log(x))
summary(m3)

p3 <- predict(m3)
p3

exp3 <- exp(p3)
exp3


r3 <- sqrt(sum(m3$residuals^2)/nrow(data))
r3

##model4

m4 <- lm(log(y)~x+I(x*x))
summary(m4)

p4 <- predict(m4)
p4

exp4 <- exp(p4)
exp4

error <- y-p4
error

r4 <- sqrt(sum(m4$residuals^2)/nrow(data))
r4
## best model is model4.
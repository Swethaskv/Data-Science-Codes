data <- read.csv(file.choose())
View(data)
summary(data)
x <- data$YearsExperience
y <- data$Salary


cor(data)


m1 <- lm(y~x)
summary(m1)

r1 <- sqrt(sum(m1$residuals^2)/nrow(data))
r1

p1 <- predict(m1)
p1


e1 <-y-p1 
e1

##model2
m2 <- lm(log(y)~x)
summary(m2)         

r2 <- sqrt(sum(m2$residuals^2)/nrow(data))
r2

p2 <- predict(m2)
p2

e2 <- exp(p2)
e2

m4 <- lm(y~x+I(x*x))
summary(m4)

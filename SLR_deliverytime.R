data <- read.csv(file.choose())
View(data)
attach(data)
x <-data$Delivery.Time 
y <-data$Sorting.Time

cor(data)
plot(data)

m1<- lm(y~x)
summary(m1)

m2 <- lm(y^2~x)
summary(m2)

m3 <- lm(log(y)~x)
summary(m3)

m4 <- lm(sqrt(y)~x)
summary(m4)

m5 <- lm(log(y)~log(x))
summary(m5) ##selected model

m6 <- lm(log(x) ~ y + I(y*y))
summary(m6)

m7 <- lm(log(x)~y+I(y*y*y))
summary(m7)


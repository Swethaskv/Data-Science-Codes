##1).   Output variable -> y -> Whether the client has subscribed a term deposit or not Binomial ("yes" or "no")

Bank <- read.csv("C:/Users/USER/Downloads/assignment sets/logistic regression/bank-full.csv",sep = ";")
View(Bank)
attach(Bank)
sum(is.na(Bank))
str(Bank)
summary(Bank)
table(y)

#####  Model Building  #####


model <- glm(y~.,data=Bank,family = "binomial")
summary(model)
prob <- predict(model,type = c("response"),Bank)
prob
confusion <- table(prob>0.5,Bank$y)
confusion


Accuracy <- sum(diag(confusion))/sum(confusion)
Accuracy

pred <- prediction(prob, Bank$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf,col=rainbow(10))



##Area under the curve is high. So the model is good.



##).    classify that a person had an affair or not

Affairs <- read.csv(file.choose())
View(Affairs)
attach(Affairs)
table(affairs)
table(gender)
table(children)
Affairs$affairs <- ifelse(affairs=="0",0,1)
table(Affairs$affairs)
str(Affairs)
summary(as.factor(Affairs$affairs))
summary(Affairs)


#####   Model   #####

Model <- glm(affairs~gender+children+factor(age)+factor(yearsmarried)+factor(religiousness)+factor(education)+factor(occupation)+factor(rating),family = "binomial",data = Affairs)
 summary(Model)


 prob <- predict(Model,type = c("response"),Affairs)
 Confusion <- table(prob>0.5,Affairs$affairs)
 Confusion

Accuracy <- sum(diag(Confusion))/sum(Confusion)
Accuracy

##ROCR Curve
library(ROCR)
pred <- prediction(prob,Affairs$affairs )
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf,col=rainbow(10))





##Area under the curve is high. So the model is good.


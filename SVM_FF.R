library(kernlab)
library(caret)
library(plyr)
FF <- read.csv(file.choose())
View(FF)
class(FF)
str(FF)
#The area value has lots of zeros

hist(FF$area)
rug(FF$area)
# Transform the Area value to Y 

FF1 <- mutate(FF, y = log(area + 1))  # default is to the base e, y is lower case
hist(FF1$y)

summary(FF) 


normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
FF$temp = normalize(FF$temp)
FF$RH   = normalize(FF$RH)
FF$wind = normalize(FF$wind)
FF$rain = normalize(FF$rain)
# We need to tweak this as a classification problem.lets base out the Size using this criteria :

attach(FF)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(FF), replace = TRUE, prob = c(0.7,0.3))
FF_train <- FF[ind==1,]
FF_test  <- FF[ind==2,]
model1<-ksvm(size_category~temp+rain+wind+RH, 
             data= FF_train,kernel = "vanilladot")

model1

Area_pred <- predict(model1, FF_test)

table(Area_pred,FF_test$size_category)

agreement <- Area_pred == FF_test$size_category
table(agreement)

prop.table(table(agreement))

model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= FF_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=FF_test)
mean(pred_rfdot==FF_test$size_category) 

model_vanilla<-ksvm(size_category~temp+rain+wind+RH, 
                    data= FF_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=FF_test)
mean(pred_vanilla==FF_test$size_category) 


model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
                      data= FF_train,kernel = "besseldot")

pred_bessel<-predict(model_besseldot,newdata=FF_test)
mean(pred_bessel==FF_test$size_category) 

model_poly<-ksvm(size_category~temp+rain+wind+RH, 
                 data= FF_train,kernel = "polydot")

pred_poly<-predict(model_poly,newdata = FF_test)
mean(pred_poly==FF_test$size_category)


library(neuralnet)
library(nnet)
library(NeuralNetTools)
ff <- read.csv(file.choose())
str(ff)
View(ff)
hist(ff$FFMC, prob = T, breaks = 30)
lines(density(ff$FFMC))
summary(ff$FFMC)

hist(ff$DMC, prob = T, breaks = 30)
lines(density(ff$DMC))
summary(ff$DMC)

hist(ff$DC, prob = T, breaks = 30)
lines(density(ff$DC))
summary(ff$DC)

hist(ff$ISI, prob = T, breaks = 30)
lines(density(ff$ISI))
summary(ff$ISI)
summary(ff)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
# Convert month and day string variables into numeric values
ff$month <- as.numeric(as.factor(ff$month))
ff$day <- as.numeric(as.factor(ff$day))
ff$size_category <- as.numeric(as.factor(ff$size_category))
str(ff)
ff_norm<-as.data.frame(lapply(ff,FUN=normalize))
summary(ff_norm$area) 
summary(ff$area)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(ff_norm), replace = TRUE, prob = c(0.7,0.3))
ff_train <- ff_norm[ind==1,]
ff_test  <- ff_norm[ind==2,]

# Creating a neural network model on training data


ff_model <- neuralnet(area~FFMC+DMC+DC+ISI+rain+wind+temp+RH,data = ff_train)
str(ff_model)

plot(ff_model, rep = "best")
summary(ff_model)

par(mar = numeric(4), family = 'serif')
plotnet(ff_model, alpha = 0.6)

# Evaluating model performance

set.seed(12323)
model_results <- compute(ff_model,ff_test[3:10])
predicted_area<- model_results$net.result

# Predicted strength Vs Actual Strength of test data.
cor(predicted_area,ff_test$area)

str_max <- max(ff$area)
str_min <- min(ff$area)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualarea_pred <- unnormalize(predicted_area,str_min,str_max)
head(Actualarea_pred)

# Improve the model performance :
set.seed(12345)
ff_model2 <- neuralnet(area~FFMC+DMC+DC+ISI+rain+wind+temp+RH,data= ff_train,
                             hidden = 5)
plot(ff_model2, rep = "best")
summary(ff_model2)

model_results2<-compute(ff_model2,ff_test[3:10])
predicted_area2<-model_results2$net.result
cor(predicted_area2,ff_test$area)

plot(predicted_area,ff_test$area)
par(mar = numeric(4), family = 'serif')
plotnet(ff_model2, alpha = 0.6)













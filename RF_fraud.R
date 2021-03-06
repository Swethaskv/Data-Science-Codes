library(randomForest)
library(caret)
library(MASS)
set.seed(123)


FraudCheck <- read.csv(file.choose())
hist(FraudCheck$Taxable.Income)
hist(FraudCheck$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("blue","red", "green","violet"))

Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
# if Taxable Income is less than or equal to 30000 then Risky else Good.
FCtemp= data.frame(FraudCheck,Risky_Good)
FC = FCtemp[,c(1:7)]

str(FC)
table(FC$Risky_Good)   

# Data Partition
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]
set.seed(213)
rf <- randomForest(Risky_Good~., data=train)
rf  
attributes(rf)
pred1 <- predict(rf, train)
head(pred1)

head(train$Risky_Good)
confusionMatrix(pred1, train$Risky_Good) 

# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good) 

plot(rf)


tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)

rf1 <- randomForest(Risky_Good~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1

pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$Risky_Good)


pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$Risky_Good)

hist(treesize(rf1), main = "No of Nodes for the trees", col = "yellow")

varImpPlot(rf1)


varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")

importance(rf1)

varUsed(rf) 
partialPlot(rf1, train, Taxable.Income, "Good")

tr1 <- getTree(rf1, 2, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, FC$Risky_Good)

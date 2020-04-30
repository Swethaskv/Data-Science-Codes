library(randomForest)
library(MASS)
library(caret)

set.seed(123)


CompanyData <- read.csv(file.choose())
hist(CompanyData$Sales, main = "Sales of Companydata",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","green", "red","yellow"))
highsales = ifelse(CompanyData$Sales<9, "No", "Yes")  # if greater than 8 then high sales else Low
CD = data.frame(CompanyData[2:11], highsales)
str(CD)

table(CD$highsales)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test  <- CD[ind==2,]
set.seed(213)
rf <- randomForest(highsales~., data=train)
rf  
attributes(rf)

pred1 <- predict(rf, train)
head(pred1)
head(train$highsales)

confusionMatrix(pred1, train$highsales)


pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsales) 

plot(rf)
# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(highsales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1

pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$highsales)  # 100 % accuracy on training data 

pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$highsales) 
hist(treesize(rf1), main = "No of Nodes for the trees", col = "blue")

varImpPlot(rf1)

varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")

importance(rf1)
varUsed(rf)

partialPlot(rf1, train, Price, "Yes")

getTree(rf, 1, labelVar = TRUE)


# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, CD$highsales)

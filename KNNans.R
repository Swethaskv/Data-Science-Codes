glass <- read.csv(file.choose())
View(glass)

table(glass$Type)

#  Type is factor with 7 levels that is 1,2...,7. We also replacing these 7 entries with...
glass$Type <- factor(glass$Type, levels = c("1","2","3","4","5","6","7"), labels = c("Win_float","Win_nonfloat","veh_win_f","veh_win_nf","con","tab","head"))
# table or proportation of entries in the datasets. What % of level entries.
round(prop.table(table(glass$Type))*100,1)
summary(glass[c("RI","Na","Mg","Al","Si","K","Ca","Ba","Fe")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization

norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to wbcd dataset
glass_n <- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)

#create training and test datasets
glass_train <- glass_n[1:109,]
glass_test <- glass_n[110:214,]

#Get labels for training and test datasets

glass_train_labels <- glass[1:109,1]
glass_test_labels <- glass[110:214,1]

# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset(bagging method)
test_acc <- NULL
train_acc <- NULL
for (i in seq(2,100,2))
{
  train_glass_pred <- knn(train=glass_train,test=glass_train,cl=glass_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_glass_pred==glass_train_labels))
  test_glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_glass_pred==glass_test_labels))
}


# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(2,100,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(2,100,2),test_acc,type="l",main="Test_accuracy",col="red")


acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(2,100,2)))
# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

?ggplot
glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=14)


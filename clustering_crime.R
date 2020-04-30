crime <- read.csv(file.choose())
View(crime)
summary(crime)
##normalise the data

nordata <- scale(crime[,2:5])
summary(nordata)

d <- dist(nordata,method = "euclidean") ##distance   
d
dm <- dist(nordata,method = "manhattan") 
dm
dc <- dist(nordata,method = "canberra") 
dc
?dist

fit <- hclust(d,method = "complete") ##average centroid single
plot(fit)
plot(fit,hang = -1)

fitA <- hclust(d,method = "average") ##average centroid single
plot(fit)
plot(fit,hang = -1)

fitC <- hclust(d,method = "centroid") ##average centroid single
plot(fit)
plot(fit,hang = -1)



groups <- cutree(fit,k=3)
rect.hclust(fit,k=3,border = "red")

group1 <- cutree(fit,k=4)
rect.hclust(fit,k=4,border = "red")

membership<-as.matrix(groups)

final<-data.frame(nordata,membership)
View(final)

install.packages("data.table")
library(data.table)
?setcolorder

setcolorder(final,c("membership"))
View(final)
class(final)

write.xlsx(final, file="final2.xlsx")
setwd("C:/Users/USER/Downloads/assignment sets/clustering")

##another method

d <- dist(nordata)
fitH <- hclust(d,"ward.D2")
plot(fitH)

plot(fitH,labels=crime$X,hang=-1)
rect.hclust(fitH,k=3,border = "red")

clusters <- cutree(fitH,3)
clusters
plot(nordata,col=clusters)

plot(fitH,labels=crime$X,hang=-1)
rect.hclust(fitH,k=4,border = "red")

clusters <- cutree(fitH,4)
clusters
plot(nordata,col=clusters)



##model based clustering
library(mclust)
fitM <- Mclust(nordata)
fitM
plot(fitM)


##density bASED clustering
library(dbscan)
kNNdistplot(nordata,k=3)
abline(h=1.4,col="red",lty=2)
fitD <- dbscan(nordata,eps=1.4,minPts =6)
fitD
plot(crime,col=fitD$cluster)

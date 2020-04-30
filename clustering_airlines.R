airlines <- read.csv(file.choose())
View(airlines)
str(airlines)
na.omit(airlines)
scaledata <- scale(airlines)
scaledata
plot(airlines)
plot(airlines$Balance,airlines$Qual_miles)
fitk <- kmeans(scaledata,3)
str(fitk)
plot(airlines,col=fitk$cluster)

##choosing k
k <- list()
for(i in 1:10){
k[[i]] <- kmeans(scaledata,i)
}
k

betweenss_totss <- list()
for(i in 1:10){
betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss 
}

plot(1:10,betweenss_totss,type = "b",
     ylab="Between SS/Total SS",xlab="clusters(k)")


for(i in 1:4){
  plot(airlines,col=k[[i]]$cluster)
}

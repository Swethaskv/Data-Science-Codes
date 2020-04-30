wine <- read.csv(file.choose())
View(wine)
summary(wine)
library(cluster)
library(fpc)
library(NbClust)
library(factoextra)
library(ggplot2)


W.pca <- princomp(wine[,-1],cor=TRUE,scores = TRUE,covmat = NULL)

##extracting pc scores
str(W.pca)
W.pca$scores


summary(W.pca1)
plot(W.pca1)
biplot(W.pca)
##cluster analysis
no_of_Clusters = NbClust(wine, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")
fviz_nbclust(no_of_Clusters) + theme_minimal()
?NbClust()

##hierarchial clustering

hclust.complete = eclust(wine, "hclust", k = 5, method = "complete", graph = FALSE) 
fviz_dend(hclust.complete, rect = TRUE, show_labels = FALSE) 

##kmeans clustering
km.7 = eclust(wine, "kmeans", k = 4, nstart = 25, graph = FALSE)
fviz_cluster(km.7, geom = "point", frame.type = "norm",ellipse = TRUE)



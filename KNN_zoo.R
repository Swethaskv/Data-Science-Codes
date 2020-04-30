set.seed(1)
library(class)
zoo <- read.csv(file.choose())
zoo <-  data.frame(zoo)
names(zoo) <- c("animal", "hair", "feathers", "eggs", "milk", "airborne",
              "aquatic", "predator", "toothed", "backbone", "breathes", "venomous",
              "fins", "legs", "tail", "domestic", "size", "type")

types <- table(zoo$type)
zoo_target <- zoo[, 18]
zoo_key <- zoo[, 1]
zoo$animal <- NULL

names(types) <- c("mammal", "bird", "reptile", "fish", "amphibian", "insect", "crustacean")
types

summary(zoo)
str(zoo)

k = sqrt(17) + 1
m1 <- knn.cv(zoo, zoo_target, k, prob = TRUE)
prediction <- m1

cmat <- table(zoo_target,prediction)
acc <- (sum(diag(cmat)) / length(zoo_target)) * 100
print(acc)

data.frame(types)
cmat
acc

install.packages("arules")
library(arules)
install.packages("arulesviz")
library(arulesViz)
Movies <- read.csv(file.choose())
View(Movies)
Movies <- Movies[,-(1:5)]
Movies <- as(as.matrix(Movies),"transactions")
class(Movies)
rules <- apriori(Movies,parameter=list(support=0.004,confidence=0.5,minlen=2))
rules
inspect(head(sort(rules,by="lift")))
head(quality(rules))
plot(rules)
plot(rules,method = "grouped")
plot(rules[1:50],method = "graph")

Rules1 <- apriori(Movies,parameter =list(support=0.003,confidence=0.6,minlen=3))
Rules1
inspect(head(sort(Rules1,by="lift")))
head(quality(Rules1))
windows()
plot(Rules1,method="grouped")
plot(Rules1,method="graph")

Rules2 <- apriori(Movies,parameter =list(support=0.005,confidence=0.7,minlen=2))
Rules2
inspect(head(sort(Rules2,by="lift")))
head(quality(Rules2))

Rules3 <- apriori(Movies,parameter =list(support=0.002,confidence=0.6,minlen=2))
Rules3
inspect(head(sort(Rules3,by="lift")))
head(quality(Rules3))
windows()
plot(Rules3,method="grouped")
plot(Rules3,method="graph")

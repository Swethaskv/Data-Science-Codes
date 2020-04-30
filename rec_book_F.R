 library(Matrix)
 library(recommenderlab)
 data=read.csv(file.choose())
 attach(data)
 View(data)

### EDA


 summary(data)

 sum(is.na(data))
 str(data)

###Booktitle and rating

rating_data=data[,-c(1,4,5)]
str(rating_data)
rating_data_matrix=as(rating_data,'realRatingMatrix')

########### popularity based #############

 recomm_model1=Recommender(rating_data_matrix,method="POPULAR")
 recommended_item1=predict(recomm_model1,rating_data_matrix[100],n=5)
 as(recommended_item1,"list")

############ user based ######################

> recomm_model1=Recommender(rating_data_matrix,method="UBCF")
> recommended_item1=predict(recomm_model1,rating_data_matrix[100],n=5)
> as(recommended_item1,"list")

### Booktitle and publish 

 publish_data=data[,-c(1,4,6)]
 str(publish_data)

 publish_data_matrix=as(publish_data,'realRatingMatrix')
 recomm_model2=Recommender(publish_data_matrix,method="POPULAR")
 recommended_item1=predict(recomm_model1,publish_data_matrix[100],n=5)
 as(recommended_item1,"list")
`$100`
 
### Booktitle and author

 author_data=data[,-c(1,5,6)]
 str(author_data)
 author_data_matrix=as(author_data,'realRatingMatrix')
 recomm_model2=Recommender(author_data_matrix,method="POPULAR")
 recommended_item1=predict(recomm_model1,author_data_matrix[100],n=5)
 as(recommended_item1,"list")
$100
 
###Recommendation list is the same even trying with different methods


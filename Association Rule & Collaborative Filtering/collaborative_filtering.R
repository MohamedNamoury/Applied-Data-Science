


library(stringi)
library(reshape2)


library(lsa)
library(matlib)



ratingmat=read.csv("courserating.csv")
tratingmat=read.csv("tcourserating.csv")
#We can now remove user ids
ratingmat = as.matrix(ratingmat[,-1])

#tratingmat<-t(ratingmat)
tratingmat= as.matrix(tratingmat[,-1])

tratingmat[is.na(tratingmat)] = 0
cosine(tratingmat)

#install.packages("recommenderlab", dependencies=TRUE)
library(recommenderlab)

#Convert ratings matrix to real rating matrx which makes it dense
ratingmat = as(ratingmat, "realRatingMatrix")


#Create Recommender Model. The parameters are UBCF and Cosine similarity. We take 10 nearest neighbours
rec_mod = Recommender(ratingmat, method = "IBCF", param=list(method="Cosine")) 

#Obtain top 5 recommendations for 1st user entry in dataset
Top_5_pred = predict(rec_mod, ratingmat[4], n=1)

#Convert the recommendations to a list
Top_5_List = as(Top_5_pred, "list")
Top_5_List





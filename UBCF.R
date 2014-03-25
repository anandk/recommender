library("recommenderlab")

setwd("C:/TW-Projects/PS-Projects/AbcamAnalytics/RSandbox/recommender")

df <- read.csv(file="FinalWeightsCombined",sep="\t",
               colClasses=c("character","character","numeric"))
colnames(df)<- c("user","abIDs","weight")
userItemMatrix <- as(df,"realRatingMatrix")
rec <- Recommender(userItemMatrix, method="POPULAR")
#as(recom,"list")
userList <- as(userItemMatrix,"list")
dfUser <- as(userList,"data.frame")
userIndex <- which(colnames(dfUser)=="-6786791769037665337")
recom <- predict(rec,userItemMatrix[407983],n=10)
library("recommenderlab")

setwd("C:/TW-Projects/PS-Projects/AbcamAnalytics/RSandbox/recommender")

df <- read.csv(file="FinalWeightsCombined",sep="\t",
               colClasses=c("character","character","numeric"))
colnames(df)<- c("user","abIDs","weight")
df$user <- format(df$user, scientific=FALSE)
userItemMatrix <- as(df,"realRatingMatrix")
rec <- Recommender(userItemMatrix, method="POPULAR")
recom <- predict(rec,userItemMatrix[44],n=10)
#as(recom,"list")

library("recommenderlab")

getRecommendations <- function(userPreferencesDF,
  targetUser,
  currentAbId,
  numRecommendations = 10,
  numRankedItems = 10){
    
userColumn <- "user"
itemColumn <- "itemIDs"
weightColumn <- "weight"

colnames(userPreferencesDF)<- c(userColumn,itemColumn,weightColumn)
itemsViewedByTargetUser <- userPreferencesDF[userPreferencesDF[,userColumn]==targetUser,itemColumn]

usersWhoViewedCurrentPage <- userPreferencesDF[userPreferencesDF[,itemColumn]==currentAbId,userColumn]
itemsViewedByTheseUsers <- userPreferencesDF[userPreferencesDF[,userColumn] %in% usersWhoViewedCurrentPage,
                                             c(itemColumn, userColumn)]

itemViewCount <- aggregate(itemsViewedByTheseUsers[-1], itemsViewedByTheseUsers[1], c)
itemViewCount$Count <- sapply(1: max(row(itemViewCount)), function(item){
                                                    return (length(unlist(itemViewCount[item, 2])))
                                                    })

rankedItemDF <- itemViewCount[order(itemViewCount$Count,decreasing=T),]
rankedItemDF <- rankedItemDF[-1,]

mostRelevantItems <- rankedItemDF[!(rankedItemDF[,1] %in% itemsViewedByTargetUser),][1:numRankedItems,1]

pageViewsOnCurrentAndRelevantItems <- userPreferencesDF[userPreferencesDF[,itemColumn] %in% c(currentAbId,mostRelevantItems),]
tmpMatrix <- t(as(as(as(pageViewsOnCurrentAndRelevantItems,"realRatingMatrix"),"dgCMatrix"),"matrix"))

viewsOnRelevantItemsMX <- cbind(row.names(tmpMatrix),tmpMatrix)
viewsOnRelevantItemsMX <- viewsOnRelevantItemsMX[order(viewsOnRelevantItemsMX[,1]),]
viewsOnRelevantItemsMX <- rbind(viewsOnRelevantItemsMX[viewsOnRelevantItemsMX[,1] == currentAbId,],
                                viewsOnRelevantItemsMX[viewsOnRelevantItemsMX[,1] != currentAbId,])

itemUserViewsMXForDistanceComputation <- matrix(as.numeric(viewsOnRelevantItemsMX[,-1]),
                                                   nrow=numRankedItems+1) 
colnames(itemUserViewsMXForDistanceComputation) <- colnames(viewsOnRelevantItemsMX)[-1]
distanceMX <- dist(itemUserViewsMXForDistanceComputation,
                   method="Manhattan", #TODO Parametrise
                   diag=T,
                   upper=T)

similarItemDistancesForCurrentAbId <- data.frame(Distances=as.matrix(distanceMX)[1,-1])
similarItemDistancesForCurrentAbId$ItemId <- viewsOnRelevantItemsMX[-1,1]

similarItemDistancesForCurrentAbId <- similarItemDistancesForCurrentAbId[
                                      order(similarItemDistancesForCurrentAbId[,1]),]

return(similarItemDistancesForCurrentAbId[,c("ItemId")])
}


setwd("C:/TW-Projects/PS-Projects/AbcamAnalytics/RSandbox/recommender/")
userItemsPreferenceDF <- read.csv(file="FinalWeightsCombined",sep="\t",colClasses=c("character","character","numeric"),header=FALSE)

targetUserId <- "-3685863687683693081"
currentItem <- 61243

recommendations <- getRecommendations(userItemsPreferenceDF, targetUserId, currentItem)
View(recommendations)
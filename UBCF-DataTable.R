library("recommenderlab")
library("data.table")

getRecommendations <- function(userPreferencesDF,     #data frame containing the user-items-weights
                               targetUser,            #user for whom recommendations are sought
                               numRecommendations=10, #Number of recommendations required
                               numRankedUsers=10) {   #Number of similar users to consider
  userColumn <- "user"
  itemColumn <- "itemIDs"
  weightColumn <- "weight"
  
  userPreferencesDT <- data.table(userPreferencesDF)
  
  setnames(userPreferencesDT, c(userColumn,itemColumn,weightColumn))
  setkey(userPreferencesDT, itemIDs)
  
  viewedPages <- userPreferencesDT[user==targetUserID][,itemIDs]
  usersWhoViewedSomeOfThosePages <- userPreferencesDT[itemIDs %in% viewedPages][, list(user,itemIDs)]
  pagesViewsBySimilarUsers <- usersWhoViewedSomeOfThosePages[,length(itemIDs),by=user]
  rankedUsersDF <- pagesViewsBySimilarUsers[order(-V1)][-1,]
  mostRelevantUsers <- rankedUsersDF[1:numRankedUsers,user]
  pageViewDataOfTargetAndSimilarUsers <- userPreferencesDT[user %in% c(targetUserID,mostRelevantUsers),]
  tmpMatrix <- as(as(as(data.frame(pageViewDataOfTargetAndSimilarUsers),"realRatingMatrix"),"dgCMatrix"),"matrix")
  relevantUsersPageViewsMX <- cbind(row.names(tmpMatrix),tmpMatrix)
  relevantUsersPageViewsMX <- relevantUsersPageViewsMX[order(relevantUsersPageViewsMX[,1]),]
  itemsViewedByRelevantUsers <- colnames(relevantUsersPageViewsMX)
  allUniqueAbIDs <- userPreferencesDT[unique(itemIDs)]
  abIDsNotViewedByRelevantUsers <- setdiff(allUniqueAbIDs,itemsViewedByRelevantUsers)
  baseMX <- matrix(nrow=numRankedUsers+1, #Since we will also include target user
                   ncol=length(abIDsNotViewedByRelevantUsers),
                   data=0)  #Initialise
  colnames(baseMX) <- abIDsNotViewedByRelevantUsers
  relevantUsersPrefsForAllItems <- cbind(c(targetUserID,mostRelevantUsers),baseMX)
  relevantUsersPrefsForAllItems <- relevantUsersPrefsForAllItems[order(relevantUsersPrefsForAllItems[,1]),]
  relevantUsersPrefsForAllItems <- cbind(relevantUsersPrefsForAllItems,relevantUsersPageViewsMX[,-1])
  relevantUsersPrefsForAllItems <- rbind(relevantUsersPrefsForAllItems[relevantUsersPrefsForAllItems[,1] == targetUserID,], 
                                         relevantUsersPrefsForAllItems[relevantUsersPrefsForAllItems[,1] != targetUserID,])
  colnames(relevantUsersPrefsForAllItems)[1] <- "userID"
  userPageViewMatrixForDistanceComputation <- matrix(as.numeric(relevantUsersPrefsForAllItems[,-1]),
                                                     nrow=numRankedUsers+1) 
  colnames(userPageViewMatrixForDistanceComputation) <- colnames(relevantUsersPrefsForAllItems)[-1]
  distanceMX <- dist(userPageViewMatrixForDistanceComputation,
                     method="Manhattan", #TODO Parametrise
                     diag=T,
                     upper=T)
  relevantUsersWeights <- as.matrix(distanceMX)[1,-1]
  highestWeight <- min(relevantUsersWeights)
  relevantUsersNormalWeights <- highestWeight/relevantUsersWeights
  relevantUsersNormalWeightsMX <- matrix(relevantUsersNormalWeights,nrow=numRankedUsers)
  weightedPagePreferenceMX <- userPageViewMatrixForDistanceComputation[-1,] * relevantUsersNormalWeightsMX[,1]
  weightedPagePreferenceMX <- rbind(weightedPagePreferenceMX,colSums(weightedPagePreferenceMX))
  pageRanks <- as.data.frame(weightedPagePreferenceMX[(numRankedUsers+1),weightedPagePreferenceMX[(numRankedUsers+1),]>0]) #TODO : parameterise
  pageRanks$AbIDs <- row.names(pageRanks)
  pageRanks <- pageRanks[order(pageRanks[,1],decreasing=TRUE),]
  newPagesToBeRecommended <- setdiff(pageRanks[,"AbIDs"],viewedPages)
  return (newPagesToBeRecommended[1:numRecommendations])
}

setwd("C:/TW-Projects/PS-Projects/AbcamAnalytics/RSandbox/recommender/")
userItemsWeightsDF <- read.csv(file="FinalWeightsCombined",sep="\t",colClasses=c("character","character","numeric"),header=FALSE)
targetUserID <- "-3685863687683693081"
system.time(getRecommendations(userItemsWeightsDF,targetUserID))
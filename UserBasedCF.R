library("recommenderlab")

getRecommendations <- function(userPreferencesDF,     #data frame containing the user-items-weights
                               targetUser,            #user for whom recommendations are sought
                               numRecommendations=10, #Number of recommendations required
                               numRankedUsers=10) {   #Number of similar users to consider
  userColumn <- "user"
  itemColumn <- "itemIDs"
  weightColumn <- "weight"
  
  colnames(userPreferencesDF)<- c(userColumn,itemColumn,weightColumn)
  viewedPages <- userPreferencesDF[which(userPreferencesDF[,userColumn]==targetUserID),itemColumn]
  usersWhoViewedSomeOfThosePages <- unique(userPreferencesDF[userPreferencesDF[,itemColumn] %in% viewedPages,
                                                             c(userColumn,itemColumn)])
  pagesViewsBySimilarUsers <- aggregate(usersWhoViewedSomeOfThosePages[-1], #All the columns except the user column
                                        usersWhoViewedSomeOfThosePages[1],  #Group by user ID
                                        c)                                  #Concatenate all the item IDs per user
  pagesViewsBySimilarUsers$Count <- sapply(1:max(row(pagesViewsBySimilarUsers)), #Do this for all relevant users
                                           function(x){
                                             return (length(unlist(pagesViewsBySimilarUsers[x,2])));
                                           })
  rankedUsersDF <- pagesViewsBySimilarUsers[order(pagesViewsBySimilarUsers$Count,decreasing=T),]
  rankedUsersDF <- rankedUsersDF[-1,]
  mostRelevantUsers <- rankedUsersDF[1:numRankedUsers,1]
  pageViewDataOfTargetAndSimilarUsers <- userPreferencesDF[userPreferencesDF[,1] %in% c(targetUserID,mostRelevantUsers),]
  tmpMatrix <- as(as(as(pageViewDataOfTargetAndSimilarUsers,"realRatingMatrix"),"dgCMatrix"),"matrix")
  relevantUsersPageViewsMX <- cbind(row.names(tmpMatrix),tmpMatrix)
  relevantUsersPageViewsMX <- relevantUsersPageViewsMX[order(relevantUsersPageViewsMX[,1]),]
  itemsViewedByRelevantUsers <- colnames(relevantUsersPageViewsMX)
  allUniqueAbIDs <- unique(userPreferencesDF[,2])
  abIDsNotViewedByRelevantUsers <- setdiff(unique(userPreferencesDF[,2]),itemsViewedByRelevantUsers)
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
getRecommendations(userItemsWeightsDF,targetUserID)
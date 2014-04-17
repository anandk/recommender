require("data.table")

setup <- function(){
  setwd("C:/TW-Projects/PS-Projects/AbcamAnalytics/RSandbox/recommender/")
  userItemsWeightsDT <- fread(input="FinalWeightsCombined",sep="\t",colClasses=c("character","numeric","numeric"),header=FALSE)
  setnames(userItemsWeightsDT, c("user","itemIDs","weight"))
  dtKeyedByItems <- copy(userItemsWeightsDT)
  setkey(userItemsWeightsDT,user)
  setkey(dtKeyedByItems, itemIDs)
  uniqueUsers <- unique(userItemsWeightsDT[,user])
  uniqueItems <- unique(dtKeyedByItems[,itemIDs])
}

getRecommendations <- function(targetUser){
  viewedPages <- userItemsWeightsDT[targetUser][,itemIDs]
  #Get the users who have viewed any page in the viewedPages list.
  #Group by user & compute the number of pages in common with viewedPages
  #Sort this in descending order & pick the top 10 excluding the target user (1st row)
  sortedRelevantUsers <- dtKeyedByItems[list(viewedPages)][,length(itemIDs),by=user][order(-V1),][2:11,]
  allItemsRelevantUsersHaveViewed <- userItemsWeightsDT[sortedRelevantUsers$user]
  recomm <- allItemsRelevantUsersHaveViewed[,sum(weight),by=itemIDs][order(-V1)][1:20,itemIDs]
  return (recomm)
}

system.time(setup())
#Takes 16.3 sec for 1000 users on my system. Hence, for 100,000 we are looking at < 30 min.
system.time(tmpTest <- t(sapply(uniqueUsers[1:10], function(x) {return (getRecommendations(x));})))
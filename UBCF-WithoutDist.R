require("data.table")

userColumn <- "user"
itemColumn <- "itemIDs"
weightColumn <- "weight"
targetUserID <- "-3685863687683693081"


setwd("C:/TW-Projects/PS-Projects/AbcamAnalytics/RSandbox/recommender/")
userItemsWeightsDT <- fread(input="FinalWeightsCombined",sep="\t",colClasses=c("character","numeric","numeric"),header=FALSE)
setnames(userItemsWeightsDT, c(userColumn,itemColumn,weightColumn))
dtKeyedByItems <- copy(userItemsWeightsDT)
setkey(userItemsWeightsDT,user)
setkey(dtKeyedByItems, itemIDs)
uniqueUsers <- unique(userItemsWeightsDT[,user])
uniqueItems <- unique(dtKeyedByItems[,itemIDs])
x1 <- function(targetUser){
viewedPages <- userItemsWeightsDT[targetUser][,itemIDs]
sortedRelevantUsers <- dtKeyedByItems[list(viewedPages)][, list(user,itemIDs)][,length(itemIDs),by=user][order(-V1),][2:11,]
allItemsRelevantUsersHaveViewed <- userItemsWeightsDT[sortedRelevantUsers$user]
recomm <- allItemsRelevantUsersHaveViewed[,sum(weight),by=itemIDs][order(-V1)][1:20,itemIDs]
return (recomm)
}

system.time(tmpTest <- sapply(uniqueUsers[1:10], function(x) {return (x1(x));}))

library("recommenderlab")
setwd("C:/TW-Projects/PS-Projects/AbcamAnalytics/RSandbox/recommender/")
weightsDF <- read.csv(file="FinalWeightsCombined",sep="\t",colClasses=c("character","character","numeric"))
colnames(weightsDF)<- c("user","abIDs","weight")
targetUserID <- "-3685863687683693081"
viewedPages <- weightsDF[which(weightsDF[,1]==targetUserID),2]
targetItem <- viewedPages[1]
usersWhoViewedThisItem <- unique(weightsDF[weightsDF[,2]==targetItem,c(1,3)])
usersWhoValuedThisItemTheMost <- (usersWhoViewedThisItem[order(usersWhoViewedThisItem[,2],decreasing=T),])[1:10,]

allTheItemsTheseUsersViewed <- weightsDF[weightsDF[,1] %in% usersWhoValuedThisItemTheMost[,1],]
rrm <- as(allTheItemsTheseUsersViewed,"realRatingMatrix")
tmpdgCMatrix <- as(rrm,"dgCMatrix")
tmpMatr <- as(tmpdgCMatrix,"matrix")
tmpMatr <- rbind(tmpMatr, colSums(tmpMatr))
sortedMatr <- t(tmpMatr[,order(tmpMatr[11,],decreasing=T)])
coOccuringItems <- sortedMatr[1:45,]
distMatr <- dist(coOccuringItems[,-11],method="Manhattan",diag=T,upper=T)

distances <- as.matrix(distMatr)[1,]
matDistances <- as.matrix(distances)
matDistances <- cbind(row.names(matDistances),matDistances)
similarItems <- (matDistances[order(matDistances[,2]),])[2:11,1]
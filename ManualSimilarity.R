df57x57 <- read.csv("../DenserMatr.csv",colClasses=c("character",rep("numeric",57)))
targetUserIndex <- which(df57x57[,1]=="-3685863687683693081")
numItemsViewedByTargetUser <- length(which(df57x57[targetUserIndex,-1]>0))
indicesOfViewedItemsByTargetUser <- which(df57x57[targetUserIndex,-1]>0)+1
howManyItemsInCommonWithTargetUser <- sapply((1:57)[-targetUserIndex],
                                 function(x) {
                                   return (length(
                                     which(
                                       df57x57[x,indicesOfViewedItemsByTargetUser]>0
                                       )
                                     ));})
meanViews <- mean(howManyItemsInCommonWithTargetUser)
thresholdViews <- ceiling(max(meanViews,numItemsViewedByTargetUser/3))
howManyItemsInCommonWithTargetUser <- c(howManyItemsInCommonWithTargetUser[1:targetUserIndex-1],
                                        -1,
                                        howManyItemsInCommonWithTargetUser[targetUserIndex:length(howManyItemsInCommonWithTargetUser)])
usersWhoAreSimilar <- which(howManyItemsInCommonWithTargetUser>thresholdViews)
similarUsers <- df57x57[c(targetUserIndex,usersWhoAreSimilar),]
distMatr <- as.matrix(dist(similarUsers[-1],method="Manhattan",upper=T,diag=T))
userIndices <- colnames(distMatr)[-1]
recommendationWeights <- 0
for(i in length(userIndices)){
  recommendationWeights <- recommendationWeights + (1/distMatr[1, i])*df57x57[userIndices[i],-1];
}
recommendationWeights <- recommendationWeights * (df57x57[targetUserIndex,-1]==0)
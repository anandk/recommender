library("recommenderlab")
options("scipen"=100, "digits"=4)

setwd("C:/TW-Projects/PS-Projects/AbcamAnalytics/RSandbox/recommender/")
weightsDF <- read.csv(file="FinalWeightsCombined",sep="\t",colClasses=c("character","character","numeric"))
colnames(weightsDF)<- c("user","abIDs","weight")
targetUserID <- "-3685863687683693081"


viewedPages <- weightsDF[which(weightsDF[,1]==targetUserID),2]
usersWhoViewedTheSomeOfThosePages <- unique(weightsDF[weightsDF[,2] %in% viewedPages,1:2])
pagesViewsBySimilarUsers <- aggregate(usersWhoViewedTheSomeOfThosePages[-1],usersWhoViewedTheSomeOfThosePages[1],c)
pagesViewsBySimilarUsers$Count <- sapply(1:max(row(pagesViewsBySimilarUsers)),
                                         function(x){return (length(unlist(pagesViewsBySimilarUsers[x,2])));}
                                        )
sortedDF <- pagesViewsBySimilarUsers[order(pagesViewsBySimilarUsers$Count,decreasing=T),]
sortedDF <- sortedDF[-1,]
mostRelevantUsers <- sortedDF[1:10,1]
meAndRelevantUserViews <- weightsDF[weightsDF[,1] %in% c(targetUserID,mostRelevantUsers),]
rrm <- as(meAndRelevantUserViews,"realRatingMatrix")
tmpdgcMatrix <- as(rrm,"dgCMatrix")
tmpMatr <- cbind(row.names(tmpMatr),as(tmpdgcMatrix,"matrix"))
tmpMatr <- tmpMatr[order(tmpMatr[,1]),]
allUniqueAbIDs <- unique(weightsDF[,2])
abIDsNotViewedByRelevantUsers <- setdiff(unique(weightsDF[,2]),colnames(tmpMatr))
distMatr <- matrix(nrow=11,ncol=length(abIDsNotViewedByRelevantUsers),data=0)
colnames(distMatr) <- abIDsNotViewedByRelevantUsers
distMatr <- cbind(c(targetUserID,mostRelevantUsers),distMatr)
distMatr <- distMatr[order(distMatr[,1]),]
distMatr <- cbind(distMatr,tmpMatr[,-1])
distMatr <- rbind(distMatr[distMatr[,1] == targetUserID,], distMatr[distMatr[,1]!=targetUserID,])
colnames(distMatr)[1] <- "userID"

numericDistMatr <- matrix(as.numeric(distMatr[,-1]),nrow=11)
colnames(numericDistMatr) <- colnames(distMatr)[-1]
distanceMatr <- dist(numericDistMatr,method="Manhattan",diag=T,upper=T)

userRelevanceWeights <- as.matrix(distanceMatr)[1,-1]
highestWeight <- min(userRelevanceWeights)
userRelevanceWeights <- highestWeight/userRelevanceWeights
weightsMatrix <- matrix(userRelevanceWeights,nrow=10)
weightedRecom <- numericDistMatr[-1,] * weightsMatrix[,1]
weightedRecom <- rbind(weightedRecom,colSums(weightedRecom))
nonZeroCols <- weightedRecom[,weightedRecom[11,]>0]


df2 <- as.data.frame(cbind(countLevelOfSimilarity[,1],as.numeric(countLevelOfSimilarity[,2])))
df2[,2]<- as.numeric(levels(df2[,2]))[as.integer(df2[,2])]
df2[,1] <- as.character(df2[,1])
df2[df2[1]==targetUserID,2] <- 0
sortedDF2 <- df2[order(df2[,2],decreasing=T),]
similarUsers <- sortedDF2[1:10,1]
distMatr <- matrix(nrow=11,ncol=1,data=0)
colnames(distMatr) <- unique(weightsDF[,2])
df33 <- data.frame()
allPagesViewedBySelectedUsers <- sapply(c(targetUserID,similarUsers),
                                        function(x){
                                        df33 <<- rbind(df33,weightsDF[weightsDF[,1]==x,]);
                                        })



#levelOfInterest <- sapply(usersWhoViewedTheSomeOfThosePages[,1],function(x){return (intersect(
#  viewedPages,
#  usersWhoViewedTheSomeOfThosePages[usersWhoViewedTheSomeOfThosePages[,1]==x,2]));})
#matrLevelOfInterest <- as.matrix(levelOfInterest)
#dfLevelOfSimilarity <- unique(as.data.frame(matrLevelOfInterest))
#countLevelOfSimilarity <- t(sapply(1:length(unique(matrLevelOfInterest[,1])),function(x){return (
#  unlist(
#    (c(row.names(matrLevelOfInterest)[x],
#       length(unlist(matrLevelOfInterest[x]))
#    )
#    )
#  )
#);}))

library("recommenderlab")

setwd("C:/TW-Projects/PS-Projects/AbcamAnalytics/RSandbox/recommender")

df <- read.csv(file="FinalWeightsCombined",sep="\t",
               colClasses=c("character","character","numeric"))
colnames(df)<- c("user","abIDs","weight")
userItemMatrix <- as(df,"realRatingMatrix")
rec <- Recommender(userItemMatrix[1:1400], method="UBCF")
finalMatr <- matrix(nrow=1,ncol=11)
recom <- predict(rec,userItemMatrix[1401],n=3)
for(index in 1:dim(userItemMatrix)[1])
{
  userID <- names(as(userItemMatrix[index],"list"))
  recom <- predict(rec,userItemMatrix[userIndex],n=10)
  finalMatr <- rbind(finalMatr, c(userID,unlist(as(recom,"list"))))
  if(index%%100 == 0)
    {
      write.table(finalMatr[-1,], file="./Output.txt", sep=",", append=TRUE, row.names=FALSE,col.names=FALSE)
      finalMatr <- matrix(nrow=1,ncol=11)
    }  
}
write.table(finalMatr[-1,], file="./Output.txt", sep=",", append=TRUE, row.names=FALSE,col.names=FALSE)


#userIndex <- 407983
#as(recom,"list")
userList <- as(userItemMatrix,"list")
dfUser <- as(userList,"data.frame")
userIndex <- which(colnames(dfUser)=="8549280749670823302")

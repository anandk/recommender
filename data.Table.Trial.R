require("data.table")
dt1 <- data.table(User=c("c", "e", "a", "e", "b", "e", "c", "e", "d", "e", "c", "c", "b", "e", "d", "b", "e", "d", "a", "b", "d", "a", "e"),
                  Page=c("P2","P1","P3","P2","P1","P3","P4","P4","P1","P5","P5","P7","P3","P6","P3","P5","P7","P4","P5","P6","P7","P7","P8"),
                  Weight=c(1,    1,   1,   1,   1,   1,   1,   1,   1,   1,   2,   2,   1,   1,   3,   3,   1,   1,   2,   1,   2,   3,   1))
setkey(dt1,Page)
targetUser = "a"
viewedPages <- unique(dt1[User==targetUser][,Page])
sortedRelevantUsers <- dt1[viewedPages][,length(Page),by=User][1:4,]
d<-dt1[User %in% sortedRelevantUsers$User]
m<-as.data.table(merge(unique(d$User),unique(d$Page)))
setnames(m,c("User","Page"))
setkey(m,User,Page)
setkey(d,User,Page)
d[m]

userColumn <- "user"
itemColumn <- "itemIDs"
weightColumn <- "weight"
setwd("C:/TW-Projects/PS-Projects/AbcamAnalytics/RSandbox/recommender/")
userItemsWeightsDF <- read.csv(file="FinalWeightsCombined",sep="\t",colClasses=c("character","character","numeric"),header=FALSE)
targetUserID <- "-3685863687683693081"
userItemsWeightsDT <- data.table(userItemsWeightsDF)
setnames(userItemsWeightsDT, c(userColumn,itemColumn,weightColumn))
setkey(userItemsWeightsDT, itemIDs)
system.time(viewedPages <- userItemsWeightsDT[user==targetUserID][,itemIDs])
setkey(userItemsWeightsDT,user)
system.time(sortedRelevantUsers <- userItemsWeightsDT[viewedPages][, length(weight),by=user][1:11,])
system.time(d12 <- userItemsWeightsDT[user %in% sortedRelevantUsers$user])
system.time(d23 <- d12[,sum(weight),by=itemIDs])
system.time(d23[order(-V1)])

#lapply for dist computation
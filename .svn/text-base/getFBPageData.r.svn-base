library(RCurl)
library(rjson)
library(xts)
# library(RMySQL)
#library(tm)
#library(RTextTools)
library(snow)
library(sqldf)

source('getFBPageData_funs.r')


datestart = Sys.Date()-30 

dateend = Sys.Date()
#get FB users and tokens	
#con = dbConnect(MySQL(), user='root', password='2eSt4SSPanH8', dbname='permissionsDB', host='localhost', port=3306)
#query = paste("Select  * FROM userToken")
#res = dbGetQuery(con, query)
#dbDisconnect(con)
infoData = array(0, c(0,10))
infoPostData = array(0, c(0,7))
colnames(infoData) = c("about", "category", "pageid", "likes", "name", "picture", "talking_about_count", "username", "rawid","owner")
colnames(infoPostData) = c("message",  "created_time", "postid", "likes", "comments","type", "message_wc")

# 	IDs = read.csv('BenchmarkFacebookPageIDs.csv')
#   
#   StoneGateIDs = read.csv('Stonegate_facebook_pages_16.05.12.csv', nrow=323)
#   StoneGateIDs = cbind(StoneGateIDs, id=unlist(lapply(strsplit(split="/", x=StoneGateIDs[,8]), last)))

IDs = read.csv('Stonegate_facebook_pages_16.05.12.csv', nrow=925)

#make a socket cluster to speed up the data pull
cl = makeCluster(10,type="SOCK")
#     res = lapply(X=IDs[,"id"], FUN=getData)
clusterExport(cl, c("getData", "getDataTry", "prepareSlave"))
clusterCall(cl, prepareSlave)
res = clusterApplyLB(cl=cl, x=as.list(IDs[,"id"]), fun=getDataTry)
stopCluster(cl=cl)
 
for(r in res){
  if(class(r)=="list"){
    if(!is.null(r$a)){
      infoData = rbind(infoData, c(r$b, r$rawid))
      infoPostData = rbind(infoPostData, r$a)
    }
  }
}


infoData[is.na(infoData[,"likes"]),"likes"] = 0
infoData[is.na(infoData[,"talking_about_count"]),"talking_about_count"] = 0		


#include more information about the pages 
pageids = unlist(strsplit(infoPostData[,"postid"], "_"))[seq(1, nrow(infoPostData)*2, by=2)]

pageLikes = rep(0, length(pageids))
pageTalkingAbout = rep(0, length(pageids))
pagePostsNum = rep(0, length(pageids))
postTab = table(pageids)
for(i in 1:length(pageids)){
  res1 = infoData[which(infoData[,"pageid"]==pageids[i]), "likes"]
  if(length(res1)>0){
    pageLikes[i] = infoData[which(infoData[,"pageid"]==pageids[i]), "likes"]
    pageTalkingAbout[i] = infoData[which(infoData[,"pageid"]==pageids[i]), "talking_about_count"]
    pagePostsNum[i] = postTab[pageids[i]]
    
  }
}
infoPostDataMod = cbind(infoPostData, pageid=pageids, pageLikes=pageLikes, pageTalkingAbout=pageTalkingAbout, 
                        likes10 = ifelse(infoPostData[,"likes"]>0, 1, 0), hour=format(as.POSIXct(infoPostData[,"created_time"], tz="GMT", format="%Y-%m-%dT%H"), "%H" ), dow=format(as.POSIXct(infoPostData[,"created_time"], tz="GMT", format="%Y-%m-%dT%H"), "%a" ),
                        message_wc2 = as.numeric(infoPostData[,"message_wc"])^2)

#combine the datasets
infoData = as.data.frame(infoData)
infoPostDataMod = as.data.frame(infoPostDataMod)
superInfoData = sqldf("SELECT * FROM infoData INNER JOIN IDs on IDs.id=infoData.rawid")
superPostInfoDataMod = sqldf("SELECT * FROM superInfoData INNER JOIN infoPostDataMod on infoPostDataMod.pageid=superInfoData.pageid")


#browser()

#print out certain analysis
cat('***************\n')
cat('aggregate', '\n')
cat('***************\n')
agg = printAnalysis(infoData, infoPostData, infoPostDataMod)

cat('***************\n')
cat('Stonegate ', '\n')
cat('***************\n')
IDsstone = sqldf("select * from IDs where Owner==' StoneGate' ")
infoDataStone = sqldf("select * from infoData INNER JOIN IDsstone on IDsstone.id=infoData.rawid")
infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")
stonegate = printAnalysis(infoDataStone, infoPostData, infoPostDataModStone)

cat('***************\n')
cat('Other ', '\n')
cat('***************\n')
IDsstone = sqldf("select * from IDs where Owner='Other' ")
infoDataStone = sqldf("select * from infoData INNER JOIN IDsstone on IDsstone.id=infoData.rawid")
infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")
Other = printAnalysis(infoDataStone, infoPostData, infoPostDataModStone)

cat('***************\n')
cat('Barracuda ', '\n')
cat('***************\n')
IDsstone = sqldf("select * from IDs where Owner='Barracuda' ")
infoDataStone = sqldf("select * from infoData INNER JOIN IDsstone on IDsstone.id=infoData.rawid")
infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")
Barracuda = printAnalysis(infoDataStone, infoPostData, infoPostDataModStone)

cat('***************\n')
cat('Brain ', '\n')
cat('***************\n')
IDsstone = sqldf("select * from IDs where Owner='Brain' ")
infoDataStone = sqldf("select * from infoData INNER JOIN IDsstone on IDsstone.id=infoData.rawid")
infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")
Brain = printAnalysis(infoDataStone, infoPostData, infoPostDataModStone)

# cat('***************\n')
# cat('HW ', '\n')
# cat('***************\n')
# IDsstone = sqldf("select * from IDs where Owner='HW' ")
# infoDataStone = sqldf("select * from infoData INNER JOIN IDsstone on IDsstone.id=infoData.rawid")
# infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")
# HW = printAnalysis(infoDataStone, infoPostData, infoPostDataModStone)

cat('***************\n')
cat('LaTasca ', '\n')
cat('***************\n')
IDsstone = sqldf("select * from IDs where Owner='LaTasca' ")
infoDataStone = sqldf("select * from infoData  INNER JOIN IDsstone on IDsstone.id=infoData.rawid ")
infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")
LaTascaAll = printAnalysis(infoDataStone, infoPostData, infoPostDataModStone)
infoDataStone = infoDataStone[-which(is.na(infoDataStone[,"category"])),]
infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")
LaTascaNoPers = printAnalysis(infoDataStone, infoPostData, infoPostDataModStone)

IDsstone = sqldf("select * from IDs where Owner='LaTascaCorporate' ")
infoDataStone = sqldf("select * from infoData INNER JOIN IDsstone on IDsstone.id=infoData.rawid")
infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")
LaTascaCorporate = printAnalysis(infoDataStone, infoPostData, infoPostDataModStone)

agganalysis=array(0, c(9, length(agg)))
colnames(agganalysis) = names(agg)
rownames(agganalysis) = c("Aggregate", "Stonegate", "LaTascaAll", "LaTascaNoPers","LaTascaCorporate", "Barracuda","Brain","HW", "Other")
agganalysis["Aggregate",] = agg
agganalysis["Stonegate", names(stonegate)] = stonegate
agganalysis["LaTascaAll", names(LaTascaAll)] = LaTascaAll
agganalysis["LaTascaNoPers", names(LaTascaNoPers)] = LaTascaNoPers
agganalysis["LaTascaCorporate", names(LaTascaCorporate)] = LaTascaCorporate
agganalysis["Barracuda", names(Barracuda)] = Barracuda
agganalysis["Brain", names(Brain)] = Brain
agganalysis["HW", names(HW)] = HW
agganalysis["Other", names(Other)] = Other

IDsstone = sqldf("select * from IDs where Owner==' StoneGate' ")
infoDataStone = sqldf("select * from infoData INNER JOIN IDsstone on IDsstone.id=infoData.rawid")
infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")

stonegateSegment = array(0, c(length(unique(na.omit(infoDataStone[,c("pageid", "Segment")])[,"Segment"])), ncol(agganalysis)))
colnames(stonegateSegment) = colnames(agganalysis)
rownames(stonegateSegment) = unique(na.omit(infoDataStone[,c("pageid", "Segment")])[,"Segment"])
for(ca in unique(na.omit(infoDataStone[,c("pageid", "Segment")])[,"Segment"])){
  cat('***************\n')
  cat('category ', ca, '\n')
  cat('***************\n')
  IDsstone = sqldf(paste("select * from IDs where Owner==' StoneGate' AND Segment=='",ca,"'",sep=""))
  infoDataStone = sqldf("select * from infoData INNER JOIN IDsstone on IDsstone.id=infoData.rawid")
  infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")
  
#   if(!is.null(dim(infoData[which(infoDataStone[,"Segment"]==ca), ]))){
#     infoDataSub = infoData[which(infoDataStone[,"Segment"]==ca), ]
#     infoPostDataModSub = infoPostDataModStone[which(infoPostDataModStone[,"pageid"]%in%infoDataSub[,"pageid"]), ]
    cata = try(printAnalysis(infoDataStone,infoPostData,infoPostDataModStone ))
    if(class(cata)!="try-error"){
      stonegateSegment[ca, names(cata)] = cata
    }
#   }
}
IDsstone = sqldf("select * from IDs where Owner=='Barracuda' ")
infoDataStone = sqldf("select * from infoData INNER JOIN IDsstone on IDsstone.id=infoData.rawid")
infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")

barracudaManager = array(0, c(length(unique(na.omit(infoDataStone[,c("pageid", "Manager")])[,"Manager"])), ncol(agganalysis)))
colnames(barracudaManager) = colnames(agganalysis)
rownames(barracudaManager) = unique(na.omit(infoDataStone[,c("pageid", "Manager")])[,"Manager"])
for(ca in unique(na.omit(infoDataStone[,c("pageid", "Manager")])[,"Manager"])){
  cat('***************\n')
  cat('manager ', ca, '\n')
  cat('***************\n')
  IDsstone = sqldf(paste("select * from IDs where Owner=='Barracuda' AND Manager=='",ca,"'",sep=""))
  infoDataStone = sqldf("select * from infoData INNER JOIN IDsstone on IDsstone.id=infoData.rawid")
  infoPostDataModStone = sqldf("select * from infoPostDataMod INNER JOIN infoDataStone on infoDataStone.pageid=infoPostDataMod.pageid")
  
  #   if(!is.null(dim(infoData[which(infoDataStone[,"Segment"]==ca), ]))){
  #     infoDataSub = infoData[which(infoDataStone[,"Segment"]==ca), ]
  #     infoPostDataModSub = infoPostDataModStone[which(infoPostDataModStone[,"pageid"]%in%infoDataSub[,"pageid"]), ]
  cata = try(printAnalysis(infoDataStone,infoPostData,infoPostDataModStone ))
  if(class(cata)!="try-error"){
    barracudaManager[ca, names(cata)] = cata
  }
  #   }
}

# for(c in unique(na.omit(infoDataStone[,c("id", "Add4")])[,"Add4"])){
#   cat('***************\n')
#   cat('category ', c, '\n')
#   cat('***************\n')
#   if(!is.null(dim(infoData[which(infoDataStone[,"Add4"]==c), ]))){
#     infoDataSub = infoData[which(infoDataStone[,"Add4"]==c), ]
#     infoPostDataModSub = infoPostDataModStone[which(infoPostDataModStone[,"pageid"]%in%infoDataSub[,"id"]), ]
#     try(printAnalysis(infoDataSub,infoPostData,infoPostDataModSub ))
#   }
# }
# 
# formula="as.numeric(likes10)~as.numeric(message_wc)+as.numeric(pageLikes)+as.numeric(pageTalkingAbout)+as.numeric(message_wc2)+as.factor(dow)+as.factor(hour)+as.numeric(pagePostsNum)+1"





# #summary(lm(formula=formula, data=as.data.frame(infoPostDataMod)))
# summary(glm(formula=formula, family=binomial(link="logit"), data=as.data.frame(infoPostDataMod)))
# 
# #summary(lm(formula=as.numeric(likes10)~as.numeric(message_wc)+as.numeric(pageLikes)+as.numeric(pageTalkingAbout)
# #	+as.numeric(message_wc2)+as.factor(dow)+as.factor(hour)+1, data=as.data.frame(infoPostDataMod)))
# 
# mse = rep(0, 10)
# restable = array(0, c(2,2))
# for(i in 1:10){
#   ind = sample(1:nrow(infoPostDataMod), round(.9*nrow(infoPostDataMod)))
#   dataTrain = infoPostDataMod[ind, ]
#   dataTest = infoPostDataMod[-ind,]
#   mod = lm(formula=formula, data=as.data.frame(infoPostDataMod))
#   #mod = glm(formula=formula, family=binomial(link="logit"), data=as.data.frame(infoPostDataMod))
#   res = predict(mod, as.data.frame(dataTest))
#   restable = restable+table(res10=res10, likes10=dataTest[,"likes10"])
#   res10 =ifelse(res>meanTrain, 1, 0)
#   meanTrain = mean(predict(mod, as.data.frame(dataTrain)))	
#   mse[i] = mean(abs(ifelse(res>meanTrain, 1, 0)-as.numeric(dataTest[,"likes"]))) 
#   
# }
# print(restable)
# save.image("FBPageData.RData")
# 
# #try another model approach...
# library(RTextTools)
# library(ROCR)	
# infoPostDataMod_train = infoPostDataMod[sample(1:nrow(infoPostDataMod), round(1.0*nrow(infoPostDataMod)), replace=F, 
#                                                prob=ifelse(infoPostDataMod[,"likes10"]==1, 1-mean(as.numeric(infoPostDataMod[,"likes10"])), mean(as.numeric(infoPostDataMod[,"likes10"])))), ] 
# 
# matrix = create_matrix(infoPostDataMod_train[,"message"], language="english",stemWords=T, toLower=T, removePunctuation=T, minDocFreq=2)
# corpus <- create_corpus(matrix,t(infoPostDataMod_train[,"likes10"]),
#                         trainSize=1:round(.9*nrow(infoPostDataMod_train)), 
#                         testSize=(round(.9*nrow(infoPostDataMod_train))+1):nrow(infoPostDataMod_train),virgin=FALSE)
# models <- train_models(corpus, algorithms=c("SVM","SLDA","RF"))
# results <- classify_models(corpus, models)
# analytics <- create_analytics(corpus, results)





#analysis for Bournemouth trials
sitesRes = as.data.frame(resFB[[1]]$sites)
IDs = as.data.frame(IDs)
IDsstone = sqldf("select * from IDs where Owner==' StoneGate' ")
infoDataStone = sqldf("select * from infoData INNER JOIN IDsstone on IDsstone.id=infoData.rawid")
sitesInRes = sqldf("select * from sitesRes inner join infoDataStone on infoDataStone.pageid=sitesRes.id")
imps = array(0, c(nrow(resFB[[1]]$day[[1]]), nrow(sitesInRes)))
colnames(imps) = sitesInRes[,"Site_Name"]

impslog = imps
impslogu = imps
write.csv(sitesInRes, "StoneGateTrialSitesData.csv")
for(i in 1:nrow(sitesInRes)){imps[,i] = resFB[[1]]$day[[sitesInRes[i,"nameid"]]][,"page_views"]}
for(i in 1:nrow(sitesInRes)){impslog[,i] = resFB[[1]]$day[[sitesInRes[i,"nameid"]]][,"page_views_login"]}
for(i in 1:nrow(sitesInRes)){impslogu[,i] = resFB[[1]]$day[[sitesInRes[i,"nameid"]]][,"page_views_login_unique"]}
imps = cbind(imps, sumDay=rowSums(imps))
impslog = cbind(impslog, sumDay=rowSums(impslog))
impslogu = cbind(impslogu, sumDay=rowSums(impslogu))
imps = cbind(imps, avgDay=rowSums(imps)/ncol(imps))
impslog = cbind(impslog, avgDay=rowSums(impslog)/ncol(impslog))
impslogu = cbind(impslogu, avgDay=rowSums(impslogu)/ncol(impslogu))
write.csv(xts(imps, order.by=seq.Date(from=Sys.Date()-nrow(imps)+1, to=Sys.Date(), by="day")), "impsByDayStonegateInsights.csv")
write.csv(xts(impslog, order.by=seq.Date(from=Sys.Date()-nrow(imps)+1, to=Sys.Date(), by="day")), "impsLoggedInByDayStonegateInsights.csv")
write.csv(xts(impslogu, order.by=seq.Date(from=Sys.Date()-nrow(imps)+1, to=Sys.Date(), by="day")), "impsLoggedInUniqueByDayStonegateInsights.csv")


#daily impressions agged by Segment
segs = unique(na.omit(sitesInRes[,c("pageid", "Segment")])[,"Segment"])
impsSeg = array(0, c(nrow(imps), length(segs)))
colnames(impsSeg) = segs
impsSeglog = impsSeg
impsSeglogu = impsSeg
pdf('ImpressionsByDayBySeg.pdf')
for(seg in segs){
  impsSeg[,seg] = apply(imps[,which(sitesInRes[,"Segment"]==seg)], 1, mean)
  impsSeglog[,seg] = apply(impslog[,which(sitesInRes[,"Segment"]==seg)], 1, mean)
  impsSeglogu[,seg] = apply(impslogu[,which(sitesInRes[,"Segment"]==seg)], 1, mean)
   ind = switch(seg,
         "Yates"=6,
         "Slug and Lettuce"=3,
         "Scream"=7,       
         "Great Trad Pubs"=5,
               "Classic Inns"=8,
               "Late Night Bars"=2,
               "Local Pubs"=NA,
               "Missoula & Unbranded"=NA
         )
  print(seg)
  print(ind)
  plot(xts(impsSeg[,seg], order.by=seq.Date(from=Sys.Date()-nrow(imps)+1, to=Sys.Date(), by="day")), main=paste('Average Impressions by day for ',seg), ylim=c(0, 100))
  if(!is.na(ind)){
    lines(xts(imps[,ind], order.by=seq.Date(from=Sys.Date()-nrow(imps)+1, to=Sys.Date(), by="day")), col="red", lwd=2)
    legend("topright", legend=c("All", "Bournemouth"), lwd=c(1,2), col=c("black", "red"))
  }
  
}
dev.off()

write.csv(xts(impsSeg, order.by=seq.Date(from=Sys.Date()-nrow(imps)+1, to=Sys.Date(), by="day")), "impsByDayBySegInsights.csv")
write.csv(xts(impsSeglog, order.by=seq.Date(from=Sys.Date()-nrow(imps)+1, to=Sys.Date(), by="day")), "impsLoggedInByDayBySegInsights.csv")
write.csv(xts(impsSeglogu, order.by=seq.Date(from=Sys.Date()-nrow(imps)+1, to=Sys.Date(), by="day")), "impsLoggedInUniqueBySegInsights.csv")

pdf('AvgImpressionsByDayBournemouthVsAll.pdf')
plot(xts(apply(imps[, which(sitesInRes[,"Add3"]!="Bournemouth")], 1,mean), order.by=seq.Date(from=Sys.Date()-nrow(imps)+1, to=Sys.Date(), by="day")), main="Avg. Impressions by Day Bournemouth v. All", ylim=c(0, 100))
lines(xts(apply(imps[, which(sitesInRes[,"Add3"]=="Bournemouth")], 1,mean), order.by=seq.Date(from=Sys.Date()-nrow(imps)+1, to=Sys.Date(), by="day")), col="red", lwd=2)
legend("topright", legend=c("All", "Bournemouth"), lwd=c(1,2), col=c("black", "red"))
dev.off()
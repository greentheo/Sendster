printAnalysis <- function (infoData, infoPostData, infoPostDataMod) {
  summaryvec = c()
  pageids = infoPostDataMod[,"pageid"]
  summaryvec = c(summaryvec, nrow(na.omit(infoData[,c("pageid","likes")])))
  summaryvec = c(summaryvec, nrow(infoPostDataMod))
  summaryvec = c(summaryvec, mean(na.omit(as.numeric(na.omit(infoData[,c("pageid", "likes")])[,"likes"]))))
  summaryvec = c(summaryvec, median(na.omit(as.numeric(na.omit(infoData[,c("pageid", "likes")])[,"likes"]))))
  summaryvec = c(summaryvec, mean(na.omit(as.numeric(na.omit(infoData[,c("pageid","talking_about_count")])[,"talking_about_count"]))))
  summaryvec = c(summaryvec, median(na.omit(as.numeric(na.omit(infoData[,c("pageid","talking_about_count")])[,"talking_about_count"]))))
  summaryvec = c(summaryvec, summary(na.omit(as.numeric(na.omit(infoData[,c("pageid", "likes")])[,"likes"]))))
  summaryvec = c(summaryvec, summary(na.omit(as.numeric(na.omit(infoData[,c("pageid","talking_about_count")])[,"talking_about_count"]))))
  summaryvec = c(summaryvec, length(unique(pageids)))
  summaryvec = c(summaryvec, summary(as.factor(infoPostDataMod[,"type"]))/nrow(infoPostDataMod))
  summaryvec = c(summaryvec, mean(as.numeric(table(pageids))))
  summaryvec = c(summaryvec, mean(na.omit(as.numeric(na.omit(infoPostDataMod[,c("pageid", "likes")])[,"likes"]))))
  summaryvec = c(summaryvec, median(na.omit(as.numeric(na.omit(infoPostDataMod[,c("pageid", "likes")])[,"likes"]))))
  summaryvec = c(summaryvec, mean(na.omit(as.numeric(na.omit(infoPostDataMod[,c("pageid", "comments")])[,"comments"]))))
  summaryvec = c(summaryvec, median(na.omit(as.numeric(na.omit(infoPostDataMod[,c("pageid", "comments")])[,"comments"]))))
  summaryvec = c(summaryvec, aggregate(x=as.numeric(infoPostDataMod[,"likes"]), list(type=as.factor(infoPostDataMod[,"type"])), FUN=sum)[,"x"]/table(as.factor(infoPostDataMod[,"type"])))
  summaryvec = c(summaryvec, aggregate(x=as.numeric(infoPostDataMod[,"comments"]), list(type=as.factor(infoPostDataMod[,"type"])), FUN=sum)[,"x"]/table(as.factor(infoPostDataMod[,"type"])))
  summaryvec = c(summaryvec, aggregate(x=as.numeric(infoPostDataMod[,"likes"]), list(type=as.factor(infoPostDataMod[,"dow"])), FUN=sum)[,"x"]/table(as.factor(infoPostDataMod[,"dow"])))
  summaryvec = c(summaryvec, aggregate(x=as.numeric(infoPostDataMod[,"likes"]), list(type=as.factor(infoPostDataMod[,"hour"])), FUN=sum)[,"x"]/table(as.factor(infoPostDataMod[,"hour"])))
  summaryvec[is.na(summaryvec)] = 0
  summaryvec[is.nan(summaryvec)] = 0
  names(summaryvec) = c(
                        "SampSize", "numPosts", "meanPageLikes", "medianPageLikes", "meanTalkingAbout", "medianTalkingAbout", 
                        paste(names(summary(na.omit(as.numeric(na.omit(infoData[,c("pageid", "likes")])[,"likes"])))),".pageLikes",sep=""),
                        paste(names(summary(na.omit(as.numeric(na.omit(infoData[,c("pageid", "talking_about_count")])[,"talking_about_count"])))),".tabc",sep=""),
                        "uniquePagesPosts", paste(names(summary(as.factor(infoPostDataMod[,"type"]))/nrow(infoPostDataMod)), ".prcntPost", sep=""), 
                        "meanPostCountperPage", "meanPostLikes","medianPostLikes","meanPostCmnts","medianPostCmnts", paste(names(summary(as.factor(infoPostDataMod[,"type"]))/nrow(infoPostDataMod)), ".likeRatePost", sep=""),
                        paste(names(summary(as.factor(infoPostDataMod[,"type"]))/nrow(infoPostDataMod)), ".commentRatePost", sep=""),
                        paste(unique(infoPostDataMod[,"dow"]), ".likeRateDOW",sep=''),
                        paste(unique(infoPostDataMod[,"hour"]), ".likeRateHour",sep='')
                          )
#   #print out a number of stats that are useful to know
#   cat('Sample Size of ', nrow(na.omit(infoData[,c("id","likes")])), ' pages and ', nrow(infoPostDataMod), 'posts \n')
#   cat('Pages **** \n')
# #   browser()
#   cat('likes mean ',mean(na.omit(as.numeric(na.omit(infoData[,c("id", "likes")])[,"likes"])))) )
#   cat(' median ', median(na.omit(as.numeric(na.omit(infoData[,c("id", "likes")])[,"likes"]))), '\n')
#   cat('talking about count mean ', mean(na.omit(as.numeric(na.omit(infoData[,c("id","talking_about_count")])[,"talking_about_count"]))))
#   cat(' median ',median(na.omit(as.numeric(na.omit(infoData[,c("id","talking_about_count")])[,"talking_about_count"]))), '\n')
#   cat('Posts **** \n')
#   cat(nrow(infoPostDataMod), ' posts representing ', length(unique(pageids)), ' pages \n')
#   print(summary(as.factor(infoPostDataMod[,"type"]))/nrow(infoPostDataMod))
#   cat('posting frequency')
#   print(mean(as.numeric(table(pageids))))
#   cat('likes mean ', mean(na.omit(as.numeric(na.omit(infoPostDataMod[,c("pageid", "likes")])[,"likes"]))))
#   cat(' likes median ', median(na.omit(as.numeric(na.omit(infoPostDataMod[,c("pageid", "likes")])[,"likes"]))), "\n")  
#   cat('comments mean ', mean(na.omit(as.numeric(na.omit(infoPostDataMod[,c("pageid", "comments")])[,"comments"]))))
#   cat(' comments median ', median(na.omit(as.numeric(na.omit(infoPostDataMod[,c("pageid", "comments")])[,"comments"]))), "\n")
# #   cat('LM likes and comments v. PageLikes')
# #   print(summary(lm('as.numeric(likes)~as.numeric(pageLikes)+0', data=as.data.frame(infoPostDataMod))))
# #   print(summary(lm('as.numeric(comments)~as.numeric(pageLikes)+0', data=as.data.frame(infoPostDataMod))))
# #   cat('LM numposts vs. likes an comments')
# #   agginfo = aggregate(x=cbind(likes=as.numeric(infoPostDataMod[,c("likes")]), comments=as.numeric(infoPostDataMod[,c("comments")])), by=list(pageid=pageids), FUN=sum)
# #   postinfo = aggregate(x=rep(1, length(pageids)), by=list(pageid=pageids), FUN=sum)
# #   aggpostinfo = cbind(agginfo, postinfo)
# #   print(summary(lm('likes~as.numeric(x)+0', data=as.data.frame(aggpostinfo)))$coefficients)
# #   print(summary(lm('comments~as.numeric(x)+0', data=as.data.frame(aggpostinfo)))$coefficients)
# #   cat('performance by type')
# #   print(summary(lm('as.numeric(likes)~as.factor(type)+0', data=as.data.frame(infoPostDataMod))))
# #   print(summary(lm('as.numeric(comments)~as.factor(type)+0', data=as.data.frame(infoPostDataMod))))
# #   print(aggregate(x=as.numeric(infoPostDataMod[,"likes"]), list(type=as.factor(infoPostDataMod[,"type"])), FUN=sum)[,"x"]/table(as.factor(infoPostDataMod[,"type"])))
# #   print(aggregate(x=as.numeric(infoPostDataMod[,"comments"]), list(type=as.factor(infoPostDataMod[,"type"])), FUN=sum)[,"x"]/table(as.factor(infoPostDataMod[,"type"])))
# #   print(sort(aggregate(x=as.numeric(infoPostDataMod[,"likes"]), list(type=as.factor(infoPostDataMod[,"dow"])), FUN=sum)[,"x"]/table(as.factor(infoPostDataMod[,"dow"]))))
# #   print(sort(aggregate(x=as.numeric(infoPostDataMod[,"comments"]), list(type=as.factor(infoPostDataMod[,"dow"])), FUN=sum)[,"x"]/table(as.factor(infoPostDataMod[,"dow"]))))
# #   print(sort(aggregate(x=as.numeric(infoPostDataMod[,"likes"]), list(type=as.factor(infoPostDataMod[,"hour"])), FUN=sum)[,"x"]/table(as.factor(infoPostDataMod[,"hour"]))))
# #   print(sort(aggregate(x=as.numeric(infoPostDataMod[,"comments"]), list(type=as.factor(infoPostDataMod[,"hour"])), FUN=sum)[,"x"]/table(as.factor(infoPostDataMod[,"hour"]))))
  
  return(summaryvec)
}



prepareSlave = function(){
  library(RCurl)
  library(rjson)
}
getAccounts = function(getID, atoken){
  cat('getting accounts for',as.character(getID), " ")
  url = paste('https://graph.facebook.com/', getID, '/accounts?access_token=', atoken,sep='')
  a = try(fromJSON(getURL(url)))
  return(a)
  
  
}
getDataTry = function(getID){
  res = try(getData(getID))
  if(class(res)!="try-error"){
    return(res)
  }else{
    return("try-error")
  }
}
getData <- function (getID) {
  
  cat('getting ',as.character(getID), " ")
  url = paste('https://graph.facebook.com/', getID, '/posts?access_token=AAACEdEose0cBACRmGoZCpxL8FUDRSTaynFNiVrjAnuZByCoWbNBFYV8fH0HyIhI6kmcUZC1BPMNSA3KIESQ7OZBdpBhKdv0ZD',sep='')
  urlpage = paste('https://graph.facebook.com/', getID, '/?access_token=AAACEdEose0cBACRmGoZCpxL8FUDRSTaynFNiVrjAnuZByCoWbNBFYV8fH0HyIhI6kmcUZC1BPMNSA3KIESQ7OZBdpBhKdv0ZD',sep='')
  a = try(fromJSON(getURL(url)))
  b = try(fromJSON(getURL(urlpage)))
  
  infoDataTempArray = c();
  #extract the most useful data
  if(class(a)!="try-error" & length(a$data)>0){
    cat('getting post information ')
    infoDataTempArray = c();
    cnames = c("message","created_time", "id", "likes", "comments","type")
    for(k in 1:length(a$data)){
      infoDataTemp=c(
        ifelse(is.null(a$data[[k]][["message"]]), NA, a$data[[k]][["message"]]),
        ifelse(is.null(a$data[[k]][["created_time"]]), NA, a$data[[k]][["created_time"]]),
        ifelse(is.null(a$data[[k]][["id"]]), NA, a$data[[k]][["id"]]),
        ifelse(is.null(a$data[[k]][["likes"]][["count"]]), 0, a$data[[k]][["likes"]][["count"]]),
        ifelse(is.null(a$data[[k]][["comments"]][["count"]]), 0, a$data[[k]][["comments"]][["count"]]),
        ifelse(is.null(a$data[[k]][["type"]]), NA, a$data[[k]][["type"]])
        )
      infoDataTemp = c(infoDataTemp, ifelse(is.na(infoDataTemp[1]), NA, length(strsplit(infoDataTemp[1], " ")[[1]])))
      
      infoDataTempArray = rbind(infoDataTempArray, infoDataTemp)  
    }
    infoDataTempArray = na.omit(infoDataTempArray)
    
    
  }else{
    cat('error in post data \n')
    print(a)}
  
  infoDataTemp = c()
  if(class(b)!="try-error" & class(b)!="logical"){
    cat('getting basic information \n')
    infoDataTemp = c()
    for(cn in c("about", "category", "id", "likes", "name", "picture", "talking_about_count", "username")){
      infoDataTemp = c(infoDataTemp, ifelse(is.null(b[[cn]]), NA, b[[cn]]))
    }
    
  }else{
    cat('error in basic data \n')
    print(b)}
  return(list(b=infoDataTemp, a=infoDataTempArray, rawid=getID))
}

downloadandUpdateData = function(urls, datestart, dateend){
  sitesnames = c()
  sitesid = c()
  sitesatoken = c()
  sitescategory = c()
  for(u in urls){
    sites = fromJSON(getURL(u))
    sitesnames = c(sitesnames, unlist(lapply(sites$data, function(x)return(x$name))))
    sitesid = c(sitesid, unlist(lapply(sites$data, function(x)return(x$id))))
    sitesatoken = c(sitesatoken, unlist(lapply(sites$data, function(x)return(x$access_token))))
    sitescategory = c(sitescategory, unlist(lapply(sites$data, function(x)return(x$category))))
  }
  sites = cbind(nameid="id", names=sitesnames, id=sitesid, atoken=sitesatoken, category=sitescategory)
  appind = which(sites[,"category"]=="Application")
  if(length(appind)>0){
    sites = sites[-appind, ]
  }
  data = vector("list", nrow(sites))
  names(data) = paste("id", sites[,"id"],sep='')
  sites[,"nameid"] = names(data)
  for(i in 1:nrow(sites)){
    print(sites[i,])
    d = try(getSiteData(sites, i, datestart, dateend, page=T))#ifelse(sites[i,"category"]=="Website", T, F)))
    if(class(d)!="try-error"){
      data[[ paste("id",sites[i,"id"], sep="") ]] = d
    }
  }
  #put into a table format
  
  #get all possible metrics
  mets = c()
  for(i in 1:nrow(sites)){
    mets = c(mets, names(data[[paste("id", sites[i,"id"],sep='')]]$agginsites))
  }
  aggregateTable = array(0, c(nrow(sites), length(unique(mets))))
  rownames(aggregateTable) = paste("id",sites[1:nrow(sites),"id"], sep="")
  colnames(aggregateTable) = as.character(unique(mets))
  
  #aggregateTable = as.data.frame(data[[  paste("id",sites[1,"id"],sep='') ]]$agginsites)
  #aggregateTableApp = as.data.frame(data[[ sites[59,1] ]]$agginsites)
  
  
  for(i in 1:nrow(sites)){
    lname = paste("id",sites[i,"id"], sep="")
    aggregateTable[lname,names(data[[lname ]]$agginsites) ]  =  as.numeric(data[[ lname ]]$agginsites)
  }
  
  #for(i in 60:nrow(sites)){
  #	aggregateTableApp = rbind(aggregateTableApp, as.data.frame(data[[sites[i,1] ]]$agginsites))
  #}
  #rownames(aggregateTableApp) = sites[59:nrow(sites), 1]
  
  dailyTable = list()
  for(i in 1:nrow(sites)){
    lname = paste("id",sites[i,"id"], sep="")
    dailyTable[[lname]] = data[[lname ]]$dailyinsites[[1]]
    for(j in 2:length(data[[ lname  ]]$dailyinsites)){
      dailyTable[[ lname ]] = cbind(dailyTable[[ lname ]], data[[ lname ]]$dailyinsites[[j]])
    }
    dailyTable[[ lname ]] = na.locf(dailyTable[[ lname ]], na.rm=T)
    colnames(dailyTable[[ lname ]]) = names(data[[ lname ]]$dailyinsites)
  }
  
  #before updating the DB make sure to get the most recent list of Branch ID's
  con = dbConnect(MySQL(), user='root', password='2eSt4SSPanH8', dbname='gsmd2', host='localhost', port=3306)
  #con = dbConnect(MySQL(), user='root', password='root', dbname='gsmd', host='mactheripper', port=8889)
  #con = dbConnect(MySQL(), user='root', password='8a465e', dbname='gsmd2', host='localhost', port=3306)
  #con = dbConnect(MySQL(), user='gsmd_dbuser', password='zjuJdJ75ZuMEHDUb', dbname='gsmd', host='socweb-test.co.uk', port=3306 )
  
  #get branch IDs
  IDs = dbGetQuery(con, 'SELECT * FROM  branches')
  #submit any new items to the branch db if needed
  submits = which(!sites[,"id"]%in%IDs[,"page_id"])
  if(length(submits)>0){
    for(i in 1:length(submits)){
      query = paste('INSERT INTO branches (page_id, page_name) VALUES (',sites[submits[i],"id"],' , "', sites[submits[i],"names"],'")', collapse='')
      dbSendQuery(con, query)
    }
  }
  
  IDs = dbGetQuery(con, 'SELECT * FROM  branches')
  #add ID's in table for ID's in sites
  for(i in 1:nrow(aggregateTable)){
    lname = paste("id",sites[i,"id"], sep="")
    query30 = paste('INSERT INTO page_insights_30daySum (branch_id,  date, ', paste(colnames(aggregateTable), collapse=","), ') VALUES (', IDs[which(IDs[,"page_id"]==sites[i,"id"]), "id"], ",'", dateend ,"'," , paste( aggregateTable[i,], collapse=","), ");", collapse="") 
    #dbSendQuery(con, query30)
    
    query1 = paste('INSERT INTO page_insights (branch_id,  date, ', paste(colnames(aggregateTable), collapse=","), ') VALUES (', IDs[which(IDs[,"page_id"]==sites[i,"id"]),"id"] , ",'", index(last(dailyTable[[lname]])), "', ", 
                   paste( last(dailyTable[[lname]][,colnames(aggregateTable)]), collapse=","), ");", collapse="") 
    dbSendQuery(con, query1)
    
  }
  
  dbDisconnect(con)
  
  
  #pagind = which(sites[,"category"]=="Website")
  objectsPage = vector("list", nrow(sites))
  #names(objectsPage) = gsub(",", "", gsub(";", "", gsub("&", "", gsub(" ", "", sites[,1]))))[pagind]
  names(objectsPage) = rownames(sites)
  for(i in 1:nrow(sites)){
    print(sites[i,])
    d = try(getSiteObjects(sites, i))
    if(class(d)!="try-error"){
      objectsPage[[paste("id", sites[i,"id"], sep='')  ]] = d
    }
  }
  
  con = dbConnect(MySQL(), user='root', password='2eSt4SSPanH8', dbname='gsmd2', host='localhost', port=3306)
  #con = dbConnect(MySQL(), user='root', password='8a465e', dbname='gsmd2', host='localhost', port=3306)
  #con = dbConnect(MySQL(), user='gsmd_dbuser', password='zjuJdJ75ZuMEHDUb', dbname='gsmd', host='socweb-test.co.uk', port=3306 )
  #update the database
  for(i in 1:nrow(sites)){
    d = objectsPage[[ paste("id",sites[i,"id"], sep='') ]]
    posts = d$posts$posts
    pageID = IDs[which(IDs[,"page_id"]==sites[i,"id"]),"id"]
    query = paste("Select fbid FROM page_posts WHERE branch_id=", pageID, sep='')
    res = dbGetQuery(con, query)
    if(length(res)>0){
      posts = posts[-which(posts[,"id"]%in%res[,"fbid"]), ]
    }
    if(is.null(dim(posts)) & length(posts)>0){
      posts = as.data.frame(t(posts))
    }
    if(!is.null(nrow(posts)) & length(posts)>0){
      if(nrow(posts)>0){
        if(!c("message") %in% colnames(posts)){
          posts = cbind(posts, message="")
        }
        posts[,"message"] = gsub("'", "", gsub('\"', '', posts[,"message"]))
        for(j in 1:nrow(posts)){
          
          query = paste('INSERT INTO page_posts (branch_id, pageid, added_time, created_time, fbid,message,type,likes,comments)
					 VALUES ("',IDs[which(IDs[,"page_id"]==sites[i,"id"]),"id"],  '", "', IDs[which(IDs[,"page_id"]==sites[i,"id"]),"page_id"], '", NOW(),"',
					posts[j,"created_time"], '", "', posts[j,"id"], '","',
                        posts[j,"message"],'", "', posts[j, "type"], '", ', posts[j, "likes"], ',', posts[j,"comments"],')', sep='')
          dbSendQuery(con, query) 
          
        }
      }
    }
  }
  for(i in 1:nrow(sites)){
    d = objectsPage[[ paste("id",sites[i,"id"], sep='') ]]
    #update the comments
    posts = d$posts$comments
    pageID = IDs[which(IDs[,"page_id"]==sites[i,"id"]),"id"]
    query = paste("Select  commentid FROM page_posts_comments WHERE branch_id=", pageID, sep='')
    res = dbGetQuery(con, query)
    if(length(res)>0){
      posts = posts[-which(posts[,"id"]%in%res[,"commentid"]), ]
    }
    if(is.null(dim(posts)) & length(posts)>0){
      posts = as.data.frame(t(posts))
    }
    if(!is.null(nrow(posts)) & length(posts)>0){
      if(nrow(posts)>0){
        if(!c("message") %in% colnames(posts)){
          posts = cbind(posts, message="")
        }
        posts[,"message"] = gsub("'", "", gsub('\"', '', posts[,"message"]))
        for(j in 1:nrow(posts)){
          
          query = paste('INSERT INTO page_posts_comments (branch_id, added_time, created_time, commentid, postid, username, userID, message )
					 VALUES ("',IDs[which(IDs[,"page_id"]==sites[i,"id"]),"id"],  '", NOW(),"',posts[j,"created_time"], '", "', posts[j,"id"], '","',
					 posts[j,"postID"],'","', posts[j,"from.name"], '","', posts[j,"from.id"], '","', posts[j,"message"],'")', sep='')
          dbSendQuery(con, query) 
          
        }
      }
    }
    
  }
  for(i in 1:nrow(sites)){
    d = objectsPage[[ paste("id",sites[i,"id"], sep='') ]]
    #update the comments
    posts = d$posts$likes
    pageID = IDs[which(IDs[,"page_id"]==sites[i,"id"]),"id"]
    query = paste("Select  postid, userID FROM page_posts_likes WHERE branch_id=", pageID, sep='')
    res = dbGetQuery(con, query)
    if(length(res)>0){
      posts = posts[-which(posts[,"postID"]%in%res[,"postid"] & posts[,"id"]%in%res[,"userID"]), ]
    }
    if(is.null(dim(posts)) & length(posts)>0){
      posts = as.data.frame(t(posts))
    }
    if(!is.null(nrow(posts)) & length(posts)>0){
      if(nrow(posts)>0){
        for(j in 1:nrow(posts)){
          
          query = paste('INSERT INTO page_posts_likes (branch_id, added_time,  postid, username, userID )
	                                 VALUES ("',IDs[which(IDs[,"page_id"]==sites[i,"id"]),"id"],  '", NOW(),"', posts[j,"postID"], '","',
	                                 posts[j,"name"], '",', posts[j,"id"], ')', sep='')
          dbSendQuery(con, query)
          
        }
      }
    }
    
  }
  dbDisconnect(con)
  
  
}
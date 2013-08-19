library(RCurl)
library(rjson)
library(xts)
library(RMySQL)

getSiteObjects = function(sites, site){

    #this function get's a specific site from within a list of sites and parses the data returned
  
	#get the raw data
  posts = fromJSON(getURL(paste("https://graph.facebook.com/",sites[site,"id"],"/posts?access_token=",sites[site,"atoken"],sep="")))
	
  #parse the raw data into vectors to be joined into a table
  feeddates = unlist(lapply(posts$data, function(x)return(x$created_time)))
	created_time = unlist(lapply(strsplit(as.character(feeddates), ""), function(x)return(paste(paste(x[1:10],collapse=""), paste(x[12:19], collapse=""), collapse=" "))))
	id = unlist(lapply(posts$data, function(x)return(x$id)))
	message = unlist(lapply(posts$data, function(x)return(x$message)))
	type = unlist(lapply(posts$data, function(x)return(x$type)))
	likes = unlist(lapply(posts$data, function(x)return(ifelse(is.null(x$likes$count), 0, x$likes$count))))
	likedata =lapply(posts$data, function(x)return(x$likes$data))
	comments = unlist(lapply(posts$data, function(x)return(x$comments$count)))
	commentdata =lapply(posts$data, function(x)return(x$comments$data))
	postTab = cbind(created_time, id, message, type, likes, comments)
	print(postTab)
	likesTab = list() 
	commentsTab = list()
	likesTabTab = array(0, c(0,3))
	commentsTabTab = array(0, c(0,6))
	if(length(likedata)>0){
		for(i in 1:length(likedata)){
			likesTab[[i]] = as.data.frame(likedata[[i]])
				
		}
		for(i in 1:length(likedata)){
			if(nrow(likesTab[[i]])>0){
				likesTabTab = rbind(likesTabTab, cbind(postTab[i,"id"], likesTab[[i]][,c("name", "id")] ))
			}
		}
	}
	colnames(likesTabTab) = c("postID", "name", "id") 
	if(length(commentdata)>0){
		for(i in 1:length(commentdata)){
			commentsTab[[i]] = as.data.frame(commentdata[[i]])			
			if(nrow(commentsTab[[i]])>0){
				x = strsplit(as.character(commentsTab[[i]][,"created_time"]),"")
				commentsTab[[i]][,"created_time"] = unlist(lapply(x, function(x)return(paste(paste(x[1:10],collapse=""), paste(x[12:19], collapse=""), collapse=" ")))) 
			}
		}
		for(i in 1:length(commentdata)){
			if(nrow(commentsTab[[i]])>0){
				commentsTabTab = rbind(commentsTabTab, cbind(postTab[i, "id"], commentsTab[[i]][,c("id", "from.name", "from.id", "message", "created_time")]
))
			}
		}
	}
	colnames(commentsTabTab) = c("postID", "id", "from.name", "from.id", "message", "created_time")

	return(list(posts=list(posts=postTab, likes=likesTabTab, comments=commentsTabTab)))#feed=feed, photos=photos, posts=posts, questions=questions, statuses=statuses, tagged=tagged, videos=videos))

}

getSiteData = function(sites,site, startdate, enddate, page=T){
	#get and parse insights data from the api
  
	if(page){	
		insites = getURL(paste("https://graph.facebook.com/",sites[site,"id"],"/insights?access_token=",sites[site,"atoken"],"&since=", as.character(startdate) ,"&until=", as.character(enddate), sep=""))
	}else{
		atoken = paste("https://graph.facebook.com/oauth/access_token?client_id=",sites[site,"id"],"&client_secret=",sites[site,"atoken"],"&grant_type=client_credentials", sep="")
		res = postForm(atoken)[1]
		token = strsplit(res, "=")[[1]][2]
		insites = getURL(paste("https://graph.facebook.com/",sites[site,2],"/insights?access_token=",token,"&since=", as.character(startdate) ,sep=""))
	}

	insites = fromJSON(insites)
	#get names of various metrics and data
	metricnames = unlist(lapply(insites$data, function(x)return(x$name)))[]
	names(insites$data) = metricnames
	#metricnames = metricnames[grep("application", metricnames)]
	insitesdata = list()
	insitesdata_day = list()
	for(m in metricnames){
		if("value"%in%names(unlist(insites$data[[m]]$values))){
		if(!class(try(as.numeric(matrix(unlist(insites$data[[m]]$values),ncol=2, byrow=T)[,1])))=="try-error"){
			a = try(all(as.numeric(matrix(unlist(insites$data[[m]]$values),ncol=2, byrow=T)[,1])==0))
		if(class(a)!="try-error" & !is.na(a)){
			if(!a){
				
				cat(m, ' ')
				cat(sum( as.numeric(matrix(unlist(insites$data[[m]]$values),ncol=2, byrow=T)[,1])), '\n')
				insitesdata[[m]] = sum( as.numeric(matrix(unlist(insites$data[[m]]$values),ncol=2, byrow=T)[,1]))
			}else{
				insitesdata[[m]]=0
			}
		}
		}
		}
		
		times = c()
		values = c()
		for(l in insites$data[[m]]$values){
			 times = c(times,paste(strsplit(l$end_time, "")[[1]][1:10], collapse=""))
			 values = c(values, ifelse(class(l$value)=="numeric", l$value, 0))
		}
		insitesdata_day[[m]] = xts(values, as.POSIXct(times))
	}
	return(list(dailyinsites = insitesdata_day, agginsites = insitesdata, raw=insites))
}
getAPPinsights = function(datestart, dateend, sites){

}

getPageinsights = function(){


}

getPageObjects = function(){
	

}

downloadandUpdateData = function(urls, datestart, dateend){
  
  #this funciton does the main loop of construction ghte API calls and the dates to get the data between
  
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
	#con = dbConnect(MySQL(), user='root', password='2eSt4SSPanH8', dbname='gsmd2', host='localhost', port=3306)
	#con = dbConnect(MySQL(), user='root', password='root', dbname='gsmd', host='mactheripper', port=8889)
	#con = dbConnect(MySQL(), user='root', password='8a465e', dbname='gsmd2', host='localhost', port=3306)
	#con = dbConnect(MySQL(), user='gsmd_dbuser', password='zjuJdJ75ZuMEHDUb', dbname='gsmd', host='socweb-test.co.uk', port=3306 )

	return(list(agg=aggregateTable, day=dailyTable, sites=sites))
	
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



#this is the actual code that the script runs in order to get the data
for(y in 1:1){
	datestart = Sys.Date()-90-y 
	
	dateend = Sys.Date()-y
	#get FB users and tokens	
# 	con = dbConnect(MySQL(), user='root', password='2eSt4SSPanH8', dbname='permissionsDB', host='localhost', port=3306)
# 	query = paste("Select  * FROM userToken WHERE prod=1")
#         res = dbGetQuery(con, query)
# 	dbDisconnect(con)
# 	urls = paste('https://graph.facebook.com/', res[,'userID'], '/accounts?access_token=',res[,'token'],sep='')
# 	urls = rbind('https://graph.facebook.com/1657628135/accounts?access_token=AAAGXFvb4kk8BALDDEXZCx11qghIqrHZBEZBuec23XgGvcrtdZAESZBcVx3kypMgbfZAqhuaXGnaNbAil1yNqZAqDNvRIjRrHP7gQgKk05beVgZDZD', urls)
	urls = 'https://graph.facebook.com/1657628135/accounts?access_token=AAAGXFvb4kk8BALDDEXZCx11qghIqrHZBEZBuec23XgGvcrtdZAESZBcVx3kypMgbfZAqhuaXGnaNbAil1yNqZAqDNvRIjRrHP7gQgKk05beVgZDZD'
  resFB=vector("list", length(urls))
	names(resFB) = urls
	for(url in urls){
		 resFB[[url]] = try(downloadandUpdateData(urls, datestart, dateend))
	}
}


#just for a 1 time data plot




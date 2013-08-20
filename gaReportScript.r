library(rjson)
library(RCurl)
library(xts)
source('common.R')

refresh2accesstoken = function(accesstoken){
  cat('refreshing accesstoken \n')
  rt = fromJSON(postForm('https://accounts.google.com/o/oauth2/token', .opts=opts,  refresh_token=accesstoken$refresh_token, client_id=client_id,
                         client_secret=client_secret,  grant_type="refresh_token", 
                         style="POST" ))
  accesstoken$access_token = rt$access_token
  accesstoken$expires_in = rt$expires_in
  return(accesstoken)
}
gaGetProfileData = function(accesstoken){
  cat('pulling profile data \n')
  profiles = fromJSON(getURL(paste('https://www.googleapis.com/analytics/v3/management/accounts?access_token=',accesstoken$access_token, sep='', collapse='')))
  for(p in 1:length(profiles$items)){
      rawdat = fromJSON(getURL(paste('https://www.googleapis.com/analytics/v3/management/accounts/',profiles$items[[p]]$id,'/webproperties?access_token=',accesstoken$access_token, sep='', collapse='')))
      if(class(rawdat$items)=="list"){
        print(unlist(lapply(rawdat$items,
                            function(x)return(x$id))))
        profiles$items[[p]][["webP"]] = rawdat$items
        for(pp in 1:length(profiles$items[[p]]$webP)){
	  #cat('url ', paste('https://www.googleapis.com/analytics/v3/management/accounts/',profiles$items[[p]]$id,'/webproperties/',profiles$items[[p]]$webP[[pp]]$id, '/profiles?access_token=',accesstoken$access_token, sep='', collapse=''), '\n')

          rawdat2 = try(fromJSON(getURL(paste('https://www.googleapis.com/analytics/v3/management/accounts/',profiles$items[[p]]$id,'/webproperties/',profiles$items[[p]]$webP[[pp]]$id, '/profiles?access_token=',accesstoken$access_token, sep='', collapse=''))))
	  if(class(rawdat2)!="try-error"){
          if(p==1 & pp==1){
            profiles$items[["profiles"]] = rawdat2$items
          }else{
            profiles$items[["profiles"]] = c(profiles$items$profiles, rawdat2$items)
          }
          }
	}
      }
  }
  
  #flatten the properties in the list
  
  return(profiles)
}
getRawDat = function(profiles,ip, startdate, enddate, metrics, dimensions, sort, accesstoken){
  
  rawdat =  fromJSON(getURL(paste('https://www.googleapis.com/analytics/v3/data/ga?access_token=',accesstoken$access_token, 
                                  '&ids=',paste("ga:",profiles$items$profiles[[ip]]$id,sep='', collapse=''),
                                  '&start-date=',startdate, '&end-date=',enddate, '&metrics=',paste(metrics, collapse=","), 
                                  '&dimensions=',paste(dimensions, collapse=","), 
                                  '&sort=',sort,sep='', collapse='')))
  #turn the rows into a dataframe
  cat('got ', length(rawdat$rows), ' of raw data ')
  if(!is.null(rawdat$rows)){
    basedata = array(0, c(length(rawdat$rows), length(rawdat$rows[[1]])))
    basedata.df = as.data.frame(basedata)
    
    if(nrow(basedata))
    for(ii in 1:nrow(basedata)){
      basedata[ii,] = rawdat$rows[[ii]]
    }
    colType = getLItem(rawdat$columnHeaders, "dataType")
    for(ii in 1:ncol(basedata)){
      basedata.df[,ii] = switch(colType[ii], 
                                "INTEGER"=as.numeric(basedata[,ii]),
                                "STRING"=as.character(basedata[,ii]))
    }
    colnames(basedata.df) = getLItem(rawdat$columnHeaders, "name")
  }else{
    basedata = array(0, c(1, length(rawdat$columnHeaders)))
    basedata.df=as.data.frame(basedata)
    colnames(basedata.df) = getLItem(rawdat$columnHeaders, "name")
  }
 
  
  return(list(data=basedata.df, rawdat=rawdat))
  cat('\n')
}
gaGetQuery=function(profiles, accesstoken, startdate, enddate, metrics, dimensions, sort,reportname){
  cat('getting report for: ')
  reps = vector("list", length(profiles$items$profiles))
  for(ppi in 1:length(profiles$items$profiles)){
    cat(profiles$items$profiles[[ppi]]$name,' ', reportname)
    
    res=try(getRawDat(profiles=profiles, ip=ppi, startdate=startdate, enddate=enddate, metrics=metrics, dimensions=dimensions, sort=sort, accesstoken=accesstoken))
    if(class(res)!="try-error"){
     reps[[ppi]] = res
    }
  }
  return(reps)
}
getProfileData = function(datestart=as.character(Sys.Date()-30 ), dateend=as.character(Sys.Date()), accesstoken){
	# 1. Create a new Google Analytics API object
	#ga <- RGoogleAnalytics()
	
	# 2. Authorize the object with your Google Analytics Account Credentials
	#ga$SetCredentials("kevin%sendster.co.uk@gtempaccount.com", "mobile99")
	#ga$SetCredentials("theosendster@gmail.com", "dec302006")
	
	# 3. Get the list of different profiles, to help build the query
# 	profiles <- ga$GetProfileData()
  profiles = gaGetProfileData(accesstoken)
	#profiles[["data"]] = vector("list", length(profiles$items$profiles))
  profiles[["data"]] = list()
	#names(profiles$data) = as.character(profiles$profile$ProfileName)
	#names(profiles$data) = unlist(lapply(profiles$items$profiles, function(x)return(x$name)))
	
	
# 		#mobile, hour, source, referralPath analysis - mhsr
# 		query$Init(start.date = datestart,
# 		            end.date = dateend,
# 			    dimensions = c("ga:date",  "ga:hour", 
# 			    "ga:isMobile", "ga:operatingSystem",
# 			    "ga:referralPath","ga:source"),
# 			    metrics = c("ga:visitors", "ga:visits", 
# 			    "ga:newVisits", "ga:timeOnSite", "ga:totalEvents"),
# 			    sort = "ga:date",
# 			    table.id = as.character(profiles$profile$TableId[i]))
#     
    metrics=c("ga:visitors", "ga:visits", 
              "ga:newVisits", "ga:timeOnSite", "ga:totalEvents")
    dimensions=c("ga:date",  "ga:hour", 
                 "ga:isMobile", "ga:operatingSystem",
                 "ga:referralPath","ga:source")
    
    #profiles$data[["mhsr"]] = gaGetQuery(profiles, accesstoken,datestart,dateend,metrics,dimensions,sort,reportname="mhsr")
    #names(profiles$data$mhsr) = unlist(lapply(profiles$items$profiles, function(x)return(x$name)))
		#5. Make a request to get the data from the API
	 	#profiles$data[[as.character(profiles$profile$ProfileName[i])]][["mhsr"]] = ga$GetReportData(query)
	
		#simple raw report - Julia's data
# 		query$Init(start.date = datestart,
# 		            end.date = dateend,
# 			    dimensions = c( "ga:hostname"),
# 			    metrics = c("ga:visitors", "ga:visits", "ga:newVisits", "ga:timeOnSite", "ga:totalEvents"),
# 			    sort = "ga:hostname",
# 			    table.id = as.character(profiles$profile$TableId[i]))
# 	
# 		profiles$data[[as.character(profiles$profile$ProfileName[i])]][["juliadata"]] = ga$GetReportData(query)
# 		
	  dimensions = c( "ga:hostname")
	  metrics = c("ga:visitors", "ga:visits", "ga:newVisits", "ga:timeOnSite", "ga:totalEvents")
	  sort = "ga:hostname"
    res = gaGetQuery(profiles, accesstoken,datestart,dateend,metrics,dimensions,sort,reportname="juliadata")
	  profiles$data[["juliadata"]] = res
    names(profiles$data$juliadata) = unlist(lapply(profiles$items$profiles, function(x)return(x$name)))
# 		#simple raw report - just visits 
# 		query$Init(start.date = datestart,
# 		            end.date = dateend,
# 			    dimensions = c( "ga:date"),
# 			    metrics = c("ga:visits"),
# 			    sort = "ga:date",
# 			    table.id = as.character(profiles$profile$TableId[i]))
# 	
# 		profiles$data[[as.character(profiles$profile$ProfileName[i])]][["visitsdata"]] = ga$GetReportData(query)
# 		
# 	
	  dimensions = c( "ga:date")
    metrics = c("ga:visits")
    sort = "ga:date"
   res = gaGetQuery(profiles, accesstoken,datestart,dateend,metrics,dimensions,sort,reportname="visitsdata")
	  profiles$data[["visitsdata"]] = res
    names(profiles$data$visitsdata) = unlist(lapply(profiles$items$profiles, function(x)return(x$name)))
# 		#simple raw report - Julia's data + sources
# 		query$Init(start.date = datestart,
# 		            end.date = dateend,
# 			    dimensions = c( "ga:source", "ga:referralPath"),
# 			    metrics = c("ga:visits", "ga:totalEvents"),
# 			    sort = "ga:source",
# 			    table.id = as.character(profiles$profile$TableId[i]))
# 	
# 		profiles$data[[as.character(profiles$profile$ProfileName[i])]][["juliadatasource"]] = ga$GetReportData(query)#aggregate(rs[,2], list(source=rs[,1]), sum)
# 		
# 			
	  dimensions = c( "ga:source", "ga:referralPath")
    metrics = c("ga:visits", "ga:totalEvents")
    sort = "ga:source"
    res = gaGetQuery(profiles, accesstoken,datestart,dateend,metrics,dimensions,sort,reportname="juliadatasource")
	  profiles$data[["juliadatasource"]] = res
    names(profiles$data$juliadatasource) = unlist(lapply(profiles$items$profiles, function(x)return(x$name)))
		#simple events  report - Julia's data
# 		query$Init(start.date = datestart,
# 		            end.date = dateend,
# 			    dimensions = c( "ga:eventCategory","ga:eventAction" ),
# 			    metrics = c("ga:visitors", "ga:visits", "ga:newVisits", "ga:timeOnSite", "ga:totalEvents"),
# 			    sort = "ga:eventAction",
# 			    table.id = as.character(profiles$profile$TableId[i]))
# 		profiles$data[[as.character(profiles$profile$ProfileName[i])]][["juliadataevents"]] = ga$GetReportData(query)
# 	
	    dimensions = c( "ga:eventCategory","ga:eventAction" )
	    metrics = c("ga:visitors", "ga:visits", "ga:newVisits", "ga:timeOnSite", "ga:totalEvents")
	    sort = "ga:eventAction"
      res = gaGetQuery(profiles, accesstoken,datestart,dateend,metrics,dimensions,sort,reportname="juliadataevents")
	    profiles$data[["juliadataevents"]] = res
      names(profiles$data$juliadataevents) = unlist(lapply(profiles$items$profiles, function(x)return(x$name)))
	return(profiles)
}


#summary stats by site  raw data table
makeRawReport = function(profiles){
  #take the profiles and raw data and aggregate
	rawdata = array(0, c(length(profiles$data$juliadata), 15))
	rownames(rawdata) = names(profiles$data$juliadata)
	colnames(rawdata) = c("AbsoluteUniqueVisitors", "Visits", "FBReferrals", "PubExplorerReferrals", "TotalInteractions",
				"FBLikes", "FBUnlikes","XmasMenuDownloads","MenuDownloads", "GooglePlacesReferrals",
				"LunchMenuDownloads","DiningMenuDownloads", "MainMenuDownloads", "timeOnSite","likeRate")
	for(i in 1:nrow(rawdata)){
	
  		n = rownames(rawdata)[i]
  		cat('compiling data for ', n, '\n')
  	
  		#basic stats
#       browser()
  		rawdata[n, "AbsoluteUniqueVisitors"] = tryErr(as.numeric(profiles$data$juliadata[[n]]$rawdat$totalsForAllResults[["ga:visitors"]]))
  		rawdata[n, "Visits"] = tryErr(as.numeric(profiles$data$juliadata[[n]]$rawdat$totalsForAllResults[["ga:visits"]]))
  		rawdata[n, "timeOnSite"] =tryErr(as.numeric(profiles$data$juliadata[[n]]$rawdat$totalsForAllResults[["ga:timeOnSite"]])	)
  		
      #referrals
  		rs = try(aggregate(as.numeric(profiles$data$juliadatasource[[n]]$data[,"ga:visits"]), list(source=profiles$data$juliadatasource[[n]]$data[,"ga:source"]), sum))
  		if(class(rs)!="try-error"){
  			rawdata[n, "FBReferrals"] = ifelse(length(which(rs[,"source"]=="facebook.com"))>0, rs[which(rs[,"source"]=="facebook.com"),2], 0)
  			rawdata[n, "PubExplorerReferrals"] = ifelse(length(which(rs[,"source"]=="pub-explorer.com"))>0, rs[which(rs[,"source"]=="pub-explorer.com"),2], 0)
  			whichgp = c((grep("maps\\.google", as.numeric(profiles$data$juliadatasource[[n]]$data[,"ga:source"]))) , (grep("\\/m\\/places", profiles$data$juliadatasource[[n]]$data[,"ga:referralPath"])))
  			rawdata[n, "GooglePlacesReferrals"] = sum(as.numeric(profiles$data$juliadatasource[[n]]$data[whichgp,"ga:visits"]))
  			
  		}
  		#rs = try(aggregate(profiles$data[[n]]$juliadatasource$data[,"ga:visits"], list(source=profiles$data[[n]]$juliadatasource$data[,"ga:referralPath"]), sum))
  		#if(class(rs)!="try-error"){
  		#	rawdata[n, "GooglePlaces"] = ifelse(length(which(rs[,"source"]=="facebook.com"))>0, rs[which(rs[,"source"]=="facebook.com"),2], 0)
  		#}
  	
  		#interactions
      if(!is.null(profiles$data$juliadata[[n]]$rawdat$totalsForAllResults[["ga:totalEvents"]])){
  		  rawdata[n, "TotalInteractions"] = profiles$data$juliadata[[n]]$rawdat$totalsForAllResults[["ga:totalEvents"]]
      }
  		rs = profiles$data$juliadataevents[[n]]$data
  		rscat = try(aggregate(sapply(rs[,3:7], function(x)as.numeric(x)), list(action=rs[,2]), sum))
  		if(class(rscat)!="try-error"){
  			rawdata[n, "FBLikes"] = ifelse(length(which(rscat[,"action"]=="like")>0), rscat[which(rscat[,"action"]=="like"),"ga:totalEvents"], 0)
  			rawdata[n, "FBUnlikes"] = ifelse(length(which(rscat[,"action"]=="unlike")>0), rscat[which(rscat[,"action"]=="unlike"),"ga:totalEvents"], 0)
  			rawdata[n, "MenuDownloads"] = ifelse(length(which(rscat[,"action"]=="Download menu")>0), rscat[which(rscat[,"action"]=="Download menu"),"ga:totalEvents"], 0)
  		}
  	
  		#xmas menu downloads
  		rscat = try(aggregate(sapply(rs[,3:7], function(x)as.numeric(x)), list(action=rs[,1]), sum))
  		if(class(rscat)!="try-error"){
  			rawdata[n, "XmasMenuDownloads"] = ifelse(length(which(rscat[,"action"]=="Menu - Christmas Menu")>0), rscat[which(rscat[,"action"]=="Menu - Christmas Menu"),"ga:totalEvents"], 0)
  			rawdata[n, "LunchMenuDownloads"] = ifelse(length(which(rscat[,"action"]=="Menu - Lunch Menu")>0), rscat[which(rscat[,"action"]=="Menu - Lunch Menu"),"ga:totalEvents"], 0)
  			rawdata[n, "DiningMenuDownloads"] = ifelse(length(which(rscat[,"action"]=="Menu - Dining Menu")>0), rscat[which(rscat[,"action"]=="Menu - Dining Menu"),"ga:totalEvents"], 0)
  			rawdata[n, "MainMenuDownloads"] = ifelse(length(which(rscat[,"action"]=="Menu - Main Menu")>0), rscat[which(rscat[,"action"]=="Menu - Main Menu"),"ga:totalEvents"], 0)
  		}
  		#like rate
      
  		if(!is.na(as.numeric(rawdata[n, "Visits"]) > 0) & (as.numeric(rawdata[n, "Visits"]) > 0) ){
        res = try(as.numeric(rawdata[n,"FBLikes"]) / as.numeric(rawdata[n, "Visits"]))
  			rawdata[n, "likeRate"] = ifelse(class(res)!="try-error", res,0)
  		}
      
	}
	return(rawdata)
}
makeSourcesReport = function(profiles, metric="ga:visits"){
  
  #this function aggregates the sources into a more easy to read table by profileID
  rawdata = array(0, c(length(profiles$data$juliadata), 15))
  rownames(rawdata) = names(profiles$data$juliadata)
	rsmain = array(0, c(0, 2))
	for(i in 1:length(profiles$data$juliadatasource)){
    
	  n = rownames(rawdata)[i]
    cat('compiling Sources for: ', n, '\n')
    
		rs = (try(aggregate(as.numeric(profiles$data$juliadatasource[[n]]$data[,metric]), list(source=profiles$data$juliadatasource[[n]]$data[,"ga:source"]), sum)))
		
    if(class(rs)!="try-error"){
			if(i==1){
				rsmain = rs
			}else{
				rsmain = rbind(rsmain, rs)
			}
		}
	}
#browser()
	colnames(rsmain) = c("source", metric)
	rsmain = aggregate(rsmain[,metric], list(rsmain[,"source"]), sum)
	colnames(rsmain) = c("source", metric)
	return(rsmain)

}
makeEventsReport = function(profiles){
  
  #this function aggregates events by profileID an returns a table of events
  rawdata = array(0, c(length(profiles$data$juliadata), 15))
  rownames(rawdata) = names(profiles$data$juliadata)
	rsmain = array(0, c(0, 2))
	for(i in 1:nrow(rawdata)){
    
		n = rownames(rawdata)[i]
		rs = try(aggregate(as.numeric(profiles$data$juliadataevents[[n]]$data[,"ga:totalEvents"]), list(source=profiles$data$juliadataevents[[n]]$data[,"ga:eventCategory"]), sum))
		
    if(class(rs)!="try-error"){
			if(i==1){
				rsmain = rs
			}else{
				rsmain = rbind(rsmain, rs)
			}
		}
		
		
	}
  
	colnames(rsmain) = c("category", "events")
	rsmain = (aggregate(rsmain[,"events"], list(rsmain[,"category"]), sum))
	colnames(rsmain) = c("category", "events")
	return(rsmain)

}
makeDailyReport = function(profiles, metric="ga:visits", aggtime='days'){
  #this function collects visits by day and profile ID
  rawdata = array(0, c(length(profiles$data$juliadata), 15))
  rownames(rawdata) = names(profiles$data$juliadata)
	rsmain = array(0, c(0, 2))
	for(i in 1:nrow(rawdata)){
		n = rownames(rawdata)[i]
		rs = try(aggregate(as.numeric(profiles$data$visitsdata[[n]]$data[,metric]), list(source=profiles$data$visitsdata[[n]]$data[,"ga:date"]), sum), silent=T)
		if(class(rs)!="try-error"){
			if(i==1){
				rsmain = rs
			}else{
				rsmain = rbind(rsmain, rs)
			}
		}else{
			cat('try error for ', n, '\n')
		}
		
		
	}
	colnames(rsmain) = c("date", metric)
	#rsmain = (aggregate(rsmain[,metric], list(rsmain[,"date"]), sum))
	colnames(rsmain) = c("date", metric)
	rsmain = xts(rsmain[,metric], order.by=as.POSIXct(rsmain[,"date"], format="%Y%m%d", tz="EST"), unique=F)
	return(period.apply(rsmain, endpoints(index(rsmain), aggtime), sum))
}

#load up the access token
client_id = "390328638386-c163hiuv5pvuha1mfrritqrofj9p1d3g.apps.googleusercontent.com"
client_secret="NbkQpdvlzQWz_Kh3Xc7FxcZF"
opts = list(verbose=T )

load('accesstoken.RData')
#refresh the access token
accesstoken = refresh2accesstoken(accesstoken)

#don't want to include these in the reports
blacklist = c("sendster.co.uk", "localmarketer.co.uk", "socweb-test.co.uk", "www.socialwebsites2.co.uk", "socweb-test.co.uk/kevinbar")


#profiles = getProfileData(datestart="2012-02-01", dateend="2012-02-28")
#profiles = getProfileData(datestart=as.character(Sys.Date()-30), dateend=as.character(Sys.Date()))

#get the previous month of data starting on the 1st all the way to the last day of the month
dates =  rev(seq(as.Date(format(Sys.Date(), "%Y-%m-01")), length=2, by="-1 month"))
dates[length(dates)] = dates[length(dates)]-1

#dates = c(as.Date("2013-07-01"), as.Date("2013-07-31"))

profiles = getProfileData(datestart=as.character(dates[1]), dateend=as.character(dates[2]), accesstoken)
#if you need to get a specific date range you can specify here
#profiles = getProfileData(datestart="2012-05-01", dateend="2012-06-01")

#reshape the data into something more useable
rawdata = makeRawReport(profiles)

rawdata = rawdata[-which(rownames(rawdata)%in%blacklist), ]

# rawdata = transform(as.data.frame(rawdata), char="numeric")
#write out the raw report in csv format
write.csv(rawdata, paste("gaReportAll",".csv", sep=""))

#print(colSums(rawdata))
# cat('avg time on site: ', sum(as.numeric(na.omit(rawdata)[,"timeOnSite"]))/sum(as.numeric(na.omit(rawdata)[,"Visits"])), '\n')

#get data by source and profile ID
sourcesData = makeSourcesReport(profiles, metric="ga:visits")
sourcesDataevents = makeSourcesReport(profiles, metric="ga:totalEvents")
eventsData = makeEventsReport(profiles)

#sources plot
sourcesData[,"source"] =  gsub(".com", "", sourcesData[,"source"])
sourcesData[,"source"] =  gsub(".co.uk", "", sourcesData[,"source"])
sourcesData = sourcesData[sort(sourcesData[,"ga:visits"], decreasing=T, index.return=T)$ix, ]
numsource=7
jpeg("sources.jpg", width=900, height=600)
barplot(sourcesData[1:numsource,"ga:visits"]/sum(sourcesData[,"ga:visits"])*100, ylab="% of Traffic", xlab="Referring Site", col=heat.colors(numsource), names.arg=sourcesData[1:numsource,"source"]) 
dev.off()
write.csv(sourcesData, "gaReportSources.csv")

#get data by profile and event
sourcesDataevents[,"source"] =  gsub(".com", "", sourcesDataevents[,"source"])
sourcesDataevents[,"source"] =  gsub(".co.uk", "", sourcesDataevents[,"source"])
sourcesDataevents = sourcesDataevents[sort(sourcesDataevents[,"ga:totalEvents"], decreasing=T, index.return=T)$ix, ]
numsource=7
jpeg("sourcesEvents.jpg", width=900, height=600)
barplot(sourcesDataevents[1:numsource,"ga:totalEvents"]/sum(sourcesDataevents[,"ga:totalEvents"])*100, ylab="% of Traffic", xlab="Referring Site", col=heat.colors(numsource), names.arg=sourcesDataevents[1:numsource,"source"]) 
dev.off()

#get data by event
#events plot
eventsData[,"category"] = gsub("Menu - ", "", eventsData[,"category"])
eventsData = eventsData[sort(eventsData[,"events"], decreasing=T, index.return=T)$ix, ]
numevents=7
jpeg("events.jpg", width=900, height=600)
barplot(eventsData[1:numevents,"events"]/sum(eventsData[,"events"])*100, ylab="% of Events", xlab="Actions", col=heat.colors(numevents), names.arg=eventsData[1:numevents,"category"]) 
dev.off()
write.csv(eventsData, "gaReportEvents.csv")

#save.image()

#get daily/weekly data report
# #dailydata
dailyVisits = makeDailyReport(profiles, metric="ga:visits")
jpeg("dailyVisits.jpg", width=900, height=600)
plot(dailyVisits[-length(dailyVisits)], main="Visits by Day")
dev.off()
# 
weeklyVisits = makeDailyReport(profiles, metric="ga:visits", "weeks")
jpeg("weeklyVisits.jpg", width=900, height=600)
plot(weeklyVisits[-length(weeklyVisits)], main="Visits by Week")
 dev.off()
# 
# #monthlyVisits = makeDailyReport(profiles, metric="ga:visits", "months")
# #jpeg("monthlyVisits.jpg", width=900, height=600)
# #plot(monthlyVisits, main="Visits by Month")
# #dev.off()
# #dailyEvents = makeDailyReport(profiles, metric="ga:totalEvents")
# #jpeg("dailyEvents.jpg", width=900, height=600)
# #plot(dailyEvents, main="Interactions by Day")
# 
# #dev.off()
# #
# ##dailyVisits vs. Events
# #jpeg("dailyVisitsEvents.jpg", width=900, height=600)
# #plot(dailyVisits, main="Visits by Day", ylim=c(min(c(dailyEvents, dailyVisits)), max(c(dailyEvents, dailyVisits))))
# #lines(dailyEvents, col="red")
# #legend("topright", legend=c("Visits", "Events"), col=c("black", "red"), lty=1)
# #dev.off()
# 
# #day of week analysis
# dailyVisitsDay = cbind(visits=as.numeric(dailyVisits), day=weekdays(index(dailyVisits)))
# dvdagg = aggregate(as.numeric(dailyVisitsDay[,"visits"]), list(day=dailyVisitsDay[,"day"]), sum)
# jpeg("DoW.jpg", width=900, height=600)
# order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
# rownames(dvdagg) = dvdagg[,"day"]
# barplot(dvdagg[order,"x"]/sum(dvdagg[,"x"])*100, ylab="% of Traffic", xlab="Day of Week", col=heat.colors(nrow(dvdagg)), names.arg=dvdagg[order,"day"]) 
# dev.off()
# 
# 
# #time of day analysis
# 
# 
# #scatter / bubble plot of the two

save.image()


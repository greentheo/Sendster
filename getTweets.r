library(RCurl)
library(rjson)

ss = "Hall Woodhouse"

dlast = "09-01-2011" 

finished=F
pg=1
textsall = c()
datesall = c()
userall = c()

while(!finished){

	#get results
	res = postForm(paste("http://search.twitter.com/search.json?q=",URLencode(ss),"&result_type=mixed&rpp=100&page=", pg,  sep=""))
	
	#parse the results
	resp = fromJSON(res)
	dates = unlist(lapply(resp$results, function(x)return(x$created_at)))
	texts = unlist(lapply(resp$results, function(x)return(x$text)))
	user = unlist(lapply(resp$results, function(x)return(x$from_user)))

	dates = as.Date(gsub("\\+0000","",((dates))), "%a, %d %b %Y %H:%M:%s")
	if(any(dates<dlast) | length(texts)<100){
		finished=T
	}
	textsall = c(textsall, texts)
	datesall = c(datesall, dates)
	userall = c(userall, user)
	pg=pg+1
}





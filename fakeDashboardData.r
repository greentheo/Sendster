sites = paste("site",1:15)
county = paste("county", 1:5)
region = paste("region", 1:3)

summarydata = array(0, c(length(sites), 14))

colnames(summarydata) = c("fblikes", "fbcomments", "fblifelikes","fblifecomments", "tweets","retweets","mentions","lifetweets","liferetweets","lifementions","sndstrVisits","sndstrMenudload", "sndstrRes","sndstrGuests")

rownames(summarydata) = sites

for(r in sites){

	summarydata[r,] = round(runif(ncol(summarydata), 0,1000))

}

sitereg = array(0, c(length(sites), 2))

colnames(sitereg) = c("county", "region")

rownames(sitereg) = sites

for(r in sites){

	sitereg[r,] = c(sample(county,1), sample(region,1))
}

sitedata = array(0, c(length(sites)*30,11))
colnames(sitedata) = c("date", "sitename","likes","comments","tweets","retweets","mentions","visits","menudownloads","reservations","guests")

for(r in 1:length(sites)){
	sitedata[((r-1)*30+1):(r*30),3:10] = matrix(round(runif(8*30)*1000/30), ncol=8)
	sitedata[((r-1)*30+1):(r*30),1] = as.character(seq(as.Date("2011-09-01"), as.Date("2011-09-30"), by="1 day")  )
	sitedata[((r-1)*30+1):(r*30),2] = sites[r] 
}

write.csv(summarydata, "summarydata.csv")
write.csv(sitereg, "site_county_region.csv")
write.csv(sitedata, "sitedata_byday.csv")


make_california_UCIPM_station_list<-function()  
  {docu<-htmlParse("http://ipm.ucdavis.edu/WEATHER/wxactstnames.html")
  els = getNodeSet(docu, "//body//table")[[2]]
  els = getNodeSet(els, "//table//tr")
  nores=TRUE
  for(i in 1:length(els))
  {x<-xmlToDataFrame(els[i])
  if(length(x)==3)
  {colnames(x)<-c("Name","Code","Interval")
  if(nores) {res<-x;nores=FALSE} else res<-rbind(res,x)}}
  
  for(l in 1:nrow(res))
  {docu<-htmlParse(paste("http://ipm.ucdavis.edu/calludt.cgi/WXSTATIONDATA?STN=",res$Code[l],sep=""))
  els = getNodeSet(docu, "//table")[[2]]
  els = getNodeSet(els, "//tr")[[6]]
  positionstring<-getChildrenStrings(els)[1]
  suppressWarnings(sp<-as.numeric(strsplit(positionstring," ")$td))
  sp<-sp[which(!is.na(sp))]
  res[l,"Lat"]<-sp[1]+sp[2]/60
  res[l,"Long"]<-sp[3]+sp[4]/60
  if(length(grep("min W",positionstring))>0) res[l,"Long"]<-(-res[l,"Long"])
  if(length(grep("min S",positionstring))>0) res[l,"Lat"]<-(-res[l,"Lat"]) 
  res[l,"Elev"]<-as.numeric(strsplit(as.character(getChildrenStrings(els)[3])," ")[[1]][2])*0.3048}
  return(res)
}


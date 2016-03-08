handle_ucipm<-function(action,location=NA,time_interval=NA,station_list=california_stations,stations_to_choose_from=25,drop_most=TRUE)
{
 #station list section

  if(is.character(action))  if(action=="list_stations")
        {if(!is.null(names(location)))
        {lat<-unlist(sapply(names(location),function(x) max(c(length(grep("lat", x, ignore.case = TRUE)),length(grep("y", x, ignore.case = TRUE))))))
        if(sum(lat)==1) lat<-as.numeric(location[which(lat==1)])
        long<-unlist(sapply(names(location),function(x) max(c(length(grep("lon", x, ignore.case = TRUE)),length(grep("x", x, ignore.case = TRUE))))))
        if(sum(long)==1) long<-as.numeric(location[which(long==1)])
        elev<-unlist(sapply(names(location),function(x) max(c(length(grep("ele", x, ignore.case = TRUE)),length(grep("alt", x, ignore.case = TRUE)),
                                                              length(grep("z", x, ignore.case = TRUE))))))
        if(sum(elev)==1) elev<-as.numeric(location[which(elev==1)]) else elev<-NA     
        } else {long<-location[1]
        lat<-location[2]
        if(length(location)==3) elev<-location[3] else elev<-NA}
    
        docu<-htmlParse("http://ipm.ucdavis.edu/WEATHER/wxactstnames.html")
        els = getNodeSet(docu, "//body//table")[[2]]
        els = getNodeSet(els, "//table//tr")
        nores=TRUE
        for(i in 1:length(els))
          {x<-xmlToDataFrame(els[i])
           if(length(x)==3)
             {colnames(x)<-c("Name","Code","Interval")
              if(nores) {res<-x;nores=FALSE} else res<-rbind(res,x)}}

        if(is.null(station_list)) station_list<-data.frame(Code=NA,Lat=NA,Long=NA,Elev=NA)
        if(!is.null(station_list)) if(is.na(sum(match(c("Code","Lat","Long","Elev"),colnames(station_list)))))
          station_list<-data.frame(Code=NA,Lat=NA,Long=NA,Elev=NA)
        
      for(l in 1:nrow(res))
        {if(res$Code[l] %in% station_list$Code)
        {res[l,"Latitude"]<-station_list[which(station_list$Code==res$Code[l]),"Lat"]
        res[l,"Longitude"]<-station_list[which(station_list$Code==res$Code[l]),"Long"]
        res[l,"Elevation"]<-station_list[which(station_list$Code==res$Code[l]),"Elev"]
        } else
        {docu<-htmlParse(paste("http://ipm.ucdavis.edu/calludt.cgi/WXSTATIONDATA?STN=",res$Code[l],sep=""))
        els = getNodeSet(docu, "//table")[[2]]
        els = getNodeSet(els, "//tr")[[6]]
        positionstring<-getChildrenStrings(els)[1]
        suppressWarnings(sp<-as.numeric(strsplit(positionstring," ")$td))
        sp<-sp[which(!is.na(sp))]
        res[l,"Latitude"]<-sp[1]+sp[2]/60
        res[l,"Longitude"]<-sp[3]+sp[4]/60
        if(length(grep("min W",positionstring))>0) res[l,"Long"]<-(-res[l,"Long"])
        if(length(grep("min S",positionstring))>0) res[l,"Lat"]<-(-res[l,"Lat"]) 
        res[l,"Elevation"]<-as.numeric(strsplit(as.character(getChildrenStrings(els)[3])," ")[[1]][2])*0.3048}}
      
        starts<-sapply(sapply(res$Interval,function(x) strsplit(as.character(x)," to")),function(x) x[[1]])
        starts<-sapply(starts,function(x) strsplit(x,"/"))
        starts<-unlist(sapply(starts,function(x) if(length(x)==3) x<-format(YEARMODA2Date(as.numeric(x[3])*10000+as.numeric(x[1])*100+as.numeric(x[2])),"%Y-%m-%d") else
                                          if(length(x)==1) x<-format(YEARMODA2Date(as.numeric(x)*10000+0101),"%Y-%m-%d")))
        res["Start_date"]<-starts
        ends<-sapply(sapply(res$Interval,function(x) strsplit(as.character(x)," to")),function(x) x[[length(x)]])
        ends[c(1:length(ends)) %in% grep("period",res$Interval)]<-"present"
        ends[!c(1:length(ends)) %in% grep("present",res$Interval)]<-format(YEARMODA2Date(as.numeric(ends[!c(1:length(ends)) %in% grep("present",ends)])*10000+1231),"%Y-%m-%d")
        ends[c(1:length(ends)) %in% grep("present",res$Interval)]<-format(Sys.time(),"%Y-%m-%d")

        res["End_date"]<-ends
        
        myPoint<-c(long,lat)
        res[,"distance"]<-round(spDistsN1(as.matrix(res[,c("Longitude","Latitude")]), myPoint, longlat=TRUE),2)
        sorted_list<-res[order(res$distance),]
        if(!is.na(elev)) sorted_list[,"elevation_diff"]<-elev-sorted_list$Elevation
        sorted_list[,"chillR_code"]<-as.character(sorted_list$Code)       
        if(!is.na(time_interval[1]))
            {interval_end<-YEARMODA2Date(time_interval[2]*10000+1231)
            interval_start<-YEARMODA2Date(time_interval[1]*10000+0101)
            sorted_list<-sorted_list[1:min(nrow(sorted_list),max(stations_to_choose_from,500)),]
            sorted_list[,"Overlap_years"]<-round(
              apply(sorted_list,1,function (x) (as.numeric(difftime(
                sort(c(x["End_date"],format(interval_end,"%Y-%m-%d")))[1],
                sort(c(x["Start_date"],format(interval_start,"%Y-%m-%d")))[2])+1)/(365+length(which(sapply(time_interval[1]:time_interval[2],leap_year)))/(time_interval[2]-time_interval[1]+1)))),2)
            sorted_list[which(sorted_list[,"Overlap_years"]<0),"Overlap_years"]<-0
            sorted_list[,"Perc_interval_covered"]<-round(sorted_list[,"Overlap_years"]/(time_interval[2]-time_interval[1]+1)*100,2)
            if(!is.na(elev))  sorted_list<-sorted_list[,c("chillR_code","Name","Latitude","Longitude","Elevation","Start_date","End_date",
                                                          "distance","elevation_diff","Overlap_years","Perc_interval_covered")] else
                                                            sorted_list<-sorted_list[,c("chillR_code","Name","Latitude","Longitude","Start_date","End_date",
                                                                                        "distance","Overlap_years","Perc_interval_covered")]} else
            if(!is.na(elev))  sorted_list<-sorted_list[,c("chillR_code","Name","Latitude","Longitude","Elevation","Start_date","End_date",
                                                      "distance","elevation_diff")] else
                                           sorted_list<-sorted_list[,c("chillR_code","Name","Latitude","Longitude","Start_date","End_date",
                                                                      "distance")]         
        return(sorted_list[1:stations_to_choose_from,])}

  
  #weather download section
  
if(is.character(action)) if(action=="download_weather")
  {
  
  if(is.na(time_interval[1])) time_interval<-c(1950,2050)
  docu<-htmlParse("http://ipm.ucdavis.edu/WEATHER/wxactstnames.html")
  els = getNodeSet(docu, "//body//table")[[2]]
  els = getNodeSet(els, "//table//tr")
  nores=TRUE
  for(i in 1:length(els))
  {x<-xmlToDataFrame(els[i])
  if(length(x)==3)
  {colnames(x)<-c("Name","Code","Interval")
  if(nores) {res<-x;nores=FALSE} else res<-rbind(res,x)}}
  
  if(!location %in% res$Code) {warning("No weather data found for this station.")} else
  {
    string<-paste("STN=",location,"&MAP=&FROMMONTH=1&FROMDAY=1&FROMYEAR=",
                  time_interval[1],"&THRUMONTH=12&THRUDAY=31&THRUYEAR=",time_interval[2],"&DT_PRECIP=1&PRECIP_BACKUP1=.&PRECIP_BACKUP2=.&",
                  "PRECIP_BACKUPAVG=.&DT_AIR=1&AIR_BACKUP1=.&AIR_BACKUP2=.&AIR_BACKUPAVG=.&DT_SOIL=1&SOIL_BACKUP1=.&SOIL_BACKUP2=.&SOIL_BACKUPAVG=.&",
                  "DT_WIND=1&WIND_BACKUP1=.&WIND_BACKUP2=.&WIND_BACKUPAVG=.&DT_RH=1&RH_BACKUP1=.&RH_BACKUP2=.&RH_BACKUPAVG=.&DT_ET=1&ET_BACKUP1=.&",
                  "ET_BACKUP2=.&ET_BACKUPAVG=.&DT_SOLAR=1&SOLAR_BACKUP1=.&SOLAR_BACKUP2=.&SOLAR_BACKUPAVG=.&UNITS=M&FFMT=T&ACTION=RETRIEVE+DATA",sep="")
    
    dat<-POST(url="http://169.237.140.1/calludt.cgi/WXDATAREPORT",body=string)
    weather<-content(dat,"text")
    record<-read.csv(textConnection(weather),allowEscapes = FALSE,skip=66,stringsAsFactors =FALSE)
    record<-record[which(!is.na(record[,2])),]

   if(is.data.frame(record))
     {record[,"Year"]<-as.numeric(sapply(as.character(record$Date),function(x) substr(x,1,4)))
      record[,"Month"]<-as.numeric(sapply(as.character(record$Date),function(x) substr(x,5,6)))
      record[,"Day"]<-as.numeric(sapply(as.character(record$Date),function(x) substr(x,7,8)))
      record<-make_all_day_table(record)}}
          
 return(list(database="UCIPM",weather=record))}
  
  #weather cleaning section
  
  if(is.list(action)) if(names(action)[1]=="database") # then we assume that this is a downloaded file to be cleaned
        {dw<-action$weather
        colnames(dw)[which(colnames(dw)=="min")]<-"Tmin"
        colnames(dw)[which(colnames(dw)=="Air.max")]<-"Tmax"
        colnames(dw)[which(colnames(dw)=="Precip")]<-"Prec"
        if(drop_most) dw<-dw[,c("Year","Month","Day","Tmin","Tmax","Prec")]
        return(list(database="UCIPM",weather=dw))}
  if(is.data.frame(action)) # then we assume that this is a downloaded file to be cleaned
        {dw<-action
        colnames(dw)[which(colnames(dw)=="min")]<-"Tmin"
        colnames(dw)[which(colnames(dw)=="Air.max")]<-"Tmax"
        colnames(dw)[which(colnames(dw)=="Precip")]<-"Prec"
        if(drop_most) dw<-dw[,c("Year","Month","Day","Tmin","Tmax","Prec")]
        return(dw)}
  
}
           
 
handle_chile_agromet<-function(action,location=NA,time_interval=NA,station_list=NULL,stations_to_choose_from=25,drop_most=TRUE)
{
  
  remove_missing<-function(tab,column,missing)
  {tab[which(tab[,column]==missing),column]<-NA
  return(tab)}
  
  if(is.character(action))  if(action=="list_stations")
        {
    if(!is.null(names(location)))
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
    
    
    if(is.null(station_list)) station_list<-read.table("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",sep=",")
    colnames(station_list)<-c("Airport ID","Name","City","Country","IATA",
                              "ICAO","Latitude","Longitude","Altitude","Timezone","DST","Tz database time zone")
    station_list[,"Altitude"]<-station_list[,"Altitude"]*0.3048
    stat_list<-station_list
    colnames(stat_list)[which(colnames(stat_list) %in% c("Longitude","Long"))]<-"Long"
    colnames(stat_list)[which(colnames(stat_list) %in% c("Latitude","Lat"))]<-"Lat"
    colnames(stat_list)[which(colnames(stat_list) %in% c("Altitude","Elev"))]<-"Elev"
    stat_list<-stat_list[which(!is.na(stat_list$Lat)&(!is.na(stat_list$Long))),]
    myPoint<-c(long,lat)
    stat_list[,"distance"]<-round(spDistsN1(as.matrix(stat_list[,c("Long","Lat")]), myPoint, longlat=TRUE),2)
    sorted_list<-stat_list[order(stat_list$distance),]
    if(!is.na(elev)) sorted_list[,"elevation_diff"]<-elev-sorted_list$Elev
    interval_end<-YEARMODA2Date(time_interval[2]*10000+1231)
    interval_start<-YEARMODA2Date(time_interval[1]*10000+0101)
    sorted_list<-sorted_list[which(!sorted_list$ICAO=="\\N"),]
    sorted_list<-sorted_list[1:max(stations_to_choose_from,500),]
    #sorted_list[,"Overlap_years"]<-
    #  apply(sorted_list,1,function (x) (as.numeric(difftime(
    #    sort(c(YEARMODA2Date(as.numeric(x["END"])),interval_end))[1],
    #    sort(c(YEARMODA2Date(as.numeric(x["BEGIN"])),interval_start))[2])+1)/(365+length(which(sapply(time_interval[1]:time_interval[2],leap_year)))/(time_interval[2]-time_interval[1]+1)    )))
    #sorted_list[which(sorted_list[,"Overlap_years"]<0),"Overlap_years"]<-0
    #sorted_list[,"Perc_interval_covered"]<-sorted_list[,"Overlap_years"]/(time_interval[2]-time_interval[1])*100
    sorted_list[,"chillR_code"]<-sorted_list$ICAO
    if(!is.na(elev))  sorted_list<-sorted_list[,c("chillR_code","Name","City","Country","Lat","Long","Elev",
                                                  "distance","elevation_diff")] else
                                                    sorted_list<-sorted_list[,c("chillR_code","Name","City","Country","Lat","Long",
                                                                                "distance")]         
    return(sorted_list[1:stations_to_choose_from,])}
  
  

 if(is.character(action)) if(action=="download_weather")
 {
   if(is.null(station_list)) station_list<-read.table("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",sep=",")
   colnames(station_list)<-c("Airport ID","Name","City","Country","IATA",
                             "ICAO","Latitude","Longitude","Altitude","Timezone","DST","Tz database time zone")
   station_list[,"Altitude"]<-station_list[,"Altitude"]*0.3048
   stat_list<-station_list
   colnames(stat_list)[which(colnames(stat_list) %in% c("Longitude","Long"))]<-"Long"
   colnames(stat_list)[which(colnames(stat_list) %in% c("Latitude","Lat"))]<-"Lat"
   colnames(stat_list)[which(colnames(stat_list) %in% c("Altitude","Elev"))]<-"Elev"
   stat_list<-stat_list[which(!is.na(stat_list$Lat)&(!is.na(stat_list$Long))),]
   if (!"chillR_code" %in% colnames(stat_list)) stat_list[,"chillR_code"]<-stat_list$ICAO
   if(location %in% stat_list$chillR_code) 
   {
   suppressWarnings(dir.create("chillRtempdirectory"))
   filepath <- "chillRtempdirectory/wunder.csv"
   startlisting=TRUE
   y=2005
   record<-NULL
   for(y in time_interval[1]:time_interval[2])
     for (m in 1:12)
     {
       Url <- paste("http://www.wunderground.com/history/airport/",location,"/",
                    y,"/",m,"/1/MonthlyHistory.html?HideSpecis=0&theprefset=SHOWMETAR&theprefvalue=1&format=1",sep="")
       ff<-suppressWarnings(try(download.file(Url,filepath)))
       if(ff==0)
         {rcsv<-read.csv("chillRtempdirectory/wunder.csv")
          colnames(rcsv)[1]<-"Date"
          if(startlisting) {record<-rcsv
          startlisting=FALSE} else record<-rbind(record,rcsv)}
     }
   closeAllConnections()
   file.remove("chillRtempdirectory/wunder.csv") 
   if(is.null(record)) {record<-NA; message("no data found")}
   if(!is.null(record)) if(nrow(record)==0) {record<-NA; message("no data found")}
   return(list(database="Wunderground",weather=record))} else 
   {warning("location does not match a record in the database. No records retrieved.")}
 }
   

   
   

  if(is.list(action)) if(names(action)[1]=="database") # then we assume that this is a downloaded file to be cleaned
      {dw<-action$weather
      colnames(dw)[which(colnames(dw)=="Mean.TemperatureC")]<-"Tmean"
      colnames(dw)[which(colnames(dw)=="Min.TemperatureC")]<-"Tmin"
      colnames(dw)[which(colnames(dw)=="Max.TemperatureC")]<-"Tmax"
      colnames(dw)[which(colnames(dw)=="Precipitationmm")]<-"Prec"
      dw[,"Year"]<-sapply(dw[,"Date"],function(x) strsplit(as.character(x),"-")[[1]][1])
      dw[,"Month"]<-sapply(dw[,"Date"],function(x) strsplit(as.character(x),"-")[[1]][2])      
      dw[,"Day"]<-sapply(dw[,"Date"],function(x) strsplit(as.character(x),"-")[[1]][3])       
      if(drop_most) dw<-dw[,c("Year","Month","Day","Tmin","Tmax","Tmean","Prec")]
      return(list(database="GSOD",weather=dw))}
      if(is.data.frame(action)) # then we assume that this is a downloaded file to be cleaned
      {dw<-action
      colnames(dw)[which(colnames(dw)=="Mean.TemperatureC")]<-"Tmean"
      colnames(dw)[which(colnames(dw)=="Min.TemperatureC")]<-"Tmin"
      colnames(dw)[which(colnames(dw)=="Max.TemperatureC")]<-"Tmax"
      colnames(dw)[which(colnames(dw)=="Precipitationmm")]<-"Prec"
      dw[,"Year"]<-sapply(dw[,"Date"],function(x) strsplit(as.character(x),"-")[[1]][1])
      dw[,"Month"]<-sapply(dw[,"Date"],function(x) strsplit(as.character(x),"-")[[1]][2])      
      dw[,"Day"]<-sapply(dw[,"Date"],function(x) strsplit(as.character(x),"-")[[1]][3])  
      if(drop_most) dw<-dw[,c("Year","Month","Day","Tmin","Tmax","Tmean","Prec")]
      return(dw)}
  
}


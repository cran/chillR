handle_gsod<-function(action,location=NA,time_interval=NA,station_list=NULL,stations_to_choose_from=25,drop_most=TRUE)
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
    
    
    if(is.null(station_list)) station_list<-read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
    stat_list<-station_list
    colnames(stat_list)[which(colnames(stat_list) %in% c("LON","Long"))]<-"Long"
    colnames(stat_list)[which(colnames(stat_list) %in% c("LAT","Lat"))]<-"Lat"
    colnames(stat_list)[which(colnames(stat_list) %in% c("ELEV.M.","Elev"))]<-"Elev"
    stat_list<-stat_list[which(!is.na(stat_list$Lat)&(!is.na(stat_list$Long))),]
    myPoint<-c(long,lat)
    stat_list[,"distance"]<-round(spDistsN1(as.matrix(stat_list[,c("Long","Lat")]), myPoint, longlat=TRUE),2)
    sorted_list<-stat_list[order(stat_list$distance),]
    if(!is.na(elev)) sorted_list[,"elevation_diff"]<-elev-sorted_list$Elev
    sorted_list<-sorted_list[1:max(stations_to_choose_from,500),]
    sorted_list[,"chillR_code"]<-paste(sorted_list$USAF,"_",sorted_list$WBAN,sep="")
    if(!is.na(time_interval[1]))
        {  
        interval_end<-YEARMODA2Date(time_interval[2]*10000+1231)
        interval_start<-YEARMODA2Date(time_interval[1]*10000+0101)
         sorted_list[,"Overlap_years"]<-round(
          apply(sorted_list,1,function (x) (as.numeric(difftime(
            sort(c(YEARMODA2Date(as.numeric(x["END"])),interval_end))[1],
            sort(c(YEARMODA2Date(as.numeric(x["BEGIN"])),interval_start))[2])+1)/(365+length(which(sapply(time_interval[1]:time_interval[2],leap_year)))/(time_interval[2]-time_interval[1]+1)))),2)
        sorted_list[which(sorted_list[,"Overlap_years"]<0),"Overlap_years"]<-0
        sorted_list[,"Perc_interval_covered"]<-round(sorted_list[,"Overlap_years"]/(time_interval[2]-time_interval[1]+1)*100,2)
        if(!is.na(elev))  sorted_list<-sorted_list[,c("chillR_code","STATION.NAME","CTRY","Lat","Long","Elev","BEGIN","END",
                                                  "distance","elevation_diff","Overlap_years","Perc_interval_covered")] else
                          sorted_list<-sorted_list[,c("chillR_code","STATION.NAME","CTRY","Lat","Long","BEGIN","END",
                                                  "distance","Overlap_years","Perc_interval_covered")]} else
        if(!is.na(elev))  sorted_list<-sorted_list[,c("chillR_code","STATION.NAME","CTRY","Lat","Long","Elev","BEGIN","END",
                                                  "distance","elevation_diff")] else
                          sorted_list<-sorted_list[,c("chillR_code","STATION.NAME","CTRY","Lat","Long","BEGIN","END",
                                                  "distance")]         
    return(sorted_list[1:stations_to_choose_from,])}
  
  

 if(is.character(action)) if(action=="download_weather")
 {if(is.null(station_list)) station_list<-read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
 stat_list<-station_list
 colnames(stat_list)[which(colnames(stat_list) %in% c("LON","Long"))]<-"Long"
 colnames(stat_list)[which(colnames(stat_list) %in% c("LAT","Lat"))]<-"Lat"
 colnames(stat_list)[which(colnames(stat_list) %in% c("ELEV.M.","Elev"))]<-"Elev"
 stat_list<-stat_list[which(!is.na(stat_list$Lat)&(!is.na(stat_list$Long))),]
 if (!"chillR_code" %in% colnames(stat_list)) stat_list[,"chillR_code"]<-paste(stat_list$USAF,"_",stat_list$WBAN,sep="")
 if(location %in% stat_list$chillR_code) 
 {
   #download data
   STN<-strsplit(location,"_")[[1]][1]
   WBAN<-strsplit(location,"_")[[1]][2]
   startlisting<-TRUE
   for(y in c(time_interval[1]:time_interval[2]))
   {
     URL<-paste("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/",y,"/",STN,"-",WBAN,"-",y,".op.gz",sep="")
     suppressWarnings(dir.create("chillRtempdirectory"))
     dest<-"chillRtempdirectory/weather.op.gz"
     ff<-suppressWarnings(try(download.file(URL, dest),silent = TRUE))
     if(ff==0)
     {gz <- gzfile(dest, open = "rt")
     close(gz)
     out<-suppressWarnings(read.fwf(gzfile(dest, open = "rt"), c(6,1,5,2,4,2,2,2,6,1,2,2,6,1,2,2,6,1,2,2,6,1,2,2,5,1,2,2,5,1,2,2,5,2,5,2,6,1,1,6,1,1,5,1,1,5,2,6), header = FALSE, sep = "\t",skip=1))
     closeAllConnections()
     file.remove("chillRtempdirectory/weather.op.gz") 
     # unlink(dest)
     colnames(out)<-c("STN---","space1","WBAN","space2","YEAR","MONTH","DAY","space3","TEMP","space4","Count1","space5","DEWP","space6","Count2",
                      "space7","SLP","space8","Count3","space9","STP","space10","Count4","space11","VISIB","space12","Count5","space13",
                      "WDSP","space14","Count6","space15","MXSPD","space16","GUST","space17","MAX","MaxFlag","space18",
                      "MIN","MinFlag","space19","PRCP","PrcpFlag","space20","SNDP","space21","FRSHTT")
     out<-out[,c("STN---","WBAN","YEAR","MONTH","DAY","TEMP","Count1","DEWP","Count2","SLP","Count3","STP","Count4","VISIB","Count5",
                 "WDSP","Count6","MXSPD","GUST","MAX","MaxFlag","MIN","MinFlag","PRCP","PrcpFlag","SNDP","FRSHTT")]
     out<-remove_missing(out,"DEWP",9999.9)
     out<-remove_missing(out,"SLP",9999.9)
     out<-remove_missing(out,"STP",9999.9)
     out<-remove_missing(out,"VISIB",999.9)
     out<-remove_missing(out,"WDSP",999.9) 
     out<-remove_missing(out,"MXSPD",999.9)  
     out<-remove_missing(out,"GUST",999.9) 
     out<-remove_missing(out,"MAX",9999.9) 
     out<-remove_missing(out,"MIN",9999.9) 
     out<-remove_missing(out,"PRCP",99.99)  
     out<-remove_missing(out,"SNDP",999.9) 
     out$TEMP<-(out$TEMP-32)/9*5
     out$MIN<-(out$MIN-32)/9*5
     out$MAX<-(out$MAX-32)/9*5
     out$PRCP<-out$PRCP*25.4
     if(startlisting) record<-out else record<-rbind(record,out)
     startlisting<-FALSE}
   }
   if(startlisting) {record<-NA
   warning("No weather data found for the time interval of interest.")}
   first_line<-record[1,]
   if(!((as.numeric(first_line["YEAR"])*10000+as.numeric(first_line["MONTH"])*100+as.numeric(first_line["DAY"]))==time_interval[1]*10000+101))
   {first_line[,]<-NA
   first_line[,c("STN---","WBAN","YEAR","MONTH","DAY")]<-c(STN,WBAN,time_interval[1],1,1)
   record<-rbind(first_line,record)}
   
   last_line<-record[nrow(record),]
   if(!((as.numeric(first_line["YEAR"])*10000+as.numeric(first_line["MONTH"])*100+as.numeric(first_line["DAY"]))==time_interval[2]*10000+1231))
   {last_line[,]<-NA
   last_line[,c("STN---","WBAN","YEAR","MONTH","DAY")]<-c(STN,WBAN,time_interval[2],12,31)
   record<-rbind(record,last_line)}
   record<-make_all_day_table(record)
   return(list(database="GSOD",weather=record))} else warning("location does not match a record in the database. No records retrieved.")
 }

  if(is.list(action)) if(names(action)[1]=="database") # then we assume that this is a downloaded file to be cleaned
      {dw<-action$weather
      colnames(dw)[which(colnames(dw)=="TEMP")]<-"Tmean"
      colnames(dw)[which(colnames(dw)=="MIN")]<-"Tmin"
      colnames(dw)[which(colnames(dw)=="MAX")]<-"Tmax"
      colnames(dw)[which(colnames(dw)=="PRCP")]<-"Prec"
      colnames(dw)[which(colnames(dw)=="YEAR")]<-"Year"
      colnames(dw)[which(colnames(dw)=="MONTH")]<-"Month"
      colnames(dw)[which(colnames(dw)=="DAY")]<-"Day"
      if(drop_most) dw<-dw[,c("Year","Month","Day","Tmin","Tmax","Tmean","Prec")]
      return(list(database="GSOD",weather=dw))}
      if(is.data.frame(action)) # then we assume that this is a downloaded file to be cleaned
      {dw<-action
        colnames(dw)[which(colnames(dw)=="TEMP")]<-"Tmean"
      colnames(dw)[which(colnames(dw)=="MIN")]<-"Tmin"
      colnames(dw)[which(colnames(dw)=="MAX")]<-"Tmax"
      colnames(dw)[which(colnames(dw)=="PRCP")]<-"Prec"
      colnames(dw)[which(colnames(dw)=="YEAR")]<-"Year"
      colnames(dw)[which(colnames(dw)=="MONTH")]<-"Month"
      colnames(dw)[which(colnames(dw)=="DAY")]<-"Day"
      if(drop_most) dw<-dw[,c("Year","Month","Day","Tmin","Tmax","Tmean","Prec")]
      return(dw)}
  
}


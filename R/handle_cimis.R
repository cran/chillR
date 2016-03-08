handle_cimis<-function(action,location=NA,time_interval=NA,station_list=NULL,stations_to_choose_from=25,drop_most=TRUE)
{
 #station list section
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
        
        suppressWarnings(dir.create("chillRtempdirectory"))
        download.file("ftp://ftpcimis.water.ca.gov/pub2/CIMIS Stations List (May 2015).xlsx", "chillRtempdirectory/a.xlsx", mode="wb")
        cimis<-suppressWarnings(read_excel("chillRtempdirectory/a.xlsx"))
        closeAllConnections()
        file.remove("chillRtempdirectory/a.xlsx") 
        unlink("chillRtempdirectory",recursive=TRUE)
        
        cimis<-cimis[which(!is.na(cimis[,1])),]
        cimis[which((!cimis$STN_DISCONNECT_DATE=="Active")),"end"]<-as.numeric(cimis[which((!cimis$STN_DISCONNECT_DATE=="Active")),"STN_DISCONNECT_DATE"])
        
        cimis$end<-as.Date(cimis$end,origin="1899/12/30")
        cimis$end[which(is.na(cimis$end))]<-Sys.time()
        cimis<-cimis[,c(1:6,8,10)]
        colnames(cimis)<-c("Stat_num","Name","County","Latitude","Longitude","Elevation","Start_date","End_date")
        cimis$Elevation<-cimis$Elevation*0.3048
        myPoint<-c(long,lat)
        cimis[,"distance"]<-round(spDistsN1(as.matrix(cimis[,c("Longitude","Latitude")]), myPoint, longlat=TRUE),2)
        sorted_list<-cimis[order(cimis$distance),]
        if(!is.na(elev)) sorted_list[,"elevation_diff"]<-elev-sorted_list$Elevation
        sorted_list[,"chillR_code"]<-as.character(sorted_list$Stat_num)       
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
            if(!is.na(elev))  sorted_list<-sorted_list[,c("chillR_code","Name","County","Latitude","Longitude","Elevation","Start_date","End_date",
                                                          "distance","elevation_diff","Overlap_years","Perc_interval_covered")] else
                                                            sorted_list<-sorted_list[,c("chillR_code","Name","County","Latitude","Longitude","Start_date","End_date",
                                                                                        "distance","Overlap_years","Perc_interval_covered")]} else
            if(!is.na(elev))  sorted_list<-sorted_list[,c("chillR_code","Name","County","Latitude","Longitude","Elevation","Start_date","End_date",
                                                      "distance","elevation_diff")] else
                                           sorted_list<-sorted_list[,c("chillR_code","Name","County","Latitude","Longitude","Start_date","End_date",
                                                                      "distance")]         
        return(sorted_list[1:stations_to_choose_from,])}

  
  #weather download section
  
if(is.character(action)) if(action=="download_weather")
  {

  #first define a download handling function to make piecing the record together easier
  download_and_process<-function(URL,y,numm,sixdays=NA)
  {suppressWarnings(dir.create("chillRtempdirectory"))
    if(!is.data.frame(sixdays))
     {
      dest<-"chillRtempdirectory/weather.zip"
      ff<-suppressWarnings(try(download.file(URL, dest),silent = TRUE))
      if(ff==0)
      {gz <- gzfile(dest, open = "rt")
      ziplist<-unzip(dest,list=TRUE)
      if(numm %in% sapply(ziplist[,1],function(x) substr(x,nchar(x)-6,nchar(x)-4)))
      {unzip(dest,files=paste(y,"daily",numm,".csv",sep=""),exdir="chillRtempdirectory")         
        yweath<-read.csv(paste("chillRtempdirectory/",y,"daily",numm,".csv",sep=""))
        file.remove(paste("chillRtempdirectory/",y,"daily",numm,".csv",sep=""))
        closeAllConnections()
        file.remove(dest)
      } else yweath<-NA
      } else yweath<-NA
     } else yweath<-sixdays
        
      if(!is.numeric(y)) y<-format(Sys.time(), "%Y")
      if(is.data.frame(yweath))
      {if(y>2013) colnames(yweath)<-c("Station Id","Date","Julian Date","Reference ETo","QC for Reference ETo","Precipitation",
                                       "QC for Precipitation","Solar Radiation Average","QC for Solar Radiation Average","Average Vapor Pressure",
                                       "QC for Average Vapor Pressure","Maximum Air Temperature","QC for Maximum Air Temperature","Minimum Air Temperature",
                                       "QC for Minimum Air Temperature","Average Air Temperature","QC for Average Air Temperature","Maximum Relative Humidity",
                                       "QC for Maximum Relative Humidity","Minimum Relative Humidity","QC for Minimum Relative Humidity","Average Relative Humidity",
                                       "QC for Average Relative Humidity","Dew Point","QC for Dew Point","Average Wind Speed","QC for Average Wind Speed",
                                       "Wind Run","QC for Wind Run","Average Soil Temperature","QC for Average Soil Temperature")
        
       if(y<2014) colnames(yweath)<-c("Station Id","Date","Julian Date","QC for Solar Radiation Average","Solar Radiation Average","QC for Average Soil Temperature",
                                       "Average Soil Temperature","QC for Maximum Air Temperature","Maximum Air Temperature","QC for Minimum Air Temperature",
                                       "Minimum Air Temperature","QC for Average Air Temperature","Average Air Temperature","QC for Average Vapor Pressure",
                                       "Average Vapor Pressure","QC for Average Wind Speed","Average Wind Speed","QC for Precipitation","Precipitation",
                                       "QC for Maximum Relative Humidity","Maximum Relative Humidity","QC for Minimum Relative Humidity","Minimum Relative Humidity",
                                       "QC for Reference ETo","Reference ETo","QC for Average Relative Humidity","Average Relative Humidity","QC for Dew Point",
                                       "Dew Point","QC for Wind Run","Wind Run")
       yweath[,c("Reference ETo","Precipitation","Solar Radiation Average","Average Vapor Pressure",
                  "Maximum Air Temperature","Minimum Air Temperature",
                  "Average Air Temperature","Maximum Relative Humidity",
                  "Minimum Relative Humidity","Average Relative Humidity",
                  "Dew Point","Average Wind Speed",
                  "Wind Run","Average Soil Temperature")]<-suppressWarnings(apply(yweath[,c("Reference ETo","Precipitation","Solar Radiation Average","Average Vapor Pressure",
                                                                                            "Maximum Air Temperature","Minimum Air Temperature",
                                                                                            "Average Air Temperature","Maximum Relative Humidity",
                                                                                            "Minimum Relative Humidity","Average Relative Humidity",
                                                                                            "Dew Point","Average Wind Speed",
                                                                                            "Wind Run","Average Soil Temperature")],2,function(x) as.numeric(as.character(x))))
      }
      unlink("chillRtempdirectory",recursive=TRUE)
      return(yweath)
  } # end of function definition, now on to processing
  

   if(is.null(station_list)) 
          {suppressWarnings(dir.create("chillRtempdirectory"))
          download.file("ftp://ftpcimis.water.ca.gov/pub2/CIMIS Stations List (May 2015).xlsx", "chillRtempdirectory/a.xlsx", mode="wb")
          station_list<-suppressWarnings(read_excel("chillRtempdirectory/a.xlsx"))
          closeAllConnections()
          file.remove("chillRtempdirectory/a.xlsx")
          unlink("chillRtempdirectory",recursive=TRUE)
   }
 
   station_list<-station_list[which(!is.na(station_list[,1])),]
   #station_list[,"chillR_code"]<-as.character(station_list$"Station Number")
   station_list[,"chillR_code"]<-as.character(station_list[,grep("Station",colnames(station_list))])
   if(location %in% station_list$chillR_code) 
      { num<-as.numeric(location)
        if(num>99) numm<-as.character(num) else
          if(num>9) numm<-paste("0",num,sep="") else
            numm<-paste("00",num,sep="") 
        
           startlisting<-TRUE
           for(y in time_interval[1]:time_interval[2]) #download annual data
             {if(!y==format(Sys.time(), "%Y"))  #the current year is only available as monthly files, see below
               {URL<-paste("ftp://ftpcimis.water.ca.gov/pub2/annualMetric/dailyStns",y,".zip",sep="")
                yweath<-download_and_process(URL,y,numm)
                 if (is.data.frame(yweath))
                   if(startlisting) {record<-yweath
                                     startlisting<-FALSE} else record<-rbind(record,yweath)}
             
               if(y==format(Sys.time(), "%Y")) #here the current year is processed
                 {current_month<-as.numeric(format(Sys.time(), "%m"))
                  for(m in 1:(current_month-1))
                  {mmm<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")[m]
                    URL<-paste("ftp://ftpcimis.water.ca.gov/pub2/monthlyMetric/dailyStns",mmm,".zip",sep="")
                    mweath<-download_and_process(URL,mmm,numm)
                    if (is.data.frame(mweath))
                      if(as.numeric(strsplit(as.character(mweath[1,2]),"/")[[1]][3])==format(Sys.time(), "%Y"))
                       {if(startlisting) {record<-mweath
                        startlisting<-FALSE} else {
                          mweath<-mweath[which(!mweath[,2] %in% record[,2]),]
                          record<-rbind(record,mweath)}}}
                      
                    #NOW ONLY THE LAST 6 DAYS

                     sixdays<-read.table(paste("ftp://ftpcimis.water.ca.gov/pub2/dailyMetric/dlymet",numm,".csv",sep=""),sep=",")
                     sixdays<-download_and_process(NA,format(Sys.time(), "%Y"),NA,sixdays)
                     if(startlisting) {record<-sixdays
                     startlisting<-FALSE} else {
                       sixdays<-sixdays[which(!sixdays[,2] %in% record[,2]),]
                       record<-rbind(record,sixdays)}}}
           
           if (startlisting) {record<-NA
                              warning("No weather data found for the time interval of interest.")}
           
      } else {record<-NA
              warning("No weather data found for this station.")}
            
   if(is.data.frame(record))
     {record[,"Year"]<-as.numeric(sapply(strsplit(as.character(record$Date),"/"),function(x) x[3]))
      record[,"Month"]<-as.numeric(sapply(strsplit(as.character(record$Date),"/"),function(x) x[1]))
      record[,"Day"]<-as.numeric(sapply(strsplit(as.character(record$Date),"/"),function(x) x[2]))
      
      first_line<-record[1,]
      if(!((as.numeric(first_line["Year"])*10000+as.numeric(first_line["Month"])*100+as.numeric(first_line["Day"]))==time_interval[1]*10000+101))
        {first_line[,]<-NA
         first_line[,c("Station Id","Year","Month","Day")]<-c(numm,time_interval[1],1,1)
         record<-rbind(first_line,record)}
          
      last_line<-record[nrow(record),]
      if(!((as.numeric(last_line["Year"])*10000+as.numeric(last_line["Month"])*100+as.numeric(last_line["Day"]))==time_interval[2]*10000+1231))
        {last_line[,]<-NA
        last_line[,c("Station Id","Year","Month","Day")]<-c(numm,time_interval[2],12,31)
        record<-rbind(record,last_line)}
      
      record<-make_all_day_table(record)}
          
 return(list(database="CIMIS",weather=record))}
  
  #weather cleaning section
  
  if(is.list(action)) if(names(action)[1]=="database") # then we assume that this is a downloaded file to be cleaned
        {dw<-action$weather
        colnames(dw)[which(colnames(dw)=="Average Air Temperature")]<-"Tmean"
        colnames(dw)[which(colnames(dw)=="Minimum Air Temperature")]<-"Tmin"
        colnames(dw)[which(colnames(dw)=="Maximum Air Temperature")]<-"Tmax"
        colnames(dw)[which(colnames(dw)=="Precipitation")]<-"Prec"
        if(drop_most) dw<-dw[,c("Year","Month","Day","Tmin","Tmax","Tmean","Prec")]
        return(list(database="CIMIS",weather=dw))}
  if(is.data.frame(action)) # then we assume that this is a downloaded file to be cleaned
        {dw<-action
        colnames(dw)[which(colnames(dw)=="Average Air Temperature")]<-"Tmean"
        colnames(dw)[which(colnames(dw)=="Minimum Air Temperature")]<-"Tmin"
        colnames(dw)[which(colnames(dw)=="Maximum Air Temperature")]<-"Tmax"
        colnames(dw)[which(colnames(dw)=="Precipitation")]<-"Prec"
        if(drop_most) dw<-dw[,c("Year","Month","Day","Tmin","Tmax","Tmean","Prec")]
        return(dw)}
  
}
           
 
#' List, download or convert to chillR format data from the Global Summary of
#' the Day database
#' 
#' This function can do three things related to the Global Summary of the Day
#' ("GSOD") database from the National Climatic Data Centre (NCDC) of the
#' National Oceanic and Atmospheric Administration (NOAA): 1. it can list
#' stations that are close to a specified position (geographic coordinates) 2.
#' it can retrieve weather data for a named weather station 3. it can 'clean'
#' downloaded data, so that they can easily be used in chillR Which of these
#' functions is carried out depends on the action argument.
#' 
#' This function can run independently, but it is also called by the
#' get_weather and weather2chillR functions, which some users might find a bit
#' easier to handle.
#' 
#' the GSOD database is described here:
#' https://data.noaa.gov/dataset/global-surface-summary-of-the-day-gsod
#' 
#' under the 'list_stations' mode, several formats are possible for specifying
#' the location vector, which can consist of either two or three coordinates
#' (it can include elevation). Possible formats include c(1,2,3), c(1,2),
#' c(x=1,y=2,z=3), c(lat=2,long=1,elev=3). If elements of the vector are not
#' names, they are interpreted as c(Longitude, Latitude, Elevation).
#' 
#' The 'chillRCode' is generated by this function, when it is run with
#' geographic coordinates as location inputs. In the list of nearby stations
#' that is returned then, the chillRCode is provided and can then be used as
#' input for running the function in 'downloading' mode. For downloading the
#' data, use the same call as before but replace the location argument with the
#' chillRCode.
#' 
#' @param action if this is the character string "list_stations", the function
#' will return a list of the weather stations from the database that are
#' closest to the geographic coordinates specified by location.  if this is the
#' character string "download_weather", the function will attempt to download
#' weather data from the database for the station named by the location
#' argument, which should then be a character string corresponding to the
#' chillRcode of the station (which you can get by running this function in
#' 'list_stations mode) if this is a downloaded weather file (downloaded by
#' running this function in 'download weather' mode), the function cleans the
#' file and makes it ready for use in chillR. If the input is just a dataframe
#' (not a list, as produced with this function), you have to specify the
#' database name with the database argument
#' @param location either a vector of geographic coordinates (for the
#' 'list_stations' mode), or the 'chillRcode' of a weather station in the
#' specified database (for the 'download_weather' mode). When running this
#' function for data cleaning only, this is not needed.
#' @param time_interval numeric vector with two elements, specifying the start
#' and end date of the period of interest. Only required when running in
#' 'list_stations' or 'download weather' mode
#' @param station_list if the list of weather stations has already been
#' downloaded, the list can be passed to the function through this argument.
#' This can save a bit of time, since it can take a bit of time to download the
#' list, which can have several MB.
#' @param stations_to_choose_from if the location is specified by geographic
#' coordinates, this argument determines the number of nearby stations in the
#' list that is returned.
#' @param drop_most boolean variable indicating if most columns should be
#' dropped from the file. If set to TRUE (default), only essential columns for
#' running chillR functions are retained.
#' @param end_at_present boolean variable indicating whether the interval of
#' interest should end on the present day, rather than extending until the end
#' of the year specified under time_interval[2] (if time_interval[2] is the
#' current year).
#' @return The output depends on the action argument. If it is 'list_stations',
#' the function returns a list of station_to_choose_from weather stations that
#' are close to the specified location. This list also contains information
#' about how far away these stations are (in km), how much the elevation
#' difference is (if elevation is specified; in m) and how much overlap there
#' is between the data contained in the database and the time period specified
#' by time_interval. If action is 'download_weather' the output is a list of
#' two elements: 1. database="GSOD" 2. the downloaded weather record, extended
#' to the full duration of the specified time interval. If action is a weather
#' data.frame or a weather record downloaded with this function (in
#' 'download_weather' mode), the output is the same data in a format that is
#' easy to use in chillR. If drop_most was set to TRUE, most columns are
#' dropped.
#' @note Many databases have data quality flags, which may sometimes indicate
#' that data aren't reliable. These are not considered by this function!
#' 
#' For many places, the GSOD database is quite patchy, and the length of the
#' record indicated in the summary file isn't always very useful (e.g. there
#' could only be two records for the first and last date). Files are downloaded
#' by year, so if we specify a long interval, this may take a bit of time.
#' 
#' @importFrom R.utils gunzip
#' 
#' @author Eike Luedeling
#' @references The chillR package:
#' 
#' Luedeling E, Kunz A and Blanke M, 2013. Identification of chilling and heat
#' requirements of cherry trees - a statistical approach. International Journal
#' of Biometeorology 57,679-689.
#' @keywords utilities
#' @examples
#' 
#' #stat_list<-handle_gsod(action="list_stations",location=c(x=-122,y=38.5),
#' #  time_interval=c(2002,2002))
#' #the line above takes longer to run than CRAN allows for examples. The line below therefore
#' #generates an abbreviated stat_list that allows running the code.
#' # stat_list<-data.frame(chillR_code=c("724828_99999","724828_93241","720576_174"),
#' #   Lat=c(38.383,38.378,38.533),Long=c(-121.967,-121.958,-121.783),
#' #   BEGIN=c(20010811,20060101,20130101),END=c(20051231,20160110,20160109))
#' 
#' # gw<-handle_gsod(action="download_weather",location="724828_93241",time_interval=c(2012,2012),
#' #  station_list = stat_list)
#' # weather<-handle_gsod(gw)$weather
#' # make_chill_plot(tempResponse(stack_hourly_temps(fix_weather(weather)),Start_JDay=300,End_JDay=50),
#' #                "Chill_Portions",start_year=2010,end_year=2012,metriclabel="Chill Portions",
#' #                 misstolerance = 50)
#' 
#' @export handle_gsod
handle_gsod<-function(action,location=NA,time_interval=NA,station_list=NULL,stations_to_choose_from=25,drop_most=TRUE,end_at_present=TRUE)
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
    
    cn<-colnames(stat_list)
    if (is.null(cn)|!((("USAF" %in% cn & "WBAN" %in% cn)|"chillR_code" %in% cn) &
          "STATION.NAME" %in% cn & "CTRY" %in% cn & "BEGIN" %in% cn & "END" %in% cn &
          ("LON" %in% cn|"Long" %in% cn) & ("LAT" %in% cn|"Lat" %in% cn)))
      {warning("Station list not according to expectations. Downloading list from NCDC.")
      stat_list<-read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")}
      
 
    colnames(stat_list)[which(colnames(stat_list) %in% c("LON","Long"))]<-"Long"
    colnames(stat_list)[which(colnames(stat_list) %in% c("LAT","Lat"))]<-"Lat"
    colnames(stat_list)[which(colnames(stat_list) %in% c("ELEV.M.","Elev"))]<-"Elev"
    stat_list<-stat_list[which(!is.na(stat_list$Lat)&(!is.na(stat_list$Long))),]
    myPoint<-c(long,lat)
    stat_list[,"distance"]<-round(spDistsN1(as.matrix(stat_list[,c("Long","Lat")]), myPoint, longlat=TRUE),2)
    sorted_list<-stat_list[order(stat_list$distance),]
    if(!is.na(elev)) sorted_list[,"elevation_diff"]<-elev-sorted_list$Elev
    sorted_list<-sorted_list[1:max(stations_to_choose_from,500),]
    if(!"chillR_code" %in% colnames(sorted_list)) sorted_list[,"chillR_code"]<-paste(sorted_list$USAF,"_",sorted_list$WBAN,sep="")
    sorted_list[,"Start_date"]<-YEARMODA2Date(sorted_list$BEGIN)
    sorted_list[,"End_date"]<-YEARMODA2Date(sorted_list$END)
    if(!is.na(time_interval[1]))
        {
        interval_end<-YEARMODA2Date(time_interval[2]*10000+1231)
        interval_start<-YEARMODA2Date(time_interval[1]*10000+0101)
        if(end_at_present) interval_end<-min(interval_end,ISOdate(format(Sys.Date(),"%Y"),format(Sys.Date(),"%m"),
                                                                  format(Sys.Date(),"%d")))
        overlap_days<-apply(sorted_list,1,function (x) (as.numeric(difftime(
          sort(c(x["End_date"],format(interval_end,"%Y-%m-%d")))[1],
          sort(c(x["Start_date"],format(interval_start,"%Y-%m-%d")))[2])+1)))
        sorted_list[,"Overlap_years"]<-round(
          apply(sorted_list,1,function (x) (as.numeric(difftime(
            sort(c(x["End_date"],format(interval_end,"%Y-%m-%d")))[1],
            sort(c(x["Start_date"],format(interval_start,"%Y-%m-%d")))[2])+1)/(365+length(which(sapply(time_interval[1]:time_interval[2],leap_year)))/(time_interval[2]-time_interval[1]+1)))),2)
        sorted_list[which(sorted_list[,"Overlap_years"]<0),"Overlap_years"]<-0
        overlap_days[which(overlap_days<0)]<-0
        sorted_list[,"Perc_interval_covered"]<-round(overlap_days/as.numeric(interval_end-interval_start+1)*100,2)
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
 {
   
   if(is.null(station_list)) station_list<-read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
   stat_list<-station_list
   
   cn<-colnames(stat_list)
   if (is.null(cn)|!((("USAF" %in% cn & "WBAN" %in% cn)|"chillR_code" %in% cn) &
                     "BEGIN" %in% cn & "END" %in% cn &
                     ("LON" %in% cn|"Long" %in% cn) & ("LAT" %in% cn|"Lat" %in% cn)))
   {warning("Station list not according to expectations. Downloading list from NCDC.")
     stat_list<-read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")}
   
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
     {gz <- R.utils::gunzip(dest)     #gzfile(dest, open = "rt")
      out<-suppressWarnings(read.fwf(gz, c(6,1,5,2,4,2,2,2,6,1,2,2,6,1,2,2,6,1,2,2,6,1,2,2,5,1,2,2,5,1,2,2,5,2,5,2,6,1,1,6,1,1,5,1,1,5,2,6), header = FALSE, sep = "\t",skip=1))
      file.remove(gz)
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
   warning("No weather data found for the time interval of interest.")
   return(record)}
   record<-record[which(record$YEAR %in% c(time_interval[1]:time_interval[2])),]
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
   record[,"Year"]<-as.numeric(record[,"YEAR"])
   record[,"Month"]<-as.numeric(record[,"MONTH"])
   record[,"Day"]<-as.numeric(record[,"DAY"])
   
   record<-make_all_day_table(record,no_variable_check=TRUE)
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
      for (cc in c("Year","Month","Day","Tmin","Tmax","Tmean","Prec"))
           dw[,cc]<-as.numeric(dw[,cc])
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
      for (cc in c("Year","Month","Day","Tmin","Tmax","Tmean","Prec"))
        dw[,cc]<-as.numeric(dw[,cc])
      
      dw<-make_all_day_table(dw,no_variable_check=TRUE)
      return(dw)}

}


get_weather<-function(location,time_interval=NA,database="UCIPM",station_list=NULL,stations_to_choose_from=25)
{
  if(length(location)==2|length(location==3)) #determine whether location is specified by coordinates. If so move to station selection function
    if(is.numeric(location[1])&is.numeric(location[2]))
    {
     if(database=="GSOD")   
       sorted_list<-handle_gsod("list_stations",location=location,time_interval=time_interval,station_list=NULL,stations_to_choose_from=25)

     if(database=="CIMIS")  
        sorted_list<-handle_cimis("list_stations",location=location,time_interval=time_interval,station_list=NULL,stations_to_choose_from=25)
        
     if(database=="Wunderground")  
       sorted_list<-handle_wunderground("list_stations",location=location,time_interval=time_interval,station_list=NULL,stations_to_choose_from=25)
 
     if(database=="UCIPM")  
       sorted_list<-handle_ucipm("list_stations",location=location,time_interval=time_interval,stations_to_choose_from=25)
     
     
      #Chile, WeatherUnderground

      return(sorted_list[1:stations_to_choose_from,])

 
    }
  
  #if location is a string, we'll test if it belongs to a station in the database and, if so, try to retrieve weather data
  if(length(location)==1&is.character(location))
  {

    if(database=="GSOD")
      return(handle_gsod("download_weather",location=location,time_interval=time_interval,station_list=station_list))
    
    if(database=="CIMIS")
      return(handle_cimis("download_weather",location=location,time_interval=time_interval,station_list=station_list))
  
    if(database=="Wunderground")
      return(handle_wunderground("download_weather",location=location,time_interval=time_interval,station_list=station_list))

    if(database=="UCIPM")
      return(handle_ucipm("download_weather",location=location,time_interval=time_interval,station_list=station_list))
    
    
    }
  
}
           
 
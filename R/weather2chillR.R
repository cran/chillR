weather2chillR<-function(downloaded_weather,database="GSOD",drop_most=TRUE)
{dw<-downloaded_weather
if(is.list(dw)) if(names(dw)[1]=="database") database=dw$database

if(database=="GSOD")  
  return(handle_gsod(dw,drop_most=drop_most))
 
  if(database=="CIMIS")
      return(handle_cimis(dw,drop_most=drop_most))

if(database=="Wunderground")
  return(handle_wunderground(dw,drop_most=drop_most))

if(database=="UCIPM")
  return(handle_ucipm(dw,drop_most=drop_most))


}

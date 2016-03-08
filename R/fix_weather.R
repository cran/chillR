fix_weather<-function(weather,start_year=0,end_year=3000,start_date=1,end_date=366,columns=c("Tmin","Tmax"))
{
    QC_weather<-function(fixedweather,start_date,end_date,columns)
    {     fixedweather[,"JDay"]<-as.numeric(ISOdate(fixedweather$Year,fixedweather$Month,fixedweather$Day)-ISOdate(fixedweather$Year-1,12,31))
    if(start_date<end_date) {fixedweather[which(fixedweather$JDay>=start_date&fixedweather$JDay<=end_date),"sea"]<-
      fixedweather[which(fixedweather$JDay>=start_date&fixedweather$JDay<=end_date),"Year"]} else
      {fixedweather[which(fixedweather$JDay>=start_date),"sea"]<-
        fixedweather[which(fixedweather$JDay>=start_date),"Year"]+1
      fixedweather[which(fixedweather$JDay<=end_date),"sea"]<-
        fixedweather[which(fixedweather$JDay<=end_date),"Year"]}
    
    if(start_date<end_date) {relevant_days<-start_date:end_date} else
    {relevant_days<-c(start_date:366,1:end_date)}
    seasons<-unique(fixedweather$sea)
    seasons<-seasons[!is.na(seasons)]
 
    if(start_date<1) start_date<-1
    if(end_date<1) end_date<-1
    
    for (sea in seasons)
    {if(leap_year(sea)) {if(end_date>366) end_date_sea<-366 else end_date_sea<-end_date} else 
      {if(end_date>365) end_date_sea<-365 else end_date_sea<-end_date}
     if(leap_year(sea)) {if(start_date>366) start_date_sea<-366 else start_date_sea<-start_date} else
       {if(start_date>365) start_date_sea<-365 else start_date_sea<-start_date}
      if(start_date<end_date) sea_days<-as.numeric(ISOdate(sea-1,12,31)+end_date_sea*86400-(ISOdate(sea-1,12,31)+start_date_sea*86400))+1 else
      sea_days<-as.numeric(ISOdate(sea-1,12,31)+end_date_sea*86400-(ISOdate(sea-2,12,31)+start_date_sea*86400))+1

    if(sea==seasons[1])
      {gaps<-data.frame(Season=paste(sea-1,"/",sea,sep=""),End_year=sea,Season_days=sea_days,
                        Data_days=length(which(fixedweather$sea==sea)))
       for (ccc in columns) gaps[1,paste("Missing_",ccc,sep="")]<-length(which(fixedweather[which(fixedweather$sea==sea),paste("no_",ccc,sep="")]))+gaps$Season_days[1]-gaps$Data_days[1]
       if(length(columns)==1) gaps[1,"Incomplete_days"]<-max(fixedweather[which(fixedweather$sea==sea),paste("no_",columns,sep="")])+gaps$Season_days[1]-gaps$Data_days[1] else
         gaps[1,"Incomplete_days"]<-max(colSums(fixedweather[which(fixedweather$sea==sea),paste("no_",columns,sep="")]))+gaps$Season_days[1]-gaps$Data_days[1]
       
      # gaps[1,"Incomplete_days"]<-length(which(fixedweather[which(fixedweather$sea==sea),"no_Tmin"]|fixedweather[which(fixedweather$sea==sea),"no_Tmax"]))
                        
      } else
        {df<-data.frame(Season=paste(sea-1,"/",sea,sep=""),End_year=sea,Season_days=sea_days,
                      Data_days=length(which(fixedweather$sea==sea)))
        for (ccc in columns) df[1,paste("Missing_",ccc,sep="")]<-length(which(fixedweather[which(fixedweather$sea==sea),paste("no_",ccc,sep="")]))+df$Season_days[1]-df$Data_days[1]
        if(length(columns)==1) df[1,"Incomplete_days"]<-max(fixedweather[which(fixedweather$sea==sea),paste("no_",columns,sep="")])+df$Season_days[1]-df$Data_days[1] else
          df[1,"Incomplete_days"]<-max(colSums(fixedweather[which(fixedweather$sea==sea),paste("no_",columns,sep="")]))+df$Season_days[1]-df$Data_days[1]
        gaps<-rbind(gaps,df)}
      }
    gaps[,"Perc_complete"]<-round((gaps$Season_days-gaps$Incomplete_days)/gaps$Season_days*100,1)
    
    return(gaps)     
    }      
  
    if((length(names(weather))==2) & ("weather" %in% names(weather))) weather<-weather$weather
    
 fixedweather<-make_all_day_table(weather[which((weather$Year>=start_year)&(weather$Year<=end_year)),])
 for(ccc in columns)
 {interp<-interpolate_gaps(fixedweather[,ccc])
  fixedweather[,ccc]<-interp$interp
  fixedweather[,paste("no_",ccc,sep="")]<-interp$missing
 }

 return(list(weather=fixedweather,QC=QC_weather(fixedweather,start_date,end_date,columns)))
}
  

  

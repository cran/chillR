#' Patch gaps in daily weather records - updated
#' 
#' This is the successor function of [chillR::patch_daily_temperatures], which will no
#' longer be updated.
#' 
#' The patch_daily_temps function uses auxiliary data sources to fill gaps in daily weather data.
#' It can accommodate multiple sources of auxiliary information, which are used
#' in the user-specified sequence. There have to be some overlapping records for
#' this to work, because without bias correction, this procedure could produce
#' erroneous records. Bias correction is done by computing the mean difference
#' between main and auxiliary data for each variable and adjusting for it in
#' filling the gaps. You can specify a maximum mean bias and a maximum standard
#' deviation of the bias to exclude unsuitable records that aren't similar
#' enough to the original data.
#' When patching records, the function breaks the calendar year down into smaller
#' intervals that can be specified with the 'time_interval' parameter (this was not
#' possible in [chillR::patch_daily_temperatures], but is recommended for accurate results).
#' 
#' @param weather chillR-compatible weather record to be patched
#' @param patch_weather list of chillR-compatible weather records to be used for
#' patching holes in weather. They are used sequentially, until all have been used
#' or until there are no holes left.
#' @param vars vector of column names to be considered in patching. Defaults to
#' c("Tmin","Tmax"), the most common variables in chillR applications.
#' @param max_mean_bias maximum mean bias of auxiliary data compared to the original
#' dataset (applied to all variables in vars). If this threshold is exceeded, the
#' respective variable from that particular dataset will not be used. Defaults to NA,
#' meaning no records are excluded.
#' @param max_stdev_bias maximum standard deviation of the bias in the auxiliary
#' data compared to the original dataset (applied to all variables in vars). If this
#' threshold is exceeded, the respective variable from that particular dataset will not
#' be used. Defaults to NA, meaning no records are excluded.
#' @param time_interval time interval for which mean bias and standard deviation of the
#' bias are to be evaluated. This defaults to "month", which means that the function
#' looks for overlapping days between weather and patch_weather for each calendar month.
#' Bias correction is then also done on a monthly basis. `time_interval` can also assume
#' other values, such as 'week' or '2 weeks'.
#' @return list of two elements: weather (the patched weather record, with additional
#' columns specifying the data source for each value) and statistics (containing
#' data.frames for each element of patch_weather that indicate the mean bias, the
#' number of values that were filled from this source and the number of missing records
#' that remained after exhausting this auxiliary data source.)
#' @author Eike Luedeling
#' @keywords temperature gap-filling
#' @examples
#' 
#' gap_weather<-KA_weather[1:100,]
#' gap_weather[c(3,4,7:15,20,22:25,27:28,35:45,55,67,70:75,80:88,95:97),"Tmin"]<-NA
#' gap_weather[c(10:25,30,36:44,50,57,65,70:80,86,91:94),"Tmax"]<-NA
#' p1<-KA_weather[65:95,]
#' p1$Tmin<-p1$Tmin-2
#' p2<-KA_weather[c(15:40,60:80),]
#' p2$Tmax<-p2$Tmax+3
#' p3<-KA_weather[12:35,]
#' p3$Tmax<-p3$Tmax-2
#' p4<-KA_weather
#' p4$Tmax<-p4$Tmax+0.5
#' patch_weather<-list(stat1=p1,st2=p2,home=p3,last=p4)
#' 
#' patch_daily_temps(gap_weather,patch_weather)
#' 
#' patch_daily_temps(gap_weather,patch_weather,max_mean_bias=0.1,time_interval="2 weeks")
#' 
#' 
#' @export patch_daily_temps
patch_daily_temps<-function(weather,patch_weather,vars=c("Tmin","Tmax"),max_mean_bias=NA,
                                   max_stdev_bias=NA,time_interval="month")
{  
  weather<-make_all_day_table(weather,no_variable_check = TRUE)
  if(!"YEARMODA" %in% colnames(weather))
    weather[,"YEARMODA"]<-weather[,"Year"]*10000+weather[,"Month"]*100+weather[,"Day"]
  
  if(is.data.frame(patch_weather)) daily<-list(patch_weather) else daily<-patch_weather
  
  if(is.null(names(daily))) names(daily)<-paste0("station_",1:length(daily))
  
  statistics<-list()

  gaps<-length(which(is.na(weather[,vars])))
  
  # add source columns to weather
  
  weather[,paste0(vars,"_source")]<-NA
  for(v in vars)
    weather[which(!is.na(weather[,v])),paste0(v,"_source")]<-"original"
  
  aux_weather<-weather
  
  for(dt in names(daily))
  {
    if(!gaps==0)
      {
      auxiliary<-make_all_day_table(daily[[dt]],no_variable_check = TRUE)
      auxiliary[,"YEARMODA"]<-auxiliary[,"Year"]*10000+auxiliary[,"Month"]*100+auxiliary[,"Day"]
      auxiliary<-auxiliary[,c("YEARMODA",vars)]
      vars2<-paste(vars,"temp",sep="")
      colnames(auxiliary)[which(colnames(auxiliary) %in% vars)]<-vars2
      
      # need to remove the TminTemp and Tmaxtemp columns again from aux_weather
      
      aux_weather<-aux_weather[,colnames(weather)]
      
      aux_weather<-merge(aux_weather,auxiliary,by.x="YEARMODA",by.y="YEARMODA",all.x=TRUE) 
      
      # assign analysis interval to each day
      aux_weather[,"Date"]<-ISOdate(aux_weather$Year,aux_weather$Month,aux_weather$Day)
      aux_weather[,"interval"]<-1
      
      for(yy in unique(aux_weather$Year))
        {start_dates<-seq(as.Date(paste0(yy-1,"/12/31")), seq.Date(as.Date(paste0(yy,"/12/31")) , length.out=2, by=time_interval )[2], time_interval)
        start_dates<-start_dates[2:length(start_dates)]
        aux_weather$interval[which(aux_weather$Year==yy)]<- sapply(aux_weather$Date[which(aux_weather$Year==yy)], function(x) min(which(start_dates>=as.Date(x))))
      }
      # evaluate interval assignment
      
      for(v in vars)
        statistics[[v]][[dt]]<-data.frame(Interval=unique(aux_weather$interval),Total_days=NA,Overlap_days=NA,Mean_bias=NA,Stdev_bias=NA,
                                          Gaps_before=NA,Filled=NA,Gaps_remain=NA)
      
      tt<-table(aux_weather[,c("Year","interval")])
      
      # since the time interval can be defined by the user, it's possible that the last interval of the year is very short.
      # the procedure may or may not work then. So we'll produce a warning:
      if(ncol(tt)>2)
        if (median(tt[,ncol(tt)])<0.5*median(tt[,2:(ncol(tt)-1)]))
          warning("The number of days in the last interval is often a lot smaller than in other intervals. Consider changing the time interval.")

      for(v in vars)
      {
       for (i in unique(aux_weather$interval))
        {int_days<-which(aux_weather$interval==i)
         statistics[[v]][[dt]][i,"Total_days"]<-length(int_days)
         statistics[[v]][[dt]][i,"Overlap_days"]<-length(which(!is.na(aux_weather[int_days,v])&!is.na(aux_weather[int_days,paste0(v,"temp")] )))
        
         bias<-mean(aux_weather[int_days,paste0(v,"temp")]-aux_weather[int_days,v],na.rm=TRUE)
         if(is.na(bias)) bias<-NA
         stdev<-sd(aux_weather[int_days,paste0(v,"temp")]-aux_weather[int_days,v],na.rm=TRUE)
         statistics[[v]][[dt]][i,"Mean_bias"]<-round(bias,3)
         statistics[[v]][[dt]][i,"Stdev_bias"]<-round(stdev,3)
         statistics[[v]][[dt]][i,"Gaps_before"]<-length(which(is.na(aux_weather[int_days,v])))
         
         # check if the data fulfills inclusion criteria
         use_data<-TRUE
         if(is.na(bias)|is.na(stdev))
           use_data<-FALSE else
             {if(!is.na(max_mean_bias))
               if(abs(bias)>max_mean_bias)
                 use_data<-FALSE
              if(!is.na(max_stdev_bias))
               if(stdev>max_stdev_bias) use_data<-FALSE
                 if(dt>1) if(statistics[[v]][[dt]][i,"Gaps_before"]==0) use_data<-FALSE
         }
         
         if(!use_data)
           statistics[[v]][[dt]][i,"Filled"]<-0
         
         if(use_data)
           {days_to_replace<-int_days[which(is.na(aux_weather[int_days,v])&
                                        !is.na(aux_weather[int_days,paste0(v,"temp")]))]
            aux_weather[days_to_replace,paste0(v,"_source")]<-dt

            statistics[[v]][[dt]][i,"Filled"]<-length(days_to_replace)
           
           aux_weather[days_to_replace,v]<-
             aux_weather[days_to_replace,paste0(v,"temp")]-bias
         }
         
         statistics[[v]][[dt]][i,"Gaps_remain"]<-length(which(is.na(aux_weather[int_days,v])))
         
         
         
       }
        
      }
        
    }}
  out_weather<-aux_weather[,colnames(weather)]
  return(list(weather=out_weather,statistics=statistics))
}





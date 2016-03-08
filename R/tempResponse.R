tempResponse <-
function (hourtemps,Start_JDay=1,End_JDay=366,models=list(Chilling_Hours=Chilling_Hours,Utah_Chill_Units=Utah_Model,Chill_Portions=Dynamic_Model,GDH=GDH),misstolerance=50)             #hourtemps is a data frame with columns Year, JDay, Hour and Temp
     {
  if((length(names(hourtemps))==2) & ("hourtemps" %in% names(hourtemps))) {QC<-hourtemps$QC; hourtemps<-hourtemps$hourtemps} else QC<-NULL
  if(Start_JDay<End_JDay) {hourtemps[which(hourtemps$JDay>=Start_JDay&hourtemps$JDay<=End_JDay),"sea"]<-
                     hourtemps[which(hourtemps$JDay>=Start_JDay&hourtemps$JDay<=End_JDay),"Year"]} else
                        {hourtemps[which(hourtemps$JDay>=Start_JDay),"sea"]<-
                          hourtemps[which(hourtemps$JDay>=Start_JDay),"Year"]+1
                        hourtemps[which(hourtemps$JDay<=End_JDay),"sea"]<-
                          hourtemps[which(hourtemps$JDay<=End_JDay),"Year"]}
                         
      if(Start_JDay<End_JDay) {relevant_days<-Start_JDay:End_JDay} else
                              {relevant_days<-c(Start_JDay:366,1:End_JDay)}
      normal_lines<-which(!(hourtemps$JDay==Start_JDay&hourtemps$Hour==0))
      normal_lines<-normal_lines[which(normal_lines>1)]

      hourtemps<-hourtemps[which(!is.na(hourtemps[,"Temp"])),]

      
      for(m in 1:length(models))
        hourtemps[,names(models)[m]]<-do.call(models[[m]], list(hourtemps[,"Temp"]), quote = FALSE, envir = parent.frame())

      seasons<-unique(hourtemps$sea)
      seasons<-seasons[!is.na(seasons)]

#summarize all models
     chillout<-data.frame(Season=paste(seasons-1,"/",seasons,sep=""),End_year=seasons)
     
     if(End_JDay>=Start_JDay)
       dt<-sapply(seasons,function(x)
         difftime(ISOdate(x-1,12,31)+End_JDay*86400,ISOdate(x-1,12,31)+Start_JDay*86400))
     if(End_JDay<Start_JDay)
       dt<-sapply(seasons,function(x)
         difftime(ISOdate(x-1,12,31)+End_JDay*86400,ISOdate(x-2,12,31)+Start_JDay*86400))     
     chillout[,"Season_days"]<-dt+1
     chillout[,"Data_days"]<-sapply(seasons,function(x) length(which(hourtemps$sea==x))/24)

                                                              
for (sea in seasons)
    #if(sea==seasons[1])
     {#chillout<-data.frame(Season=paste(sea-1,"/",sea,sep=""),End_year=sea,Days=length(which(hourtemps$sea==sea))/24)
      seas<-hourtemps[which(hourtemps$sea==sea),]
      if("no_Tmin" %in% names(hourtemps)&"no_Tmax" %in% names(hourtemps))
        chillout[which(chillout$End_year==sea),"Interpolated_days"]<-length(which(seas$no_Tmin|seas$no_Tmax))/24
        chillout[,"Perc_complete"]<-NA
      if(sea==seasons[1]) last_end<-hourtemps[1,]-hourtemps[1,] else last_end<-hourtemps[min(which(hourtemps$sea==sea))-1,]
        for(m in 1:length(models))
          chillout[which(chillout$End_year==sea),names(models)[m]]<-seas[nrow(seas),names(models)[m]]-last_end[,names(models)[m]]
}
     if("no_Tmin" %in% names(hourtemps)&"no_Tmax" %in% names(hourtemps))
       chillout[,"Perc_complete"]<-(chillout[,"Data_days"]-chillout[,"Interpolated_days"])/chillout[,"Season_days"]*100 else
         chillout[,"Perc_complete"]<-chillout[,"Data_days"]/chillout[,"Season_days"]*100
           
     chillout<-chillout[which(chillout$Perc_complete>=100-misstolerance),]
     
     return(chillout)

}


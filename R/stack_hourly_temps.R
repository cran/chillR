stack_hourly_temps <-
function(weather=NULL,latitude=50,hour_file=NULL)
   {if(is.null(weather)&(!is.null(hour_file))) weather<-hour_file
     #this part is for the case where users specify and named variable 'hour_file'
     #This is necessary for backward compatibility
    if((length(names(weather))==2) & ("weather" %in% names(weather)) & ("QC" %in% names(weather))) 
      {THourly<-make_hourly_temps(latitude,weather$weather)
      QC<-weather$QC}
    if((length(names(weather))>24) & ("Hour_23" %in% names(weather)))
      {THourly<-weather
      QC<-NA}
   if((length(names(weather))>2) & ("Tmax" %in% names(weather)))
      {THourly<-make_hourly_temps(latitude,weather)
      QC<-NA}   
    
    if(is.null(weather)&is.null(hour_file)) {tt=NA;QC=NA} else
      {preserve_columns<-colnames(THourly)[1:(ncol(THourly)-24)]
      THourly[,"index"]<-row(THourly)[,1]
      ss<-stack(THourly,select=c(paste("Hour_",1:24,sep="")))
      colnames(ss)<-c("Temp","Hour")
      ss$Hour<-as.numeric(sapply(strsplit(as.character(ss$Hour),"_"),"[",2))
      for (pc in preserve_columns)
      {ss[,pc]<-rep(THourly[,pc],24)}
      tt<-ss[with(ss, order(Year, JDay,Hour)), ]  
      tt<-tt[c(preserve_columns,"Hour","Temp")]}
    
    return(list(hourtemps=tt,QC=QC))  
  }
          
    

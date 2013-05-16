stack_hourly_temps <-
function(hour_file)
   {preserve_columns<-colnames(hour_file)[1:(ncol(hour_file)-24)]
    
    hour_file[,"index"]<-row(hour_file)[,1]
    ss<-stack(hour_file,select=c(paste("Hour_",1:24,sep="")))
    colnames(ss)<-c("Temp","Hour")
    ss$Hour<-as.numeric(sapply(strsplit(as.character(ss$Hour),"_"),"[",2))
    for (pc in preserve_columns)
      {ss[,pc]<-rep(hour_file[,pc],24)}
    tt<-ss[with(ss, order(Year, JDay,Hour)), ]  
    tt<-tt[c(preserve_columns,"Hour","Temp")]
          
    return(tt)      
          }

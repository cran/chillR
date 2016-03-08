daily_chill <-
function(hourtemps=NULL,running_mean=1,models=list(Chilling_Hours=Chilling_Hours,Utah_Chill_Units=Utah_Model,
                                          Chill_Portions=Dynamic_Model,GDH=GDH),THourly=NULL)
{  if(is.null(hourtemps) & !is.null(THourly)) hourtemps<-THourly
if((length(names(hourtemps))==2) & ("hourtemps" %in% names(hourtemps))) {QC<-hourtemps$QC; hourtemps<-hourtemps$hourtemps} else QC<-NULL


  for(m in 1:length(models))
    hourtemps[,names(models)[m]]<-do.call(models[[m]], list(HourTemp=hourtemps[,"Temp"],summ=F), quote = FALSE, envir = parent.frame())
  
hourtemps$YYMMDD<-hourtemps$Year*10000+hourtemps$Month*100+hourtemps$Day
  
dc<-aggregate(x=hourtemps[,names(models)],by=list(hourtemps$YYMMDD),FUN=sum)
colnames(dc)<-c("YYMMDD",names(models))
  dc$Year<-trunc(dc[,1]/10000)
  dc$Month<-trunc((dc[,1]-dc$Year*10000)/100)
  dc$Day<-trunc((dc[,1]-dc$Year*10000-dc$Month*100))
  dc<-dc[,c("YYMMDD","Year","Month","Day",names(models))]
  dc[,"Tmean"]<-aggregate(x=hourtemps[,"Temp"],by=list(hourtemps$YYMMDD),FUN=mean)[,2]

  dc[,names(models)]<-sapply(names(models),function (x) runn_mean(dc[,x],running_mean))
  
  if("no_Tmin" %in% colnames(hourtemps)) dc[,"no_Tmin"]<-aggregate(x=hourtemps[,"no_Tmin"],by=list(hourtemps$YYMMDD),FUN=sum)[,2]>0
  if("no_Tmax" %in% colnames(hourtemps)) dc[,"no_Tmax"]<-aggregate(x=hourtemps[,"no_Tmax"],by=list(hourtemps$YYMMDD),FUN=sum)[,2]>0
  
  if(!is.null(QC)) if(is.data.frame(QC)) QC<-QC else QC<-NA else QC<-NA
    #chillout<-cbind(chillout,QC[which(QC$End_year %in%chillout$End_year),(which(colnames(QC)=="Data_days")+1):ncol(QC)])
  
  
 return(list(object_type="daily_chill",daily_chill=dc,QC=QC))
}

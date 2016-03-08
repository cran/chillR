make_all_day_table<-function(tab) #tab should have columns named Year, Month and Day (or YEAR, MONTH, DAY; or YEARMODA)
{
 columns<-colnames(tab)  
  if("YEAR" %in% colnames(tab)) colnames(tab)[which(colnames(tab)=="YEAR")]<-"Year"
  if("year" %in% colnames(tab)) colnames(tab)[which(colnames(tab)=="year")]<-"Year"
  if("MONTH" %in% colnames(tab)) colnames(tab)[which(colnames(tab)=="MONTH")]<-"Month"
  if("month" %in% colnames(tab)) colnames(tab)[which(colnames(tab)=="month")]<-"Month"
  if("DAY" %in% colnames(tab)) colnames(tab)[which(colnames(tab)=="DAY")]<-"Day"
  if("day" %in% colnames(tab)) colnames(tab)[which(colnames(tab)=="day")]<-"Day"
  if("YEARMODA" %in% colnames(tab))
      {tab[,"Year"]<-trunc(tab[,"YEARMODA"]/10000)
       tab[,"Month"]<-trunc((tab[,"YEARMODA"]-tab[,"Year"]*10000)/100)
       tab[,"Day"]<-tab[,"YEARMODA"]-tab[,"Year"]*10000-tab[,"Month"]*100
      }
tab[,"YEARMODA"]<-as.numeric(tab[,"Year"])*10000+as.numeric(tab[,"Month"])*100+as.numeric(tab[,"Day"])
tab[,"DATE"]<-ISOdate(tab[,"Year"], tab[,"Month"], tab[,"Day"])
datevec<-seq(tab$DATE[1], tab$DATE[nrow(tab)], "day")
tab2<-tab[,!(names(tab) %in% c("Year","Day","Month","Day","DATE"))]

output<-data.frame(DATE=datevec)
output[,"Year"]<-as.numeric(format(output[,"DATE"], "%Y"))
output[,"Month"]<-as.numeric(format(output[,"DATE"], "%m"))
output[,"Day"]<-as.numeric(format(output[,"DATE"], "%d"))
output[,"YEARMODA"]<-output[,"Year"]*10000+output[,"Month"]*100+output[,"Day"]

output<-merge(output,tab2,by="YEARMODA",all.x=TRUE)
#output<-output[,c(2,3,4,5,11,12)]
#colnames(output)<-c("DATE","Year","Month","Day","Tmin","Tmax")
return(output)
}

#KA_weather_mod<-KA_weather[1:100,]
#KA_weather_mod<-rbind(KA_weather_mod,c(Year=1998,Month=6,Day=3,Tmax=26,Tmin=14))
#tab<-KA_weather_mod
#make_all_day_table(KA_weather_mod)

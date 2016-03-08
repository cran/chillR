chilling <-
function (hourtemps=NULL,Start_JDay=1,End_JDay=366,THourly=NULL,misstolerance=50)             #hourtemps is a data frame with columns Year, JDay, Hour and Temp
     {if(is.null(hourtemps) & !is.null(THourly)) hourtemps<-THourly
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

      #Chilling Hours
      CH_range<-which(hourtemps$Temp<=7.2&hourtemps$Temp>=0)
      hourtemps[,"CH_weights"]<-0
      hourtemps[CH_range,"CH_weights"]<-1
      hourtemps[,"CH"]<-0
#      for (nl in normal_lines) {hourtemps[nl,"CH"]<-hourtemps[nl-1,"CH"]+hourtemps[nl,"CH_weights"]}
      
      #Utah Model
      Utah_range_0.5<-which(hourtemps$Temp<=2.4&hourtemps$Temp>1.4|
                          hourtemps$Temp<=12.4&hourtemps$Temp>9.1)
      Utah_range_1.0<-which(hourtemps$Temp<=9.1&hourtemps$Temp>2.4)
      Utah_range_min0.5<-which(hourtemps$Temp<=18.0&hourtemps$Temp>15.9)
      Utah_range_min1.0<-which(hourtemps$Temp>18.0)
      hourtemps[,"Utah_weights"]<-0
      hourtemps[Utah_range_0.5,"Utah_weights"]<-0.5
      hourtemps[Utah_range_1.0,"Utah_weights"]<-1
      hourtemps[Utah_range_min0.5,"Utah_weights"]<-(-0.5)
      hourtemps[Utah_range_min1.0,"Utah_weights"]<-(-1)
      hourtemps[,"Utah_Model"]<-0
#      for (nl in normal_lines) {hourtemps[nl,"Utah_Model"]<-hourtemps[nl-1,"Utah_Model"]+hourtemps[nl,"Utah_weights"]}
     seasons<-unique(hourtemps$sea)
     seasons<-seasons[!is.na(seasons)]


#Dynamic Model
e0<-4153.5
e1<-12888.8
a0<-139500
a1<-2567000000000000000
slp<-1.6
tetmlt<-277
aa<-a0/a1
ee<-e1-e0

hourtemps[,"TK"]<-hourtemps$Temp+273
hourtemps[,"ftmprt"]<-slp*tetmlt*(hourtemps[,"TK"]-tetmlt)/hourtemps[,"TK"]
hourtemps[,"sr"]<-exp(hourtemps[,"ftmprt"])
hourtemps[,"xi"]<-hourtemps[,"sr"]/(1+hourtemps[,"sr"])
hourtemps[,"xs"]<-aa*exp(ee/hourtemps[,"TK"])
hourtemps[,"ak1"]<-a1*exp(-e1/hourtemps[,"TK"])
hourtemps[1,"interE"]<-0

memo<-new.env(hash=TRUE)

posi<-1
assign(x=paste(1),value=0,envir=memo)
E=0

xs<-hourtemps[,"xs"]
xi<-hourtemps[,"xi"]
ak1<-hourtemps[,"ak1"]
S<-ak1
S[1]<-0
E<-S
options(scipen=30)

for (l in 2:nrow(hourtemps))  {if(E[l-1]<1)
                              {S[l]<-E[l-1]
                               E[l]<-xs[l]-(xs[l]-S[l])*exp(-ak1[l])} else
                              {S[l]<-E[l-1]-E[l-1]*xi[l-1]
                              E[l]<-xs[l]-(xs[l]-S[l])*exp(-ak1[l])}
                             }
hourtemps[,"interE"]<-E

hourtemps[which(hourtemps$interE<1),"delt"]<-0
hourtemps[which(hourtemps$interE>=1),"delt"]<-hourtemps[which(hourtemps$interE>=1),"interE"]*hourtemps[which(hourtemps$interE>=1),"xi"]

Stress<-1
Tb<-4
Tu<-25
Tc<-36

hourtemps[,"GDH_weight"]<-0
hourtemps[which(hourtemps$Temp>=Tb&hourtemps$Temp<=Tu),"GDH_weight"]<-Stress*(Tu-Tb)/2*
     (1+cos(pi+pi*(hourtemps[which(hourtemps$Temp>=Tb&hourtemps$Temp<=Tu),"Temp"]-Tb)/(Tu-Tb)))
hourtemps[which(hourtemps$Temp>Tu&hourtemps$Temp<=Tc),"GDH_weight"]<-Stress*(Tu-Tb)*
     (1+cos(pi/2+pi/2*(hourtemps[which(hourtemps$Temp>Tu&hourtemps$Temp<=Tc),"Temp"]-Tu)/(Tc-Tu)))



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
      {if("no_Tmin" %in% names(hourtemps)&"no_Tmax" %in% names(hourtemps))
         chillout[which(chillout$End_year==sea),"Interpolated_days"]<-length(which(
           hourtemps[which(hourtemps$sea==sea),"no_Tmin"]|hourtemps[which(hourtemps$sea==sea),"no_Tmax"]))/24
       chillout[,"Perc_complete"]<-NA
       chillout[which(chillout$End_year==sea),"Chilling_Hours"]<-sum(hourtemps[which(hourtemps$sea==sea),"CH_weights"])
       chillout[which(chillout$End_year==sea),"Utah_Model"]<-sum(hourtemps[which(hourtemps$sea==sea),"Utah_weights"])
       chillout[which(chillout$End_year==sea),"Chill_portions"]<-sum(hourtemps[which(hourtemps$sea==sea),"delt"])
       chillout[which(chillout$End_year==sea),"GDH"]<-sum(hourtemps[which(hourtemps$sea==sea),"GDH_weight"])
}
if("no_Tmin" %in% names(hourtemps)&"no_Tmax" %in% names(hourtemps))
  chillout[,"Perc_complete"]<-(chillout[,"Data_days"]-chillout[,"Interpolated_days"])/chillout[,"Season_days"]*100 else
    chillout[,"Perc_complete"]<-chillout[,"Data_days"]/chillout[,"Season_days"]*100

chillout<-chillout[which(chillout$Perc_complete>=100-misstolerance),]

return(chillout)

}

chilling_hourtable <-
function (hourtemps,Start_JDay)             #hourtemps is a data frame with columns Year, JDay, Hour and Temp
{
  
  if((length(names(hourtemps))==2) & ("hourtemps" %in% names(hourtemps)) & ("QC" %in% names(hourtemps))) 
  {hourtemps<-hourtemps$hourtemps
  QC<-hourtemps$QC}
  
  cols<-colnames(hourtemps)
  
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
  
  
  add_up_weights<-function(hourtemps,outcol,weightcol,SDay)
  {weights<-hourtemps[,weightcol]
   SD<-hourtemps$JDay==SDay
   temp<-weights
   temp[1]<-0
   nn<-nrow(hourtemps)
   for (l in 2:nn)
     if (SD[l]) {temp[l]<-0} else
     {temp[l]<-temp[l-1]+weights[l]}
   hourtemps[,outcol]<-temp  
   return(hourtemps)
  }
  
  hourtemps<-add_up_weights(hourtemps,"Chilling_Hours","CH_weights",Start_JDay)
  hourtemps<-add_up_weights(hourtemps,"Chill_Portions","delt",Start_JDay)
  hourtemps<-add_up_weights(hourtemps,"Chill_Units","Utah_weights",Start_JDay)
  hourtemps<-add_up_weights(hourtemps,"GDH","GDH_weight",Start_JDay)
  
  return(hourtemps[,c(cols,"Chilling_Hours","Chill_Portions","Chill_Units","GDH")])
  
}

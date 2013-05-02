daily_chill <-
function(THourly,running_mean)
{  
  #Chilling Hours
  CH_range<-which(THourly$Temp<=7.2&THourly$Temp>=0)
THourly[,"CH_weights"]<-0
THourly[CH_range,"CH_weights"]<-1
THourly[,"CH"]<-0
#      for (nl in normal_lines) {THourly[nl,"CH"]<-THourly[nl-1,"CH"]+THourly[nl,"CH_weights"]}

#Utah Model
Utah_range_0.5<-which(THourly$Temp<=2.4&THourly$Temp>1.4|
                        THourly$Temp<=12.4&THourly$Temp>9.1)
Utah_range_1.0<-which(THourly$Temp<=9.1&THourly$Temp>2.4)
Utah_range_min0.5<-which(THourly$Temp<=18.0&THourly$Temp>15.9)
Utah_range_min1.0<-which(THourly$Temp>18.0)
THourly[,"Utah_weights"]<-0
THourly[Utah_range_0.5,"Utah_weights"]<-0.5
THourly[Utah_range_1.0,"Utah_weights"]<-1
THourly[Utah_range_min0.5,"Utah_weights"]<-(-0.5)
THourly[Utah_range_min1.0,"Utah_weights"]<-(-1)
THourly[,"Utah_Model"]<-0
#      for (nl in normal_lines) {THourly[nl,"Utah_Model"]<-THourly[nl-1,"Utah_Model"]+THourly[nl,"Utah_weights"]}
seasons<-unique(THourly$Season)
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


THourly[,"TK"]<-THourly$Temp+273
THourly[,"ftmprt"]<-slp*tetmlt*(THourly[,"TK"]-tetmlt)/THourly[,"TK"]
THourly[,"sr"]<-exp(THourly[,"ftmprt"])
THourly[,"xi"]<-THourly[,"sr"]/(1+THourly[,"sr"])
THourly[,"xs"]<-aa*exp(ee/THourly[,"TK"])
THourly[,"ak1"]<-a1*exp(-e1/THourly[,"TK"])
THourly[1,"interE"]<-0

memo<-new.env(hash=TRUE)



posi<-1
assign(x=paste(1),value=0,envir=memo)
E=0

xs<-THourly[,"xs"]
xi<-THourly[,"xi"]
ak1<-THourly[,"ak1"]
S<-ak1
S[1]<-0
E<-S
options(scipen=30)

for (l in 2:nrow(THourly))  {if(E[l-1]<1)
{S[l]<-E[l-1]
 E[l]<-xs[l]-(xs[l]-S[l])*exp(-ak1[l])} else
 {S[l]<-E[l-1]-E[l-1]*xi[l-1]
  E[l]<-xs[l]-(xs[l]-S[l])*exp(-ak1[l])}
}
THourly[,"interE"]<-E




#for (l in 2:nrow(THourly))  {if(E<1)
#                              {S[l]<-E
#                              a<-xs[l]-(xs[l]-get(paste(l-1),memo))*exp(-ak1[l])
#                              assign(x=paste(l),value=a,pos=1,envir=memo)} else
#                              {S<-E-E*xi[l-1]
#                              a<-xs[l]-(xs[l]-(get(paste(l-1),memo)*(1-xi[l-1]))*exp(-ak1[l]))
#                              assign(x=paste(l),value=a,pos=1,envir=memo)}
#                             }
#THourly[,"row"]<-row(THourly)[,1]
#getstuff<-function(r) {return(get(as.character(r),memo))}
#THourly[,"interE"]<-unlist(lapply(THourly$row,getstuff))
#options(scipen=0)
THourly[which(THourly$interE<1),"delt"]<-0
THourly[which(THourly$interE>=1),"delt"]<-THourly[which(THourly$interE>=1),"interE"]*THourly[which(THourly$interE>=1),"xi"]

Stress<-1
Tb<-4
Tu<-25
Tc<-36

THourly[,"GDH_weight"]<-0
THourly[which(THourly$Temp>=Tb&THourly$Temp<=Tu),"GDH_weight"]<-Stress*(Tu-Tb)/2*
  (1+cos(pi+pi*(THourly[which(THourly$Temp>=Tb&THourly$Temp<=Tu),"Temp"]-Tb)/(Tu-Tb)))
THourly[which(THourly$Temp>Tu&THourly$Temp<=Tc),"GDH_weight"]<-Stress*(Tu-Tb)*
  (1+cos(pi/2+pi/2*(THourly[which(THourly$Temp>Tu&THourly$Temp<=Tc),"Temp"]-Tu)/(Tc-Tu)))

THourly$YYMMDD<-THourly$Year*10000+THourly$Month*100+THourly$Day
  
daily_chill<-cbind(aggregate(x=THourly$CH_weights,by=list(THourly$YYMMDD),FUN=sum),
                   aggregate(x=THourly$Utah_weights,by=list(THourly$YYMMDD),FUN=sum),
                   aggregate(x=THourly$delt,by=list(THourly$YYMMDD),FUN=sum),
                   aggregate(x=THourly$GDH_weight,by=list(THourly$YYMMDD),FUN=sum))
  daily_chill$Year<-trunc(daily_chill[,1]/10000)
  daily_chill$Month<-trunc((daily_chill[,1]-daily_chill$Year*10000)/100)
  daily_chill$Day<-trunc((daily_chill[,1]-daily_chill$Year*10000-daily_chill$Month*100))
  daily_chill<-daily_chill[,c(1,9,10,11,2,4,6,8)]
  colnames(daily_chill)<-c("YYMMDD","Year","Month","Day","Chilling_Hours","Utah_Chill_Units","Chill_Portions","GDH")
 
  #running mean
  if(!running_mean==1)
    {runn_mean<-running_mean
    for (cc in colnames(daily_chill)[5:8])
    {ww<-daily_chill[,cc]
      rr<-daily_chill[,cc]        
      for (dd in 1:length(ww))
       {if (dd<ceiling(runn_mean/2))
          {rr[dd]<-mean(ww[1:(dd+floor(runn_mean/2))])}
        if ((dd>=ceiling(runn_mean/2))&(dd<=length(ww)-ceiling(runn_mean/2)))
          {rr[dd]<-mean(ww[(dd-floor(runn_mean/2)):(dd+floor(runn_mean/2))])}
        if (dd>(length(ww)-ceiling(runn_mean/2)))
          {rr[dd]<-mean(ww[(dd-floor(runn_mean/2)):length(ww)])}
       }
     daily_chill[,cc]<-rr
   }
  }
 return(list(object_type="daily_chill",daily_chill=daily_chill))
}

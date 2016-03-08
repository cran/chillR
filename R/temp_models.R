step_model<-function(HourTemp,
                     df=data.frame(
                       lower=c(-1000,1.4,2.4,9.1,12.4,15.9,18),
                       upper=c(1.4,2.4,9.1,12.4,15.9,18,1000),
                       weight=c(0,0.5,1,0.5,0,-0.5,-1)),summ=TRUE)
    {lower<-df$lower;upper<-df$upper;weight<-df$weight
      if (summ==TRUE) return(cumsum(sapply(HourTemp,function(x) weight[which(x>lower&x<=upper)]))) else
                      return(sapply(HourTemp,function(x) weight[which(x>lower&x<=upper)]))
    }

#df=data.frame(
#  lower=c(-1000,1,2,3,4,5,6),
#  upper=c(1,2,3,4,5,6,1000),
#  weight=c(0,1,2,3,2,1,0),
#  lower_include_equal=c(F,F,T,T,T,T,T),
#  upper_include_equal=c(F,F,T,T,T,T,T))


Utah_Model<-function(HourTemp,summ=TRUE)
  return(step_model(HourTemp,df=data.frame(lower=c(-1000,1.4,2.4,9.1,12.4,15.9,18),upper=c(1.4,2.4,9.1,12.4,15.9,18,1000),weight=c(0,0.5,1,0.5,0,-0.5,-1)),summ=summ))
  


#Utah_Model<-function(HourTemp)
#  {#Utah Model
#  Utah_range_0.5<-which(HourTemp<=2.4&HourTemp>1.4|
#                          HourTemp<=12.4&HourTemp>9.1)
#  Utah_range_1.0<-which(HourTemp<=9.1&HourTemp>2.4)
#  Utah_range_min0.5<-which(HourTemp<=18.0&HourTemp>15.9)
#  Utah_range_min1.0<-which(HourTemp>18.0)
#  Utah_weights<-rep(0,length(HourTemp))
#  Utah_weights[Utah_range_0.5]<-0.5
#  Utah_weights[Utah_range_1.0]<-1
#  Utah_weights[Utah_range_min0.5]<-(-0.5)
#  Utah_weights[Utah_range_min1.0]<-(-1)
#  return(cumsum(Utah_weights))
#}
  
Chilling_Hours<-function(HourTemp,summ=TRUE)
{
  CH_range<-which(HourTemp<=7.2&HourTemp>=0)
  CH_weights<-rep(0,length(HourTemp))
  CH_weights[CH_range]<-1
  if(summ==TRUE) return(cumsum(CH_weights)) else
    return(CH_weights)
}

Dynamic_Model<-function(HourTemp,summ=TRUE)
  {#Dynamic Model
  e0<-4153.5
  e1<-12888.8
  a0<-139500
  a1<-2567000000000000000
  slp<-1.6
  tetmlt<-277
  aa<-a0/a1
  ee<-e1-e0
  
  TK<-HourTemp+273
  ftmprt<-slp*tetmlt*(TK-tetmlt)/TK
  sr<-exp(ftmprt)
  xi<-sr/(1+sr)
  xs<-aa*exp(ee/TK)
  ak1<-a1*exp(-e1/TK)
  interE<-0
  
  memo<-new.env(hash=TRUE)
  
  posi<-1
  assign(x=paste(1),value=0,envir=memo)
  E=0
  
  S<-ak1
  S[1]<-0
  E<-S
  options(scipen=30)
  
  for (l in 2:length(HourTemp))  {if(E[l-1]<1)
  {S[l]<-E[l-1]
  E[l]<-xs[l]-(xs[l]-S[l])*exp(-ak1[l])} else
  {S[l]<-E[l-1]-E[l-1]*xi[l-1]
  E[l]<-xs[l]-(xs[l]-S[l])*exp(-ak1[l])}
  }
  interE<-E
  delt<-rep(0,length(HourTemp))
  delt[which(interE>=1)]<-interE[which(interE>=1)]*xi[which(interE>=1)]
  if (summ==TRUE) return(cumsum(delt)) else
    return(delt)
}

GDH<-function(HourTemp,summ=TRUE)
  {Stress<-1
  Tb<-4
  Tu<-25
  Tc<-36
  
  GDH_weight<-rep(0,length(HourTemp))
  GDH_weight[which(HourTemp>=Tb&HourTemp<=Tu)]<-Stress*(Tu-Tb)/2*
    (1+cos(pi+pi*(HourTemp[which(HourTemp>=Tb&HourTemp<=Tu)]-Tb)/(Tu-Tb)))
  GDH_weight[which(HourTemp>Tu&HourTemp<=Tc)]<-Stress*(Tu-Tb)*
    (1+cos(pi/2+pi/2*(HourTemp[which(HourTemp>Tu&HourTemp<=Tc)]-Tu)/(Tc-Tu)))
  if (summ) return(cumsum(GDH_weight)) else
    return(GDH_weight)
}


#Johann_model<-function(HourTemp,summ=TRUE)
#  if(summ) return(cumsum(0.6702*(exp(-0.148*HourTemp)))) else
#    return(0.6702*(exp(-0.148*HourTemp)))


#models<-list(Chilling_Hours=Chilling_Hours,Utah_Chill_Units=Utah_Model,Chill_Portions=Dynamic_Model,GDH=GDH,experimental=step_model,Johann=Johann_model)

#tempResponse(stack,Start_JDay = 305,End_JDay = 60,models)



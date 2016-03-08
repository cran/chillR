get_last_date<-function(dates,first=FALSE)
{pdates<-dates[order(dates)]
gaps<-c(pdates[2:length(pdates)]-pdates[1:(length(pdates)-1)],
        which(pdates[1]==c(180:366,1:179))-which(pdates[length(pdates)]==c(180:366,1:179)))
gaps[which(gaps<0)]<-365+gaps[which(gaps<0)]
last<-max(pdates[which(gaps==max(gaps))])
if(which(gaps==max(gaps))==length(pdates)) firsty<-pdates[1] else firsty<-pdates[which(gaps==max(gaps))+1]
#first<-max(pdates[which(gaps==max(gaps))])
if(first) return(firsty) else return(last)
}

make_daily_chill_figures <-
function(daily_chill,file_path,models=c("Chilling_Hours","Utah_Chill_Units","Chill_Portions","GDH"),
         labels=NA)
{
  if(daily_chill[1]=="daily_chill")
    {dc<-daily_chill$daily_chill
      
     dc[,"JDay"]<-strptime(paste(dc$Month,"/",dc$Day,"/",dc$Year,sep=""),"%m/%d/%Y")$yday+1
     df<-data.frame(JDay=1:365)
     
     Kendall_0<-function(x,y)
     {if(sum(y)==0) out<-list(sl=NA,tau=NA) else out<-Kendall(x,y)
      return(out)}
     
     for(m in models)
       for(i in 1:365) {df[i,paste(m,"_mean",sep="")]<-mean(dc[which(dc$JDay == i),m])
                        df[i,paste(m,"_sd",sep="")]<-sd(dc[which(dc$JDay == i),m])
                        df[i,paste(m,"_Kendall_p",sep="")]<-Kendall_0(dc[which(dc$JDay == i),"Year"],dc[which(dc$JDay == i), m])$sl[1]
                        df[i,paste(m,"_Kendall_tau",sep="")]<-Kendall_0(dc[which(dc$JDay == i),"Year"],dc[which(dc$JDay == i), m])$tau[1]
       }
     

     make_plot<-function(JDay,means,sds,name,label,file_name)
     {
       months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
       leg<-strptime(strptime(paste("01/",1,"/2001",sep=""),format="%d/%m/%Y")-86400+JDay*86400,"%Y-%m-%d")
       tick_marks<-leg[sapply(strsplit(as.character(leg),"-"),"[",3)=="01"]
       tick_label_pos<-leg[sapply(strsplit(as.character(leg),"-"),"[",3)=="15"]
       tick_labels<-as.Date(tick_marks,origin="1999-1-2")
       tick_labels<-as.POSIXlt(tick_labels)$mon
       tick_labels<-months[tick_labels+1]
       tick_marks<-as.numeric(format(strptime(tick_marks,"%Y-%m-%d"),format="%j"))
       ticks_labels<-as.numeric(format(strptime(tick_label_pos,"%Y-%m-%d"),format="%j"))
       
       png(file_name,width=2000,height=1400,pointsize = 20)
       par(mar=c(6.1,9.1,6.1,2.1))
       plot(JDay,means,main=paste(label,"accumulation"),ylab=NA,xlab=NA,xaxs="i",yaxs="i",xaxt="n",cex.lab=4,cex.axis=4,cex.main=5,type="l",lwd=3,col="BLACK",ylim=c(min(means-sds),max(means+sds)))
       arrows(JDay,means+sds,JDay,means-sds, angle=90, code=3,lwd=6,length=0,col="GRAY")
       lines(JDay,means,lwd=5)
       axis(1,lwd.ticks=3,labels=FALSE,at=tick_marks,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
       axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
       mtext(side=2,text=paste(label,"per day"),line=5,cex=4)
       dev.off()
     }
  
     if(is.na(labels[1])) labels<-models
     
     for(m in models)
          make_plot(JDay=df$JDay,means=df[,paste(m,"_mean",sep="")],
               sds=df[,paste(m,"_sd",sep="")],name=m,label=labels[which(models==m)],file_name=paste(file_path,"unit_accumulation_",m,".png",sep=""))
  
     return(list(daily_chill_figure_summary=df))
      
    } else {"Error: not a daily chill object; use function daily_chill to make one from hourly temperature data"}
    
}

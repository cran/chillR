make_daily_chill_figures <-
function(daily_chill,file_path)
{
  require(Kendall)
  if(daily_chill[1]=="daily_chill")
    {dc<-daily_chill$daily_chill
      
     dc[,"JDay"]<-strptime(paste(dc$Month,"/",dc$Day,"/",dc$Year,sep=""),"%m/%d/%Y")$yday+1
     df<-data.frame(JDay=NA,CH_mean=NA,CH_sd=NA,CH_Kendall_p=NA,CH_Kendall_tau=NA,
                    UCU_mean=NA,UCU_sd=NA,UCU_Kendall_p=NA,UCU_Kendall_tau=NA,
                    CP_mean=NA,CP_sd=NA,CP_Kendall_p=NA,CP_Kendall_tau=NA,
                    GDH_mean=NA,GDH_sd=NA,GDH_Kendall_p=NA,GDH_Kendall_tau=NA)
     Kendall_0<-function(x,y)
     {if(sum(y)==0) out<-list(sl=NA,tau=NA) else out<-Kendall(x,y)
      return(out)}
     
     
     for (i in 1:365) df[i, ] <- c(i,
                                   mean(dc[which(dc$JDay == i),"Chilling_Hours"]),
                                   sd(dc[which(dc$JDay == i),"Chilling_Hours"]),
                                   Kendall_0(dc[which(dc$JDay == i),"Year"],dc[which(dc$JDay == i), "Chilling_Hours"])$sl[1], 
                                   Kendall_0(dc[which(dc$JDay == i), "Year"], dc[which(dc$JDay == i), "Chilling_Hours"])$tau[1],
                                   mean(dc[which(dc$JDay == i), "Utah_Chill_Units"]),
                                   sd(dc[which(dc$JDay == i), "Utah_Chill_Units"]),
                                   Kendall_0(dc[which(dc$JDay == i), "Year"], dc[which(dc$JDay == i), "Utah_Chill_Units"])$sl[1], 
                                   Kendall_0(dc[which(dc$JDay == i), "Year"], dc[which(dc$JDay == i), "Utah_Chill_Units"])$tau[1],
                                   mean(dc[which(dc$JDay == i), "Chill_Portions"]),
                                   sd(dc[which(dc$JDay == i), "Chill_Portions"]),
                                   Kendall_0(dc[which(dc$JDay == i), "Year"], dc[which(dc$JDay == i), "Chill_Portions"])$sl[1], 
                                   Kendall_0(dc[which(dc$JDay == i), "Year"], dc[which(dc$JDay == i), "Chill_Portions"])$tau[1],
                                   mean(dc[which(dc$JDay == i), "GDH"]),
                                   sd(dc[which(dc$JDay == i), "GDH"]), 
                                   Kendall_0(dc[which(dc$JDay == i), "Year"], dc[which(dc$JDay == i), "GDH"])$sl[1],
                                   Kendall_0(dc[which(dc$JDay == i), "Year"], dc[which(dc$JDay == i), "GDH"])$tau[1])
     
     make_plot<-function(JDay,means,sds,name,ylabel,file_name)
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
       plot(JDay,means,main=paste(name,"accumulation"),ylab=NA,xlab=NA,xaxs="i",yaxs="i",xaxt="n",cex.lab=4,cex.axis=4,cex.main=5,type="l",lwd=3,col="BLACK",ylim=c(min(means-sds),max(means+sds)))
       arrows(JDay,means+sds,JDay,means-sds, angle=90, code=3,lwd=6,length=0,col="GRAY")
       lines(JDay,means,lwd=5)
       axis(1,lwd.ticks=3,labels=FALSE,at=tick_marks,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
       axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
       mtext(side=2,text=ylabel,line=5,cex=4)
       dev.off()
     }
  
     make_plot(JDay=df$JDay,means=df$CH_mean,sds=df$CH_sd,name="Chilling Hour","Chilling Hours per day",file_name=paste(file_path,"unit_accumulation_Chilling_Hours.png",sep=""))     
     make_plot(JDay=df$JDay,means=df$UCU_mean,sds=df$UCU_sd,name="Utah Chill Unit","Utah Chill Units per day",file_name=paste(file_path,"unit_accumulation_Utah_Chill_Units.png",sep=""))     
     make_plot(JDay=df$JDay,means=df$CP_mean,sds=df$CP_sd,name="Chill Portion","Chill Portions per day",file_name=paste(file_path,"unit_accumulation_Chill_Portions.png",sep=""))     
     make_plot(JDay=df$JDay,means=df$GDH_mean,sds=df$GDH_sd,name="Growing Degree Day","Growing Degree Days per day",file_name=paste(file_path,"unit_accumulation_Growing_Degree_Days.png",sep=""))     
     
     
    return(list(daily_chill_figure_summary=df))
      
    } else {"Error: not a daily chill object; use function daily_chill to make one from hourly temperature data"}
    
}

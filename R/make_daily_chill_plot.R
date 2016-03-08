
make_daily_chill_plot <-function(daily_chill,metrics=NA,startdate=1,enddate=366,useyears=NA,metriclabels=NA,
                                 focusyears="none",cumulative=FALSE,image_type=NA,outpath=NA,filename=NA,fonttype='serif')
{   
  
 make_plot<-function(plottable,name,ylabel,focusyears,imageout,fonttype="default")
   {if (imageout) {cex.main<-4;linelwd=3} else {cex.main<-1;linelwd=2}
   
    JDay<-plottable$JDay
    means=plottable$Mean
    sds=plottable$Sd
    months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    leg<-strptime(strptime(paste("01/",1,"/2001",sep=""),format="%d/%m/%Y")-86400+JDay*86400,"%Y-%m-%d")
    tick_marks<-leg[sapply(strsplit(as.character(leg),"-"),"[",3)=="01"]
    tick_label_pos<-leg[sapply(strsplit(as.character(leg),"-"),"[",3)=="15"]
    tick_labels<-as.Date(tick_label_pos,origin="1999-1-2")
    tick_labels<-as.POSIXlt(tick_labels)$mon
    tick_labels<-months[tick_labels+1]
    tick_marks<-as.numeric(format(strptime(tick_marks,"%Y-%m-%d"),format="%j"))
    if(enddate<=startdate) tick_marks[which(tick_marks>=startdate)]<-tick_marks[which(tick_marks>=startdate)]-366
    ticks_labels<-as.numeric(format(strptime(tick_label_pos,"%Y-%m-%d"),format="%j"))
    if(enddate<=startdate) ticks_labels[which(ticks_labels>=startdate)]<-ticks_labels[which(ticks_labels>=startdate)]-366

    if(!(focusyears[1]=="none")) layout(matrix(c(1,2),1,2,byrow=TRUE),widths=c(3,1))   else par(mfcol=c(1,1))
    
    if(imageout) par(mar=c(5.1,7.1,4.1,0.5))  else par(mar=c(3.1,4.1,2.1,0.5))
    suppressWarnings(if(!(focusdata[1]=="none")) {ymin<-min(c(means-sds,min(plottable[,as.character(focusyears)],na.rm=TRUE)),na.rm=TRUE)
       ymax<-max(c(means+sds,max(plottable[,as.character(focusyears)],na.rm=TRUE)),na.rm=TRUE)} else  
      {ymin<-min(means-sds,na.rm=TRUE)
       ymax<-max(means+sds,na.rm=TRUE)})
    ymin<-ymin-(ymax-ymin)/20
    ymax<-ymax+(ymax-ymin)/20
    if(length(which(is.na(means)))==0)
      plot(JDay,means,main=paste(name,"accumulation"),cex.main=cex.main,ylab=NA,xlab=NA,xaxs="i",yaxs="i",xaxt="n",yaxt="n",type="l",col="BLACK",ylim=c(ymin,ymax)) else
    plot(JDay,rep(NA,length(JDay)),main=paste(name,"accumulation"),cex.main=cex.main,ylab=NA,xlab=NA,xaxs="i",yaxs="i",xaxt="n",yaxt="n",type="l",col="BLACK",ylim=c(0,10))
    if(length(which(is.na(means)))==0)  arrows(JDay,means+sds,JDay,means-sds, angle=90, code=3,lwd=linelwd,length=0,col="GRAY")
    if(length(which(is.na(means)))==0)  lines(JDay,means,lwd=linelwd)
    if(length(which(is.na(means)))==0)  suppressWarnings(if(!(focusyears[1]=="none")) 
    for (focusyear in focusyears)  lines(JDay,plottable[,as.character(focusyear)],lwd=linelwd,col=rainbow(length(focusyears))[which(focusyear==focusyears)]))
    
    if (!imageout) {
      axis(1,labels=FALSE,at=tick_marks,padj=1,lwd.ticks=3,lwd=2)
      axis(2,lwd=2,lwd.ticks=2)
      axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,lwd=2)
      mtext(side=2,text=ylabel,line=3,font=2)
      box(which="plot",lwd=2)}
    
    if(imageout) {
      axis(1,labels=FALSE,at=tick_marks,padj=1,lwd.ticks=3,lwd=3)
      axis(2,lwd=3,lwd.ticks=3,cex.axis=3,padj=0)
      axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,lwd=3,cex.axis=3,padj=1)
      mtext(side=2,text=ylabel,line=4,cex=4,font=2)
      box(which="plot",lwd=3)}
    
    

    if(length(which(is.na(means)))>0)  text(x=mean(JDay),y=5,"no data to show")
    if(!(focusyears[1]=="none"))
    {
      if(!imageout)
        {par(mar=c(3.1,0,2.1,0))
        yend<-min(-(length(focusyears)+3),-20)
        plot(-100,ylim=c(yend,0),xlim=c(0,5),axes=FALSE,xlab="")
        for (focusyear in focusyears)  lines(c(0.5,2),y=rep(-3-which(focusyear==focusyears),2),
            lwd=2,col=rainbow(length(focusyears))[which(focusyear==focusyears)])
        text(y=-3-1:length(focusyears),x=2.5,labels=focusyears,adj=0)
        lines(c(0.5,2),y=c(-1,-1),lwd=2,col="BLACK")
        text(y=-1,x=2.5,labels="Mean",adj=0)
        lines(c(0.5,2),y=c(-2,-2),lwd=2,col="GRAY")
         text(y=-2,x=2.5,labels="Std. dev.",adj=0)}
      
      if(imageout)
      {par(mar=c(5.1,0,4.1,0))
        yend<-min(-(length(focusyears)+3),-20)
        plot(-100,ylim=c(yend,0),xlim=c(0,5),axes=FALSE,xlab="")
        for (focusyear in focusyears)  lines(c(0.5,2),y=rep(-3-which(focusyear==focusyears),2),
                                             lwd=4,col=rainbow(length(focusyears))[which(focusyear==focusyears)])
        text(y=-3-1:length(focusyears),x=2.5,labels=focusyears,adj=0,cex=3)
        lines(c(0.5,2),y=c(-1,-1),lwd=4,col="BLACK")
        text(y=-1,x=2.5,labels="Mean",adj=0,cex=3)
        lines(c(0.5,2),y=c(-2,-2),lwd=4,col="GRAY")
        if(fonttype=="mono")  text(y=-2,x=2.5,labels="St.dev.",adj=0,cex=3) else
          text(y=-2,x=2.5,labels="Std. dev.",adj=0,cex=3)}      

    }
 }
 


 

dc<-daily_chill$daily_chill
dc[,"JDay"]<-strptime(paste(dc$Month,"/",dc$Day,"/",dc$Year,sep=""),"%m/%d/%Y")$yday+1
dc<-dc[1:max(which(!(dc$no_Tmin|dc$no_Tmax))),]
if(enddate>startdate) relevant_days<-c(startdate:enddate) else relevant_days<-c(startdate:366,1:enddate)
relevant_days<-relevant_days[which(relevant_days %in% unique(dc$JDay))]

if(enddate>startdate) dc[which(dc$JDay %in% relevant_days),"End_year"]<-dc[which(dc$JDay %in% relevant_days),"Year"] else
{dc[which(dc$JDay %in% c(1:enddate)),"End_year"]<-dc[which(dc$JDay %in% c(1:enddate)),"Year"]
dc[which(dc$JDay %in% c(startdate:366)),"End_year"]<-dc[which(dc$JDay %in% c(startdate:366)),"Year"]+1}
if(is.na(useyears[1])) useyears<-unique(dc$End_year)[which(!is.na(unique(dc$End_year)))]
dc<-dc[which(dc$End_year %in% useyears),]
if(!focusyears[1]=="none") focusyears<-focusyears[which(focusyears %in% dc$End_year)]
if(length(focusyears)==0) focusyears<-"none"

if(is.na(metrics[1])) metrics<-colnames(dc) [which(!colnames(dc) %in% c("YYMMDD","Year","Month","Day","Tmean","JDay","End_year","no_Tmin","no_Tmax"))]
if(is.na(metriclabels[1])) metriclabels<-metrics

df<-list()
focusdata=list()

if(startdate<enddate) Jdays<-relevant_days else
  Jdays<-c((c(startdate:366)-366),c(1:enddate))

for (met in metrics)
{
  
  if(cumulative==TRUE)
        for(e in unique(dc$End_year))
         dc[which(dc$End_year==e&(dc$JDay %in% relevant_days)),met]<-cumsum(dc[which(dc$End_year==e&(dc$JDay %in% relevant_days)),met])
  dc[which(!dc$JDay %in% relevant_days)]<-0
  
  df[[met]]<-data.frame(JDay=NA,Mean=NA,Sd=NA)

  if(cumulative==TRUE)
  {
  complete<-data.frame(End_year=unique(dc$End_year),ndays=NA)
  for(e in unique(dc$End_year))
   complete[which(complete$End_year==e),"ndays"]<-length(which(dc$End_year==e&dc$JDay %in% relevant_days))
  
  exclude_from_mean<-complete[which(complete$ndays<max(complete$ndays)*0.9),"End_year"]
  } else exclude_from_mean=NULL
  
  for (i in 1:length(relevant_days)) {df[[met]][i, ] <- c(Jdays[i],mean(dc[which((!dc$End_year %in% exclude_from_mean)&(dc$JDay == relevant_days[i])),met],na.rm=TRUE),
                                                          sd(dc[which((!dc$End_year %in% exclude_from_mean)&(dc$JDay == relevant_days[i])),met],na.rm=TRUE))
  df[[met]][which(is.na(df[[met]][,"Sd"]))  ,"Sd" ]<-0
  }
  
  
  if(!is.null(focusyears)) if(!(focusyears[1]=="none")) {
  for(focusyear in focusyears)  
  {  
  focusdata<-dc[which(dc$End_year==focusyear),]
  if(startdate<enddate) focusdata[,"Jdays"]<-focusdata[,"JDay"] else
  {focusdata[which(focusdata$JDay<startdate),"Jdays"]<-focusdata$JDay[which(focusdata$JDay<startdate)]
  focusdata$Jdays[which(focusdata$JDay>=startdate)]<-focusdata$JDay[which(focusdata$JDay>=startdate)]-366
  }
  df[[met]][which(df[[met]]$JDay %in% focusdata$Jdays),as.character(focusyear)]<-focusdata[which(focusdata$Jdays %in% df[[met]]$JDay),met]
  }} else focusdata[[met]]<-"none" else focusdata[[met]]<-"none"  
}  
  
#if(metric=="Utah_Model") metric<-"Utah_Chill_Units"



for(met in metrics)
{metlab<-metriclabels[which(met==metrics)]
if(!is.na(image_type)) if(image_type=="png") {dir.create(outpath)
  png(file.path(outpath,paste(filename,"_",met,".png",sep="")),width=1250,height=1000)
  imageout<-TRUE} else imageout<-FALSE else imageout<-FALSE
par(family=fonttype)
if (!cumulative)  make_plot(plottable=df[[met]],name=metlab,ylabel=paste(metlab,"per day"),focusyears,imageout,fonttype)
if (cumulative)   make_plot(plottable=df[[met]],name=metlab,ylabel=paste("Cumulative", metlab),focusyears,imageout,fonttype)
if(!imageout) if(length(metrics)>1&!which(metrics==met)==length(metrics)) readline()
if(!is.na(image_type)) if(image_type=="png") dev.off()
}
  
  #else "no data to show"
return(df)
}




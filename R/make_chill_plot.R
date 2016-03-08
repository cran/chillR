
make_chill_plot<-function(chill,model,start_year=1990,end_year=2020,metriclabel=NULL,misstolerance=10,
                          image_type=NA,outpath=NA,filename=NA,fonttype='serif')
{

  if(is.null(metriclabel)) metriclabel<-model
  allyears<-c(start_year:end_year)
  NAy<-allyears[which(!(allyears %in% chill$End_year))]
  NAy<-sort(c(NAy,chill$End_year[which(chill$Perc_complete<100-misstolerance)]))
  NAyears<-which(allyears %in% NAy)
  
  missingyears<-allyears[which(!(allyears %in% chill$End_year ))]
  percMissDay<-1+10*round(100-chill$Perc_complete)[which(chill$End_year %in% allyears)]
  names(percMissDay)<-chill$End_year[which(chill$End_year %in% allyears)]
  percMissDay<-c(percMissDay,rep(1,length(missingyears)))
  names(percMissDay)<-c(chill$End_year[which(chill$End_year %in% allyears)],missingyears)
  percMissDay<-percMissDay[sort(names(percMissDay))]
  QC_colors<-gray.colors(1001,0,1)[percMissDay]
  
  metric<-percMissDay
  metric[]<-NA
  metric[which(names(metric) %in% chill$End_year)]<-chill[which(chill$End_year %in% allyears),model]
  metric[which(names(metric) %in% NAy)]<-NA
  
  if(!is.na(image_type)) if(image_type=="png") {dir.create(outpath)
    png(file.path(outpath,paste(filename,"_",model,".png",sep="")),width=1250,height=1000)
    imageout<-TRUE} else imageout<-FALSE else imageout<-FALSE
      par(family=fonttype)
      layout(t(1:2), widths=c(10,1))
      if (!imageout)
        {par(mar=rep(.5, 4), oma=c(3,4.5,.5,3))
        if(length(which(!is.na(metric)))==0) {yl<-c(0,10);starY<-1} else {yl<-c(min(metric,na.rm=TRUE),max(metric,na.rm=TRUE)*1.02)
                                                                          if(yl[1]>0) yl[1]<-0
                                                                          starY<-max(metric/10,na.rm=TRUE)}
        b<-barplot(metric,ylab=metriclabel,xlab="Year (end of chilling season)",col=QC_colors,border=QC_colors,ylim=yl,names.arg="")
        labs<-pretty(as.numeric(names(metric)))
        labs<-labs[which(labs %in% names(metric))]
        axis(1,at=b[which(names(metric) %in% labs)],labels=labs,padj=0.5)
        text(x=b[NAyears,1],y=starY,labels="*",cex=2)
        mtext("Year (end of time interval)",1,line=2.3)
        mtext(metriclabel,2,line=3,par(las=0))
        box(which="plot")
        image(1, c(0:100), t(seq_along(c(1:100))), col=gray.colors(1001,0,1), axes=FALSE)}     
      
      if (imageout)  {par(mar=rep(1, 4), oma=c(7,7.5,1,7))
        if(length(which(!is.na(metric)))==0) {yl<-c(0,10);starY<-1} else {yl<-c(min(metric,na.rm=TRUE),max(metric,na.rm=TRUE)*1.02)
                                                                          if(yl[1]>0) yl[1]<-0
                                                                          starY<-max(metric/10,na.rm=TRUE)}      
        b<-barplot(metric,ylab=metriclabel,xlab="Year (end of chilling season)",col=QC_colors,border=QC_colors,ylim=yl,
                   cex.axis=3,cex.names=3,axisnames=TRUE,names.arg="")
        labs<-pretty(as.numeric(names(metric)))
        labs<-labs[which(labs %in% names(metric))]
        axis(1,at=b[which(names(metric) %in% labs)],labels=labs,lwd=3,cex.axis=3,padj=0.5)
        axis(2,lwd=3,labels=FALSE)
        text(x=b[NAyears,1],y=starY,labels="*",cex=3)
        mtext("Year (end of time interval)",1,line=5.5,cex=4,font=2)
        mtext(metriclabel,2,line=5,par(las=0),cex=4,font=2)
        box(which="plot",lwd=3)
        image(1, c(0:100), t(seq_along(c(1:100))), col=gray.colors(1001,0,1), axes=FALSE)
        }
        
  

     
  if(!imageout)   
     {rect(0,misstolerance,2,100, density = NULL, angle = 45,
     col = "WHITE", border = NA, lty = par("lty"), lwd = par("lwd"))
     lines(x=c(-1,5),y=c(misstolerance,misstolerance))
      text(x=1,y=mean(c(100,misstolerance)),labels="*",cex=2)
      axis(4)
      mtext(4,text="Missing values (%)",par(las=0),line=2)
      box(which="plot")}
     
  if(imageout)   
     {rect(0,misstolerance,2,100, density = NULL, angle = 45,
           col = "WHITE", border = NA, lty = par("lty"), lwd =3)
       lines(x=c(-1,5),y=c(misstolerance,misstolerance),lwd=3)
       text(x=1,y=mean(c(100,misstolerance)),labels="*",cex=4)
       axis(4,lwd=3,cex.axis=3,padj=1)
       mtext(4,text="Missing values (%)",par(las=0),line=6,cex=4,font=2)
       box(which="plot",lwd=3)}
  
  if(!is.na(image_type)) if(image_type=="png") dev.off()
}


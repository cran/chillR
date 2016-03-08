make_multi_pheno_trend_plot <-
  function(pheno_list,fixed_weather,
           split_month=6,   #last month in same year
           outpath=NA,
           file_name=NA,
           image_type="png",
           fonttype="serif",percol=5)
    
  {
    read_pheno_flex<-function(phfile)
    {csv<-read.csv(phfile)
    if(length(colnames(csv))>=2&length(grep("Year",colnames(csv),ignore.case=TRUE)>0)&
       length(grep("Pheno",colnames(csv),ignore.case=TRUE))>0)
      out<-csv else
      {csv<-read.table(phfile,sep=";",header=TRUE)
      if(length(colnames(csv))>=2&length(grep("Year",colnames(csv),ignore.case=TRUE)>0)&
         length(grep("Pheno",colnames(csv),ignore.case=TRUE))>0)
        out<-csv else warning(paste(phfile,"is not a recognized pheno file"))}
    colnames(out)<-c("Year","pheno")
    out$Year<-as.numeric(out$Year)
    out$pheno<-as.numeric(as.character(out$pheno))
    return(out)
    }
    
    divable<-function(x,div=2)
    {x/div==floor(x/div)}
    
    weather<-fixed_weather$weather
    #if(!use_Tmean) weather[,"Tmean"]<-(weather$Tmax+weather$Tmin)/2
    
    stack<-stack_hourly_temps(fixed_weather,34.71)$hourtemps
    stack[which(stack$Month<=split_month),"Season"]<-stack[which(stack$Month<=split_month),"Year"]
    stack[which(stack$Month>split_month),"Season"]<-stack[which(stack$Month>split_month),"Year"]+1
    stack[,"Date"]<-stack$Month*100+stack$Day
    stack[,"JDay"]<-strptime(paste(stack$Month,"/",stack$Day,"/",stack$Year,sep=""),"%m/%d/%Y")$yday+1
    sea<-unique(stack$Season)
    res<-data.frame(Season=sea,Chill_Tmean=NA,Heat_Tmean=NA,Year_Tmean=NA)
    
    k<-list()
    chillmax<-chillmin<-heatmax<-heatmin<-phenomin<-phenomax<-NA
    for (cult in 1:nrow(pheno_list))
    {
      sea<-unique(stack$Season)
      res<-data.frame(Season=sea,Chill_Tmean=NA,Heat_Tmean=NA,Year_Tmean=NA)
      
      if(pheno_list$Start_chill[cult]>pheno_list$End_chill[cult])
        chill_days<-c(pheno_list$Start_chill[cult]:366,1:pheno_list$End_chill[cult]) else
          chill_days<-pheno_list$Start_chill[cult]:pheno_list$End_chill[cult]
      if(pheno_list$Start_heat[cult]>pheno_list$End_heat[cult])
        heat_days<-c(pheno_list$Start_heat[cult]:366,1:pheno_list$End_heat[cult]) else
          heat_days<-pheno_list$Start_heat[cult]:pheno_list$End_heat[cult]
      
      for (s in sea)
      {res[which(res$Season==s),"Chill_Tmean"]<-mean(stack[which(stack$Season==s&stack$JDay %in% chill_days),"Temp"])
      res[which(res$Season==s),"Heat_Tmean"]<-mean(stack[which(stack$Season==s&stack$JDay %in% heat_days),"Temp"])
      res[which(res$Season==s),"Year_Tmean"]<-mean(stack[which(stack$Season==s),"Temp"])}
      
      pheno<-suppressWarnings(read_pheno_flex(as.character(pheno_list$Link[cult])))
      
      for (i in 1:nrow(pheno))
      {if(pheno[i,1] %in% res$Season)
      {pheno[i,"Chill_Tmean"]<-res[which(res$Season==pheno[i,1]),"Chill_Tmean"]
      pheno[i,"Heat_Tmean"]<-res[which(res$Season==pheno[i,1]),"Heat_Tmean"]
      pheno[i,"Year_Tmean"]<-res[which(res$Season==pheno[i,1]),"Year_Tmean"]
      }}
      
      pheno[,2]<-as.numeric(as.character(pheno[,2]))
      pheno<-pheno[which(!is.na(pheno[,"pheno"])),]
      pheno<-pheno[which(!is.na(pheno[,"Chill_Tmean"])),]
      pheno<-pheno[which(!is.na(pheno[,"Heat_Tmean"])),]
      pheno<-pheno[which(!is.na(pheno[,"Year_Tmean"])),]
      k[[cult]]<-Krig(x=as.matrix(pheno[,c("Chill_Tmean","Heat_Tmean")]),Y=pheno$pheno)
      chillmin<-min(c(chillmin,pheno$Chill_Tmean),na.rm=TRUE)
      chillmax<-max(c(chillmax,pheno$Chill_Tmean),na.rm=TRUE)
      heatmin<-min(c(heatmin,pheno$Heat_Tmean),na.rm=TRUE)
      heatmax<-max(c(heatmax,pheno$Heat_Tmean),na.rm=TRUE)
      phenomax<-max(c(phenomax,pheno$pheno),na.rm=TRUE)
      phenomin<-min(c(phenomin,pheno$pheno),na.rm=TRUE)
    }
    
     xlabel<-paste("Mean temperature during the chilling phase (deg. C)",sep="")
     ylabel<-paste("Mean temperature during the forcing phase (deg. C)",sep="")
    
    if(image_type=="png")
    {png(paste(outpath,file_name,".png",sep=""),width=1200,height=1000)
      lwds<-2;cexs<-3} else {lwds<-1;cexs<-1}
    
    par(family=fonttype)
    perrow<-ceiling(length(k)/percol)
    par(mfcol=c(percol,perrow))
    if(image_type=="png") par(oma=c(8,8,8,22)) else par(oma=c(4,5,4,14))
    par(mar=c(0,0,0,0))
    for(i in 1:length(k))
    {par(mar=c(0,0,0,0))
      image(predictSurface(k[[i]]),xlim=c(chillmin,chillmax),ylim=c(heatmin-(heatmax-heatmin)/6,heatmax),zlim=c(phenomin,phenomax),
            xaxs="i",axes=FALSE,col = tim.colors())
      if(image_type=="png") contour(predictSurface(k[[i]]),add=TRUE,nlevels=5,labcex=1.5) else
        contour(predictSurface(k[[i]]),add=TRUE,nlevels=5)
      box("plot",lwd=lwds)
      if(image_type=="png") mtext(pheno_list$varieties[i],line=-2,side=1,cex=1.6) else
        mtext(pheno_list$varieties[i],line=-1.2,side=1,cex=0.8)
      if(image_type=="png")
      {if (divable(i,percol)&divable(i/percol)) axis(1,tck=-0.03,lwd=lwds,cex.axis=cexs,padj=1)
        if ((i<=percol)&!divable(i)) axis(2,tck=-0.03,lwd=lwds,cex.axis=cexs)  
        if (round((i/percol-floor(i/percol))*percol,7)==1&divable(floor(i/percol)))
          axis(3,tck=-0.03,lwd=lwds,cex.axis=cexs)
        if ((i>length(k)-percol)&divable(i)) axis(4,tck=-0.03,lwd=lwds,cex.axis=cexs,padj=1)
      } else
      {if (divable(i,percol)&divable(i/percol)) axis(1,tck=-0.03,lwd=lwds,cex.axis=cexs)
        if ((i<=percol)&!divable(i)) axis(2,tck=-0.03,lwd=lwds,cex.axis=cexs)  
        if (round((i/percol-floor(i/percol))*percol,7)==1&divable(floor(i/percol)))
          axis(3,tck=-0.03,lwd=lwds,cex.axis=cexs)
        if ((i>length(k)-percol)&divable(i)) axis(4,tck=-0.03,lwd=lwds,cex.axis=cexs)
      }
      axis(1,labels=FALSE,tck=0.02,lwd=lwds)
      axis(2,labels=FALSE,tck=0.03,lwd=lwds)
      axis(3,labels=FALSE,tck=0.03,lwd=lwds)
      axis(4,labels=FALSE,tck=0.03,lwd=lwds)
    }
    if(image_type=="png")    
    {mtext(xlabel,side=1,outer=TRUE,line=6,cex=2.5)
      mtext(xlabel,side=3,outer=TRUE,line=4,cex=2.5)
      mtext(ylabel,side=2,outer=TRUE,line=5,cex=2.5)
      mtext(ylabel,side=4,outer=TRUE,line=6,cex=2.5)} else
      {mtext(xlabel,side=1,outer=TRUE,line=2.5,cex=1.3*cexs)
        mtext(xlabel,side=3,outer=TRUE,line=2.5,cex=1.3*cexs)
        mtext(ylabel,side=2,outer=TRUE,line=3,cex=1.3*cexs)
        mtext(ylabel,side=4,outer=TRUE,line=3,cex=1.3*cexs)}
    
    opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    if(image_type=="png") {par(oma=c(12,0,12,4)) 
      image.plot(predictSurface(k[[i]]),xlim=c(chillmin,chillmax),ylim=c(heatmin,heatmax),zlim=c(phenomin,phenomax),
                 xaxs="i",axes=FALSE,col = tim.colors(),legend.only=TRUE,axis.args=list(cex.axis=cexs),
                 legend.lab="Bloom date (day of the year)",legend.cex=2.5,legend.line=6,lwd=lwds)} else
                 {par(oma=c(4,0,4,0))
                   image.plot(predictSurface(k[[i]]),xlim=c(chillmin,chillmax),ylim=c(heatmin,heatmax),zlim=c(phenomin,phenomax),
                              xaxs="i",axes=FALSE,col = tim.colors(),legend.only=TRUE,axis.args=list(cex.axis=cexs),
                              legend.lab="Bloom date (day of the year)",legend.cex=1.3,legend.line=3,lwd=lwds)}
    par(opar)
    if(image_type=="png") dev.off()
  }



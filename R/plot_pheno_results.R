#This function makes a pretty plot of the PLS regression results as a bmp image
#and saves it to the specified location.
#It also outputs there a table of the results.

plot_PLS<-function(PLS_output, PLS_results_path, VIP_threshold=0.8,colorscheme="color")
{ 
  if(!"object_type" %in% names(PLS_output)) return(
    "Error: not a PLS results object produced with chillR.")
  if(!PLS_output$object_type %in% c("PLS_Temp_pheno","PLS_chillforce_pheno")) return(
    "Error: not a PLS results object produced with chillR.")
  if(PLS_output$object_type=="PLS_Temp_pheno")
  {
      PLS_obj<-PLS_output["PLS_summary"]
      pnames<-colnames(PLS_obj$PLS_summary)
      PLS_obj<-as.data.frame(PLS_obj)
      colnames(PLS_obj)<-pnames
      VIPs<-PLS_obj[,"VIP"]
      Coefs<-PLS_obj[,"Coef"]
      
      lc <- 365
      leg <- 1:365
      leg <- strptime(strptime(paste("01/", trunc(PLS_obj$Date[1]/100), "/2001", 
                                     sep = ""), format = "%d/%m/%Y") - 86400 + leg * 86400,"%Y-%m-%d")
      tick_marks <- leg[sapply(strsplit(as.character(leg), "-"),"[", 3) == "01"]
      tick_labels <- as.Date(tick_marks, origin = "1999-1-2")
      tick_labels <- as.POSIXlt(tick_labels)$mon
      tick_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[tick_labels + 1]
      mean_clim <- PLS_obj$Tmean
      dev_clim <- PLS_obj$Tstdev
      tick_label_pos <- leg[sapply(strsplit(as.character(leg), 
                                            "-"), "[", 3) == "15"]
      
      bmp(paste(PLS_results_path, ".bmp", sep = ""), width = 2000, 
          height = 2000, pointsize = 20)
      par(mfcol = c(3, 1))
      par(mar = c(6.1, 9.4, 6.1, 2.1))
      par(mgp = c(4, 1.5, 0))
      
      if (colorscheme=="bw")     color_bars<-color_bar_maker(VIPs[1:lc],VIPs[1:lc],threshold=VIP_threshold,col1="BLACK",col2="BLACK",col3="GREY") else
        color_bars<-color_bar_maker(VIPs[1:lc],VIPs[1:lc],threshold=VIP_threshold,col1="DARK BLUE",col2="DARK BLUE",col3="DARK GREY")
      
      plot(leg, VIPs[1:lc], main = "VIP", xlab = NA, ylab = NA, 
           xaxs = "i", xaxt = "n", yaxs = "i", cex.lab = 4, cex.axis = 4, 
           cex.main = 5, type = "h", col = color_bars, lwd = 6)
      tick_marks <- grconvertX(tick_marks, from = "user", to = "user")
      X_coords <- grconvertX(leg, from = "user", to = "user")
      ticks_labels <- grconvertX(tick_label_pos, from = "user", 
                                 to = "user")
      axis(1, lwd.ticks = 3, at = tick_marks, labels = FALSE, cex.axis = 4, 
           padj = 1)
      axis(2, lwd.ticks = 3, labels = FALSE)
      box(which = "plot", lwd = 3)
      axis(1, lwd.ticks = 0, at = ticks_labels, labels = tick_labels, 
           cex.axis = 4, padj = 1)
      mtext(side = 2, text = "VIP", line = 6, cex = 3)
      
      if (colorscheme=="bw")     color_bars<-color_bar_maker(VIPs[1:lc], Coefs[1:lc],threshold=VIP_threshold,col1="BLACK",col2="#CECECE",col3="#7B7B7B") else
        color_bars<-color_bar_maker(VIPs[1:lc], Coefs[1:lc],threshold=VIP_threshold,col1 = "RED", col2 = "DARK GREEN",col3 = "DARK GREY")

      plot(leg, Coefs[1:lc], main = "Model coefficients", 
           ylab = NA, xlab = NA, xaxs = "i", xaxt = "n", yaxs = "i", 
           cex.lab = 4, cex.axis = 4, cex.main = 5, type = "h", 
           col = color_bars, lwd = 6)
      axis(1, lwd.ticks = 3, at = tick_marks, labels = FALSE, cex.axis = 4, 
           padj = 1)
      axis(2, lwd.ticks = 3, labels = FALSE)
      box(which = "plot", lwd = 3)
      axis(1, lwd.ticks = 0, at = ticks_labels, labels = tick_labels, 
           cex.axis = 4, padj = 1)
      mtext(side = 2, text = "Model coefficient", line = 6, cex = 3)

      if (colorscheme=="bw")     color_bars<-color_bar_maker(VIPs[1:lc], Coefs[1:lc],threshold=VIP_threshold,col1="BLACK",col2="#CECECE",col3="#7B7B7B") else
        color_bars<-color_bar_maker(VIPs[1:lc], Coefs[1:lc],threshold=VIP_threshold,col1 = "RED", col2 = "DARK GREEN",col3 = "DARK GREY")

      plot(leg, mean_clim[1:lc], main = "Mean temperature", ylab = NA, 
           xlab = NA, xaxs = "i", yaxs = "i", xaxt = "n", cex.lab = 4, 
           cex.axis = 4, cex.main = 5, type = "l", lwd = 3, col = "BLACK", 
           ylim = c(min(mean_clim[1:lc] - dev_clim[1:lc]), max(mean_clim[1:lc] + 
                                                                 dev_clim[1:lc])))
      arrows(X_coords, mean_clim[1:lc] + dev_clim[1:lc], X_coords, 
             mean_clim[1:lc] - dev_clim[1:lc], angle = 90, code = 3, 
             lwd = 6, length = 0, col = color_bars)
      lines(leg, mean_clim[1:lc], lwd = 3)
      axis(1, lwd.ticks = 3, at = tick_marks, labels = FALSE, cex.axis = 4, 
           padj = 1)
      axis(2, lwd.ticks = 3, labels = FALSE)
      box(which = "plot", lwd = 3)
      axis(1, lwd.ticks = 0, at = ticks_labels, labels = tick_labels, 
           cex.axis = 4, padj = 1)
      mtext(side = 2, text = expression("Mean temperature ("^"o" * 
                                          "C)"), line = 5, cex = 3)
      dev.off()
      write.csv(PLS_obj, paste(PLS_results_path, ".csv", sep = ""), 
                row.names = FALSE)
  }
  
  if(PLS_output$object_type=="PLS_chillforce_pheno")
  {
    HM<-"GDH"
    for (CM in c("Chilling_Hours","Utah_Chill_Units","Chill_Portions"))
    {
    PLS_obj<-PLS_output[[CM]]["PLS_summary"]
    pnames<-colnames(PLS_obj$PLS_summary)
    PLS_obj<-as.data.frame(PLS_obj)
    colnames(PLS_obj)<-pnames

    
    lc <- 365
    leg <- 1:365
    leg <- strptime(strptime(paste("01/", trunc(PLS_obj$Date[1]/100), "/2001", 
                                   sep = ""), format = "%d/%m/%Y") - 86400 + leg * 86400,"%Y-%m-%d")
    tick_marks <- leg[sapply(strsplit(as.character(leg), "-"),"[", 3) == "01"]
    tick_labels <- as.Date(tick_marks, origin = "1999-1-2")
    tick_labels <- as.POSIXlt(tick_labels)$mon
    tick_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[tick_labels + 1]
    mean_clim <- PLS_obj$CHmean
    dev_clim <- PLS_obj$CHstdev
    tick_label_pos <- leg[sapply(strsplit(as.character(leg), 
                                          "-"), "[", 3) == "15"]
    
    

    
    bmp(paste(PLS_results_path,"_",CM,"_",HM,".bmp", sep = ""), width = 4000, 
        height = 2000, pointsize = 20)    

    par(mfcol=c(3,2))
    par(mar=c(6.1,9.1,6.1,2.1))
    par(mgp=c(4, 1.5, 0))                       
    for (graph in c("Chill","Heat"))
    {
      if(graph=="Chill") {VIPs<-PLS_obj[1:lc,"VIP"]
                          Coefs<-PLS_obj[1:lc,"Coef"]
                          mean_climg<-PLS_obj[1:lc,"CHmean"]
                          dev_climg<-PLS_obj[1:lc,"CHstdev"]}
      if(graph=="Heat") {VIPs<-PLS_obj[-(1:lc),"VIP"]
                         Coefs<-PLS_obj[-(1:lc),"Coef"]
                         mean_climg<-PLS_obj[-(1:lc),"CHmean"]
                         dev_climg<-PLS_obj[-(1:lc),"CHstdev"]}
      
      
      if (colorscheme=="bw")     color_bars<-color_bar_maker(VIPs,VIPs,threshold=VIP_threshold,col1="BLACK",col2="BLACK",col3="GREY") else
        color_bars<-color_bar_maker(VIPs,VIPs,threshold=VIP_threshold,col1="DARK BLUE",col2="DARK BLUE",col3="DARK GREY")
      plot(leg,VIPs,main="VIP",xlab=NA,ylab=NA,xaxs="i",xaxt="n",yaxs="i",cex.lab=4,cex.axis=4,cex.main=5,type="h",col=color_bars,lwd=6)
      tick_marks<-grconvertX(tick_marks, from="user", to="user");X_coords<-grconvertX(leg, from="user", to="user")
      ticks_labels<-grconvertX(tick_label_pos, from="user", to="user")
      axis(1,lwd.ticks=3,at=tick_marks,labels=FALSE,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
      axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,cex.axis=4,padj=1)  
      mtext(side=2,text="VIP",line=6,cex=3)
      if (colorscheme=="bw")     color_bars<-color_bar_maker(VIPs,Coefs,threshold=VIP_threshold,col1="BLACK",col2="#CECECE",col3="#7B7B7B") else
        color_bars<-color_bar_maker(VIPs,Coefs,threshold=VIP_threshold,col1="RED",col2="DARK GREEN",col3="DARK GREY")
      plot(leg,Coefs,main="Model coefficients",ylab=NA,xlab=NA,xaxs="i",xaxt="n",yaxs="i",cex.lab=4,cex.axis=4,cex.main=5,type="h",col=color_bars,lwd=6)
      axis(1,lwd.ticks=3,at=tick_marks,labels=FALSE,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
      axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,cex.axis=4,padj=1)
      
      mtext(side=2,text="Model coefficient",line=6,cex=3)
      
    
      if (colorscheme=="bw") color_bars<-color_bar_maker(VIPs,Coefs,threshold=VIP_threshold,col1="BLACK",col2="#CECECE",col3="#7B7B7B") else
        color_bars<-color_bar_maker(VIPs,Coefs,threshold=VIP_threshold,col1="RED",col2="DARK GREEN",col3="DARK GREY")
      if(graph=="Chill") plot(leg,mean_climg,main="Chill accumulation",ylab=NA,xlab=NA,xaxs="i",yaxs="i",xaxt="n",cex.lab=4,cex.axis=4,cex.main=5,type="l",lwd=3,col="BLACK",ylim=c(min(mean_climg-dev_climg),max(mean_climg+dev_climg)))
      if(graph=="Heat") plot(leg,mean_climg,main="Heat accumulation",ylab=NA,xlab=NA,xaxs="i",yaxs="i",xaxt="n",cex.lab=4,cex.axis=4,cex.main=5,type="l",lwd=3,col="BLACK",ylim=c(min(mean_climg-dev_climg),max(mean_climg+dev_climg)))
      
      
      arrows(X_coords,mean_climg+dev_climg,X_coords,mean_climg-dev_climg, angle=90, code=3,lwd=6,length=0,col=color_bars)
      lines(leg,mean_climg[1:lc],lwd=3)
      axis(1,lwd.ticks=3,at=tick_marks,labels=FALSE,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
      axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,cex.axis=4,padj=1) 
      if(graph=="Chill") 
      {ccc<-unlist(strsplit(CM,"_")); cctemp<-ccc[1]; if(length(ccc)>1) {for (i in 2:length(ccc)) cctemp<-paste(cctemp,ccc[i])}
       mtext(side=2,text=paste(cctemp,"per day"),line=5,cex=3)}
      if(graph=="Heat") 
      {ccc<-unlist(strsplit(HM,"_")); cctemp<-ccc[1]; if(length(ccc)>1) {for (i in 2:length(ccc)) cctemp<-paste(cctemp,ccc[i])}
       mtext(side=2,text=paste(cctemp,"per day"),line=5,cex=3)}  
    }
    dev.off()
    write.csv(PLS_obj, paste(PLS_results_path,"_",CM,"_",HM,".csv",sep=""), 
              row.names = FALSE)
    }
  }
}

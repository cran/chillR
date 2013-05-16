PLS_pheno <-
function (weather_data_frame,bio_data_frame,split_month,PLS_results_path,runn_mean,crossvalidate=TRUE,use_Tmean=FALSE)
      {
        require(pls)
        
        weather_file<-weather_data_frame
        weather_file$Tmax<-suppressWarnings(as.numeric(as.character(weather_file$Tmax)))
        weather_file$Tmin<-suppressWarnings(as.numeric(as.character(weather_file$Tmin)))
        suppressWarnings(weather_file[which(weather_file$Tmin>weather_file$Tmax),c("Tmin","Tmax")]<-NA)

        
        
        bio_data<-bio_data_frame
        
        
        runn_mean<-11
        
            
        Tmin_gaps<-interpolate_gaps(weather_file$Tmin)
        weather_file$Tmin<-Tmin_gaps[[1]]
        weather_file[,"Tmin_interpolated"]<-0
        suppressWarnings(weather_file[Tmin_gaps[[2]],"Tmin_interpolated"]<-1)
        Tmax_gaps<-interpolate_gaps(weather_file$Tmax)
        weather_file$Tmax<-Tmax_gaps[[1]]
        weather_file[,"Tmax_interpolated"]<-0
        suppressWarnings(weather_file[Tmax_gaps[[2]],"Tmax_interpolated"]<-1)
        
        if(use_Tmean)
        {Tmean_gaps<-interpolate_gaps(weather_file$Tmean)
         weather_file$Tmean<-Tmean_gaps[[1]]
         weather_file[,"Tmean_interpolated"]<-0
         weather_file[Tmean_gaps[[2]],"Tmean_interpolated"]<-1} else
            
        {weather_file[,"Tmean"]<-(weather_file$Tmax+weather_file$Tmin)/2}
        
        weather_file[which(weather_file$Month<=split_month),"Season"]<-weather_file[which(weather_file$Month<=split_month),"Year"]
        weather_file[which(weather_file$Month>split_month),"Season"]<-weather_file[which(weather_file$Month>split_month),"Year"]+1
        weather_file[,"Date"]<-weather_file$Month*100+weather_file$Day
        weather_file[,"JDay"]<-strptime(paste(weather_file$Month,"/",weather_file$Day,"/",weather_file$Year,sep=""),"%m/%d/%Y")$yday+1

        #running mean
        
        ww<-weather_file[,"Tmean"]
        rr<-weather_file[,"Tmean"]        
        for (dd in 1:length(ww))
            {if (dd<ceiling(runn_mean/2))
                 {rr[dd]<-mean(ww[1:(dd+floor(runn_mean/2))])}
             if ((dd>=ceiling(runn_mean/2))&(dd<=length(ww)-ceiling(runn_mean/2)))
                 {rr[dd]<-mean(ww[(dd-floor(runn_mean/2)):(dd+floor(runn_mean/2))])}
             if (dd>(length(ww)-ceiling(runn_mean/2)))
                 {rr[dd]<-mean(ww[(dd-floor(runn_mean/2)):length(ww)])}
            }
        weather_file[,"runn"]<-rr
        
        VIP <- function(object) {
            if (object$method != "oscorespls")
                stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
            if (nrow(object$Yloadings) > 1)
                stop("Only implemented for single-response models")
        
            SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
            Wnorm2 <- colSums(object$loading.weights^2)
            SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
            sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
        }
        
        pls_ncomp<-function(indep,dep,threshold=30)
        {dat<-data.frame(dep)
         dat$runn<-indep
         if(length(dep)>15)
         {
         suppressWarnings(pls_out<-plsr(dep ~ runn, data=dat, ncomp=10, validation = "CV"))
         suppressWarnings(pls_cv<-crossval(pls_out,segments=10))
         res<-data.frame(ncomp=c(1:10),explvar=explvar(pls_out),cumul=NA)
         res$cumul[1]<-res$explvar[1]
         for (i in 2:nrow(res)) {res$cumul[i]<-res$cumul[i-1]+res$explvar[i]}
         ncomp<-which(res$cumul>threshold)[1]
         if(is.na(ncomp)) ncomp<-10} else ncomp<-2
         return(ncomp)
        }

        
        
        seasons<-unique(weather_file$Season)
        
             for (yy in seasons)
                {yearweather<-weather_file[which(weather_file$Season==yy),]
                 weathervector<-c(yearweather$runn[1:365])
                 if(yy==seasons[1]) year_res<-weathervector else
                 year_res<-rbind(year_res,weathervector)
                 if(nrow(yearweather)==365) {labdates<-yearweather$Date;labJdates<-yearweather$JDay}
                 }
                 
                 colnames(year_res)<-c(paste("runn_",1:365,sep=""))
                          year_res<-cbind(Season=seasons,year_res)
                 data_set<-year_res
 
        
        full_seasons<-which(!is.na(rowMeans(data_set)))
        data_set<-data_set[full_seasons,]
        newseasons<-data_set[,"Season"]
            suppressWarnings(bio_data<-bio_data[which(bio_data[,"Year"] %in% newseasons),])
            suppressWarnings(bio_data<-bio_data[which(!is.na(as.numeric(as.character(bio_data$pheno)))),])
            suppressWarnings(bio<-as.numeric(as.character(bio_data$pheno)))
            indep<-as.matrix(data_set[which(data_set[,"Season"] %in% bio_data$Year),])
            indep<-indep[,2:ncol(indep)]

        if(crossvalidate) {ncomp<-pls_ncomp(indep=indep,dep=bio)
            } else ncomp=crossvalidate
 
        sdindep<-apply(indep,2,sd)
        sdindep[which(sdindep==0)]<-1
        
        PLS_output<-plsr(bio ~ indep,ncomp,validation="none",method="oscorespls",scale=sdindep)#    data=res_soil,validation="CV",method="oscorespls")
        
        
        
       #     PLS_output<-plsr(bio ~ indep,ncomp,validation="CV",method="oscorespls")#    data=res_soil,validation="CV",method="oscorespls")

            out<-data.frame()
            out[1:365,"Date"]<-labdates
            out[1:365,"JDay"]<-labJdates
            out[1:365,"Coeff"]<-coef(PLS_output)
            out[1:365,"VIP"]<-VIP(PLS_output)[ncomp,]
            colnames(out)<-c("Date","JDay","Coefficient","VIP")
            if(is.na(out[1,"VIP"])) {out[,"VIP"]<-0; out[,"coef"]<-0;nothing<-TRUE} else {nothing<-FALSE}

 
            lc<-365
            leg<-1:365

        strptime(paste("01/",split_month+1,"/2001",sep=""),format="%d/%m/%Y")-86400
        
            leg<-strptime(strptime(paste("01/",split_month+1,"/2001",sep=""),format="%d/%m/%Y")-86400+leg*86400,"%Y-%m-%d")
            tick_marks<-leg[sapply(strsplit(as.character(leg),"-"),"[",3)=="01"]
            tick_labels<-as.Date(tick_marks,origin="1999-1-2")
            tick_labels<-as.POSIXlt(tick_labels)$mon
            tick_labels<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")[tick_labels+1]
            mean_clim<-colMeans(indep,na.rm=TRUE)
            dev_clim<-apply(indep,2,sd,na.rm=TRUE)
            tick_label_pos<-leg[sapply(strsplit(as.character(leg),"-"),"[",3)=="15"]
        
        
        


            bmp(paste(PLS_results_path,".bmp",sep=""),width=2000,height=2000,pointsize = 20)
            par(mfcol=c(3,1))
            par(mar=c(6.1,9.4,6.1,2.1))
            par(mgp=c(4, 1.5, 0))
            color_bars<-color_bar_maker(out[1:lc,"VIP"],out[1:lc,"VIP"],threshold=0.8,col1="DARK BLUE",col2="DARK BLUE",col3="DARK GREY")
            plot(leg,out[1:lc,"VIP"],main="VIP",xlab=NA,ylab=NA,xaxs="i",xaxt="n",yaxs="i",cex.lab=4,cex.axis=4,cex.main=5,type="h",col=color_bars,lwd=6)
            tick_marks<-grconvertX(tick_marks, from="user", to="user");X_coords<-grconvertX(leg, from="user", to="user")
            ticks_labels<-grconvertX(tick_label_pos, from="user", to="user")
        axis(1,lwd.ticks=3,at=tick_marks,labels=FALSE,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
        axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,cex.axis=4,padj=1)  
            mtext(side=2,text="VIP",line=6,cex=3)
                if(nothing) {text(x=(max(X_coords)-min(X_coords))/2+min(X_coords),y=0.5,adj=0.5,labels=("No yield produced in any model run"),cex=2)}
            color_bars<-color_bar_maker(out[1:lc,"VIP"],out[1:lc,"Coefficient"],threshold=0.8,col1="RED",col2="DARK GREEN",col3="DARK GREY")
            plot(leg,out[1:lc,"Coefficient"],main="Model coefficients",ylab=NA,xlab=NA,xaxs="i",xaxt="n",yaxs="i",cex.lab=4,cex.axis=4,cex.main=5,type="h",col=color_bars,lwd=6)
        axis(1,lwd.ticks=3,at=tick_marks,labels=FALSE,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
        axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,cex.axis=4,padj=1)  
            mtext(side=2,text="Model coefficient",line=6,cex=3)
 
                if(nothing) {text(x=(max(X_coords)-min(X_coords))/2+min(X_coords),y=0.5,adj=0.5,labels=("No yield produced in any model run"),cex=2)}

            color_bars<-color_bar_maker(out[1:lc,"VIP"],out[1:lc,"Coefficient"],threshold=0.8,col1="RED",col2="DARK GREEN",col3="DARK GREY")
            plot(leg,mean_clim[1:lc],main="Mean temperature",ylab=NA,xlab=NA,xaxs="i",yaxs="i",xaxt="n",cex.lab=4,cex.axis=4,cex.main=5,type="l",lwd=3,col="BLACK",ylim=c(min(mean_clim[1:lc]-dev_clim[1:lc]),max(mean_clim[1:lc]+dev_clim[1:lc])))
            arrows(X_coords,mean_clim[1:lc]+dev_clim[1:lc],X_coords,mean_clim[1:lc]-dev_clim[1:lc], angle=90, code=3,lwd=6,length=0,col=color_bars)
            lines(leg,mean_clim[1:lc],lwd=3)
        axis(1,lwd.ticks=3,at=tick_marks,labels=FALSE,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
        axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,cex.axis=4,padj=1)  
        
        mtext(side=2,text=expression('Mean temperature ('^'o'*'C)'),line=5,cex=3)
 
            dev.off()
            write.csv(out,paste(PLS_results_path,".csv",sep=""),row.names=FALSE)
            write.csv(weather_file,paste(PLS_results_path,"_interpolated_weather.csv",sep=""),row.names=FALSE)
         return(list(weather_file=weather_file,PLS_output=out))
         }

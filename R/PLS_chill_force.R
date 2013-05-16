PLS_chill_force <-
function (daily_chill,bio_data_frame,split_month,PLS_results_path,crossvalidate=TRUE,image_type="png",colorscheme="normal")
{
  if(daily_chill[1]=="daily_chill")
  {dc<-daily_chill$daily_chill
   
     
  require(pls)
  
  weather_file<-daily_chill$daily_chill
  
  
  bio_data<-bio_data_frame
  
  
  
  weather_file[which(weather_file$Month<=split_month),"Season"]<-weather_file[which(weather_file$Month<=split_month),"Year"]
  weather_file[which(weather_file$Month>split_month),"Season"]<-weather_file[which(weather_file$Month>split_month),"Year"]+1
  weather_file[,"Date"]<-weather_file$Month*100+weather_file$Day
  weather_file[,"JDay"]<-strptime(paste(weather_file$Month,"/",weather_file$Day,"/",weather_file$Year,sep=""),"%m/%d/%Y")$yday+1
  
   
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

   
   chill_models<-c("Chilling_Hours","Utah_Chill_Units","Chill_Portions")
   heat_models<-c("GDH")
   
for(CM in chill_models)
  for(HM in heat_models)
  {
#   CM<-"Chill_Portions"
#   HM<-"GDH"

   
   
  #a loop for multiple model evaluations should start here
   
   
   for (yy in seasons)
  {yearweather<-weather_file[which(weather_file$Season==yy),]
   weathervector<-c(yearweather[1:365,CM],yearweather[1:365,HM])
   if(yy==seasons[1]) year_res<-weathervector else
     year_res<-rbind(year_res,weathervector)
   if(nrow(yearweather)==365) {labdates<-yearweather$Date;labJdates<-yearweather$JDay}
  }
  
  colnames(year_res)<-c(paste("Chill_",1:365,sep=""),paste("Heat_",1:365,sep=""))
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
  
  out<-data.frame()
  out[1:730,"Date"]<-labdates
  out[1:730,"Type"]<-c(rep("Chill",365),rep("Heat",365))
  out[1:730,"JDay"]<-labJdates
  out[1:730,"Coeff"]<-coef(PLS_output)
  out[1:730,"VIP"]<-VIP(PLS_output)[ncomp,]
  colnames(out)<-c("Date","Type","JDay","Coefficient","VIP")
  if(is.na(out[1,"VIP"])) {out[,"VIP"]<-0; out[,"coef"]<-0;nothing<-TRUE} else {nothing<-FALSE}
  
  color_bar_maker<-function(columns_yn,columns_quant,threshold,col1,col2,col3)
  {
    color_bars<-c(rep(NA,length(columns_yn)))
    color_bars[which(columns_yn>=threshold&columns_quant<0)]<-col1
    color_bars[which(columns_yn>=threshold&columns_quant>=0)]<-col2
    color_bars[which(!columns_yn>=threshold)]<-col3
    return(color_bars)}
  
  lc<-365
  leg<-1:365
  
#  strptime(paste("01/",split_month+1,"/2001",sep=""),format="%d/%m/%Y")-86400
  
  leg<-strptime(strptime(paste("01/",split_month+1,"/2001",sep=""),format="%d/%m/%Y")-86400+leg*86400,"%Y-%m-%d")
  tick_marks<-leg[sapply(strsplit(as.character(leg),"-"),"[",3)=="01"]
  tick_labels<-as.Date(tick_marks,origin="1999-1-2")
  tick_labels<-as.POSIXlt(tick_labels)$mon
  tick_labels<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")[tick_labels+1]
  mean_clim<-colMeans(indep,na.rm=TRUE)
  dev_clim<-apply(indep,2,sd,na.rm=TRUE)
   tick_label_pos<-leg[sapply(strsplit(as.character(leg),"-"),"[",3)=="15"]
   #ticks_labels<-as.numeric(format(strptime(tick_label_pos,"%Y-%m-%d"),format="%j"))
   
  
if (image_type=="tiff") tiff(paste(PLS_results_path,"_",CM,"_",HM,".tif",sep=""),width=4000,height=2000,pointsize = 20) else
  png(paste(PLS_results_path,"_",CM,"_",HM,".png",sep=""),width=4000,height=2000,pointsize = 20)
   #layout(matrix(data=c(1,2,3,4,5,6,7,8),nrow=2),height(0.2,1,1,1))
   par(mfcol=c(3,2))
   par(mar=c(6.1,9.1,6.1,2.1))
  par(mgp=c(4, 1.5, 0))                       
   for (graph in c("Chill","Heat"))
  {
  if(graph=="Chill") {outg<-out[which(out$Type=="Chill"),]
                      mean_climg<-mean_clim[which(out$Type=="Chill")]
                      dev_climg<-dev_clim[which(out$Type=="Chill")]}
  if(graph=="Heat") {outg<-out[which(out$Type=="Heat"),]
                     mean_climg<-mean_clim[which(out$Type=="Heat")]
                     dev_climg<-dev_clim[which(out$Type=="Heat")]}
  

  if (colorscheme=="bw")     color_bars<-color_bar_maker(outg[1:lc,"VIP"],outg[1:lc,"VIP"],threshold=0.8,col1="BLACK",col2="BLACK",col3="GREY") else
    color_bars<-color_bar_maker(outg[1:lc,"VIP"],outg[1:lc,"VIP"],threshold=0.8,col1="DARK BLUE",col2="DARK BLUE",col3="DARK GREY")
  plot(leg,outg[1:lc,"VIP"],main="VIP",xlab=NA,ylab=NA,xaxs="i",xaxt="n",yaxs="i",cex.lab=4,cex.axis=4,cex.main=5,type="h",col=color_bars,lwd=6)
  tick_marks<-grconvertX(tick_marks, from="user", to="user");X_coords<-grconvertX(leg, from="user", to="user")
  ticks_labels<-grconvertX(tick_label_pos, from="user", to="user")
  axis(1,lwd.ticks=3,at=tick_marks,labels=FALSE,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
  axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,cex.axis=4,padj=1)  
  mtext(side=2,text="VIP",line=6,cex=3)
  if(nothing) {text(x=(max(X_coords)-min(X_coords))/2+min(X_coords),y=0.5,adj=0.5,labels=("No yield produced in any model run"),cex=2)}
  if (colorscheme=="bw")     color_bars<-color_bar_maker(outg[1:lc,"VIP"],outg[1:lc,"Coefficient"],threshold=0.8,col1="BLACK",col2="#CECECE",col3="#7B7B7B") else
      color_bars<-color_bar_maker(outg[1:lc,"VIP"],outg[1:lc,"Coefficient"],threshold=0.8,col1="RED",col2="DARK GREEN",col3="DARK GREY")
  plot(leg,outg[1:lc,"Coefficient"],main="Model coefficients",ylab=NA,xlab=NA,xaxs="i",xaxt="n",yaxs="i",cex.lab=4,cex.axis=4,cex.main=5,type="h",col=color_bars,lwd=6)
  axis(1,lwd.ticks=3,at=tick_marks,labels=FALSE,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
  axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,cex.axis=4,padj=1)
  
  mtext(side=2,text="Model coefficient",line=6,cex=3)
  
  if(nothing) {text(x=(max(X_coords)-min(X_coords))/2+min(X_coords),y=0.5,adj=0.5,labels=("No yield produced in any model run"),cex=2)}

  if (colorscheme=="bw") color_bars<-color_bar_maker(outg[1:lc,"VIP"],outg[1:lc,"Coefficient"],threshold=0.8,col1="BLACK",col2="#CECECE",col3="#7B7B7B") else
      color_bars<-color_bar_maker(outg[1:lc,"VIP"],outg[1:lc,"Coefficient"],threshold=0.8,col1="RED",col2="DARK GREEN",col3="DARK GREY")
  if(graph=="Chill") plot(leg,mean_climg[1:lc],main="Chill accumulation",ylab=NA,xlab=NA,xaxs="i",yaxs="i",xaxt="n",cex.lab=4,cex.axis=4,cex.main=5,type="l",lwd=3,col="BLACK",ylim=c(min(mean_climg[1:lc]-dev_climg[1:lc]),max(mean_climg[1:lc]+dev_climg[1:lc])))
  if(graph=="Heat") plot(leg,mean_climg[1:lc],main="Heat accumulation",ylab=NA,xlab=NA,xaxs="i",yaxs="i",xaxt="n",cex.lab=4,cex.axis=4,cex.main=5,type="l",lwd=3,col="BLACK",ylim=c(min(mean_climg[1:lc]-dev_climg[1:lc]),max(mean_climg[1:lc]+dev_climg[1:lc])))
  
  
  arrows(X_coords,mean_climg[1:lc]+dev_climg[1:lc],X_coords,mean_climg[1:lc]-dev_climg[1:lc], angle=90, code=3,lwd=6,length=0,col=color_bars)
  lines(leg,mean_climg[1:lc],lwd=3)
  axis(1,lwd.ticks=3,at=tick_marks,labels=FALSE,cex.axis=4,padj=1);axis(2,lwd.ticks=3,labels=FALSE);box(which="plot",lwd=3);
  axis(1,lwd.ticks=0,at=ticks_labels,labels=tick_labels,cex.axis=4,padj=1) 
  if(graph=="Chill") 
    {ccc<-unlist(strsplit(CM,"_")); cctemp<-ccc[1]; if(length(ccc)>1) {for (i in 2:length(ccc)) cctemp<-paste(cctemp,ccc[i])}
     mtext(side=2,text=paste(cctemp,"per day"),line=5,cex=3)}
  if(graph=="Heat") 
  {ccc<-unlist(strsplit(HM,"_")); cctemp<-ccc[1]; if(length(ccc)>1) {for (i in 2:length(ccc)) cctemp<-paste(cctemp,ccc[i])}
   mtext(side=2,text=paste(cctemp,"per day"),line=5,cex=3)}  
  
  #mtext(side=2,text=expression('Mean temperature ('^'o'*'C)'),line=5,cex=3)
  

}
  dev.off()
  write.csv(out,paste(PLS_results_path,"_",CM,"_",HM,".csv",sep=""),row.names=FALSE)
  }
   write.csv(weather_file,paste(PLS_results_path,"_interpolated_weather.csv",sep=""),row.names=FALSE)
  
   return(list(weather_file=weather_file,PLS_output=out))
  } else {"Error: not a daily chill object; use function daily_chill to make one from hourly temperature data"}
}

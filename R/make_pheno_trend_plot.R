make_pheno_trend_plot <-
function(weather_data_frame,
                                split_month=6,   #last month in same year
                                pheno,
                                use_Tmean=FALSE,
                                Start_JDay_chill,
                                End_JDay_chill,
                                Start_JDay_heat,
                                End_JDay_heat,
                                outpath,
                                file_name,
                                plot_title,
                                ylabel=NA,
                                xlabel=NA,
                                legend_label=NA,
                                image_type="png",
                                colorscheme="normal")

{

  
weather_file<-weather_data_frame
weather_file$Tmax<-suppressWarnings(as.numeric(as.character(weather_file$Tmax)))
weather_file$Tmin<-suppressWarnings(as.numeric(as.character(weather_file$Tmin)))
suppressWarnings(weather_file[which(weather_file$Tmin>weather_file$Tmax),c("Tmin","Tmax")]<-NA)

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

sea<-unique(weather_file$Season)
res<-data.frame(Season=sea,Chill_Tmean=NA,Heat_Tmean=NA,Year_Tmean=NA)

if(Start_JDay_chill>End_JDay_chill) chill_days<-c(Start_JDay_chill:366,1:End_JDay_chill) else
  chill_days<-Start_JDay_chill:End_JDay_chill
  

if(Start_JDay_heat>End_JDay_heat) heat_days<-c(Start_JDay_heat:366,1:End_JDay_heat) else
  heat_days<-Start_JDay_heat:End_JDay_heat


for (s in sea)
{res[which(res$Season==s),"Chill_Tmean"]<-mean(weather_file[which(weather_file$Season==s&weather_file$JDay %in% chill_days),"Tmean"])
 res[which(res$Season==s),"Heat_Tmean"]<-mean(weather_file[which(weather_file$Season==s&weather_file$JDay %in% heat_days),"Tmean"])
 res[which(res$Season==s),"Year_Tmean"]<-mean(weather_file[which(weather_file$Season==s),"Tmean"])
}


for (i in 1:nrow(pheno))
  {if(pheno[i,1] %in% res$Season)
   {pheno[i,"Chill_Tmean"]<-res[which(res$Season==pheno[i,1]),"Chill_Tmean"]
   pheno[i,"Heat_Tmean"]<-res[which(res$Season==pheno[i,1]),"Heat_Tmean"]
  pheno[i,"Year_Tmean"]<-res[which(res$Season==pheno[i,1]),"Year_Tmean"]
  }}


Make_date<-function(Jday)
{
  leg<-strptime(strptime(paste(Jday,"/2001",sep=""),format="%j/%Y"),"%Y-%m-%d")
 comps<-strsplit(as.character(leg),"-")
  JM<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")[as.numeric(comps[[1]][2])]
  return(paste(comps[[1]][3],JM))
}


if (is.na(xlabel)) xlabel<-paste("Mean temperature during the chilling phase (",Make_date(Start_JDay_chill)," to ",Make_date(End_JDay_chill),"; deg. C)",sep="")
if (is.na(ylabel)) ylabel<-paste("Mean temperature during the forcing phase (",Make_date(Start_JDay_heat)," to ",Make_date(End_JDay_heat),"; deg. C)",sep="")


pheno[,2]<-as.numeric(as.character(pheno[,2]))
pheno<-pheno[which(!is.na(pheno[,"pheno"])),]
pheno<-pheno[which(!is.na(pheno[,"Chill_Tmean"])),]
pheno<-pheno[which(!is.na(pheno[,"Heat_Tmean"])),]
pheno<-pheno[which(!is.na(pheno[,"Year_Tmean"])),]

k<-Krig(x=as.matrix(pheno[,c("Chill_Tmean","Heat_Tmean")]),Y=pheno$pheno)
#plot(k)

if (is.na(legend_label)) legend_label<-"Flowering date (Julian Day)"

if(image_type=="tiff") tiff(paste(outpath,file_name,".tif",sep=""),width=1000,height=1000) else
png(paste(outpath,file_name,".png",sep=""),width=1000,height=1000)
par(list(oma=c(0,2,0,2.2),mar=c(5.1,5.1,4.1,2.1)))
surface( k, type="C",xlab=xlabel,ylab=ylabel,cex.lab=2,cex.axis=1.5,labcex=1.5,asp=1,axis.args=list(cex.axis=2),legend.args=list(text=legend_label,side=4,cex=2,line=4.5))

if (colorscheme=="bw") colors<-colorRampPalette(c("#F9F9F9","#343434"))(255) else
  colors<-tim.colors(255)

surface( k, col=colors,type="C",xlab=xlabel,ylab=ylabel,cex.lab=2,cex.axis=1.5,labcex=1.5,asp=1,axis.args=list(cex.axis=2),legend.args=list(text=legend_label,side=4,cex=2,line=4.5))
         
mtext(text=plot_title,side=3,cex=2.5,line=2.3)
points(pheno[,c("Chill_Tmean","Heat_Tmean")],pch=16)
#if (is.na(legend_label)) mtext("Flowering date (Julian Day)",side=4,cex=2,line=6) else
#  mtext(legend_label,side=4,cex=2,line=6)
dev.off()

pheno<-pheno[,which(!is.na(pheno[1,]))]
write.csv(pheno,paste(outpath,file_name,".csv",sep=""),row.names=FALSE)
return(list(pheno=pheno,ylabel=ylabel,xlabel=xlabel))
}

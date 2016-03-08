make_hourly_temps <-
function (latitude,year_file)

 {
         year_file<-year_file[which(!is.na(year_file$Tmin)&!is.na(year_file$Tmax)),]
  
         if(!"JDay" %in% colnames(year_file))
         year_file[,"JDay"]<-strptime(paste(year_file$Month,"/",year_file$Day,"/",year_file$Year,sep=""),"%m/%d/%Y")$yday+1
  
         preserve_columns<-colnames(year_file)
         year_file$Gamma<-2*pi/365*((year_file$JDay)-1)
         year_file$Delta<-180/pi*(0.006918-0.399912*cos(year_file$Gamma)+0.070257*sin(year_file$Gamma)-0.006758*cos(year_file$Gamma)+0.000907*sin(year_file$Gamma)-0.002697*cos(3*(year_file$Gamma))+0.00148*sin(3*(year_file$Gamma)))
         year_file$CosWo<-(sin(-0.8333/360*2*pi)-sin(latitude/360*2*pi)*sin(year_file$Delta/360*2*pi))/(cos(latitude/360*2*pi)*cos(year_file$Delta/360*2*pi))
         year_file$Sunrise[which(year_file$CosWo>=-1&year_file$CosWo<=1)]<-12-acos(year_file$CosWo)/(15/360*2*pi)
         if(length(which(year_file$CosWo<(-1)||year_file$CosWo>1))>0) {year_file$Sunrise[which(year_file$CosWo<-1||year_file$CosWo>1)]<--99}
         year_file$Sunset[which(year_file$CosWo>=-1&year_file$CosWo<=1)]<-12+acos(year_file$CosWo)/(15/360*2*pi)
         if(length(which(year_file$CosWo<(-1)||year_file$CosWo>1))>0) {year_file$Sunset[which(year_file$CosWo<-1||year_file$CosWo>1)]<--99}
         year_file$Daylength[which(year_file$CosWo>=-1&year_file$CosWo<=1)]<-2*acos(year_file$CosWo)/(15/360*2*pi)
         if(length(which(year_file$CosWo<(-1)||year_file$CosWo>1))>0) {year_file$Daylength[which(year_file$CosWo<-1||year_file$CosWo>1)]<--99}
         year_file$prev_max<-year_file$Tmax[c(nrow(year_file),1:(nrow(year_file)-1))]
         year_file$next_min<-year_file$Tmin[c(2:nrow(year_file),1)]
         year_file$prev_min<-year_file$Tmin[c(nrow(year_file),1:(nrow(year_file)-1))]
         year_file$Tsunset<-year_file$Tmin+(year_file$Tmax-year_file$Tmin)*
                            sin((pi*(year_file$Sunset-year_file$Sunrise)/(year_file$Daylength+4)))
         year_file$prev_Tsunset<-year_file$prev_min+(year_file$prev_max-year_file$prev_min)*
                            sin((pi*(year_file$Sunset-year_file$Sunrise)/(year_file$Daylength+4)))
         colnum<-ncol(year_file)+1

         hourcol<-c(colnum:(colnum+23))

     for (hourcount in 0:23)
             {

              if(length(which(year_file$Daylength==-99))>0) {year_file[which(year_file$Daylength==-99),colnum+hourcount]<-(year_file$Tmax+year_file$Tmin)/2}

              c_morn<-which(hourcount<=year_file$Sunrise)
              c_day<-which(hourcount>year_file$Sunrise&hourcount<year_file$Sunset+1)
              c_eve<-which(hourcount>=year_file$Sunset+1)
              nn<-colnum+hourcount

              if(length(which(year_file$Daylength>(-99)))>0)
                   {if(length(c_morn)>0)     #before sunrise
                          {year_file[c_morn,nn]<-
                                year_file$prev_Tsunset[c_morn]-  #prev temp at sunset
                               ((year_file$prev_Tsunset[c_morn]-year_file$Tmin[c_morn])/
                                  log(24-year_file$Daylength[c_morn])*
                                  log(hourcount+24-year_file$Sunset[c_morn]))}

                    if(length(c_day)>0)     #between sunrise and an hour after sunset
                          {year_file[c_day,colnum+hourcount]<-
                            year_file$Tmin[c_day]+
                            (year_file$Tmax[c_day]-year_file$Tmin[c_day])*
                            sin((pi*(hourcount-year_file$Sunrise[c_day])/
                                  (year_file$Daylength[c_day]+4)))}

                    if(length(c_eve)>0)                   #after sunset
                           {year_file[c_eve,colnum+hourcount]<-
                               year_file$Tsunset[c_eve]- #temp at sunset
                               ((year_file$Tsunset[c_eve]-year_file$next_min[c_eve])/
                                  log(24-year_file$Daylength[c_eve])*
                                  log(hourcount-year_file$Sunset[c_eve]))}

                 }}
                 colnames(year_file)[(ncol(year_file)-23):(ncol(year_file))]<-c(paste("Hour_",1:24,sep=""))
                 year_file<-year_file[,c(preserve_columns,paste("Hour_",1:24,sep=""))]
                 year_file[1,(ncol(year_file)-23):(ncol(year_file))][which(is.na(year_file[1,(ncol(year_file)-23):(ncol(year_file))]))]<-year_file[1,"Tmin"]
                 year_file[nrow(year_file),(ncol(year_file)-23):(ncol(year_file))][which(is.na(year_file[nrow(year_file),(ncol(year_file)-23):(ncol(year_file))]))]<-year_file[nrow(year_file),"Tmin"]
         
         return(year_file)
                 }

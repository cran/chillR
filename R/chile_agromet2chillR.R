chile_agromet2chillR<-function(downloaded_weather_file,drop_most=TRUE)
{xml <- htmlParse(downloaded_weather_file)
 ns <- getNodeSet(xml, '//tr')
 ns<-ns[-c(1:5)]
 days<-lapply(ns, function(x) {event <- t(xmlToDataFrame(downloaded_weather_file))})
 caption<-days[[1]]
 caption<-caption[which(!is.na(caption))]
 days<-days[which(sapply(days,length)==as.numeric(names(sort(table(sapply(days,length)),decreasing=TRUE)[1])))]
 res<-data.frame(matrix(unlist(days),ncol=length(days[[2]]),byrow=T))
 comma_replace<-function(y) {st<-strsplit(as.character(y),",")[[1]]
 if(length(st)==2) y<-as.numeric(st[1])+as.numeric(st[2])/10
 if(st[[1]]=="-") y<-NA
 return(y)}
 for(i in 1:ncol(res)) res[,i]<-unlist(sapply(res[,i],comma_replace))
 for(i in 1:ncol(res)) if(!length(strsplit(as.character(res[1,i]),"-")[[1]])>1) res[,i]<-as.numeric(res[,i])
 colnames(res)<-caption
 res[,"Year"]<-as.numeric(sapply(strsplit(as.character(res$FECHA),"-"),function(x) x[3]))
 res[,"Month"]<-as.numeric(sapply(strsplit(as.character(res$FECHA),"-"),function(x) x[2]))
 res[,"Day"]<-as.numeric(sapply(strsplit(as.character(res$FECHA),"-"),function(x) x[1]))
 colnames(res)[grep("xim",colnames(res))]<-"Tmax"
 colnames(res)[grep("nim",colnames(res))]<-"Tmin"
 colnames(res)[grep("Aire",colnames(res))]<-"Tmean"
 colnames(res)[grep("Prec",colnames(res))]<-"Prec"
 colnames(res)[grep("FECHA",colnames(res))]<-"Date"
 name_seq<-c("Year","Month","Day","Tmin","Tmean","Tmax","Prec")
 namess<-name_seq[which(name_seq %in% colnames(res))]
 othernames<-colnames(res)[which(!colnames(res) %in% namess)]
 if(drop_most) res<-res[,namess] else res<-res[,c(namess,othernames)]
return(res)}



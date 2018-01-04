#' Compute sunrise and sunset times, and daylength
#' 
#' This function computes sunrise time, sunset time and daylength for a
#' particular location and day of the year (Julian day). This is done using
#' equations by Spencer (1971) and Almorox et al. (2005).
#' 
#' @param latitude numeric value specifying the geographic latitude
#' (in decimal degrees) of the location of interest
#' @param JDay numeric (usually integer) value or vector specifying the
#' Julian day (day of the year), for which calculations should be done.
#' @return list with three elements Sunrise, Sunset and Daylength. For days
#' without sunrise or sunset (polar days or nights), all values become -99.
#' @author Eike Luedeling
#' @references  
#' Spencer JW, 1971. Fourier series representation of the position of the Sun.
#' Search 2(5), 172.
#' 
#' Almorox J, Hontoria C and Benito M, 2005. Statistical validation of
#' daylength definitions for estimation of global solar radiation in Toledo,
#' Spain. Energy Conversion and Management 46(9-10), 1465-1471)
#' @keywords utility
#' @examples
#' 
#' daylength(latitude=50,JDay=40)
#' plot(daylength(latitude=35,JDay=1:365)$Daylength)
#' 
#' 
#' @export daylength
daylength<-function(latitude,JDay)
{
  if(missing(latitude)) stop("'latitude' not specified")
  if(missing(JDay)) stop("'JDay' not specified")
  if (!isTRUE(all(is.numeric(JDay)))) stop("'JDay' contains non-numeric values")
  if(length(latitude)>1) stop("'latitude' has more than one element")
  if (!is.numeric(latitude)) stop("'latitude' is not numeric")
  if(latitude>90|latitude<(-90)) warning("'latitude' is usually between -90 and 90")

  Gamma<-2*pi/365*((JDay)-1)
  Delta<-180/pi*(0.006918-0.399912*cos(Gamma)+0.070257*sin(Gamma)-0.006758*cos(Gamma)+
                   0.000907*sin(Gamma)-0.002697*cos(3*(Gamma))+0.00148*sin(3*(Gamma)))
  CosWo<-(sin(-0.8333/360*2*pi)-sin(latitude/360*2*pi)*
            sin(Delta/360*2*pi))/(cos(latitude/360*2*pi)*cos(Delta/360*2*pi))
  
  if(length(CosWo)==1)
    if(CosWo>=-1&CosWo<=1) 
      {Sunrise<-12-acos(CosWo)/(15/360*2*pi)
       Sunset<-12+acos(CosWo)/(15/360*2*pi)
       Daylength<-2*acos(CosWo)/(15/360*2*pi)
      }  else 
      {Sunrise<--99
       Sunset<--99
       Daylength=-99}
  if(length(CosWo)>1)
  {Sunrise<-rep(-99,length(CosWo))
   Sunset<-rep(-99,length(CosWo))
   Daylength<-rep(-99,length(CosWo))
   Sunrise[which(CosWo>=-1&CosWo<=1)]<-12-acos(CosWo[which(CosWo>=-1&CosWo<=1)])/(15/360*2*pi)
   Sunset[which(CosWo>=-1&CosWo<=1)]<-12+acos(CosWo[which(CosWo>=-1&CosWo<=1)])/(15/360*2*pi)
   Daylength[which(CosWo>=-1&CosWo<=1)]<-2*acos(CosWo[which(CosWo>=-1&CosWo<=1)])/(15/360*2*pi)
  }
 Sunset[which(is.na(JDay))]<-NA
 Sunrise[which(is.na(JDay))]<-NA
 Daylength[which(is.na(JDay))]<-NA
 
 return(list(Sunrise=Sunrise,Sunset=Sunset,Daylength=Daylength))  
}

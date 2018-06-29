#' Extract climate data from the ClimateWizard database
#' 
#' This function makes use of an API provided by the International
#' Center for Tropical Agriculture (CIAT) to access climate scenario
#' data for a location of interest. Climate model runs are queried
#' and data returned and summarized according to the specified parameters.
#' A number of metrics are available for several climate models, which are listed in
#' https://redmine.cde.unibe.ch/attachments/download/673/DocumentationRESTAPIforWOCAT_v3.pdf.
#' Refer to this document for details on what can be downloaded. This function
#' provides the additional option of automatically retrieving all data referring to changes
#' in daily temperature extremes (by month), by setting the ```metric``` parameter to
#' "monthly_min_max_temps". It also offers the option to automatically obtain data for
#' all climate models included in the database (as of January 2018).
#' #' 
#' @param coordinates position of the point of interest, specified by a vector
#' with two elements that are called longitude and latitude (e.g. c(longitude=10,
#' latitude=20)).
#' @param scenario representative concentration pathway scenario. Can only be
#' "historical", "rcp45" or "rcp85".
#' @param start_year start year of the interval, for which data is to be summarized.
#' @param end_year end year of the interval, for which data is to be summarized.
#' @param baseline numeric vector of length 2 indicating the time interval to be used
#' as baseline for the climate scenario. The function then returns projected values relative
#' to this baseline. Defaults to c(1950,2005) for the standard
#' baseline of the ClimateWizard dataset, but can also assume different values. Needs
#' to be set to NA for the function to return absolute values.
#' @param metric vector of metrics to output, from a list specified in the reference
#' provided above. This can also be "monthly_min_max_temps", which returns all
#' mean monthly minimum and maximum temperatures.
#' @param GCMs vector of GCMs to be accessed, from a list specified in the above
#' reference. This can also be "all" for all available GCMs (as of January 2018).
#' @param temperature_generation_scenarios parameter to indicate whether the scenarios to be
#' generated should be formatted in such a way that they are direclty usable by
#' chillR's temperature_generation function. This is only applicable, when metric==
#' 'monthly_min_max_temps'.
#' @return data.frame containing the requested information.
#' 
#' @references Girvetz E, Ramirez-Villegas J, Navarro C, Rodriguez C, Tarapues J, undated.
#' ClimateWizard REST API for querying climate change data.
#' https://redmine.cde.unibe.ch/attachments/download/673/DocumentationRESTAPIforWOCAT_v3.pdf
#' 
#' @author Eike Luedeling
#' @keywords utility
#' @examples
#' 
#' getClimateWizardData(coordinates=c(longitude=10.613975,latitude=34.933439),
#'   scenario="historical",
#'   start_year=1970,
#'   end_year=2000,
#'   metric=c("CD18","R02"),
#'   GCMs=c("bcc-csm1-1","BNU-ESM"))
#' 
#' 
#' @export getClimateWizardData
getClimateWizardData<-function(coordinates,scenario,start_year,end_year,
                               baseline=c(1950,2005),metric="monthly_min_max_temps",GCMs="all",
                               temperature_generation_scenarios=FALSE)
{
  coordinates_usable<-FALSE
  if(is.null(names(coordinates)))
    if(length(coordinates)==2) {longitude<-coordinates[1]
                                latitude<-coordinates[2]
      warning("Coordinates not named. Interpreting the first ",
              "element (",longitude,") as longitude and the second (",
              latitude,") as latitude.")
                                coordinates_usable<-TRUE}
  if(!is.null(names(coordinates)))
    {if(!length(coordinates)==2)
      {warning("Unable to interpret coordinates parameter. ",
               "Needs to be a vector with two numeric elements, ",
               "ideally called 'x' and 'y' or 'longitude' and 'latitude'")
      return()}
    if(length(which(c("latitude","longitude") %in% names(coordinates)))==2)
      {longitude<-as.numeric(coordinates["longitude"])
       latitude<-as.numeric(coordinates["latitude"])
       coordinates_usable<-TRUE}
    if(length(which(c("x","y") %in% names(coordinates)))==2)
      {longitude<-as.numeric(coordinates["x"])
       latitude<-as.numeric(coordinates["y"])
       coordinates_usable<-TRUE}
    if(length(which(c("long","lat") %in% names(coordinates)))==2)
      {longitude<-as.numeric(coordinates["long"])
       latitude<-as.numeric(coordinates["lat"])
       coordinates_usable<-TRUE}
  }
  if(coordinates_usable)
   {if(is.na(longitude)) coordinates_usable<-FALSE
    if(is.na(latitude)) coordinates_usable<-FALSE}
  if(!coordinates_usable)
  {warning("Unable to interpret coordinates parameter. ",
           "Needs to be a vector with two numeric elements, ",
           "ideally called 'x' and 'y' or 'longitude' and 'latitude'")
    return()}
  
  if(!scenario %in% c("historical","rcp45","rcp85"))
  {warning('Scenario not available. Must be "historical", "rcp45" or "rcp85".')
    return()}
  
  all_available_gcms<-c("bcc-csm1-1","BNU-ESM","CanESM2","CESM1-BGC","MIROC-ESM","CNRM-CM5","ACCESS1-0","CSIRO-Mk3-6-0",
                        "GFDL-CM3","GFDL-ESM2G","GFDL-ESM2M","inmcm4",
                        "IPSL-CM5A-LR","IPSL-CM5A-MR",
                        "CCSM4")
  if(GCMs[1]=="all") gcms<-all_available_gcms else
  {if(length(which(!GCMs %in% all_available_gcms))>0)
  {warning(GCMs[which(!GCMs %in% all_available_gcms)]," not contained in the database (as far as the getClimateWizardData knows). Models that are available are ",
           toString(all_available_gcms))
    return()}
    
    gcms<-GCMs}
  
  metric_GCMs<-data.frame(GCM=gcms,metric_available=NA)
  
  for(gcm in gcms)
  {if(metric[1]=="monthly_min_max_temps")
  {available<-TRUE
  for(Textreme in c("tasmin","tasmax"))
    for(i in 1:12)
    {if(available)
    {tmetric<-paste(Textreme,i,sep="")
    if(!is.na(baseline[1]))
      checkgcm<-jsonlite::fromJSON(paste("http://maprooms.ciat.cgiar.org/climatewizard/service?lat=",
                             latitude,"&lon=",longitude,
                             "&index=",tmetric,"&scenario=",scenario,"&gcm=",gcm,"&range=",
                             start_year,"-",end_year,"&baseline=",baseline[1],"-",baseline[2],"&avg=true",sep="")) else
         checkgcm<-jsonlite::fromJSON(paste("http://maprooms.ciat.cgiar.org/climatewizard/service?lat=",
                                latitude,"&lon=",longitude,
                                "&index=",tmetric,"&scenario=",scenario,"&gcm=",gcm,"&range=",
                                start_year,"-",end_year,"&avg=true",sep=""))
    if(length(checkgcm)==1) {if(!is.null(checkgcm$error)) available<-FALSE}
    if (available) if(checkgcm$values[[2]][1]=="out of range") {warning("Time interval not (fully) contained in the dataset - no data retrieved")
      #return()
    }
    if(available) metric_GCMs[which(metric_GCMs$GCM==gcm),tmetric]<-as.numeric(checkgcm$values[[2]][1])}
    }
  } else
  {for (met in metric)
  {available=TRUE
  if(!is.na(baseline[1]))
  checkgcm<-jsonlite::fromJSON(paste("http://maprooms.ciat.cgiar.org/climatewizard/service?lat=",
                           latitude,"&lon=",longitude,
                           "&index=",met,"&scenario=",scenario,"&gcm=",gcm,"&range=",
                           start_year,"-",end_year,"&avg=true",sep="")) else
      checkgcm<-jsonlite::fromJSON(paste("http://maprooms.ciat.cgiar.org/climatewizard/service?lat=",
                                         latitude,"&lon=",longitude,
                                         "&index=",met,"&scenario=",scenario,"&gcm=",gcm,"&range=",
                                         start_year,"-",end_year,"&baseline=",baseline[1],"-",baseline[2],"&avg=true",sep=""))
  
  if(length(checkgcm)==1) {if(!is.null(checkgcm$error)) available<-FALSE}
  if (available) if(checkgcm$values[[2]][1]=="out of range") {warning("Time interval not (fully) contained in the dataset - no data retrieved")
    #return()
  }
  if(available) metric_GCMs[which(metric_GCMs$GCM==gcm),met]<-as.numeric(checkgcm$values[[2]][1])
  }}
    if(length(metric)==1) metric_GCMs[which(metric_GCMs$GCM==gcm),"metric_available"]<-available
  }
  #if(is.na(metric_GCMs$metric_available[1]))
  metric_GCMs<-metric_GCMs[,-2]
  
  checkmetrics<-metric
  if(checkmetrics[1]=="monthly_min_max_temps")
    checkmetrics<-c(paste("tasmin",1:12,sep=""),paste("tasmax",1:12,sep=""))
  if(length(which(!checkmetrics %in% colnames(metric_GCMs))>0))
    warning("Data for ",toString(checkmetrics[which(!checkmetrics %in% colnames(metric_GCMs))])," not available")
  
  
  output<-list(data=metric_GCMs,scenario=scenario,start_year=start_year,end_year=end_year,scenario_year=median(c(start_year,end_year)),
               reference_year=NA,scenario_type="absolute",labels=list())
  if(!is.na(baseline[1])) {output$reference_year<-median(baseline[1]:baseline[2])
                           output$scenario_type<-"relative"}
  
  if(metric[1]=="monthly_min_max_temps" & temperature_generation_scenarios)
    {outputs<-list()
    for (i in 1:nrow(output$data))
      {outputs[[i]]<-output
       outputs[[i]]$data<-data.frame(Tmin=as.numeric(output$data[i,2:13]),Tmax=as.numeric(output$data[i,14:25]))
       outputs[[i]]$labels<-as.character(output$data[i,1])
    }
    names(outputs)<-output$data$GCM
    output<-outputs
  }
  
  return(output)
}

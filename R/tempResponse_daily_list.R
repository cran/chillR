#' Calculation of climatic metrics from lists of daily temperature records
#' 
#' Wrapper for the tempResponse function, to facilitate its use on lists
#' of daily temperature records, e.g. those produced by the 
#' temperature_generation function. Daily temperature records are converted
#' into hourly records using the stack_hourly_temps function. These hourly
#' records are then used as input into the tempResponse function, to which
#' most parameters are passed. See the documentation of tempResponse for
#' more details.
#' 
#' @param temperature_list list of daily temperature records, as produced
#' by temperature_generation.
#' @param latitude latitude of the location of interest (used for generating
#' hourly records).
#' @param Start_JDay the start date (in Julian date, or day of the year) of the
#' period, for which chill and heat should be quantified.
#' @param End_JDay the end date (in Julian date, or day of the year) of the
#' period, for which chill and heat should be quantified.
#' @param models named list of models that should be applied to the hourly
#' temperature data. These should be functions that take as input a vector of
#' hourly temperatures. This defaults to the set of models provided by the
#' chilling function.
#' @param misstolerance maximum percentage of values for a given season that
#' can be missing without the record being removed from the output. Defaults to
#' 50.
#' @param whole_record boolean parameter indicating whether the metrics should
#' be summed over the entire temperature record. If set to TRUE (default is
#' FALSE), then the function ignores the specified start and end dates and
#' simply returns the totals of each metric that accumulated over the entire
#' temperature record.
#' @return data frame showing totals for all specified models for the
#' respective periods for all seasons included in the temperature records.
#' Columns are Season, End_year (the year when the period ended) and Days (the
#' duration of the period), as well as one column per model, which receives the
#' same name as the function in the models list. If the weather input consisted
#' of a list with elements stack and QC, the output also contains columns from
#' QC that indicate the completeness of the weather record that the
#' calculations are based on.
#' @author Eike Luedeling
#' @references The chillR package:
#' 
#' Luedeling E, Kunz A and Blanke M, 2013. Identification of chilling and heat
#' requirements of cherry trees - a statistical approach. International Journal
#' of Biometeorology 57,679-689.
#' @keywords chill and heat calculation
#' @examples
#' 
#' 
#' weather<-fix_weather(KA_weather[which(KA_weather$Year>2006),])
#' temperature_list<-list(weather,weather,weather)
#' 
#' tempResponse_daily_list(temperature_list,latitude=50.4)
#' 
#' 
#' @export tempResponse_daily_list
tempResponse_daily_list <-
function (temperature_list,latitude,Start_JDay=1,End_JDay=366,
          models=list(Chilling_Hours=Chilling_Hours,Utah_Chill_Units=Utah_Model,Chill_Portions=Dynamic_Model,GDH=GDH),
          misstolerance=50,whole_record=FALSE)             
     {
  
  if(is.data.frame(temperature_list))
    temperature_list<-list(temperature_list)
  
  output<-list()
  
  for(i in 1:length(temperature_list))
    {hourtemps<-stack_hourly_temps(temperature_list[[i]],latitude=latitude)
     output[[i]]<-tempResponse(hourtemps,Start_JDay=Start_JDay,End_JDay=End_JDay,
                            models=models,misstolerance=misstolerance,
                            whole_record=whole_record)}
  names(output)<-names(temperature_list)
  return(output)
}


#' Converts list of change scenarios to data.frame or vice versa
#'
#'  Allows the user to convert a list of change scenarios to a single data.frame or 
#'  vice versa. If it converts from data.frame to list, the user can decide if the
#'  returned list should be flat or structured. In case of a list of change scenarios, the 
#'  list should have named elements. In case of composite names, the function assumes 
#'  that the location is the first part of the composite name, composite elements are
#'  seperated by dot. 
#'
#' @param scenario_object can be either a data.frame or a list of change scenarios. 
#' If it is a data.frame, it  containing the relative change scenarios
#'
#' @param give_structure boolean, by default set TRUE. If set TRUE, then the output is a
#' nested list of the structure: 1) Location 2)SSP 3)GCM 4)Timepoint.
#' If set FALSE, then returns flat list with names following the scheme:
#' Location.SSP.GCM.Timepoint.
#'
#' @return list / data.frame with relative change scenarios 
#'
#' @author Lars Caspersen
#'
#' @examples \dontrun{
#' download_cmip6_ecmwfr(scenario = 'ssp1_2_6',
#'                       area = c(55, 5.5, 47, 15.1),
#'                       user = 'write user id here',
#'                       key = 'write key here',
#'                       model = 'AWI-CM-1-1-MR',
#'                       frequency = 'monthly',
#'                       variable = c('Tmin', 'Tmax'),
#'                       year_start = 2015,
#'                       year_end = 2100)
#'
#'download_baseline_cmip6_ecmwfr(
#'     area = c(55, 5.5, 47, 15.1),
#'     user = 'write user id here',
#'     key = 'write key here',
#'     model = 'AWI-CM-1-1-MR',
#'     frequency = 'monthly',
#'
#' station <- data.frame(
#'       station_name = c('Zaragoza', 'Klein-Altendorf', 'Sfax',
#'       'Cieza', 'Meknes', 'Santomera'),
#'       longitude = c(-0.88,  6.99, 10.75, -1.41, -5.54, -1.05),
#'       latitude = c(41.65, 50.61, 34.75, 38.24, 33.88, 38.06))
#'
#' extracted <- extract_cmip6_data(stations = station)
#'
#' scenario_df <- gen_rel_change_scenario(extracted)
#'
#' scenario_list <- convert_scen_information(scenario_df)
#'
#' }
#'
#' @importFrom purrr map
#'
#' @export convert_scen_information
#'
convert_scen_information <- function(scenario_object,
                                     give_structure = TRUE) {
  
 # scenario_df <- test
  
  if(is.data.frame(scenario_object)){
    operation <- 'df_to_list'
  } else {
    operation <- 'list_to_df'
  }
  
  
  
  if(operation == 'df_to_list'){
    #make for each combination of ssp, timepoint, gcm, location a relative change scenario
    scen_split <-
      split(scenario_object, f = ~ location + scenario + labels + scenario_year)
    
    rows_in_data <- lapply(scen_split,nrow) == 12
    none_empty_scens<-scen_split[rows_in_data]
    
    #create scenario object based on flat list
    scenario_list <- purrr::map(none_empty_scens, function(scen) {
      #scen <- scen_split[[1]]
      possible_variables <- c('Tmin', 'Tmax', 'Prec')
      present_variables <-
        possible_variables[possible_variables %in% colnames(scen)]
      
      
      scen_list <- list(
        data =  scen[, present_variables],
        scenario = unique(scen$scenario),
        start_year = unique(scen$start_year),
        end_year = unique(scen$end_year),
        scenario_year = unique(scen$scenario_year),
        reference_year = unique(scen$reference_year),
        scenario_type = unique(scen$scenario_type),
        labels = unique(scen$labels)
      )
      
      return(scen_list)
      
    })
    
    
    # scenario_list$`Klein-Altendorf.ssp126.ACCESS-CM2.2050`
    

    if (give_structure == FALSE) {
      return(scenario_list)
    }
    
    if (give_structure == TRUE) {
      scenario_list_structured <- list()
      for (nam in names(scenario_list))
        {splits <- strsplit(nam, '\\.')[[1]]
        scenario_list_structured[[splits[1]]][[splits[2]]][[splits[3]]][[splits[4]]] <-
          scenario_list[[nam]]
      }
      return(scenario_list_structured)
    }
   
   

  }

  if(operation == 'list_to_df'){
    
    uniq_entries <- names(unlist(scenario_object))
    splits <- unique(lapply(strsplit(uniq_entries, '\\.'), function (x) x[1:4]))
    
    scenario_df <- data.frame()
    
    for (spl in splits)
    {
      s<-spl
      current<-scenario_object[[s[1]]][[s[2]]][[s[3]]][[s[4]]]
      scen_df <- current$data
      scen_df$scenario <- current$scenario
      scen_df$start_year <- current$start_year
      scen_df$end_year <- current$end_year
      scen_df$scenario_year <- current$scenario_year
      scen_df$reference_year <- current$reference_year
      scen_df$scenario_type <- current$scenario_type
      scen_df$labels <- current$labels
      scen_df$location <- s[1]
      
      scenario_df <- rbind(scenario_df,scen_df)
    }
    
    return(scenario_df)
    
  }
  
}

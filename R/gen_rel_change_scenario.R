#' Generates relative climate change scenarios based on extracted CMIP6 data
#' 
#' Takes the extracted CMIP6 data and returns climate change scenarios, which
#' can then be used to generate weather data. 
#' 
#' @param downloaded_list list of data.frames, generated using the
#' extract_cmip6_data function. Elements are named after the shared socioeconomic
#' pathway ('SSP') and global climate model ('GCM')
#' 
#' @param years_local_weather by default set to NULL. If provided, this states
#' the earliest and latest baseline year for which the relative scenario should
#' be generated. The same values will be used for all weather stations in this
#' case. Either years_local_weather or weather_list needs to be provided.
#' 
#' @param weather_list by default set to NULL. If provided, should be a list of
#' data.frames containing the locally observed weather. This is used to determine
#' the earliest and latest years for which we have observations. 
#' 
#' @param times numeric vector, states the future years, for which the climate
#' change scenario should be generated. By default set to c(2050, 2085).
#' 
#' @param baseline_year_relative_change numeric, states for which year within
#' the downloaded CMIP6 data the relative change should be calculated.
#' By default set to 2022.
#' 
#' @param baseline_window_width numeric, sets the window width of the running
#' mean calculation for the mean temperatures of the year indicated by
#' baseline_year_relative_change.
#' 
#' @param future_window_width numeric, sets the window width of the running mean
#' calculation for the mean temperatures of the years indicated by times.
#' 
#' 
#' @return list of relative climate change scenarios. The list is ordered by
#' three levels: 1) location 2) SSP_GCM and 3) timepoint of interest.
#' 
#' @author Lars Caspersen
#' 
#' @examples \dontrun{
#' download_cmip6_ecmwfr(scenario = 'ssp1_2_6', 
#'                       area = c(55, 5.5, 47, 15.1),
#'                       user = 'write user id here'
#'                       key = 'write key here',
#'                       model = 'AWI-CM-1-1-MR',
#'                       frequency = 'monthly', 
#'                       variable = c('Tmin', 'Tmax'),
#'                       year_start = 2015, 
#'                       year_end = 2100)
#' station <- data.frame(
#'       station_name = c('Zaragoza', 'Klein-Altendorf', 'Sfax',
#'       'Cieza', 'Meknes', 'Santomera'),
#'       longitude = c(-0.88,  6.99, 10.75, -1.41, -5.54, -1.05),
#'       latitude = c(41.65, 50.61, 34.75, 38.24, 33.88, 38.06))
#'       
#'       extracted <- extract_cmip6_data(stations = station)
#'       extracted$`ssp126_AWI-CM-1-1-MR`
#'       scenario <- gen_rel_change_scenario(extracted, years_local_weather = c(1992, 2021))
#'       scenario$`Klein-Altendorf`$`ssp126_AWI-CM-1-1-MR`$'2050'
#' }
#' 
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom purrr map2
#'  
#' @export gen_rel_change_scenario

#higher-level function to create temperature scenario
gen_rel_change_scenario <- function(downloaded_list, 
                                    years_local_weather,
                                    weather_list = NULL,
                                    times = c(2050, 2085), 
                                    baseline_year_relative_change = 2022,
                                    baseline_window_width = 15, 
                                    future_window_width = 31){
  
  
  
  #lower level function to generate temperature scenario for one weather station
  create_scenario_list <- function(cmip6_one_station, 
                                   reference_year,
                                   times = c(2050, 2085), 
                                   baseline_year_relative_change = 2022,
                                   baseline_window_width = 15, 
                                   future_window_width = 31){
    
    
    
    #iterate over the different points of time we are interested in (2050, 2085)
    int <- purrr::map(times, function(time){
      
      #create baseline scenario (usually 2015)
      clim_senc <- chillR::temperature_scenario_from_records(weather = cmip6_one_station, 
                                                             runn_mean = baseline_window_width,
                                                             year = baseline_year_relative_change)
      
      #create scenario for future point in time we are interested in (usually 2050 or 2085)
      clim_senc_later <- chillR::temperature_scenario_from_records(weather = cmip6_one_station, 
                                                                   runn_mean = future_window_width,
                                                                   year = time)
      
      #calculate the realtive change and put that into a list, mimicking the structure in chillR
      clim_scen_adjusted <- list(data =  clim_senc_later[[1]]$data - clim_senc[[1]]$data,
                                 scenario = unique(cmip6_one_station$ssp),
                                 start_year = time - baseline_window_width,
                                 end_year = time + baseline_window_width,
                                 scenario_year = time,
                                 reference_year = reference_year,
                                 scenario_type = 'relative',
                                 labels = unique(cmip6_one_station$model))
      
    })
    
    #adjust names
    names(int) <- times
    
    return(int)
    
  }
  
  
  # downloaded_list <- extracted
  # weather_list <- local_weather
  
  
  #iterate over the different weather stations (locally observed data and climate change data)
  cat('Generating climate change scenario\n')
  scenario <- purrr::map(downloaded_list, function(x){
    
    
    # x <- downloaded_list[[1]]
    # y <- weather_list[[1]]
    # 
    
    if(is.null(years_local_weather) & is.null(weather_list) == FALSE){
      #calculate the mean year of observation
      
      reference_years <- purrr::map_dbl(weather_list, function(y) mean(c(min(y$Year), max(y$Year))))
      
    } else {
      reference_years <- rep(mean(years_local_weather), length(unique(x$location)))
    }
    
    weather_down_splitted <- split(x, x$location)
    
    
    purrr::map2(weather_down_splitted, reference_years, function(cmip6, ref_year){
      #run the function on the list of scenario for one station
      create_scenario_list(cmip6_one_station = cmip6, 
                           reference_year = ref_year,
                           times = times,
                           baseline_year_relative_change = baseline_year_relative_change,
                           baseline_window_width = baseline_window_width, 
                           future_window_width = future_window_width)
    })
    
    
  }, .progress = TRUE)
  
  #rearrange the scenario so that the hierachy is: 1) location; 2)GCM + SSP; 3)time 
  scenario_rearranged <- purrr::map(names(scenario[[1]]), function(loc) purrr::map(scenario, loc))
  
  names(scenario_rearranged) <- names(scenario[[1]])
  
  return(scenario_rearranged)
  
}


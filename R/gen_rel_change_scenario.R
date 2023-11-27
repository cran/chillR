#' Generates relative climate change scenarios based on extracted CMIP6 data
#' 
#' Takes the extracted CMIP6 data and returns climate change scenarios, which
#' can then be used to generate weather data. 
#' 
#' @param downloaded_list list of data.frames, generated using the
#' extract_cmip6_data function. Elements are named after the shared socioeconomic
#' pathway ('SSP') and global climate model ('GCM')
#' 
#' @param scenarios numeric vector, states the future years, for which the climate
#' change scenarios should be generated. By default set to c(2050, 2085).
#' 
#' @param reference_period numeric vector specifying the years to be used as the
#' reference period. Defaults to c(1986:2014).
#' 
#' @param future_window_width numeric, sets the window width of the running mean
#' calculation for the mean temperatures of the years indicated by scenarios
#' 
#' 
#' @return data.frame for the calculated relative change scenarios, all locations, SSPs, timepoints, GCMs combined 
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
#' gen_rel_change_scenario(extracted)
#' 
#' }
#' 
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom magrittr "%>%"
#' 
#'  
#' @export gen_rel_change_scenario

#higher-level function to create temperature scenario
gen_rel_change_scenario <- function(downloaded_list, 
                                    scenarios = c(2050, 2085),
                                    reference_period = c(1986:2014),
                                    future_window_width = 30){
  
  #downloaded_list <- extracted
  
  #---------------------#
  #match scenarios with historic ones
  #---------------------#
  
  
  
  #first get all the historic scenarios
  hist_i <- grep(x = names(downloaded_list), pattern = 'historical')
  
  models_with_baseline <- 
    gsub(x = names(downloaded_list)[hist_i],
         pattern = 'historical_',
         replacement = '')
  
  scenario_list <- list()
  
  
  for(m in models_with_baseline){
    #m <- models_with_baseline[1]
    
    scenario_list[[m]] <- list()
    
    m_hist <- downloaded_list[[paste0('historical_', m)]]
    
    m_hist_mean <- m_hist[,c("Month", "location", "Tmin", "Tmax")]
    
    m_hist_mean <- tidyr::pivot_longer(m_hist_mean,
                                       cols = -c('location', 'Month'))

    m_hist_mean <- stats::aggregate(m_hist_mean[,"value"],
                                    by = list(location = m_hist_mean$location,
                                              Month = m_hist_mean$Month,
                                              name = m_hist_mean$name),
                                    FUN = function(x) mean(x, na.rm=TRUE) 
                                    )[, c("location", "Month", "name", "value")]
    
    # m_hist_mean <- dplyr::group_by(m_hist_mean,
    #                                location,
    #                                Month,
    #                                name)
    # 
    # m_hist_mean <- dplyr::summarise(m_hist_mean,
    #                                 value = mean(value),
    #                                 .groups = 'keep')

 
    #get indices of scenarios matching with the model (exclude the historic scenario)
    future_i <- grep(pattern = m, 
                     names(downloaded_list)[-hist_i]) 
    
    #iterate over 
    for(i in future_i){
      #i <- future_i[1]
      scenario_list[[m]][[as.character(i)]] <- list()
      
      scenario <- downloaded_list[-hist_i][[i]]$ssp %>% 
        unique()
      
      #iterate over timepoints
      for(t in scenarios){
        scenario_list[[m]][[as.character(i)]][[as.character(t)]] <- list()
        #t <- scenarios[1]
        t_upper <- t + floor(future_window_width/2)
        t_lower <- t - floor(future_window_width/2)
        
        m_future_mean <- downloaded_list[-hist_i][[i]]
        m_future_mean <- m_future_mean[m_future_mean$Year >= t_lower & 
                                         m_future_mean$Year <= t_upper,]
        m_future_mean <- dplyr::select(m_future_mean,-c('Date', 'Year', 'Day', 'lat', 'lon', 'model', 'ssp'))
        m_future_mean <- tidyr::pivot_longer(m_future_mean, cols = -c('location', 'Month'))
        m_future_mean <- stats::aggregate(m_future_mean[,"value"],
                                        by = list(location = m_future_mean$location,
                                                  Month = m_future_mean$Month,
                                                  name = m_future_mean$name),
                                        FUN = function(x) mean(x, na.rm=TRUE) 
        )[, c("location", "Month", "name", "value")]
        
         
        m_change <- merge(m_future_mean, m_hist_mean, 
                          by = c('location', 'Month', 'name'), suffixes = c('.future', '.hist'))
        
        m_change[,"change"] <- m_change$value.future - m_change$value.hist
       
        m_change <- dplyr::select(m_change,-c('value.future', 'value.hist'))
        m_change <- tidyr::pivot_wider(m_change, id_cols = c('location', 'Month'), values_from = 'change')
        
        
        
        # 
        # m_future_mean <- downloaded_list[-hist_i][[i]] %>%
        #   dplyr::filter(Year >= t_lower, Year <= t_upper) %>%
        #   dplyr::select(-c('Date', 'Year', 'Day', 'lat', 'lon', 'model', 'ssp')) %>%
        #   tidyr::pivot_longer(cols = -c('location', 'Month')) %>%
        #   dplyr::group_by(location, Month, name) %>%
        #   dplyr::summarise(value = mean(value), .groups = 'keep')
        # 
        # 
        # m_change <- m_future_mean %>% 
        #   merge(m_hist_mean, by = c('location', 'Month', 'name'), suffixes = c('.future', '.hist')) %>% 
        #   dplyr::mutate(change = value.future - value.hist) %>% 
        #   dplyr::select(-c('value.future', 'value.hist')) %>% 
        #   tidyr::pivot_wider(id_cols = c('location', 'Month'), values_from = 'change')
        # 
        # 
        
        
        
        #split by location
        m_change <- split(m_change, f = m_change$location)
        
        #iterate over each location and save scenario file
        for(l in 1:length(m_change)){
          
          scenario_list[[m]][[as.character(i)]][[as.character(t)]][[l]] <-      
            m_change[[l]] %>% 
            dplyr::mutate(scenario = scenario,
                          start_year = t_lower,
                          end_year = t_upper,
                          scenario_year = t,
                          reference_year = median(reference_period),
                          scenario_type = 'relative',
                          labels = m)
          
          scenario_list[[m]][[as.character(i)]][[as.character(t)]][[l]] <-
            scenario_list[[m]][[as.character(i)]][[as.character(t)]][[l]][order(as.numeric(scenario_list[[m]][[as.character(i)]][[as.character(t)]][[l]]$Month)),]
        }
        
        scenario_list[[m]][[as.character(i)]][[as.character(t)]] <- dplyr::bind_rows(scenario_list[[m]][[as.character(i)]][[as.character(t)]])
      }
      
      scenario_list[[m]][[as.character(i)]] <- dplyr::bind_rows( scenario_list[[m]][[as.character(i)]])
    }
    
    scenario_list[[m]] <- dplyr::bind_rows(scenario_list[[m]])
    
  }

  scenario_list <- dplyr::bind_rows(scenario_list)
  

  return(scenario_list)
  
}


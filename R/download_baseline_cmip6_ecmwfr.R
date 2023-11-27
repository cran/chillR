#' Download historical CMIP6 Data via the ecwfr package
#' 
#' Accesses the CMIP6 data of the Copernicus API via the
#' \code{\link[ecmwfr:wf_request_batch]{ecmwfr}} package. Saves the downloaded
#' files as .zip objects in the specified path in a subfolder with the
#' coordinates of the downloaded area as subfolder name. You can either specify
#' the GCMs by name or you can take all GCMs for which you downloaded climate 
#' change scenarios (model = "match_downloaded").
#' 
#' @param area numeric vector of length 4. Sets the spatial boundaries of the
#' downloaded data. Coordinates are supplied in the following format: 
#' c(maximum latitude, minimum longitude, minimum latitude, maximum longitude),
#' which corresponds to the northern extent, western extent, southern extent
#' and eastern extent of the area of interest.
#' 
#' @param user a character, user name from the Copernicus climate data store.
#' See 'Details' for more information.
#' 
#' @param key a character. Can be found just beneath the user id on the profile
#' when registering for the Copernicus website. Should be provided as a
#' character (so in quotation marks).
#' 
#' @param model character, by default "match_downloaded". Looks up the already downloaded GCMs for
#' the climate change scenarios of the "download_cmip6_ecmwfr()" function. You can
#' also specify the models by name as a vector.
#' 
#' @param service character, by default 'cds'. Decides which database is used.
#' For more details see in the documentation of
#' \code{\link[ecmwfr:wf_set_key]{ecmwfr::wf_set_key()}}.
#' 
#' @param frequency character, can be either 'daily' or 'monthly'. Sets
#' if the downloaded CMIP6 data is in daily or monthly format.
#' 
#' @param variable vector of characters, decides which variables get downloaded. 
#' Currently, the options "Tmin" (Daily minimum temperature in degree centigrade), 
#' "Tmax" (Daily maximum temperature in degree centigrade) 
#' and "Prec" (Daily sum of precipitation in mm) are the only valid options.
#' 
#' @param year_start numeric, earliest year for downloaded CMIP6 data. By default set
#' to 1985.
#' 
#' @param year_end numeric, latest year for downloaded CMIP6 data. By default set
#' to 2014.
#' 
#' @param month numeric vector, sets for which months data should be downloaded.
#' By default set to 1:12.
#' 
#' @param sec_wait numeric, sets the maximum waiting time per requested file.
#' By default is 3600, so 1 hour.
#' 
#' @param n_try numeric, number of repeated calls for the API. For more
#' information see 'Details'.
#' 
#' @param update_everything logical, by default set to FALSE. When set to FALSE,
#' scenarios with matching names that have already been downloaded are skipped.
#' If set to TRUE, then files are downloaded regardless if a file with the same
#' name is already present.
#' 
#' @param path_download character, sets the path for the download of the CMIP6
#' file. If not already present, then a new folder will be created. 
#' The path is relative to the working directory.
#' 
#' @return NULL, the downloaded files are saved in the stated directory
#' 
#' @details Registering for cds.climate.coperincus.eu:
#' \url{https://cds.climate.copernicus.eu/cdsapp#!/home}
#' 
#' @author Lars Caspersen
#' 
#' @examples 
#' \dontrun{
#' # example with one specified GCM 
#' download_baseline_cmip6_ecmwfr(
#'     area = c(55, 5.5, 47, 15.1),
#'     user = 'write user id here',
#'     key = 'write key here',
#'     model = 'AWI-CM-1-1-MR',
#'     frequency = 'monthly', 
#'     variable = c('Tmin', 'Tmax'))
#'  }
#'     
#' @importFrom purrr map
#'  
#' @export download_baseline_cmip6_ecmwfr

download_baseline_cmip6_ecmwfr <- function(area,
                              user,
                              key,
                              model =  'match_downloaded',
                              service = 'cds',
                              frequency = 'monthly', 
                              variable = c('Tmin', 'Tmax'),
                              year_start = 1986, 
                              year_end = 2014, 
                              month = 1:12,
                              sec_wait = 3600,
                              n_try = 10,
                              update_everything = FALSE,
                              path_download = 'cmip6_downloaded'){
  
  
  #check which models have been already downloaded and then select only those
  if(model == 'match_downloaded'){
    
    if(dir.exists(paste0(path_download, '/', paste(area, collapse = '_'))) == FALSE){
      stop('There is no directory matching your arguments. Check if you really downloaded the 
           SSP scenarios and that you used the same area argument.')
    }
    
    f <- list.files(paste0(path_download, '/', paste(area, collapse = '_')))
    
    if(length(f) == 0){
      stop('The directory is empty. Please chekc if the "path_download" and the "area" 
           argument are correct')
    }
    
    
    model <- f %>%
      strsplit('_') %>% 
      purrr::map(function(x) paste(x[5:(length(x) - 5)], collapse = '_')) %>% 
      unique() %>% 
      unlist()
  }
  
  #run download function
  download_cmip6_ecmwfr(scenarios = 'historical',
                        area = area,
                        user = user,
                        key = key,
                        model = model,
                        service = service,
                        frequency = frequency, 
                        variable = variable,
                        year_start = year_start, 
                        year_end = year_end, 
                        month = month,
                        sec_wait = sec_wait,
                        n_try = n_try,
                        update_everything = update_everything,
                        path_download = path_download)
  
}

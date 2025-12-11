#' Unpacks and formats downloaded CMIP6 data
#' 
#' Opens the downloaded .zip files and returns the CMIP6 climate projections
#' for specified locations .
#' 
#' @param stations data.frame with the locations of interest, for which the
#' CMIP6 data should be extracted. Needs to contain the columns 'longitude',
#' 'latitude' and 'station_name'.
#' 
#' @param variable character, decides which variables from the downloaded files
#' get read. Currently, valid options are "Tmin", "Tmax" and "Prec". The value
#' is usually the same as in download_cmip6_ecmwfr function. 
#' 
#' @param download_path character, sets the path for the download of the CMIP6
#' file. If not already present, then a new folder will be created. The path is
#' relative to working directory.
#' 
#' @param keep_downloaded Boolean, by default set to TRUE. If TRUE, the
#' function will not delete the downloaded .nc files. This makes sense when the
#' user may want to use the climate change data for other locations.
#' 
#' @return named list of data.frames. Element names follow the syntax
#' 'SSP'_'GCM', where SSP is the shared socioeconomic pathway and GCM is the
#' global climate model that generated the weather data. The data.frames contain
#' the extracted values for the requested locations.
#' 
#' @author Lars Caspersen
#' 
#' @examples \dontrun{
#' scenario<-c("ssp126", "ssp245", "ssp370", "ssp585")
#' 
#' download_cmip6_ecmwfr(scenario,
#'                          key = 'your-key-here'
#'                          user = 'your-user-name-here',
#'                          area =  c(52, -7, 33, 8) )
#' 
#' 
#' station <- data.frame(
#'     station_name = c('Zaragoza', 'Klein-Altendorf', 'Sfax', 'Cieza',
#'         'Meknes', 'Santomera'),
#'     longitude = c(-0.88,  6.99, 10.75, -1.41, -5.54, -1.05),
#'     latitude = c(41.65, 50.61, 34.75, 38.24, 33.88, 38.06))
#'
#' extracted <- extract_cmip6_data(
#'     stations = station)
#'     
#' scenario <- gen_rel_change_scenario(
#'     extracted, years_local_weather = c(1992, 2021))
#'
#' }
#'
#' 
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.dir
#' @importFrom stats setNames
#' @importFrom dplyr filter
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @importFrom metR ReadNetCDF
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' @importFrom assertthat is.string
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom purrr map_chr 
#' @importFrom utils unzip
#' @importFrom magrittr extract
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate date
#'  
#' @export extract_cmip6_data

extract_cmip6_data <- function(stations,
                               variable = c('Tmin', 'Tmax'),
                               download_path = 'cmip6_downloaded',
                               keep_downloaded = TRUE){
  
  
  #-----------------------------#
  #check the inputs
  #-----------------------------#
  
  assertthat::assert_that(all(variable %in% c('Tmin', 'Tmax', 'Prec')))
  assertthat::is.dir(download_path)
  assertthat::assert_that(all(c('station_name', 'longitude', 'latitude') %in% colnames(stations)))
  assertthat::assert_that(is.numeric(stations$longitude))
  assertthat::assert_that(is.numeric(stations$latitude))
  
  
  
  #check if ncdf4 and PCICt are installed
  if(system.file(package='ncdf4') == ''){
    stop('You need to have the package ncdf4 installed for the function to work properly.')
  }
  # if(system.file(package = 'PCICt') == ''){
  #   stop('You need to have the package PCICt installed for the function to work properly.')
  # }
  
  #need to transform the latitude because netcdf latitude only goes from 0 to 360
  #stations$longitude_old <- stations$longitude
  stations$longitude_mod <- stations$longitude + 360

  #detect if there are folders with data
  sub_dirs <- list.dirs(download_path)
  
  if(length(sub_dirs) > 1){
    path_download_vector <- sub_dirs[2:length(sub_dirs)]
  } else{
    path_download_vector <- download_path
  }
  
  #go other each dir and make the same routine
  all_weather <- purrr::map(path_download_vector, function(download_path){
    
    #download_path <- path_download_vector[2]
    
    #show for which area we are currently extracting
    if(length(path_download_vector) > 1){
      download_path %>% 
        stringr::str_split(pattern = '/') %>% 
        purrr::map_chr(2) %>% 
        cat('-------------------------------\n', 'area: ',. ,'\n-------------------------------\n\n')
    }
    
    
    #give a warning if there are different extents --> may cause a problem
    #give a warning if at least one location may not be covered by the maps
    #check if zip files are present in the folder, if so assume that we have to extract the zip files, otherwise directly open them
    fnames <- list.files(path = download_path, pattern = '.zip')
    
    extends <- stringr::str_split(fnames, '_') %>% 
      purrr::map(function(x) x[(length(x)-3):(length(x))] %>% 
                   gsub(x = .,pattern = '.zip', replacement = '') %>% 
                   as.numeric()) %>% 
      unique() %>% 
      purrr::map(function(x) data.frame(lat_high = x[1], lon_low = x[2],  lat_low = x[3], lon_high = x[4])) %>% 
      bind_rows() 
    
    
    
    #at least one stations is not covered by the downloaded area
    if(any(extends$lon_low > stations$longitude | extends$lon_high < stations$longitude) |
       any(extends$lat_low > stations$latitude | extends$lat_high < stations$latitude)){
      
      drop_station_id <- which((extends$lon_low > stations$longitude | extends$lon_high < stations$longitude) |
                                 (extends$lat_low > stations$latitude | extends$lat_high < stations$latitude))
      
      #warning('The stations: ', paste(stations$station_name[drop_station_id], collapse = ', '), ' are not covered by the downloaded files. Consider checking either station coordinates or changing the "area" argument when downloading the file. The mentioned stations will be dropped from the extraction.')
      
      stations_extract <- stations[-drop_station_id,]
      
      if(nrow(stations_extract) == 0){
        stop('After dropping stations not covered by the downloaded files no more stations are left to work with.\nStopping the extraction')
      }
    } else stations_extract <- stations
    
    
    #---------------------------------#
    #Unzip the zip files 
    #---------------------------------#
    
    cat('Unzipping files\n')
    #open zip files
    if(length(fnames) > 0){
      
      #get all file names
      fnames_fullname <- list.files(path = download_path, pattern = '.zip', full.names = TRUE)
      
      
      #translate the variable name to the search term 
      f_index <- purrr::map(tolower(variable), grep, fnames) %>% 
        unlist()
      #grep(tolower(variable), fnames)
      
      #take only files for which I have downloaded data for the variables of interest
      f_occur <- fnames %>% 
        magrittr::extract(f_index) %>% 
        substring(first = 6) %>% 
        table() %>% 
        data.frame() %>% 
        stats::setNames(c('id', 'freq')) %>% 
        dplyr::filter(.data$freq == length(variable))
      
      #iterate over all downloads with tmax and tmin present
      purrr::walk(f_occur$id, function(f){
        
        index_match <- grep(pattern = f, fnames_fullname)
        
        purrr::walk(index_match, function(ind){
          #identify which file in the zip to use
          take_f1<- grep(pattern =  '\\.nc$', utils::unzip(fnames_fullname[ind], list = TRUE)$Name)[1]
          unzip_f1 <- utils::unzip(fnames_fullname[ind], list = TRUE)$Name[take_f1]
          #unzip the file
          utils::unzip(zipfile = fnames_fullname[ind], files = unzip_f1, exdir = download_path)
        })
      }, .progress = TRUE)
    }
    
    #-----------------------------------------------------------------#
    #Match files in pairs (eg Tmin and Tmax of the same ssp and gcm)
    #-----------------------------------------------------------------#
    
    
    #now read all the .nc files which are present in the folder
    fnames <- list.files(download_path, pattern = '.nc')
    fnames_fullname <- list.files(path = download_path, pattern = '.nc', full.names = TRUE)
    
    #translate the selected variable names to the variable names of the unzipped / downloaded cmip6 data
    varname_nc <- factor(variable, levels = c('Tmin', 'Tmax', 'Prec'), labels = c('tasmin', 'tasmax', 'pr')) %>% 
      as.vector()
    
    #translate the variable name to the search term 
    f_index <- purrr::map(varname_nc, grep, fnames) %>% 
      unlist()
    #grep(tolower(variable), fnames)
    
    #make rmd check shut up
    .<-NULL
    
    #only take pairs which have the same number of matches present
    f_occur <- fnames %>% 
      magrittr::extract(f_index) %>% 
      gsub(pattern = paste(varname_nc, collapse = '|'), replacement = '', x = .) %>% 
      table() %>% 
      data.frame() %>% 
      stats::setNames(c('id', 'freq')) %>% 
      dplyr::filter(.data$freq == length(variable))
    
    #for each of the pairs, find out their position in the vector of file names
    index_match_list <- purrr::map(f_occur$id, function(f) grep(pattern = paste0(varname_nc , f, collapse = '|') , fnames_fullname))
    
    #create a 
    
    #-------------------------------#
    #Open the ncdf files
    #-------------------------------#
    
    
    #this a three-level nested loop and I am not proud of that
    #but I don't have good ideas how to change that
    #loop1: going over the general ssp-gcm combinations
    #loop2: for each ssp-gcm combination, take the individual variables (because they are saved to seperate files)
    #loop3: for each variable of the ssp-gcm combination extract the individual stations coordinates
    
    cat('Extracting downloaded CMIP6 files\n')
    #open all files and read them 
    
    #####
    #loop1: go over the individual ssp-gcm combinations
    ####
    all_weather <- purrr::map(f_occur$id, function(f){
      
      #f <- f_occur$id[1]
      
      #filter for the passing full file names, only allow matches of variables which were
      #specified by the user
      index_match <- grep(pattern = paste0(varname_nc , f, collapse = '|') , fnames_fullname)
      
      ######
      #loop2: go over the individual variabls, because they are saved in individual files
      ######
      Datatemp <- purrr::map(index_match, function(ind){
        
        #extract the information from the file name
        split_name <- fnames[ind] %>% 
          strsplit('_')
        
        variable_1 <- split_name[[1]][1]
        gcm <- split_name[[1]][3]
        ssp <- split_name[[1]][4]
        
        #######
        #loop3: read the files for the individual locations
        #######
        Datatemp_raw_1 <- purrr::map(1:nrow(stations_extract), function(i){
          
          #try to read file
          tmp <- try(suppressWarnings(metR::ReadNetCDF(file = fnames_fullname[ind], 
                                                       vars = variable_1, 
                                                       subset = list(lon = stations_extract[i, "longitude"], 
                                                                     lat = stations_extract[i, "latitude"]))))
          
          #check if there was an error, if so send a warning message in the end and 
          #return empty data.frame
          error_read_file <- FALSE
          if (inherits(tmp, "try-error")) error_read_file <- TRUE
          
          #if there was no error but the tmp is empty, then it might be that people
          #are still using the old metR version
          #--> try running the function with latitude projected between 0 and 360
          
          if(error_read_file == FALSE){
            if(is.null(tmp)){
              tmp <- metR::ReadNetCDF(file = fnames_fullname[ind], 
                                      vars = variable_1, 
                                      subset = list(lon = stations_extract[i, "longitude_mod"], 
                                                    lat = stations_extract[i, "latitude"]))
            }
          }
          
          #if it still is empty, then the error must be somewhere else
          if(is.null(tmp)) error_read_file <- TRUE
          
          if(error_read_file){
            warning(paste("Failed to properly read file:", 
                          fnames_fullname[ind]), "\nreturned NULL instead")
            return(data.frame(ssp = ssp, model = gcm, 
                              location = stations_extract$station_name[i], 
                              time = NA, lat = stations_extract$latitude[i], 
                              lon = stations_extract$longitude[i]))
          } 
          
          #if there is no problem, add some extra info to the file
          tmp$location <- stations_extract$station_name[i]
          tmp$model <- gcm
          tmp$ssp <- ssp
          
          return(tmp)
        }) 
        ####end loop3
        
        #sometimes there can be empty files, if you read them they have a single row
        #in all other cases we assume it worked fine, so we combine the individual data.frames to a big one and drop possible NAs
        if(nrow(Datatemp_raw_1[[1]]) != 1){
          Datatemp_raw_1 %>% 
            dplyr::bind_rows() %>% 
            stats::na.omit()
        } else {
          Datatemp_raw_1[[1]]
        }
        
        
        #bind the individual files of the same ssp - gcm but different variables (Tmin, Tmax, possibly also Prec) to one data.frame
        
      })  %>%      ####end loop 2
        purrr::compact() %>% 
        purrr::reduce(left_join, by = c('time', 'lat', 'lon', 'location', 'model', 'ssp')) %>% 
        dplyr::mutate(Month = lubridate::month(as.Date(.data$time)),
                      Year = lubridate::year(.data$time),
                      Date = lubridate::date(.data$time),
                      Day = lubridate::day(.data$time))
      
      
      #change names and convert units
      if('tasmax' %in% colnames(Datatemp)){
        Datatemp$Tmax <- round(Datatemp$tasmax - 273.15, digits = 4)
      }
      if('tasmin' %in% colnames(Datatemp)){
        Datatemp$Tmin <- round(Datatemp$tasmin - 273.15, digits = 4)
      }
      if('pr' %in% colnames(Datatemp)){
        Datatemp$Prec <- round(Datatemp$pr * 60 * 60 * 24, digits = 4)
      }
      
      #if(nrow(Datatemp)<=1){print(paste("WARNING!!!!!!:", ssp, gcm))}
      
      if(all(variable %in% colnames(Datatemp))){
        return(Datatemp %>% 
                 dplyr::select("Date", "Year", "Month", "Day", "lat", "lon", "location", "model", "ssp", dplyr::all_of(variable)))
      } else{
        return(Datatemp)
      }
      
      
      
    }, .progress = TRUE) ####end loop 1
    
    #assign names to the individual ssp-gcm combinations
    names(all_weather) <- purrr::map_chr(all_weather, function(x) paste0(unique(x$ssp), '_', unique(x$model)))
    
    #if there is only one row, remove the entry and replace with NA
    all_weather <- purrr::map(all_weather, function(x) if(nrow(x) == 1) return(NULL) else x)
    
    #remove unzipped files
    unlink(fnames_fullname)
    
    return(all_weather)
  })
  
  #match elements of all weather based on their names and rbind
  
  #get all names of each list
  all_weather_combined <- purrr::map(all_weather, names) %>% 
    unlist() %>% 
    unique() %>% 
    purrr::map( function(n) purrr::map(all_weather, function(l) l[[n]]) %>% 
                  dplyr::bind_rows())
  
  #set names
  names(all_weather_combined) <- purrr::map(all_weather, names) %>% 
    unlist() %>% 
    unique()
  
  
  #check if some of the stations are not even in one of the list entries, if so 
  #warn the user
  extracted_stations <- purrr::map(all_weather_combined, function(t) unique(t$location)) %>% 
    unlist() %>% 
    unique()
  
  if(any(!stations$station_name %in% extracted_stations)){
    warn_station <- which(!stations$station_name %in% extracted_stations)
    warning('The supplied location: ', paste(stations$station_name[warn_station], collapse = ', '), ' were not covered by any of the downloaded files. Consider adjusting the "area" argument when downloading the files.')
  }
  
  
  
  #in case the user does not want to keep the raw files 
  if(keep_downloaded == FALSE){
    unlink(paste0(download_path,'/'), recursive = TRUE)
  }
  
  
  return(all_weather_combined)
  
}

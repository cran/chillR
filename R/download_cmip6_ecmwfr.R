#' Download CMIP6 Data via the ecwfr package
#' 
#' Accesses the CMIP6 data of the Copernicus API via the
#' \code{\link[ecmwfr:wf_request_batch]{ecmwfr}} package. Saves the downloaded
#' files as .zip objects in the specified path in a subfolder with the
#' coordinates of the downloaded area as subfolder name. You can either specify
#' the GCMs by name, take the combinations of scenario and GCM that worked in
#' the past (model = 'default') or you can try out all GCMs for a scenario and
#' take the ones for which there is data (model = 'all').
#' 
#' @param scenarios vector of characters specifying the shared socioeconomic
#' pathway scenarios (SSP) to be downloaded. Currently the values 'ssp126',
#' 'ssp245', 'ssp370' and 'ssp585' are the only accepted options. These are the
#' standard scenarios of CMIP6.
#' 
#' @param area numeric vector of length 4. Sets the spatial boundaries of the
#' downloaded data. Coordinates are supplied in the following format: 
#' c(maximum latitude, minimum longitude, minimum latitude, maximum longitude),
#' which corresponds to the northern extent, western extent, southern extent
#' and eastern extent of the area of interest.
#' 
#' @param model character, by default "default". Decides which global climate
#' models are requested. If set to "default" then depending on the scenario and
#' temporal resolution around 20 models are selected for which we know that
#' certain combinations of scenario and variables are available. If this is set
#' to "all", then all potential models are requested. You can also hand-pick the
#' models you want to download as a vector of the model names.
#' You can check \url{https://cds.climate.copernicus.eu/datasets/projections-cmip6?tab=download}
#' for the list of models. In case a certain request fails because either the
#' model name is wrong or the requested combination of SSP, time period and
#' variable is not available, then the model is dropped from the requests and
#' the function carries on with the remaining requests. The user will get a
#' warning in these cases. 
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
#' to 2015.
#' 
#' @param year_end numeric, latest year for downloaded CMIP6 data. By default set
#' to 2100.
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
#' @param user a character, user name provided by ECMWF data service. The default 
#' "ecmwfr" should be fine. Otherwise provide the email address which was
#' used to sign-up at ECMWF / Copernicus Climate Data Store 
#' 
#' @param key a character. Can be found just beneath the user id on the profile
#' when registering for the Copernicus website next to "Personal Access Token". 
#' Should be provided as a character (so in quotation marks).
#' 
#' @return NULL, the downloaded files are saved in the stated directory
#' 
#' @details Registering for cds.climate.coperincus.eu:
#' \url{https://cds.climate.copernicus.eu/}
#' 
#' Finding the user id and the key:
#' 
#' On the website of the Copernicus climate data store, navigate to the user
#' profile and scroll to the bottom to "API key". There you can find the item
#' "UID". The user id should be provided as character (within quotation marks).
#' Just below, you can also find the key, which is also needed when using this
#' function.
#' 
#' After successful registration some extra steps are needed in order to be
#' able to download CMIP6 data. In addition to the "Terms of use of the
#' Copernicus Climate Store" and the "Data Protection and Privacy Agreement",
#' you also need to agree to the "CMIP6 - Data Access - Terms of Use". 
#' This needs to be done after registering. You can agree to the terms via the
#' following link:
#' \url{https://cds.climate.copernicus.eu/datasets/projections-cmip6?tab=download#manage-licences}. 
#' 
#' Alternatively, you can navigate to the terms within the Copernicus webpage.
#' Go to "Datasets", you can find it in the upper ribbon of the main page. 
#' There you need to search for "CMIP6" using the search field
#' and choose the first result, which is named "CMIP6 climate projections". 
#' There you need to click on "Download data" and scroll to the very bottom of
#' the page to the field "Terms of Use". There you need to click on the button
#' saying "Accept Terms". If you do not accept the terms the download via the
#' API (and consequently via this function) will not be possible!
#' 
#' 
#' Sometimes the server is not responding in time, which can make the download
#' fail. In such cases, after a short waiting time of 5 seconds, the request is
#' started again. If the error reoccurs several times, the requested model
#' will be dropped from the list of requests. By default the number of allowed
#' repeated requests is 10. The user will get a warning if the model is dropped
#' from the requests. 
#' 
#' @author Lars Caspersen, Antonio Picornell
#' 
#' @examples 
#' \dontrun{
#' # example with one specified GCM 
#' download_cmip6_ecmwfr(
#'     scenarios = 'ssp126', 
#'     area = c(55, 5.5, 47, 15.1),
#'     model = 'AWI-CM-1-1-MR',
#'     frequency = 'monthly', 
#'     variable = c('Tmin', 'Tmax'),
#'     year_start = 2015, 
#'     year_end = 2100)
#' 
#' # example with default combinations of scenario and GCM
#' download_cmip6_ecmwfr(
#'     scenarios = 'ssp126', 
#'     area = c(55, 5.5, 47, 15.1),
#'     model = 'default',
#'     frequency = 'monthly', 
#'     variable = c('Tmin', 'Tmax'),
#'     year_start = 2015, 
#'     year_end = 2100)
#' 
#' # example with all possible combinations of scenario and GCM
#' # this may take a little longer
#' download_cmip6_ecmwfr(
#'     scenarios = 'ssp126', 
#'     area = c(55, 5.5, 47, 15.1),
#'     model = 'all',
#'     frequency = 'monthly', 
#'     variable = c('Tmin', 'Tmax'),
#'     year_start = 2015, 
#'     year_end = 2100)
#' }
#' @importFrom assertthat is.string
#' @importFrom assertthat is.number
#' @importFrom assertthat are_equal
#' @importFrom assertthat is.dir
#' @importFrom assertthat assert_that
#' @importFrom ecmwfr wf_set_key
#' @importFrom ecmwfr wf_get_key
#' @importFrom purrr compact
#' @importFrom ecmwfr wf_request_batch
#' @importFrom keyring key_delete
#'  
#' @export download_cmip6_ecmwfr
#' 
#' 
download_cmip6_ecmwfr <- function(scenarios, 
                                  area,
                                  model =  'default',
                                  service = 'cds',
                                  frequency = 'monthly', 
                                  variable = c('Tmin', 'Tmax'),
                                  year_start = 2015, 
                                  year_end = 2100, 
                                  month = 1:12,
                                  sec_wait = 3600,
                                  n_try = 10,
                                  update_everything = FALSE,
                                  path_download = 'cmip6_downloaded',
                                  user = 'ecmwfr',
                                  key = NULL){
  
  #---------------------------#
  #check inputs
  #---------------------------#
  
  
  #check that arguments are sensible
  assertthat::assert_that(is.character(scenarios))
  assertthat::is.string(user)
  assertthat::assert_that(is.character(key) | is.null(key))
  assertthat::is.string(frequency)
  assertthat::is.number(n_try)
  assertthat::is.number(year_start)
  assertthat::is.number(year_end)
  assertthat::are_equal(length(area), 4)
  assertthat::assert_that(is.character(path_download))
  
  
  #check if dplyr is installed
  if(system.file(package = 'dplyr') == ''){
    stop('You need to have the package dplyr installed for the function to work properly.')
  }
  
  
  #check if packages are loaded, load them if not
  if('dplyr' %in% (.packages()) == FALSE){
    stop('The dplyr package needs to be loaded for the function to work properly. Try running library(dplyr) or library(tidyverse) and re-run the download function')
  }
  
  
  #make sure the folder exists
  if(dir.exists(path_download) == FALSE){
    dir.create(path_download)
  }
  assertthat::is.dir(path_download)
  
  #within that folder make subfolder for each area downloaded
  if(dir.exists(paste0(path_download, '/', paste(area, collapse = '_'))) == FALSE){
    dir.create(paste0(path_download, '/', paste(area, collapse = '_')))
  }
  #make the subfolder the download path
  path_download_old <- path_download
  path_download <- paste0(path_download, '/', paste(area, collapse = '_'))
  
  assertthat::assert_that(all(variable %in% c('Tmin', 'Tmax', 'Prec')))
  stopifnot(is.numeric((month)))
  assertthat::is.number(sec_wait)
  stopifnot(is.numeric((area)))
  assertthat::assert_that(is.character(model))
  
  #need to check the area on certain things
  #first and third entry must be in -90, and 90
  assertthat::assert_that(area[1] <= 90 & area[1] >= -90)
  assertthat::assert_that(area[3] <= 90 & area[3] >= -90)
  #first entry must be greater than third
  assertthat::assert_that(area[1] > area[3])
  #second and fourth entry must be in -180 to 180
  assertthat::assert_that(area[2] <= 180 & area[2] >= -180)
  assertthat::assert_that(area[4] <= 180 & area[4] >= -180)
  #second entry must be smaller than fourth
  assertthat::assert_that(area[2] < area[4])
  
  
  
  #----------------------------------#
  #checking if the keys for API work #
  #----------------------------------#
  
  #check if there is a key set
  key_retrieved <- try(ecmwfr::wf_get_key(), silent = TRUE)
  
  #if there is an error message from getting the key, then it means 
  #there is no key set so far
  if(inherits(key_retrieved, "try-error")){
    key_retrieved <- NULL
  }
  
  #when both provided and retrieved key are NULL
  #--> force user to set a key
  if(is.null(key_retrieved) & is.null(key)){
    message('You have not set a key / personal access token to download the CMIP6 data.
            You need to sign in / register your ECM. When you scroll down
            your profile you can find the point "Personal Access Token". Copy the
            token to the dialog window that opened in R. Alternatively, you can set
            the token via: ecmwfr::wf_set_key(key = "your key here")')
    ecmwfr::wf_set_key( )
  }
  
  #when there is a key supplied, and there is no key retrieved
  #--> set the supplied key as the new key
  if(is.null(key_retrieved) & is.null(key) == FALSE){
    ecmwfr::wf_set_key(key = key, user = user)
  }
  
  #when there is a key provided and a key is set
  #--> compare the two: 
  #                     if they are the same, great! dont do anything
  #                     if they are different, warning. set the provided key as the new key
  
  if(is.null(key) == FALSE & is.null(key_retrieved) == FALSE){
    
    if(key != key_retrieved){
      warning('Provided key differs from the key set in your system. Will set
              provided key as the new key.')
      ecmwfr::wf_set_key(key = key, user = user)
    }
    
  }
  
  # #--> then we do not need the arguments anymore (user, key)
  # #that would be nice
  # 
  # #set key for download
  # ecmwfr::wf_set_key(key = '28b3c7fd-df41-46f5-a0c0-0fec2a33e03c', user = user)
  
  
  
  
  #--------------------------------------#
  #handling climate scenarios and models
  #--------------------------------------#
  
  #translate common ssp names to names used by the data provider
  if('ssp126' %in% scenarios){
    scenarios[scenarios == 'ssp126'] <- 'ssp1_2_6'
  }
  
  if('ssp245' %in% scenarios){
    scenarios[scenarios == 'ssp245'] <- 'ssp2_4_5'
  }
  
  if('ssp370' %in% scenarios){
    scenarios[scenarios == 'ssp370'] <- 'ssp3_7_0'
  }
  
  if('ssp585' %in% scenarios){
    scenarios[scenarios == 'ssp585'] <- 'ssp5_8_5'
  }
  
  
  
  #placeholder
  Models <- NULL
  
  #waiting time in seconds when api does not respond
  wait <- 5
  
  #standard selection
  default_gcm_list <- list('daily' = list('historical' = c("access_cm2",       "awi_cm_1_1_mr" ,   "cmcc_esm2"     ,   "cnrm_cm6_1_hr"   ,
                                                           "cnrm_cm6_1" ,      "cnrm_esm2_1"    ,  "fgoals_g3"    ,    "gfdl_esm4"    ,  "inm_cm4_8"       ,
                                                           "inm_cm5_0"   ,     "kiost_esm"       , "miroc6"        ,   "miroc_es2l"   ,    "mpi_esm1_2_lr"   , "mri_esm2_0"      ,
                                                           "noresm2_mm"      ,  "ec_earth3_cc"  ,   "ec_earth3_veg_lr"   ,
                                                           "ipsl_cm6a_lr"  ,   "kace_1_0_g"       ,"ukesm1_0_ll"     , "canesm5"        ,  "nesm3"    ),
                                          'ssp1_2_6' = c("access_cm2", "awi_cm_1_1_mr", "bcc_csm2_mr", "cesm2", "cmcc_esm2", "cnrm_cm6_1_hr",
                                                         "cnrm_cm6_1", "cnrm_esm2_1", "fgoals_g3", "gfdl_esm4", "iitm_esm", "inm_cm4_8",
                                                         "inm_cm5_0", "kiost_esm","miroc6", "miroc_es2l", "mpi_esm1_2_lr", "mri_esm2_0",
                                                         "noresm2_lm", "noresm2_mm"),
                                          'ssp2_4_5' = c("access_cm2", "awi_cm_1_1_mr", "bcc_csm2_mr", "cesm2", "cesm2_waccm", "cmcc_esm2",
                                                         "cnrm_cm6_1", "cnrm_esm2_1", "ec_earth3_cc", "ec_earth3_veg_lr", "giss_e2_1_g", "inm_cm4_8",
                                                         "inm_cm5_0","ipsl_cm6a_lr", "kace_1_0_g","kiost_esm","miroc6", "miroc_es2l", "mpi_esm1_2_lr",
                                                         "mri_esm2_0","noresm2_lm", "noresm2_mm", "ukesm1_0_ll"),
                                          'ssp5_8_5' = c("access_cm2", "awi_cm_1_1_mr", "bcc_csm2_mr", "canesm5","cesm2", "cmcc_esm2",
                                                         "cnrm_cm6_1", "cnrm_esm2_1", "ec_earth3_cc", "ec_earth3_veg_lr","gfdl_esm4", "inm_cm4_8",
                                                         "inm_cm5_0", "kace_1_0_g","kiost_esm",
                                                         "miroc6", "miroc_es2l", "mpi_esm1_2_lr", "mri_esm2_0","nesm3", "noresm2_lm", "noresm2_mm")
  ),
  'monthly' = list('ssp1_2_6' = c('access_cm2', 'awi_cm_1_1_mr', 'bcc_csm2_mr', 'canesm5', 'cmcc_esm2',
                                  'cnrm_cm6_1_hr', 'fio_esm_2_0', 
                                  'inm_cm5_0', 'ipsl_cm6a_lr', 'miroc6', 'miroc_es2l', 'mri_esm2_0',
                                  'cesm2','cnrm_cm6_1','cnrm_esm2_1','ec_earth3_veg_lr',
                                  'fgoals_g3','gfdl_esm4','inm_cm4_8', 'mpi_esm1_2_lr',
                                  'nesm3','ukesm1_0_ll'),
                   'ssp2_4_5' = c('access_cm2','awi_cm_1_1_mr','bcc_csm2_mr','cmcc_esm2',
                                  'cnrm_cm6_1_hr','fio_esm_2_0', 
                                  'inm_cm5_0','ipsl_cm6a_lr',  'miroc6','miroc_es2l', 'mri_esm2_0',
                                  'cesm2','cnrm_cm6_1','cnrm_esm2_1',
                                  'ec_earth3_cc', 'ec_earth3_veg_lr','fgoals_g3','gfdl_esm4',
                                  'inm_cm4_8','mpi_esm1_2_lr','nesm3','ukesm1_0_ll'),
                   'ssp3_7_0' = c('access_cm2','awi_cm_1_1_mr', 'bcc_csm2_mr',  'cnrm_cm6_1_hr',
                                  'ec_earth3_aerchem',  'inm_cm5_0','ipsl_cm6a_lr',  'miroc6',
                                  'miroc_es2l',    'mri_esm2_0','cesm2','cnrm_cm6_1',    'cnrm_esm2_1',
                                  'ec_earth3_veg_lr',   'fgoals_g3','gfdl_esm4',  'inm_cm4_8',
                                  'mpi_esm1_2_lr',    'ukesm1_0_ll'),
                   'ssp5_8_5' = c('access_cm2', 'awi_cm_1_1_mr','bcc_csm2_mr',
                                  'cmcc_esm2',  'cnrm_cm6_1_hr','fio_esm_2_0',
                                  'inm_cm5_0',    'ipsl_cm6a_lr',  'miroc6',  'miroc_es2l',
                                  'mri_esm2_0',   'cesm2','ciesm','cnrm_cm6_1',
                                  'cnrm_esm2_1',    'ec_earth3_cc',    'ec_earth3_veg_lr',
                                  'fgoals_g3',    'gfdl_esm4',    'inm_cm4_8',
                                  'mpi_esm1_2_lr',    'nesm3',      'ukesm1_0_ll'))
  )
  
  #in case it is default
  if(length(model) == 1){
    
    
    if(model == 'default'){
      
      Models <- 'default'
    }
    
    #last checked 1st sept 2023
    #maybe also have a colum with the complete name as on the website, because people probably do not
    #know the api name
    if(model == 'all'){
      Models <- c('access_cm2', 'awi_cm_1_1_mr', 'bcc_csm2_mr', 'cams_csm1_0',
                  'canesm5_canoe', 'cesm2_fv2', 'cesm2_waccm_fv2', 'cmcc_cm2_hr4',
                  'cmcc_esm2', 'cnrm_cm6_1_hr', 'e3sm_1_0', 'e3sm_1_1_eca',
                  'ec_earth3_aerchem', 'ec_earth3_veg', 'fgoals_f3_l',
                  'fio_esm_2_0', 'giss_e2_1_g', 'hadgem3_gc31_ll',
                  'iitm_esm', 'inm_cm5_0', 'ipsl_cm6a_lr', 'kiost_esm',
                  'miroc6', 'miroc_es2l', 'mpi_esm1_2_hr', 'mri_esm2_0',
                  
                  'norcpm1', 'noresm2_mm', 'taiesm1', 
                  'access_esm1_5', 'awi_esm_1_1_lr', 'bcc_esm1', 'canesm5',
                  'cesm2', 'cesm2_waccm', 'ciesm', 'cmcc_cm2_sr5',
                  'cnrm_cm6_1', 'cnrm_esm2_1', 'e3sm-1_1', 'ec_earth3',
                  'ec_earth3_cc', 'ec_earth3_veg_lr', 'fgoals_g3', 'gfdl_esm4',
                  'giss_e2_1_h', 'hadgem3_gc31_mm', 'inm_cm4_8', 'ipsl_cm5a2_inca',
                  'kace_1_0_g', 'mcm_ua_1_0', 'miroc_es2h', 'mpi_esm_1_2_ham', 
                  'mpi_esm1_2_lr', 'nesm3', 'noresm2_lm', 'sam0_unicon',
                  'ukesm1_0_ll')
      
    }
  }
  
  #match the model, then take the api name
  gcm_lookup_df <- data.frame(model_name =       c('ACCESS-CM2','ACCESS-ESM1-5', 'AWI-CM-1-1-MR',      'AWI-ESM-1-1-LR',
                                                   'BCC-CSM2-MR',      'BCC-ESM1',      'CAMS-CSM1-0',      'CanESM5',
                                                   'CanESM5-CanOE',      'CESM2',      'CESM2-FV2',      'CESM2-WACCM',
                                                   'CESM2-WACCM-FV2',      'CIESM',      'CMCC-CM2-HR4',      'CMCC-CM2-SR5',
                                                   'CMCC-ESM2',      'CNRM-CM6-1',      'CNRM-CM6-1-HR',      'CNRM-ESM2-1',
                                                   'E3SM-1-0',      'E3SM-1-1',      'E3SM-1-1-ECA',      'EC-Earth3',
                                                   'EC-Earth3-AerChem',      'EC-Earth3-CC',      'EC-Earth3-Veg',
                                                   'EC-Earth3-Veg-LR',      'FGOALS-f3-L',      'FGOALS-g3',
                                                   'FIO-ESM-2-0',      'GFDL-ESM4',      'GISS-E2-1-G',      'GISS-E2-1-H',
                                                   'HadGEM3-GC31-LL',      'HadGEM3-GC31-MM',      'IITM-ESM',
                                                   'INM-CM4-8',      'INM-CM5-0',      'IPSL-CM5A2-INCA',      'IPSL-CM6A-LR',
                                                   'KACE-1-0-G',      'KIOST-ESM',      'MCM-UA-1-0',      'MIROC6',
                                                   'MIROC-ES2H',      'MIROC-ES2L',      'MPI-ESM-1-2-HAM',      'MPI-ESM1-2-HR',
                                                   'MPI-ESM1-2-LR',      'MRI-ESM2-0',      'NESM3',      'NorCPM1',
                                                   'NorESM2-LM',      'NorESM2-MM',      'SAM0-UNICON',      'TaiESM1',
                                                   'UKESM1-0-LL' ),
                              api_name = c('access_cm2', 'access_esm1_5', 
                                           'awi_cm_1_1_mr', 'awi_esm_1_1_lr', 
                                           'bcc_csm2_mr', 'bcc_esm1',
                                           'cams_csm1_0','canesm5',
                                           'canesm5_canoe', 'cesm2', 
                                           'cesm2_fv2',  'cesm2_waccm', 
                                           'cesm2_waccm_fv2', 'ciesm', 
                                           'cmcc_cm2_hr4','cmcc_cm2_sr5',
                                           'cmcc_esm2', 'cnrm_cm6_1',
                                           'cnrm_cm6_1_hr',   'cnrm_esm2_1',
                                           'e3sm_1_0',  'e3sm-1_1', 
                                           'e3sm_1_1_eca', 'ec_earth3',
                                           'ec_earth3_aerchem',   'ec_earth3_cc',
                                           'ec_earth3_veg',  'ec_earth3_veg_lr',
                                           'fgoals_f3_l',  'fgoals_g3',
                                           'fio_esm_2_0',  'gfdl_esm4',
                                           'giss_e2_1_g',  'giss_e2_1_h',
                                           'hadgem3_gc31_ll', 'hadgem3_gc31_mm', 
                                           'iitm_esm',  'inm_cm4_8', 
                                           'inm_cm5_0',   'ipsl_cm5a2_inca',
                                           'ipsl_cm6a_lr',  'kace_1_0_g', 
                                           'kiost_esm', 'mcm_ua_1_0', 
                                           'miroc6',   'miroc_es2h', 
                                           'miroc_es2l',  'mpi_esm_1_2_ham', 
                                           'mpi_esm1_2_hr', 'mpi_esm1_2_lr', 
                                           'mri_esm2_0', 'nesm3', 
                                           'norcpm1',  'noresm2_lm', 
                                           'noresm2_mm',  'sam0_unicon',
                                           'taiesm1',   'ukesm1_0_ll'))
  
  #in all other cases take the user selection
  if(is.null(Models)){
    
    #user selection is probably the names on the website, but I need to tranlsate the name to the api request
    match_row <- match(model, gcm_lookup_df$model_name)
    
    Models <- gcm_lookup_df$api_name[match_row]
    
    #if the match was not successfull, then take the original entry and try it with that one
    if(any(is.na(Models))){
      warning(paste('At least one of the provided model name did match any of the known GCM model names. The function will try to download it anyway, but it is likely to fail. Please check if there are no typos in the GCMs name.\nThis affects the provided model names:', 
                    paste0(model[is.na(match_row)], collapse = ' ') )
      )
      
      #in case there are no matches in the lookup, then use for these the raw user input
      Models[is.na(match_row)] <- model[is.na(match_row)]
    }
    
    
  }
  
  
  #---------------------#
  #check if there is a blacklist file and read it
  #---------------------#
  blacklist <- NULL 
  
  if(file.exists(paste0(path_download_old, '/blacklist.txt'))){
    
    x <- scan(paste0(path_download_old, '/blacklist.txt'), what="list", quiet = TRUE)
    
    #remove everything with [] and $
    drop <- grep(pattern = c('\\['), x)
    if(length(drop) > 0){
      x <- x[-drop]
    }
    
    
    split_i <- grep(pattern = '\\$', x)
    blacklist <- list()
    
    for(i in 1:length(split_i)){
      
      #get the index of the $ sign
      s <- split_i[i]
      #clean the name
      name_clean <- gsub(pattern = "\\$", replacement = '', x[s])
      
      #in case it is the last index, go until end of vector
      if(i == length(split_i)){
        blacklist[[name_clean]] <- x[(s+1):length(x)]
      } else{
        blacklist[[name_clean]] <- x[(s+1):(split_i[i+1]-1)]
        
      }
      
    }
    #remove duplicates per scenario
    blacklist <- purrr::map(blacklist, unique)
    
  }
  
  
  
  #loop for the different scenarios
  blacklist_updated <- invisible(lapply(scenarios, FUN = function(scenario){
    
    
    #drop blacklisted stations
    if(Models[1] == 'default'){
      Model_download <- default_gcm_list[[frequency]][[scenario]] 
    } else {
      Model_download <- Models
    }
    
    
    #remove the models which were blacklisted, but let the user know about it
    if(is.null(blacklist) == FALSE){
      if(scenario %in% names(blacklist)){
        if(length(blacklist[[scenario]]) >= 1){
          
          blacklist[[scenario]]
          #check if the blacklisted names are in api style or website style
          to_be_dropped <- purrr::map_chr(blacklist[[scenario]], function(mod){
            if(mod %in% gcm_lookup_df$model_name){
              return(gcm_lookup_df$api_name[gcm_lookup_df$model_name == mod])
            } else {
              return(mod)
            }
          })
          
          Model_download <- Model_download[!(Model_download %in% to_be_dropped)]
          
          
        }
      }
    }
    
    cat('\n')
    cat(paste0(rep('-', 15), collapse = ''))
    cat(paste0('\nDownload of scenario: ', scenario, '\n'))
    cat(paste0(rep('-', 15), collapse = ''))
    cat('\n')
    
    #-----------------------------------
    #generate the request for the api
    #-----------------------------------
    
    #make a batch of requests..........
    prec_request <- tmax_request <- tmin_request <- NULL
    
    
    if('Tmax' %in% variable & length(Model_download) != 0){
      tmax_request <-   purrr::map(Model_download, function(mod){
        
        #file name
        #maybe add the area to the file name so that you can download for different areas but same scenario. this would not be possible right now
        fname <- paste0("tmax_",scenario,"_",mod, '_',  frequency, '_' ,paste0(area, collapse = '_'),".zip")
        
        #construct request
        request_max <- list(
          temporal_resolution = frequency,
          experiment = scenario,
          level = "single_levels",
          variable = "daily_maximum_near_surface_air_temperature",
          model = mod,
          year = as.character(year_start:year_end),
          month = as.character(month),
          area = area,
          format = "zip",
          dataset_short_name = "projections-cmip6",
          target = fname)
        
        #if already present return nothing
        if(fname %in% list.files(path_download) & update_everything == FALSE){
          cat(paste0('File ', fname, ' is already downloaded\n'))
          return(NULL)
        } else {
          #otherwise return the request
          return(request_max)
        }
        
      }) %>% 
        purrr::compact()
    }
    
    if('Tmin' %in% variable  & length(Model_download) != 0){
      tmin_request <-   purrr::map(Model_download, function(mod){
        
        
        
        #file name
        fname <- paste0("tmin_",scenario,"_",mod, '_', frequency, '_' ,  paste0(area, collapse = '_'),".zip")
        
        #construct request
        request_min <- list(
          temporal_resolution = frequency,
          experiment = scenario,
          level = "single_levels",
          variable = "daily_minimum_near_surface_air_temperature",
          model = mod,
          year = as.character(year_start:year_end),
          month = as.character(month),
          area = area,
          format = "zip",
          dataset_short_name = "projections-cmip6",
          target = fname)
        
        #if already present return nothing
        if(fname %in% list.files(path_download) & update_everything == FALSE){
          cat(paste0('File ', fname, ' is already downloaded\n'))
          return(NULL)
        } else {
          #otherwise return the request
          return(request_min)
        }
        
      }) %>% 
        purrr::compact()
    }
    
    if('Prec' %in% variable  & length(Model_download) != 0){
      prec_request <-   purrr::map(Model_download, function(mod){
        
        #file name
        #maybe add the area to the file name so that you can download for different areas but same scenario. this would not be possible right now
        fname <- paste0("prec_",scenario,"_",mod, '_', frequency, '_' ,paste0(area, collapse = '_'),".zip")
        
        #construct request
        request_prec <- list(
          temporal_resolution = frequency,
          experiment = scenario,
          level = "single_levels",
          variable = "precipitation",
          model = mod,
          year = as.character(year_start:year_end),
          month = as.character(month),
          area = area,
          format = "zip",
          dataset_short_name = "projections-cmip6",
          target = fname)
        
        #if already present return nothing
        if(fname %in% list.files(path_download) & update_everything == FALSE){
          cat(paste0('File ', fname, ' is already downloaded\n'))
          return(NULL)
        } else {
          #otherwise return the request
          return(request_prec)
        }
        
      }) %>% 
        purrr::compact()
    }
    
    
    request <- list()
    if(any(c(length(prec_request), length(tmin_request), length(tmax_request)) >= 1)){
      #maybe I can combine the request, so that the same variables of one model are fetched after each other insteaf of doing first Tmax, then tmin, then precipitation
      for(i in 1:max(length(tmax_request), length(tmin_request), length(prec_request))){
        
        if(i <= length(tmax_request)){
          request <- c(request, tmax_request[i])
        }
        if(i <= length(tmin_request)){
          request <- c(request, tmin_request[i])
        }
        if(i <= length(prec_request)){
          request <- c(request, prec_request[i])
        }
      }
    }
    
    
    #combine the requests
    #request <- c(tmax_request, tmin_request, prec_request)
    
    
    #if the request is empty, then stop this iteration and go to the next one
    if(length(request) == 0){
      cat(paste0('In case of ', scenario, ': The download request is empty after filtering blacklisted scenario-model-variable combinations and already downloaded files.\n'))
      if(scenario %in% names(blacklist)){
        return(blacklist[[scenario]])
      } else {
        return(NULL)
      }
    }
    
    
    #-------------------------------------#
    #run the download
    #-------------------------------------#
    #cycle through the requests, if a request was failed for a model, then drop the model from the requests
    
    
    #flags that control the download of the files
    
    #indicates that we keep trying to download, when turned to FALSE, then download stops
    run_request <- TRUE
    
    #flags controling which warning messages will be sent and for which requests
    send_warning_message_missing_model <- send_warning_message_2100 <- send_warning_message_no_points <- send_warning_message_unknown_error <- FALSE
    dropped_model_names <- dropped_model_names_no_points <- dropped_model_names_2100 <-  dropped_model_names_unknown_error <- c()
    
    #counts the failed download attemps
    n <- 1
    #keeps track for which model the download failed. this controls how often we keep 
    #trying to download
    model_2100_before <- ''
    model_2100_current <- ''
    
    #cases of error messages indicating an error from the serve to answer fast enough
    #update this vector in case we discover new error messages
    error_messages_process_error <- c('Process error: 2100', 
                                      'Process error: 2056', 
                                      'Error : internal server error---')
    
    
    
    #say how many files will be downloaded
    cat(paste0('\nDownloading: ', length(request), ' file(s)\n'))
    
    #thus loops runs the download attempts
    while(run_request){
      
      tmp <- try(ecmwfr::wf_request_batch(request_list = request,
                                          user = user,
                                          path = path_download, 
                                          time_out = sec_wait), silent = TRUE) %>% 
        suppressMessages()
      
      #allows to end the while loop
      run_request <- FALSE
      
      #check if there is an error
      if(inherits(tmp, "try-error")){
        
        #users did not agree to terms and condtions of cmip6 dataset
        if(grepl("Error : permission deniedrequired licences", tmp)){
          stop("User did not agree to the required terms and conditions to download CMIP6 data.\nPlease go to 'https://cds.climate.copernicus.eu/datasets/projections-cmip6?tab=download#manage-licences' or see the Details section of the download_cmip6_ecmwfr function")
        }
        
        #user set a wrong key that does not work
        else if(grepl('Error : permission deniedAuthentication failed401', tmp)){
          stop('User provided a key that does not work.\nThe function will remove the faulty key now from the system.\nEither try running ecmwfr::wf_set_key() and select the Personal Access Token from your profile in the Climate Data Store')
          keyring::key_delete(service = service, username = user)
        }
        
        #if it contains the error message that the No matching data for request
        else if(grepl('No matching data for request', tmp)){
          
          #send warning message at the end
          send_warning_message_missing_model <- TRUE
          
          #------#
          #extract model name from error message   #
          #------#
          
          #then kick this station out and try again
          split_error <- strsplit(tmp, '\\{')
          
          #take second option
          split_again <- strsplit(split_error[[1]][2], ',')
          
          model_fragment <- split_again[[1]][grep('model', split_again[[1]])]
          
          model_name <- gsub(pattern = " 'model': ", replacement = '', model_fragment)
          model_name <- gsub("'", '', model_name)
          
          
          dropped_model_names <- c(dropped_model_names, model_name)
          
          #---------#
          #remove the model name from the Models argument   #
          #---------#
          
          #get the names of the requested models
          request_models <- purrr::map_chr(request, 'model')
          
          #translate the name if no match
          
          #if name is not in the requested model names, then use the lookup table
          if((model_name %in% request_models) == FALSE){
            drop_request <- which(request_models %in% gcm_lookup_df$api_name[gcm_lookup_df$model_name == model_name])
          } else {
            drop_request <- which(request_models %in% model_name)
          }
          
          request <- request[-drop_request]
          
          #check which files are already downloaded and drop them from the list, too
          
          already_downloaded <- which(purrr::map_chr(request, 'target') %in% list.files(path_download))
          if(length(already_downloaded) != 0){
            request <- request[-already_downloaded]
          }
          
          #if there are remaining requests, then run it again
          if(length(request) > 0){
            run_request <- TRUE
          }
          
          
        }
        
        
        #in case of this error message, also remove the stations
        else if(grepl('There were no valid data points found in the requested subset', tmp)){
          
          #flag to send later on warning message
          send_warning_message_no_points <- TRUE
          
          #extract the model which caused the problem
          dropped_model_names_no_points <- c(dropped_model_names_no_points, request[[1]]$model)
          
          #drop the model from the requests
          drop_request <- which(purrr::map_chr(request, 'model') %in% dropped_model_names_no_points)
          request <- request[-drop_request]
          
          #also drop already downloaded files from the request
          already_downloaded <- which(purrr::map_chr(request, 'target') %in% list.files(path_download))
          if(length(already_downloaded) != 0){
            request <- request[-already_downloaded]
          }
          
          #if there are remaining requests, then run it again
          if(length(request) > 0){
            run_request <- TRUE
          }
        }
        
        #in case of failure of website to answer, try again after some seconds
        #if it fails to answer too often for the same model, then skip it
        else if(purrr::map_lgl(error_messages_process_error, function(err_msg) grepl(err_msg, tmp)) %>% any()){
          
          #update the list of already downloaded files
          #clean the to-do list, in case files have been already downloaded
          already_downloaded <- which(purrr::map_chr(request, 'target') %in% list.files(path_download))
          if(length(already_downloaded) != 0){
            request <- request[-already_downloaded]
          }
          
          #this controls a counter, if we fail to download a station because of that error for too often, then we will drop the station
          model_2100_before <- model_2100_current
          model_2100_current <- request[[1]]$model
          
          #in case it failed for that file in the previous step, increase the counter
          if(model_2100_before == model_2100_current){
            n <- n + 1
          } else {
            n <- 1
          }
          
          #in case we have not tried more often then try-threshold
          if(n < n_try){
            #wait a moment before repeating the download
            Sys.sleep(wait)
            run_request <- TRUE
          } else {
            
            #flag to send later warning message
            send_warning_message_2100 <- TRUE
            
            dropped_model_names_2100 <- c(dropped_model_names_2100, request[[1]]$model)
            
            #drop the model from the requests
            drop_request <- which(purrr::map_chr(request, 'model') %in% dropped_model_names_2100)
            request <- request[-drop_request]
            
            #only run again if there is something remaining
            if(length(request) > 0){
              run_request <- TRUE
            }
          }
          
        }   else {
          
          #---------------#
          #unknown error
          #---------------#
          
          #flag to send later on warning message
          send_warning_message_unknown_error <- TRUE
          
          #extract the model which caused the problem
          dropped_model_names_unknown_error <- c(dropped_model_names_unknown_error, request[[1]]$model)
          
          #drop the model from the requests
          drop_request <- which(purrr::map_chr(request, 'model') %in% dropped_model_names_unknown_error)
          request <- request[-drop_request]
          
          #also drop already downloaded files from the request
          already_downloaded <- which(purrr::map_chr(request, 'target') %in% list.files(path_download))
          if(length(already_downloaded) != 0){
            request <- request[-already_downloaded]
          }
          
          #if there are remaining requests, then run it again
          if(length(request) > 0){
            run_request <- TRUE
          }
        }
        
      }
    } #end of while loop which downloads the data
    
    #----------------------#
    #send warning messages
    #----------------------#
    
    if(send_warning_message_missing_model){
      message(paste0('Dropped model: ', paste0(dropped_model_names, collapse = ', '), ' from the request, because the requested combination of\n SSP: ', scenario, ', Model: ', paste0(dropped_model_names, collapse = ', '), ', and Variables: ', paste0(variable, collapse = ', '),
                     ' does not exist.'))
    }
    if(send_warning_message_no_points){
      
      modelname_warning_message <- gcm_lookup_df$model_name[gcm_lookup_df$api_name %in% dropped_model_names_no_points]
      
      message(paste0('Dropped model: ', paste0(modelname_warning_message, collapse = ', '), ' from the request, because the requested area and / or time period was not covered by the models'))
    }
    if(send_warning_message_2100){
      
      modelname_warning_message <- gcm_lookup_df$model_name[gcm_lookup_df$api_name %in% dropped_model_names_2100]
      
      message(paste0('Dropped model: ', paste0(modelname_warning_message, collapse = ', '), ' from the request, the server failed to answer to that request too often.'))
    }
    
    if(send_warning_message_unknown_error){
      
      modelname_warning_message <- gcm_lookup_df$model_name[gcm_lookup_df$api_name %in% dropped_model_names_unknown_error]
      #error message which I have not encountered yet
      message(paste0('Unknown error when downloading the CMIP6 data. Error happended for model ', modelname_warning_message, '\nThe model was dropped from the request list.\n'))
    }
    
    
    #in case the scenario did not have already blacklisted entries
    if(scenario %in% names(blacklist) == FALSE){
      return(c(dropped_model_names, dropped_model_names_no_points) )
    } else {
      return(c(blacklist[[scenario]], dropped_model_names, dropped_model_names_no_points) %>% 
               unique())
      
    }
  }))
  
  names(blacklist_updated) <- scenarios
  utils::capture.output(blacklist_updated, file = paste0(path_download_old, '/blacklist.txt'))
  
  
  
}


PLS_pheno <- function (weather_data,bio_data,split_month=7,runn_mean=11,expl.var=30,
                       ncomp.fix=NULL,use_Tmean=FALSE,return.all=FALSE,crossvalidate="none",end_at_pheno_end=TRUE) {

  # Interpolate missing temperatures and calculate Tmean from Tmin and Tmax - NEW: only if necessary
    if (!use_Tmean) { 
      # Remove missing temperature data at the beginning or end of the time series
      cl.dat <- which(!is.na(weather_data[,"Tmax"])|!is.na(weather_data[,"Tmin"]))
      weather_file <- weather_data[min(cl.dat):max(cl.dat),]
      weather_file [which(weather_file$Tmin > weather_file$Tmax), c("Tmin", "Tmax")] <- NA
      Tmin_gaps <- interpolate_gaps(weather_file$Tmin)
      weather_file$Tmin <- Tmin_gaps[[1]]
      Tmax_gaps <- interpolate_gaps(weather_file$Tmax)
      weather_file$Tmax <- Tmax_gaps[[1]]
      weather_file[, "Tmean"] <- (weather_file$Tmax + weather_file$Tmin)/2 }
    
    if (use_Tmean) {
      # Remove missing temperature data at the beginning or end of the time series
      cl.dat <- which(!is.na(weather_data[,"Tmean"]))
      weather_file <- weather_data[min(cl.dat):max(cl.dat),]
      Tmean_gaps <- interpolate_gaps(weather_file$Tmean)
      weather_file$Tmean <- Tmean_gaps[[1]] }
    
    # Adjust dates  - "Season" = current year before the cutpoint, following year after the cutpoint
    weather_file[weather_file$Month<=split_month,"Season"] <- weather_file[weather_file$Month<=split_month,"Year"]
    weather_file[weather_file$Month>split_month,"Season"] <- weather_file[weather_file$Month>split_month,"Year"] + 1
    weather_file[, "Date"] <- weather_file$Month*100 + weather_file$Day
    weather_file[, "JDay"] <- strptime(paste(weather_file$Month,"/", weather_file$Day,"/", weather_file$Year, sep = ""), 
        "%m/%d/%Y")$yday + 1
    ww <- weather_file[, "Tmean"]
    rr <- weather_file[, "Tmean"]
    
    # Calculate running means of temperatures
    for (dd in 1:length(ww)) {
        if (dd < ceiling(runn_mean/2)) { rr[dd] <- mean(ww[1:(dd + floor(runn_mean/2))]) }
        if ((dd >= ceiling(runn_mean/2)) & (dd <= length(ww)-ceiling(runn_mean/2))) {
            rr[dd] <- mean(ww[(dd-floor(runn_mean/2)):(dd+floor(runn_mean/2))])  }
        if (dd > (length(ww) - ceiling(runn_mean/2))) {rr[dd] <- mean(ww[(dd-floor(runn_mean/2)):length(ww)])} }
        weather_file[, "runn"] <- rr
    
    pls_ncomp <- function(indep,dep,threshold) {  ### NEW: Variable threshold of explained variation
        dat <- data.frame(dep)
        dat$runn <- indep
        if (length(dep) > 15) {
            suppressWarnings(pls_out <- plsr(dep~runn, data=dat, ncomp=10, validation="none"))  ### NEW: no cross-validation
            ncomp <- which(cumsum(explvar(pls_out)) > threshold)[1]
            if (is.na(ncomp)) ncomp <- 10 }
        else ncomp <- 2
        return(ncomp) }
        
    # Matrix with temperatures for each day of the year (columns) and each season (rows)    
    seasons <- unique(weather_file$Season)    # One-year periods between cutpoints
    for (yy in seasons) {
    	yearweather <- weather_file[weather_file$Season==yy, ]
        weathervector <- yearweather$runn[1:365]     # runn = running mean temperature
        if (yy == seasons[1]) 
            year_res <- weathervector
        else year_res <- rbind(year_res, weathervector)
        if (nrow(yearweather) == 365) {
            labdates <- yearweather$Date
            labJdates <- yearweather$JDay  }  }
    
    
    
    colnames(year_res) <- paste("runn_", 1:365, sep="")
    year_res <- cbind(Season = seasons, year_res)
    data_set <- year_res
    full_seasons <- which(!is.na(rowMeans(data_set)))
    data_set <- data_set[full_seasons, ]
    newseasons <- data_set[, "Season"]
    suppressWarnings(bio_data <- bio_data[which(bio_data[, 
                                                         "Year"] %in% newseasons), ])
    suppressWarnings(bio_data <- bio_data[which(!is.na(as.numeric(as.character(bio_data$pheno)))), 
                                          ])
    #all_outputs[["pheno"]]<-bio_data
    suppressWarnings(bio <- as.numeric(as.character(bio_data$pheno)))
    indep <- as.matrix(data_set[which(data_set[, "Season"] %in% 
                                        bio_data$Year), ])
    indep <- indep[, 2:ncol(indep)]
    
    pheno_end<-get_last_date(as.numeric(as.character(bio_data$pheno)))
    
    if(is.numeric(end_at_pheno_end)) pheno_end<-end_at_pheno_end
    
    if(end_at_pheno_end) if (pheno_end %in% labJdates) {dayskeep<-1:which(labJdates==pheno_end)
    labJdates<-labJdates[dayskeep]
    labdates<-labdates[dayskeep]
    
    indep<-indep[,dayskeep]}
    
    
    
   # colnames(year_res) <- paste("runn_", 1:365, sep="")
  #  data_set <- cbind(Season=seasons, year_res)
  #  data_set <- data_set[which(!is.na(rowMeans(data_set))), ]  # Only seasons with temperatures from first to last day
  #  newseasons <- data_set[, "Season"]  # vector with full seasons
    
  #  # Reduce phenological data to those of full seasons; Reduce both datasets to those with phenology data
  #  bio_data <- bio_data[!is.na(bio_data$pheno), ]
  #  bio_data <- subset(bio_data,bio_data$Year %in% newseasons)
  #  bio <- as.numeric(as.character(bio_data$pheno))
  #  indep <- data_set[data_set[,"Season"] %in% bio_data$Year, ]
  #  indep <- indep[,-1]
    
  #  pheno_end<-max(as.numeric(as.character(bio_data$pheno)),na.rm=TRUE)
    
  #  if(is.numeric(end_at_pheno_end)) pheno_end<-end_at_pheno_end
  #  
  #  if(end_at_pheno_end) if (pheno_end %in% labJdates) {dayskeep<-1:which(labJdates==pheno_end)
  #  labJdates<-labJdates[dayskeep]
  #  labdates<-labdates[dayskeep]
  #  
  #  indep<-indep[,dayskeep]}
    
    if (is.null(ncomp.fix)) {ncomp <- pls_ncomp(indep=indep,dep=bio,threshold=expl.var) } else {ncomp <- ncomp.fix}   ### NEW, works with fixed ncomp
   
    ### PLS regression
    # Temperatures of each day are scaled by their standard deviations, so that all regressors have same variation
    sdindep <- apply(indep,2,sd)  # SD of temperatures on each day of the season
    sdindep[which(sdindep == 0)] <- 1    # Avoids impossible cases when scaling by standard deviation
    PLS_output <- plsr(bio~indep,ncomp,validation=crossvalidate,method="oscorespls",scale=sdindep)
    d1 <- switch(split_month,31,59,89,120,151,181,212,243,274,304,335,365)
	labJdates[labJdates>d1] <- labJdates[labJdates>d1]-365
    out <- data.frame()
    tablength<-length(coef(PLS_output))
    out[1:tablength,"Date"] <- labdates
    out[1:tablength,"JDay"] <- labJdates
    out[1:tablength,"Coef"] <- coef(PLS_output)
    out[1:tablength,"VIP"] <- VIP(PLS_output)[ncomp, ]
    out[1:tablength,"Tmean"] <- colMeans(indep)
    out[1:tablength,"Tstdev"] <- apply(indep, 2, sd, na.rm = TRUE)
    if (return.all) return(list(object_type="PLS_Temp_pheno",pheno=bio_data,PLS_summary=out,PLS_output=PLS_output)) else return(list(object_type="PLS_Temp_pheno",pheno=bio_data,PLS_summary=out))  }


PLS_chill_force<-function (daily_chill_obj, bio_data_frame, split_month, expl.var = 30, 
                           ncomp.fix = NULL, return.all = FALSE, crossvalidate = "none",end_at_pheno_end=TRUE,
                           chill_models=c("Chilling_Hours","Utah_Chill_Units","Chill_Portions"),
                           heat_models=c("GDH")) 
{
  if (daily_chill_obj[1] == "daily_chill") {
    dc <- daily_chill_obj$daily_chill
    weather_file <- daily_chill_obj$daily_chill
    bio_data <- bio_data_frame
    weather_file[which(weather_file$Month <= split_month), 
                 "Season"] <- weather_file[which(weather_file$Month <= 
                                                   split_month), "Year"]
    weather_file[which(weather_file$Month > split_month), 
                 "Season"] <- weather_file[which(weather_file$Month > 
                                                   split_month), "Year"] + 1
    weather_file[, "Date"] <- weather_file$Month * 100 + 
      weather_file$Day
    weather_file[, "JDay"] <- strptime(paste(weather_file$Month, 
                                             "/", weather_file$Day, "/", weather_file$Year, sep = ""), 
                                       "%m/%d/%Y")$yday + 1
    pls_ncomp_old <- function(indep, dep, threshold = 30) {
      dat <- data.frame(dep)
      dat$runn <- indep
      if (length(dep) > 15) {
        suppressWarnings(pls_out <- plsr(dep ~ runn, 
                                         data = dat, ncomp = 10, validation = "CV"))
        suppressWarnings(pls_cv <- crossval(pls_out, 
                                            segments = 10))
        res <- data.frame(ncomp = c(1:10), explvar = explvar(pls_out), 
                          cumul = NA)
        res$cumul[1] <- res$explvar[1]
        for (i in 2:nrow(res)) {
          res$cumul[i] <- res$cumul[i - 1] + res$explvar[i]
        }
        ncomp <- which(res$cumul > threshold)[1]
        if (is.na(ncomp)) 
          ncomp <- 10
      }
      else ncomp <- 2
      return(ncomp)
    }
    pls_ncomp <- function(indep, dep, threshold) {
      dat <- data.frame(dep)
      dat$runn <- indep
      if (length(dep) > 15) {
        suppressWarnings(pls_out <- plsr(dep ~ runn, 
                                         data = dat, ncomp = 10, validation = "none"))
        ncomp <- which(cumsum(explvar(pls_out)) > threshold)[1]
        if (is.na(ncomp)) 
          ncomp <- 10
      }
      else ncomp <- 2
      return(ncomp)
    }
    seasons <- unique(weather_file$Season)
    #chill_models <- c("Chilling_Hours", "Utah_Chill_Units", 
    #                  "Chill_Portions")
    #heat_models <- c("GDH")
    all_outputs <- list(object_type = "PLS_chillforce_pheno")
    for (CM in chill_models) for (HM in heat_models) {
      for (yy in seasons) {
        yearweather <- weather_file[which(weather_file$Season == 
                                            yy), ]
        weathervector <- c(yearweather[1:365, CM], yearweather[1:365, 
                                                               HM])
        if (yy == seasons[1]) 
          year_res <- weathervector else year_res <- rbind(year_res, weathervector)
        if (nrow(yearweather) == 365) {
          labdates <- yearweather$Date
          labJdates <- yearweather$JDay
        }
      }
      colnames(year_res) <- c(paste("Chill_", 1:365, sep = ""), 
                              paste("Heat_", 1:365, sep = ""))
      year_res <- cbind(Season = seasons, year_res)
      data_set <- year_res
      full_seasons <- which(!is.na(rowMeans(data_set)))
      data_set <- data_set[full_seasons, ]
      newseasons <- data_set[, "Season"]
      suppressWarnings(bio_data <- bio_data[which(bio_data[, 
                                                           "Year"] %in% newseasons), ])
      suppressWarnings(bio_data <- bio_data[which(!is.na(as.numeric(as.character(bio_data$pheno)))), 
                                            ])
      all_outputs[["pheno"]]<-bio_data
      suppressWarnings(bio <- as.numeric(as.character(bio_data$pheno)))
      indep <- as.matrix(data_set[which(data_set[, "Season"] %in% 
                                          bio_data$Year), ])
      indep <- indep[, 2:ncol(indep)]
      
      #pheno_end<-max(as.numeric(as.character(bio_data$pheno)))
      
      pheno_end<-get_last_date(as.numeric(as.character(bio_data$pheno)))
      
      if(is.numeric(end_at_pheno_end)) pheno_end<-end_at_pheno_end
      
      if(end_at_pheno_end) if (pheno_end %in% labJdates) {dayskeep<-1:which(labJdates==pheno_end)
      labJdates<-labJdates[dayskeep]
      labdates<-labdates[dayskeep]
      
      indep<-indep[,c(dayskeep,365+dayskeep)]}
      
      if (is.null(ncomp.fix)) {
        ncomp <- pls_ncomp(indep = indep, dep = bio, 
                           threshold = expl.var)
      }      else {        ncomp <- ncomp.fix      }
      sdindep <- apply(indep, 2, sd)
      sdindep[which(sdindep == 0)] <- 1
      PLS_output <- plsr(bio ~ indep, ncomp, validation = "none", 
                         method = "oscorespls", scale = sdindep)
      d1 <- switch(split_month, 31, 59, 89, 120, 151, 181, 
                   212, 243, 274, 304, 335, 365)
      labJdates[labJdates > d1] <- labJdates[labJdates > 
                                               d1] - 365
      out <- data.frame()
      tablength<-length(coef(PLS_output))
      out[1:tablength, "Date"] <- labdates
      out[1:tablength, "Type"] <- c(rep("Chill", tablength/2), rep("Heat", 
                                                                   tablength/2))
      out[1:tablength, "JDay"] <- labJdates
      out[1:tablength, "Coef"] <- coef(PLS_output)
      if(ncomp>1) out[1:tablength, "VIP"] <- VIP(PLS_output)[ncomp, ] else out[1:tablength, "VIP"] <- VIP(PLS_output)[ncomp]
      out[1:tablength, "MetricMean"] <- colMeans(indep)
      out[1:tablength, "MetricStdev"] <- apply(indep, 2, sd, na.rm = TRUE)
      if (return.all) 
        all_outputs[[CM]][[HM]] <- list(PLS_summary = out, 
            PLS_output = PLS_output)  else all_outputs[[CM]][[HM]] <- list(PLS_summary = out)
    }
    return(all_outputs)
  }
  else {
    "Error: not a daily chill object; use function daily_chill to make one from hourly temperature data"
  }
}


bloom_prediction <-
function (HourChillTable, Chill_model, Chill_req, Heat_req) 
{
  results <- data.frame()
  HCT <- HourChillTable
  chill <- HCT[, Chill_model]
  chill2 <- chill[c(2:length(chill), 1)]
  Creqfull <- which((chill<=Chill_req)&(chill2>Chill_req)) + 1
  Creqfull <- Creqfull[!chill[Creqfull] == 0]
  Creqfull <- Creqfull[which(!is.na(Creqfull))]
  results <- data.frame(Creqfull = Creqfull)
  results[, c("Creq_year", "Creq_month", "Creq_day", "Creq_JDay")] <- HCT[Creqfull, 
                                                                          c("Year", "Month", "Day", "JDay")]
  results[, "Hreqfull"]<-rep(NA,nrow(results))
  results[, "Hreq_year"]<-rep(NA,nrow(results))
  results[, "Hreq_month"]<-rep(NA,nrow(results))
  results[, "Hreq_day"]<-rep(NA,nrow(results))
  results[, "Hreq_JDay"]<-rep(NA,nrow(results))
  
  if(nrow(results)>0)
  {for (i in 1:nrow(results)) {
    if (!i == nrow(results)) 
      tabend <- results$Creqfull[i + 1]
    else tabend <- nrow(HCT)
    temp <- HCT[results$Creqfull[i]:tabend, ]
    temp[, "GDH"] <- temp[, "GDH"] - temp[1, "GDH"]
    GDH <- temp[, "GDH"]
    GDH2 <- GDH[c(2:length(GDH), 1)]
    Hreqfull <- which((GDH - Heat_req) * (GDH2 - Heat_req) < 
                        0) + 1
    Hreqfull <- Hreqfull[!GDH[Hreqfull] <= 0]
    Hreqfull <- Hreqfull[which(!is.na(Hreqfull))][1]
    results[i, c("Hreqfull", "Hreq_year", "Hreq_month", "Hreq_day", 
                 "Hreq_JDay")] <- c(Hreqfull, temp[Hreqfull, c("Year", 
                                                               "Month", "Day", "JDay")])
  }
   results <- results[which(results[, "Hreq_year"] - results[, 
                                                             "Creq_year"] < 2), ]
  }
  return(results)
}

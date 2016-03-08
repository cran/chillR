runn_mean<-function(vec,runn_mean)
{ww <- vec
rr <- vec
for (dd in 1:length(ww)) {
  if (dd < ceiling(runn_mean/2)) {
    rr[dd] <- mean(ww[1:(dd + floor(runn_mean/2))])
  }
  if ((dd >= ceiling(runn_mean/2)) & (dd <= length(ww) - 
                                      ceiling(runn_mean/2))) {
    rr[dd] <- mean(ww[(dd - floor(runn_mean/2)):(dd + 
                                                   floor(runn_mean/2))])
  }
  if (dd > (length(ww) - ceiling(runn_mean/2))) {
    rr[dd] <- mean(ww[(dd - floor(runn_mean/2)):length(ww)])
  }
}
return(rr)}

#' Running mean of a vector
#' 
#' Function to calculate the running mean of a numeric vector
#' 
#' 
#' @param vec numeric vector
#' @param runn_mean number of vector elements to use for calculating the
#' running mean
#' @param na.rm ignore NA values when calculating means. Defaults to FALSE.
#' @param exclude_central_value exclude central value in calculating means.
#' Defaults to FALSE.
#' @param FUN function to be applied. For a running mean, this is usually mean (the
#' default), but other functions can also be specified here (the na.rm parameter
#' won't work then, and the function has to be dependent on one numeric variable only.
#' @return numeric vector containing the running mean
#' @author Eike Luedeling
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' 
#' plot(runn_mean(rnorm(1000),150))
#' 
#' @export runn_mean
runn_mean<-function(vec,runn_mean,na.rm=FALSE,exclude_central_value=FALSE,FUN=mean)
{
  if(identical(FUN,mean)) if(na.rm==TRUE) FUN<-function(x) mean(x,na.rm=TRUE) else FUN<-function(x) mean(x,na.rm=FALSE)
  
  ww <- vec
  rr <- vec
  for (dd in 1:length(ww)) {
    if (dd < ceiling(runn_mean/2)) {
      if(!exclude_central_value) rr[dd] <- sapply(list(ww[1:(dd + floor(runn_mean/2))]),FUN)
      if(exclude_central_value) rr[dd] <- sapply(list(ww[(1:(dd + floor(runn_mean/2)))[which(!(1:(dd + floor(runn_mean/2)))==dd)]]),FUN)
    }
    if ((dd >= ceiling(runn_mean/2)) & (dd <= length(ww) - 
                                        ceiling(runn_mean/2))) {
      if(!exclude_central_value) rr[dd] <- sapply(list(ww[(dd - floor(runn_mean/2)):(dd + 
                                                                                       floor(runn_mean/2))]),FUN)
      if(exclude_central_value) rr[dd] <- sapply(list(ww[((dd - floor(runn_mean/2)):(dd + 
                                                                                       floor(runn_mean/2)))[
                                                                                         which(!((dd - floor(runn_mean/2)):(dd + 
                                                                                                                              floor(runn_mean/2)))==dd)]]),FUN)
    }
    if (dd > (length(ww) - ceiling(runn_mean/2))) {
      if(!exclude_central_value) rr[dd] <- sapply(list(ww[(dd - floor(runn_mean/2)):length(ww)]),FUN)
      if(exclude_central_value) rr[dd] <- sapply(list(ww[((dd - floor(runn_mean/2)):length(ww))[
        which(!((dd - floor(runn_mean/2)):length(ww))==dd)]]),FUN)
    }
  }
  return(rr)}
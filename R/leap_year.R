leap_year<-function(x) if(!x/4==trunc(x/4)) FALSE else
                         if(!x/100==trunc(x/100)) TRUE else
                           if(!x/400==trunc(x/400)) FALSE else TRUE

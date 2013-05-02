interpolate_gaps <-
function(x)   #x is the vector to work with
            {
            suppressWarnings(x<-as.numeric(as.character(x)))
            gaps<-list()
            gap_length<-c()
            miss<-which(is.na(x))
            if(length(miss)>0)
            {for (na in 1:length(miss))
                {if(na==1) {curr<-miss[na]
                           curr_gap<-1} else
                   {if(miss[na]==miss[na-1]+1) {curr<-c(curr,miss[na])
                                              curr_gap<-curr_gap+1} else
                                              {curr<-miss[na]
                                               curr_gap<-1}}
                   if(na==length(miss))
                      {gaps[[length(gaps)+1]]<-curr
                       gap_length<-c(gap_length,curr_gap)} else
                   if(!miss[na]==miss[na+1]-1)
                      {gaps[[length(gaps)+1]]<-curr
                       gap_length<-c(gap_length,curr_gap)}}
            for (gg in 1:length(gap_length))
                {start_val<-x[min(gaps[[gg]])-1]
                end_val<-x[max(gaps[[gg]])+1]
                valdiff<-end_val-start_val
                for (gl in 1:gap_length[gg])
                   {x[gaps[[gg]][gl]]<-start_val+(valdiff/(gap_length[gg]+1))*gl}
                }}
            if(length(miss)==0) miss<-rep(FALSE,length(x)) else {mi<-rep(FALSE,length(x))
                                                                 mi[miss]<-TRUE
                                                                 miss<-mi}
                return(list(interp=x,missing=miss))
        }

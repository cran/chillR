color_bar_maker <-
function(column_yn,column_quant,threshold,col1,col2,col3)
{
  color_bars<-c(rep(NA,length(column_yn)))
  color_bars[which(column_yn>=threshold&column_quant<0)]<-col1
  color_bars[which(column_yn>=threshold&column_quant>=0)]<-col2
  color_bars[which(!column_yn>=threshold)]<-col3
  return(color_bars)}

YEARMODA2Date<-function(YEARMODA)
{if(is.numeric(YEARMODA))
  {Year<-trunc(YEARMODA/10000)
Month<-trunc((YEARMODA-Year*10000)/100)
Day<-YEARMODA-Year*10000-Month*100
return(ISOdate(Year,Month,Day))} 
}

elf_pct_chg <- function(pct_inputs = list()){
  ruslope <- pct_inputs$ruslope
  ruint <- pct_inputs$ruint
  biometric_title <- pct_inputs$biometric_title 
  flow_title <- pct_inputs$flow_title
  Feature.Name <- pct_inputs$Feature.Name
  pct_chg <- pct_inputs$pct_chg
  #sampres <- pct_inputs$sampres
  startdate <- pct_inputs$startdate
  enddate <- pct_inputs$enddate
 
  its <- seq(1, 500, 1)
  pct <- (its -((pct_chg/100)*its))
  sa <- (ruslope*log(its))+ruint
  sb <- (ruslope*log(pct))+ruint
  pct_chgb <- (((sa-sb)/sa)*100)
  pct_chgs = c(pct_chgb)
  xvalues = c(its)
  
  slope_table = data.frame(xvalues,pct_chgs)
  #print(head(slope_table))
  
#title_projname <- sampres

#Plot titles
ptitle <- paste("Change in ",biometric_title," at ",pct_chg,"% Flow Reduction","\n", Feature.Name," (",startdate," to ",enddate,")\n",sep="")
xaxis_title <- paste("\n",flow_title,sep="");
yaxis_title <- paste("% Decrease in ", biometric_title,"\n", sep="");

if(flow_title == "Drainage Area (km^2)"){
	yup_lim <- 6
} else {
	yup_lim <- 4
}


plt2 <- ggplot(slope_table, aes(x=xvalues, y=pct_chgs)) + 
  geom_line()+
  scale_color_manual(
    "Legend",
    values=c("red"),
    labels=c("Analysis Point")
  ) + 
  ggtitle(ptitle)+
  labs(x=xaxis_title,y=yaxis_title)+
  theme(axis.text.x = element_text(colour="grey20",size=20,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=15,hjust=.5,vjust=.5,face="plain"))

}


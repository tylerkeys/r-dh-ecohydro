elf_pct_chg <- function(pct_inputs = list()){
  ruslope <- pct_inputs$ruslope
  ruint <- pct_inputs$ruint
  biometric_title <- pct_inputs$biometric_title 
  flow_title <- pct_inputs$flow_title
  Feature.Name <- pct_inputs$Feature.Name
  pct_chg <- pct_inputs$pct_chg
  sampres <- pct_inputs$sampres
  startdate <- pct_inputs$startdate
  enddate <- pct_inputs$enddate

  its <- seq(1, 1000, 0.1)
  xvalues = c(its)
  sa <- (ruslope*log(its))+ruint
  pct_list <- c(5,10,20,30,40,50)
  slope_table <- data.frame(xvalues=xvalues,
                            stringsAsFactors=FALSE)
  
  #i <- 1
  for (i in 1:length(pct_list)) {
    pct <- (its -((pct_list[i]/100)*its))
    sb <- (ruslope*log(pct))+ruint
    pct_chgb <- (((sa-sb)/sa)*100)
    pct_chgs = c(pct_chgb)
    slope_table_i = data.frame(pct_chgs)
    names(slope_table_i) <- c(paste("pct_chg_",pct_list[i],sep=""))
    slope_table <- cbind(slope_table, slope_table_i)
  }
  
title_projname <- sampres

#Plot titles
ptitle <- paste("Change in ",biometric_title," at Various % Flow Reductions","\n", Feature.Name," (",startdate," to ",enddate,")\n",title_projname," grouping",sep="")
xaxis_title <- paste("\n",flow_title,sep="");
yaxis_title <- paste("% Decrease in ", biometric_title,"\n", sep="");

plt2 <- ggplot(slope_table, aes(x=xvalues, y=pct_chgs_20)) + 
  
  geom_line(data = slope_table, aes(x=xvalues,y=pct_chg_50,color = "black")) + 
  geom_line(data = slope_table, aes(x=xvalues,y=pct_chg_40,color = "blue")) + 
  geom_line(data = slope_table, aes(x=xvalues,y=pct_chg_30,color = "green"))+
  geom_line(data = slope_table, aes(x=xvalues,y=pct_chg_20,color = "red"))+
  geom_line(data = slope_table, aes(x=xvalues,y=pct_chg_10,color = "violet"))+
  geom_line(data = slope_table, aes(x=xvalues,y=pct_chg_5,color = "wheat"))+
  
  scale_color_manual(
    "Flow Reduction",
     values=c("black","blue","forestgreen","red","darkmagenta","sienna4"),
     labels=c("50%","40%","30%","20%","10%","5%")
    ) + 
  
  scale_x_log10(
    #limits = c(0.1,500),
    limits = c(1,500),
    breaks = trans_breaks("log", function(x) {10^x})
 #   breaks = trans_breaks("log10", function(x) {10^x})#,
    #labels = trans_format("log10", math_format(10^.x))
    #labels = round(x,digits = 0)
  ) +
  annotation_logticks(sides = "b")+
  
  ggtitle(ptitle)+
  labs(x=xaxis_title,y=yaxis_title)+
  theme(axis.text.x = element_text(colour="grey20",size=15,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=15,hjust=.5,vjust=.5,face="plain"))

}


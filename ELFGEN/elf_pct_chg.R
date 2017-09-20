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
 

#pct_chg <- 10

pct_1 <- (1 -((pct_chg/100)*1))
pct_5 <- (5 -((pct_chg/100)*5))
pct_10 <-(10 -((pct_chg/100)*10))
pct_25 <-(25 -((pct_chg/100)*25))
pct_50 <-(50 -((pct_chg/100)*50))
pct_100 <-(100 -((pct_chg/100)*100))
pct_200 <-(200 -((pct_chg/100)*200))
pct_500 <-(500 -((pct_chg/100)*500))

sa_1 <- (ruslope*log(1))+ruint
sa_5 <- (ruslope*log(5))+ruint
sa_10 <- (ruslope*log(10))+ruint
sa_25 <- (ruslope*log(25))+ruint
sa_50 <- (ruslope*log(50))+ruint
sa_100 <- (ruslope*log(100))+ruint
sa_200 <- (ruslope*log(200))+ruint
sa_500 <- (ruslope*log(500))+ruint

sb_1 <- (ruslope*log(pct_1))+ruint
sb_5 <- (ruslope*log(pct_5))+ruint
sb_10 <- (ruslope*log(pct_10))+ruint
sb_25 <- (ruslope*log(pct_25))+ruint
sb_50 <- (ruslope*log(pct_50))+ruint
sb_100 <- (ruslope*log(pct_100))+ruint
sb_200 <- (ruslope*log(pct_200))+ruint
sb_500 <- (ruslope*log(pct_500))+ruint

pct_chg_1 <- (((sa_1-sb_1)/sa_1)*100)
pct_chg_5 <- (((sa_5-sb_5)/sa_5)*100)
pct_chg_10 <- (((sa_10-sb_10)/sa_10)*100)
pct_chg_25 <- (((sa_25-sb_25)/sa_25)*100)
pct_chg_50 <- (((sa_50-sb_50)/sa_50)*100)
pct_chg_100 <- (((sa_100-sb_100)/sa_100)*100)
pct_chg_200 <- (((sa_200-sb_200)/sa_200)*100)
pct_chg_500 <- (((sa_500-sb_500)/sa_500)*100)

pct_chgs = c(pct_chg_1,pct_chg_5,pct_chg_10,pct_chg_25,pct_chg_50,pct_chg_100,pct_chg_200,pct_chg_500)
xvalues = c("1","5","10","25","50","100","200","500")
#value = c("1","2","3","4","5","6","7","8")

slope_table = data.frame(xvalues,pct_chgs)

title_projname <- sampres

#Plot titles
ptitle <- paste("Change in ",biometric_title," at ",pct_chg,"% Flow Reduction","\n", Feature.Name," (",startdate," to ",enddate,")\n",title_projname," grouping",sep="")
xaxis_title <- paste("\n",flow_title,sep="");
yaxis_title <- paste("% Decrease in ", biometric_title,"\n", sep="");

if(flow_title == "Drainage Area (km^2)"){
	yup_lim <- 6
} else {
	yup_lim <- 4
}



plt2 <- ggplot(slope_table, aes(x=reorder(xvalues, -pct_chgs),y=pct_chgs)) + geom_bar(stat = "identity")+
ylim(0,yup_lim)+
ggtitle(ptitle)+
labs(x=xaxis_title,y=yaxis_title)+
theme(axis.text.x = element_text(colour="grey20",size=20,hjust=.5,vjust=.5,face="plain"),
      axis.text.y = element_text(colour="grey20",size=15,hjust=.5,vjust=.5,face="plain"))
}

library(pander);
library(httr);
library(hydroTSM);
save_directory <- "/var/www/html/files/fe/plots"
#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
fxn_locations = '/usr/local/home/git/r-dh-ecohydro/Analysis';
source(paste(fxn_locations,"fn_vahydro-1.0.R", sep = "/"));  
source(paste(fxn_locations,"fn_iha.R", sep = "/"));  
dirname(rstudioapi::getActiveDocumentContext()$path);

elid = 339865; # Frederick Co pump-store
runid = 200;
# get a single variable in a timeseries summarized by day, keyed by thisdate
#elevs <- fn_get_rundata(elid, runid, "impoundment_lake_elev");
#plot(elevs);
# get all data from the run file, keyed by timestamp (at whatever timestep model is run)
dat <- fn_get_runfile(elid, runid);
# Convert to numeric
dat$month <- as.numeric(dat$month);
min(dat$timestamp)
wudat <- window(dat, start = as.Date(paste(min(dat$year),"-10-01",sep='')), end = as.Date(paste(max(dat$year),"-09-30",sep='')));
plot(wudat$impoundment_Qout,ylim=c(0,10))

as.numeric(as.character( wudat$Qreach ))
# For some reason we need to convert these numeric fields to char, then to number
# before sending to zoo since their retrieval is classifying them as factors instead of nums
# now there may be away to get around that but...
flows <- zoo(as.numeric(as.character( wudat$Qintake )), order.by = wudat$thisdate);
fn_iha_7q10(flows)
fn_iha_mlf(flows,8)
g2 <- group2(flows);
# plot monthly 10% flows?
oneyr <- window(wudat, start = as.Date("1998-10-01"), end = as.Date("1999-09-30"));
plot(oneyr$Qintake,ylim=c(0,300));
fdc(oneyr$Qintake, main="Flow Duration", log='', xlab="Flow Exceedence",
    ylab="Q cfs", verbose=FALSE);

# plot drawdown
wudat$impoundment_use_remain_acft <- 3.07 * as.numeric(as.character(wudat$impoundment_use_remain_mg));
wudat$impfull_pct <- 100.0 * as.numeric(as.character(wudat$impoundment_use_remain_acft)) / as.numeric(as.character(wudat$impoundment_max_usable))

par(las=2)
plot(wudat$impfull_pct, ylim = c(0.0, 1.0))
impfull_pct <- zoo(as.numeric(as.character( wudat$impfull_pct )), order.by = wudat$thisdate);
g2_imp <- group2(impfull_pct);
imp_modat <- group1(impfull_pct,'calendar','min')  # IHA function that calculates minimum monthly statistics for our data by water year	 
# Monthly median storage minimums
x <- quantile(imp_modat, 0.5, na.rm = TRUE);
# Median % Full
z <- cbind(
  as.matrix(quantile(imp_modat[,"January"], probs = 0.5, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"February"], probs = 0.5, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"March"], probs = 0.5, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"April"], probs = 0.5, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"May"], probs = 0.5, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"June"], probs = 0.5, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"July"], probs = 0.5, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"August"], probs = 0.5, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"September"], probs = 0.5, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"October"], probs = 0.5, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"November"], probs = 0.5, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"December"], probs = 0.5, na.rm = TRUE)) 
)
# 10th % Full
z <- cbind(
  as.matrix(quantile(imp_modat[,"January"], probs = 0.1, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"February"], probs = 0.1, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"March"], probs = 0.1, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"April"], probs = 0.1, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"May"], probs = 0.1, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"June"], probs = 0.1, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"July"], probs = 0.1, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"August"], probs = 0.1, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"September"], probs = 0.1, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"October"], probs = 0.1, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"November"], probs = 0.1, na.rm = TRUE)), 
  as.matrix(quantile(imp_modat[,"December"], probs = 0.1, na.rm = TRUE)) 
)
barplot(z)
pander(z)
# 10th % Full
z = cbind(
  as.matrix(subset(wudat, month == 1)), 
  as.matrix(subset(wudat, month == 2)), 
  as.matrix(subset(wudat, month == 3)), 
  as.matrix(subset(wudat, month == 4)), 
  as.matrix(subset(wudat, month == 5)), 
  as.matrix(subset(wudat, month == 6)), 
  as.matrix(subset(wudat, month == 7)), 
  as.matrix(subset(wudat, month == 8)), 
  as.matrix(subset(wudat, month == 9)), 
  as.matrix(subset(wudat, month == 10)), 
  as.matrix(subset(wudat, month == 11)), 
  as.matrix(subset(wudat, month == 12))
);

boxplot(
  as.numeric(subset(wudat, month == 1)$"impfull_pct"), 
  as.numeric(subset(wudat, month == 2)$"impfull_pct"), 
  as.numeric(subset(wudat, month == 3)$"impfull_pct") , 
  as.numeric(subset(wudat, month == 4)$"impfull_pct"), 
  as.numeric(subset(wudat, month == 5)$"impfull_pct"), 
  as.numeric(subset(wudat, month == 6)$"impfull_pct"), 
  as.numeric(subset(wudat, month == 7)$"impfull_pct"), 
  as.numeric(subset(wudat, month == 8)$"impfull_pct"), 
  as.numeric(subset(wudat, month == 9)$"impfull_pct"), 
  as.numeric(subset(wudat, month == 10)$"impfull_pct"), 
  as.numeric(subset(wudat, month == 11)$"impfull_pct"), 
  as.numeric(subset(wudat, month == 12)$"impfull_pct")
);

boxplot(
  as.numeric(imp_modat[,"January"]),
  as.numeric(imp_modat[,"February"]),
  as.numeric(imp_modat[,"March"]),
  as.numeric(imp_modat[,"April"]),
  as.numeric(imp_modat[,"May"]),
  as.numeric(imp_modat[,"June"]),
  as.numeric(imp_modat[,"July"]),
  as.numeric(imp_modat[,"August"]),
  as.numeric(imp_modat[,"September"]),
  as.numeric(imp_modat[,"October"]),
  as.numeric(imp_modat[,"November"]),
  as.numeric(imp_modat[,"December"]),
  main=paste(
    "Annual Min. % Full (runid=", runid,
    ") at ", 
    round(mean(as.numeric(wudat$safeyield_mgd)),2), 
    " MGD",sep=""
  ),
  names=molabels
);
round(mean(as.numeric(wudat$impoundment_demand_met_mgd),3));
round(mean(as.numeric(wudat$impoundment_demand),2));

plot(wudat$impoundment_demand,ylim=c(4,10));
lines(wudat$impoundment_demand_met_mgd)

pander(z)
library(pander);
library(httr);
save_directory <- "/var/www/html/files/fe/plots"
#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
fxn_locations = '/usr/local/home/git/r-dh-ecohydro/Analysis';
source(paste(fxn_locations,"fn_vahydro-1.0.R", sep = "/"));  
source(paste(fxn_locations,"fn_iha.R", sep = "/"));  

library(dataRetrieval);

#elid = 339865; # Frederick Co pump-store
#elid = 209793; # Bath Co
elid = 252285; # Sith River Martinsville
runid = 0;
# get a single variable in a timeseries summarized by day, keyed by thisdate
#elevs <- fn_get_rundata(elid, runid, "impoundment_lake_elev");
#plot(elevs);
# get all data from the run file, keyed by timestamp (at whatever timestep model is run)
dat <- fn_get_runfile(elid, runid);
plot(dat$Qout,ylim=c(0,10))

as.numeric(as.character( dat$Qout ))
# For some reason we need to convert these numeric fields to char, then to number
# before sending to zoo since their retrieval is classifying them as factors instead of nums
# now there may be away to get around that but...
flows <- zoo(as.numeric(as.character( dat$Qout )), order.by = dat$thisdate);
fn_iha_7q10(flows)
fn_iha_mlf(flows,8)
g2 <- group2(flows);

# plot drawdown
dat$cons_pct <- as.numeric(as.character(dat$cons_remain_acft)) / as.numeric(as.character(dat$cons_pool_acft))

par(las=2)
plot(dat$cons_pct, ylim = c(0.0, 1.0));

fdc(oneyr$Qintake, main="Flow Duration", log='', xlab="Flow Exceedence",
    ylab="Q cfs", verbose=FALSE);

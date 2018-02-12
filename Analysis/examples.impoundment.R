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
library(hydroTSM);

#elid = 339865; # Frederick Co pump-store
#elid = 209793; # Bath Co
#elid = 252285; # Smith River Martinsville
elid = 339991; 
# Difficult Run - 339991 
# * Pond 2 - 340073 - 
# * Martins Lake - 340114 - 0.5 sqmi
# * Lake Audubon - 340098 - 0.1 / 2.37 sqmi
# ** Lake Thoreau - 340116 - 0.57 - 2.37
# ** NOTE: Audubon & THoreau both report 2.37 sqmi
#    but only sum to 0.67 SQMI, so, -1.6 s- qmi DA
# * Lake Fairfax - 340106 - 2.89 / 4.22 sqmi
runid = 232018;
# get a single variable in a timeseries summarized by day, keyed by thisdate
#elevs <- fn_get_rundata(elid, runid, "impoundment_lake_elev");
#plot(elevs);
# get all data from the run file, keyed by timestamp (at whatever timestep model is run)
dat <- fn_get_runfile(elid, runid);
plot(dat$Qout,ylim=c(0,100))
#fdc(dat$Qout, main="Flow Duration", log='', xlab="Flow Exceedence",
#    ylab="Q cfs", verbose=FALSE);

#as.numeric(as.character( dat$Qout ))
# For some reason we need to convert these numeric fields to char, then to number
# before sending to zoo since their retrieval is classifying them as factors instead of nums
# now there may be away to get around that but...
#flows <- zoo(as.numeric(as.character( dat$Qout )), order.by = dat$thisdate);
#fn_iha_7q10(flows)
#fn_iha_mlf(flows,8)
#g2 <- group2(flows);

# plot drawdown
par(las=2)
plot(dat$impoundment_lake_elev, ylim = c(0.0, 10.0));
plot(dat$impoundment_Qin, ylim = c(0.0, 10.0));
points(dat$impoundment_Qout);

wudat <- window(dat, start = as.Date("1997-04-27"), end = as.Date("1997-05-02"));
plot(wudat$impoundment_Qin,ylim=c(0,3))
points(wudat$impoundment_Qout);


stash <- cbind(as.numeric(dat$impoundment_Qin));
stash <- cbind(stash, as.numeric(dat$impoundment_Qout))
fdc(stash);

#fdc(oneyr$Qintake, main="Flow Duration", log='', xlab="Flow Exceedence",
#    ylab="Q cfs", verbose=FALSE);

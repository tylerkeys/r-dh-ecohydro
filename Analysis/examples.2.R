# Load Libraries
fxn_locations = '/usr/local/home/git/r-dh-ecohydro/Analysis';
source(paste(fxn_locations,"fn_vahydro-1.0.R", sep = "/"));  
source(paste(fxn_locations,"fn_iha.R", sep = "/"));  
elid = 340136;
runid = 997;
# get a single variable in a timeseries summarized by day, keyed by thisdate
#elevs <- fn_get_rundata(elid, runid, "impoundment_Qout");
#plot(elevs);
# get all data from the run file, keyed by timestamp (at whatever timestep model is run)
dat <- fn_get_runfile(elid, runid);
plot(dat$impoundment_Qout,ylim=c(0,50), main = paste("Element ", elid, " with riser diam=", 3))
points(dat$impoundment_Qin)

# Just show a specific design storm
destorm <- window(dat, start = as.Date("1989-05-06"), end = as.Date("1989-05-08"));
plot(destorm$impoundment_Qout,ylim=c(0,60), labels = TRUE, main = paste("Element ", elid, " with riser diam=", 3))
points(destorm$impoundment_Qin);
points(destorm$impoundment_Qout, pch = 10);
lines(destorm$impoundment_lake_elev, lty = 3);
line(destorm$impoundment_Qin);
dstorm = dataframe;
# Create a smaller labeled dataframe to allow easier comparisons of variables of interest
destorm2 <- cbind(destorm$impoundment_Qout,destorm$impoundment_lake_elev, destorm$impoundment_riser_mode, destorm$impoundment_riser_head);
colnames(destorm2) <- c('Qout', 'elev', 'Mode', 'Head');

pander(destorm2)

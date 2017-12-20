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
plot(
  dat$impoundment_Qout,ylim=c(0,75), 
  main = paste("Element ", elid, " with riser diam=", 3)
);
points(dat$impoundment_Qin);
lines(dat$impoundment_Qin);

# Just show a specific design storm 
destorm <- window(
  dat, 
  start = as.POSIXct("1989-05-05 16:00"), 
  end = as.POSIXct("1989-05-07 12:00")
);
par(pch=22, col="red") # plotting symbol and color
plot(
  destorm$impoundment_Qout,ylim=c(0,75), 
  main = paste("Flow Alteration from Riser Structure in Small Impoundment"
  )
);
par(pch=22, col="blue") # plotting symbol and color 
points(destorm$impoundment_Qin);
lines(destorm$impoundment_Qin);

#points(destorm$impoundment_Storage, pch = 10);
legend(
  "topright", 
  c("Qout","Qin")
);

# Format for tabular output
destorm$impoundment_Qout <- round(as.numeric(destorm$impoundment_Qout,1));
destorm$impoundment_lake_elev <- round(as.numeric(destorm$impoundment_lake_elev),1);
destorm$impoundment_Storage <- round(as.numeric(destorm$impoundment_Storage),1);
destorm$deltaS <- round(((3.07 / 1.547) / 24) * (as.numeric(destorm$impoundment_Qin) - as.numeric(destorm$impoundment_Qout)),1);
# Create a smaller labeled dataframe to allow easier comparisons of variables of interest
destorm2 <- cbind(destorm$impoundment_Qout, 
                  destorm$impoundment_lake_elev, 
                  destorm$impoundment_riser_mode, 
                  destorm$impoundment_Storage, 
                  destorm$deltaS
);
colnames(destorm2) <- c('Qout (cfs)', 'Elev (ft)', 'Mode', 'S (ac-ft)', 'dS');


pander(destorm2)

destormFP <- cbind(destorm$local_channel_Qin, 
                  destorm$local_channel_Rin, 
                  destorm$local_channel_Qout
);
colnames(destormFP) <- c('Qin (cfs)','Rin (cfs)',  'Qout (cfs)');
pander(destormFP);

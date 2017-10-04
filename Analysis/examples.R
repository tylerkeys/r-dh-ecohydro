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

library(dataRetrieval)
vignette("dataRetrieval",package = "dataRetrieval")
# 3. Import data, select site, code, start/end dates
# example for the Dan River at Paces, VA
siteNo <- "02075500"
pCode <- "00060"
start.date <- "1980-01-01"
end.date <- "1999-09-30"
yahara <- readNWISdv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

# names with codes
names(yahara)
# cleans up names
yahara <- renameNWISColumns(yahara)
# make date posix
#datv$thisdate <- as.POSIXct(datv$thisdate)
flows <- zoo(yahara[,"Flow"], order.by = yahara$Date);
fn_iha_7q10(flows)
fn_iha_mlf(flows,8)

head(yahara)
summary(yahara)

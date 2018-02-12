#This script will combine elf_retrieve_data and elf_user_inputs into a single function
#This script is incapable of batch processing as is

library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);

fxn_locations <- "C:\\Users\\Elaina\\Documents\\HARP\\github\\r-dh-ecohydro\\ELFGEN\\external\\" 

inputs <- list(
  #Save inputs
  x_metric = 'nhdp_drainge_sqkm',
  y_metric = 'aqbio_nt_total',
  target_hydrocode = 'nhd_huc8_03010101',
  xaxis_thresh = 15000,
  #sampres <-,
  analysis_timespan = 'full', #specify years of interest
  station_agg = 'MAX',
  pct_chg = 10,
  quantile = 0.80,
  glo = 100, #PWIT Breakpoint lower guess (sqmi/cfs)
  ghi = 530  #PWIT Breakpoint lower guess (sqmi/cfs)
)

#elf_retrieve_data <- function(inputs = list()){
  
  #must set equal to "YES" to run the method
  quantreg = "NO"
  qr_plot = "NO"
  pw_it = "YES"  
  pw_it_plot = "YES"
  #ymax = "NO"       
  #twopoint = "NO"   

#load data from external source
data <- read.csv(file = "C:\\Users\\Elaina\\Documents\\HARP\\github\\r-dh-ecohydro\\ELFGEN\\external\\upperroan.csv", header = TRUE)
      
#makes sure all metric values are numeric and not factorial (fixes error with ni, total)
data$metric_value <- as.numeric(data$metric_value)
#Subset by date range 

data$tstime <- as.Date(data$tstime,format="%m/%d/%Y",origin="1970-01-01")

if (inputs$analysis_timespan != 'full') {
  #Need to convert timespan paramteter into startdate and endate format for subsetting data 
  startdate <- paste(unlist(strsplit(inputs$analysis_timespan, "[-]"))[[1]],"-01-01",sep="")
  enddate <- paste(unlist(strsplit(inputs$analysis_timespan, "[-]"))[[2]],"-12-31",sep="")
  print(paste("startdate: ", startdate))
  print(paste("enddate: ", enddate))
  data <- subset(data, tstime > startdate & tstime < enddate)
  startdate <- paste("subset: ",startdate,sep="")
} else {        
  startdate <- paste("full timespan: ",min(data$tstime),sep="") #if plotting for full timespan, display start and end dates above plot
  enddate <- max(data$tstime)   #no dates set with REST, only "full" for analysis_timespan propcode
}


#ADD COLUMN OF RATIO OF DRAINAGE AREA TO MEAN FLOW 
data["ratio"] <- (data$drainage_area)/(data$qmean_annual)
#REMOVE ALL STATIONS WHERE THE RATIO OF DA:Q IS GREATER THAN 1000
data<-data[!(data$ratio > 1000),]

#USE ONLY MAX NT VALUE FOR EACH STATION
if(inputs$station_agg == "max"){ 
  aa <- data[order(data$hydrocode, data$metric_value, decreasing=TRUE),]
  aa <- aa[!duplicated(aa$hydrocode),]
  aa <- aa[order(aa$hydrocode, aa$metric_value),]
  data <- aa
}

#subsets data to exclude anything with a flowmetric value greater than 
# the "xaxis_thresh" specified in the user inputs file
data <- subset(data, attribute_value >= .001 & attribute_value < inputs$xaxis_thresh);

print(paste("Found ", nrow(data), sep=''));
#If statement needed in case geographic region does not contain more than 3 points
if(nrow(data) <= 3) {
  print(paste("... Skipping (fewer than 3 datapoints available)",sep=''))
  next
} 


#Skip if there is only 1 or 2 unique flow metric values for this watershed (either only a single EDAS station, or multiple with the same flow metric, which would result in a vertical bar of points in the plot)
station_attribute_value <- data$attribute_value
remove_da_duplicates <- unique(station_attribute_value, incomparables = FALSE)
if(length(remove_da_duplicates) == 1 | length(remove_da_duplicates) == 2) {
  print(paste("... Skipping (the points are all organized in 1 or 2 vertical lines in ", Watershed_Hydrocode[i],")", sep=''));
  next 
} #closes bar of points skip if-statement (rare)

#Skip if there is only 1 or 2 unique biometric values for this watershed
station_metric_value <- data$metric_value
remove_metric_duplicates <- unique(station_metric_value, incomparables = FALSE)
if(length(remove_metric_duplicates) == 1 | length(remove_metric_duplicates) == 2) {
  print(paste("... Skipping (the points are all organized in 1 or 2 horizontal lines in ", Watershed_Hydrocode[i],")", sep=''));
  next 
} #closes bar of points skip if-statement (rare)

#---------------------------------------------------------------------     
source(paste(fxn_locations,"elf_qr_calc.R", sep = ""));  #loads elf_qr_calc function
if(quantreg == "YES") {
  print(paste("CALCULATING - method quantreg breakpoint ...",sep="")) 
  qr_calc_out <-  elf_qr_calc (inputs, data, startdate, enddate)
  source(paste(fxn_locations,"elf_qr_plot.R", sep = ""));  #loads elf_qr_plot function
  source(paste(fxn_locations,"elf_pct_chg.R", sep = ""));  #loads elf_pct_chg function
  if(qr_plot == "YES") {
    print(paste("PLOTTING - method quantreg breakpoint ...",sep=""))
    qr_plot <- elf_qr_plot (inputs,qr_calc_out,data,startdate,enddate)
  } #closes quantreg plotting function
} #closes quantreg function

source(paste(fxn_locations,"elf_pw_it_calc.R", sep = ""));          #loads ef_pw_it_calc function       
if(pw_it == "YES") {
  print(paste("CALCULATING - method quantreg breakpoint using piecewise function...",sep="")) 
  pw_it_calc_out <- elf_pw_it_calc (inputs, data, startdate, enddate)
  source(paste(fxn_locations,"elf_pw_it_plot.R", sep = ""));#loads elf_qr_plot function
  source(paste(fxn_locations,"elf_pct_chg.R", sep = ""));  #loads elf_pct_chg function
  #do I need to reload this or can I reload this? Should I just load it at the beginning?
  #source(paste(fxn_locations,"elf_pct_chg.R", sep = ""));  #loads elf_pct_chg function
  if(pw_it_plot == "YES") {
    print(paste("PLOTTING - method quantreg breakpoint using piecewise function...",sep=""))
    pw_it_plot <- elf_pw_it_plot (inputs,pw_it_calc_out,data,startdate,enddate)
  } #closes quantreg plotting function
} #closes quantreg function


#Load Functions               
#source(paste(fxn_locations,"elf_ymax.R", sep = ""));           #loads elf_ymax function

#source(paste(fxn_locations,"elf_twopoint.R", sep = ""));       #loads elf_twopoint function


#if(ymax == "YES") {print(paste("PLOTTING - method quantreg breakpoint at y-max...",sep="")) 
 # elf_ymax (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}

#if(twopoint == "YES") {print(paste("PLOTTING - method two-point function...",sep=""))
 # elf_twopoint (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}


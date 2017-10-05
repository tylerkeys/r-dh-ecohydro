library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);

elf_retrieve_data <- function(inputs = list()){

  x_metric <- inputs$x_metric 
  #print(x_metric)
  y_metric <- inputs$y_metric 
  ws_ftype <- inputs$ws_ftype
  target_hydrocode <- inputs$target_hydrocode
  offset_x_metric <- inputs$offset_x_metric
  offset_y_metric <- inputs$offset_y_metric
  offset_ws_ftype <- inputs$offset_ws_ftype
  offset_hydrocode <- inputs$offset_hydrocode
  site <- inputs$site
  xaxis_thresh <- inputs$xaxis_thresh
  sampres <- inputs$sampres
 # startdate <- inputs$startdate
 # enddate <- inputs$enddate
  
  analysis_timespan <- inputs$analysis_timespan
  
  station_agg <- inputs$station_agg
  quantreg <- inputs$quantreg 
  ymax <- inputs$ymax   
  pw_it <- inputs$pw_it  
  twopoint <- inputs$twopoint
 # glo <- inputs$glo 
 # ghi <- inputs$ghi
  token <- inputs$token

for (l in offset_ws_ftype:length(ws_ftype)) {
     
  print(paste("ws_ftype ",l,". of ",length(ws_ftype),". ",ws_ftype[l],sep=""))
  
  #Automatic bundle specification (WILL BE ELIMINATED ONCE WE UPDATE VAHYDRO STORAGE SCHEME)
  if(ws_ftype[l] == "hwi_region"){
    bundle <- "ecoregion"
  } else if(ws_ftype[l] == "state") {
    bundle <- "landunit" 
  } else if(ws_ftype[l] == "ecoregion_iii") {
    bundle <- "ecoregion" 
  } else if(ws_ftype[l] == "ecoregion_iv") {
    bundle <- "ecoregion" 
  } else if(ws_ftype[l] == "ecoiii_huc6") {
    bundle <- "ecoregion" 
  } else {
    bundle <- "watershed" 
  }
  
  #Pull in full list of Virginia watersheds for the specified ftype
  #If we define a hydrocode > 'XXXXXX' it will retrieve that single one
  HUClist_url_base <- paste(site,"/?q=export-regions/",bundle, sep = "");
  if (!(target_hydrocode == '')) {
    HUClist_url_full <- paste(HUClist_url_base, ws_ftype[l], target_hydrocode, sep = "/");
  } else {
    HUClist_url_full <- paste(HUClist_url_base, ws_ftype[l], sep = "/");
  }
  #print(HUClist_url_full)
  HUClist <- read.table(HUClist_url_full,header = TRUE, sep = ",")
  Watershed_Hydrocode <- HUClist$Hydrocode
  Feature.Name <- HUClist$Feature.Name
  Hydroid <- HUClist$HydroID
  
for (k in offset_y_metric:length(y_metric)) {
  print(paste("y_metric ", k, ". of ",length(y_metric),". Beginning loop for ", y_metric[k], sep=''));
  for (j in offset_x_metric:length(x_metric)) {
    print(paste("x_metric ", j, ". of 14. Beginning loop for ", x_metric[j], sep=''));
    for (i in offset_hydrocode:length(Watershed_Hydrocode)) {
      print(paste("Feature ", i, ". of ",length(Watershed_Hydrocode),". Searching for stations from ", Watershed_Hydrocode[i], sep=''));
      search_code <- Watershed_Hydrocode[i];
      Feature.Name_code <- as.character(Feature.Name[i]);
      Hydroid_code <- Hydroid[i];
      ws_ftype_code <- ws_ftype[l]
      x_metric_code <-  x_metric[j];
      y_metric_code <-  y_metric[k];

         
      #note: add a 0 for the HUC6's or else the url doesn't work
      if (ws_ftype_code == 'nhd_huc6') {
        if (length(Watershed_Hydrocode[i]) <= 5) {
          search_code <- paste('0', Watershed_Hydrocode[i], sep='');
        }   
      }
      if (ws_ftype_code == 'nhd_huc10') {
        if (length(Watershed_Hydrocode[i]) < 10) {
          search_code <- paste('0', Watershed_Hydrocode[i], sep='');
        }   
      }
      
      uri <- paste(site,"export_fe_data_stripped",x_metric_code,y_metric_code,bundle,ws_ftype_code,sampres,search_code,sep="/")
      print(paste("Using ", uri, sep=''));
      data <- read.csv(uri, header = TRUE, sep = ",")
      
      #makes sure all metric values are numeric and not factorial (fixes error with ni, total)
      data$metric_value <- as.numeric(data$metric_value)
      #Subset by date range 
      data$tstime <- as.Date(data$tstime,origin="1970-01-01")
      
      #Need to convert timespan paramteter into startdate and endate format for subsetting data 
      startdate <- paste(unlist(strsplit(analysis_timespan, "[-]"))[[1]],"-01-01",sep="")
      enddate <- paste(unlist(strsplit(analysis_timespan, "[-]"))[[2]],"-12-31",sep="")
        print(paste("startdate: ", startdate))
        print(paste("enddate: ", enddate))
      
      data <- subset(data, tstime > startdate & tstime < enddate)

      #ADD COLUMN OF RATIO OF DRAINAGE AREA TO MEAN FLOW 
      data["ratio"] <- (data$drainage_area)/(data$qmean_annual)
      #REMOVE ALL STATIONS WHERE THE RATIO OF DA:Q IS GREATER THAN 1000
      data<-data[!(data$ratio > 1000),]
      
      #write.csv(data, file = "data.csv", row.names = TRUE)
      #POTENTIAL EROM CLEAN-UP METHOD, REMOVE ALL POINTS WITH DA ABOVE 500
      #data <- subset(data,  drainage_area < 500);
      
      #USE ONLY MAX NT VALUE FOR EACH STATION
      if(station_agg == "max"){ 
        aa <- data[order(data$hydrocode, data$metric_value, decreasing=TRUE),]
        aa <- aa[!duplicated(aa$hydrocode),]
        aa <- aa[order(aa$hydrocode, aa$metric_value),]
        data <- aa
      }
      
      #subsets data to exclude anything with a flowmetric value greater than the "xaxis_thresh" specified in the user inputs file
      data <- subset(data, attribute_value >= .001 & attribute_value < xaxis_thresh);
      
      #Export data as spreadsheet
      ##write.table(data, paste(save_directory,"data.tsv",sep=""), sep="\t")
      
      print(paste("Found ", nrow(data), sep=''));
      #If statement needed in case geographic region does not contain more than 3 points
      if(nrow(data) <= 3) {
        print(paste("... Skipping (fewer than 3 datapoints in ", Watershed_Hydrocode[i],")",sep=''))
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
      
      #Load Functions               
      source(paste(fxn_locations,"elf_quantreg.R", sep = ""));       #loads elf_quantreg function
      source(paste(fxn_locations,"elf_ymax.R", sep = ""));           #loads elf_ymax function
      source(paste(fxn_locations,"elf_pw_it.R", sep = ""));          #loads ef_pw_it function
      source(paste(fxn_locations,"elf_twopoint.R", sep = ""));       #loads elf_twopoint function
      source(paste(fxn_locations,"elf_pct_chg.R", sep =""));         #loads percent change barplot function
      source(paste(fxn_locations,"elf_store_data.R", sep = ""));     #loads function used to store ELF stats to VAHydro
      
      if(quantreg == "YES") {print(paste("PLOTTING - method quantreg breakpoint ...",sep="")) 
                            elf_quantreg (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
      if(ymax == "YES") {print(paste("PLOTTING - method quantreg breakpoint at y-max...",sep="")) 
                            elf_ymax (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
      if(pw_it == "YES") {print(paste("PLOTTING - method quantreg breakpoint using piecewise function...",sep="")) 
                            elf_pw_it (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
      if(twopoint == "YES") {print(paste("PLOTTING - method two-point function...",sep=""))
                            elf_twopoint (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
      

        } #closes watershed for loop  
      } #closes x_metric for loop
    } #closes y_metric for loop
  } #closes ws_ftype for loop
} #close function

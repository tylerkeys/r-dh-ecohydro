library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);

save_directory <- "C:\\Users\\Elaina\\Documents\\HARP\\github\\plots"  

#__________________________________________
elf_qr_calc <- function(inputs, data, startdate, enddate){
  
  #Load inputs
  x_metric <- inputs$x_metric 
  y_metric <- inputs$y_metric 
  target_hydrocode <- inputs$target_hydrocode
  xaxis_thresh <- inputs$xaxis_thresh
  analysis_timespan <- inputs$analysis_timespan
  station_agg <- inputs$station_agg
  ghi <- inputs$ghi  
  glo <-inputs$glo   
  pct_chg <-inputs$pct_chg
  quantile <- inputs$quantile
  
  
  full_dataset <- data
  
  if(x_metric=="nhdp_drainage_sqkm") {
  #Convert ghi input from sqmi to sqkm, and remove datapoints greater than the ghi DA threashold
  data<-data[!(data$drainage_area > (ghi * 2.58999)),]
  subset_n <- length(data$metric_value) }
  else #if looking at flow, remove datapoints greater than the ghi flow threshold
    data<-data[!(data$attribute_value > ghi),]
  subset_n <- length(data$metric_value) }
  
  # do not convert ghi here since we want to store breakpoint as sqmi not sqkm
  stat_quantreg_bkpt <- ghi
  
  #If statement needed in case there are fewer than 4 datapoints to the left of x-axis inflection point, or if there are more than 3 points but all have the same attribute_value
  duplicates <- unique(data$attribute_value, incomparables = FALSE)
  if(nrow(data) && length(duplicates) > 3) {  
    
    up90 <- rq(metric_value ~ log(attribute_value),data = data, tau = quantile) #calculate the quantile regression
    newy <- c(log(data$attribute_value)*coef(up90)[2]+coef(up90)[1])            #find the upper quantile values of y for each value of DA based on the quantile regression
    upper.quant <- subset(data, data$metric_value > newy)                        #create a subset of the data that only includes the stations with NT values higher than the y values just calculated
    
    print(paste("Upper quantile has ", nrow(upper.quant), "values"));
    #If statement needed in case there ae fewer than 4 datapoints in upper quantile of data set
    if (nrow(upper.quant) > 3) {
      
      regupper <- lm(metric_value ~ log(attribute_value),data = upper.quant)  
      ru <- summary(regupper)                                                  #regression for upper quantile
      
      #If statement needed in case slope is "NA"
      if (nrow(ru$coefficients) > 1) {
          ruint = round(ru$coefficients[1,1], digits = 6)                         #intercept 
          ruslope = round(ru$coefficients[2,1], digits = 6)                       #slope of regression
          rurs = round(ru$r.squared, digits = 6)                                  #r squared of upper quantile
          rursadj = round(ru$adj.r.squared, digits = 6)                           #adjusted r squared of upper quantile
          rup = round(ru$coefficients[2,4], digits = 6)                           #p-value of upper quantile
          rucor = round(cor.test(log(upper.quant$attribute_value),upper.quant$metric_value)$estimate, digits = 6) #correlation coefficient of upper quantile
          rucount = length(upper.quant$metric_value)
          regfull = lm(metric_value ~ log(attribute_value),data = data)            
          rf = summary(regfull)                                                   #regression for full dataset
          rfint = round(rf$coefficients[1,2], digits = 6)                         #intercept 
          rfslope = round(rf$coefficients[2,1], digits = 6)                       #slope of regression
          rfrs = round(rf$r.squared, digits = 6)                                  #r squared of full dataset linear regression
          rfp = round(rf$coefficients[2,4], digits = 6)                           #p-value of full dataset
          rfcor = round(cor.test(log(data$attribute_value),data$metric_value)$estimate, digits = 6) #correlation coefficient of full dataset
          rfcount = length(data$metric_value) 
          
        #Set yaxis threshold = to maximum biometric value in database 
        yaxis_thresh <- max(data$metric_value)
        
        stats <- list(
          stat_quantreg_targ = target_hydrocode,
          stat_quantreg_x = x_metric,
          stat_quantreg_y = y_metric,
          analysis_timespan = analysis_timespan,
          station_agg =station_agg,
          stat_quantreg_qu = quantile,
          stat_quantreg_max = yaxis_thresh,
          stat_quantreg_glo = 0,
          stat_quantreg_ghi = ghi,
          stat_quantreg_bkpt = stat_quantreg_bkpt,
          stat_quantreg_n = rucount,
          stat_quantreg_m = ruslope,
          stat_quantreg_b = ruint,
          stat_quantreg_p = rup,
          stat_quantreg_rsq = rurs,
          stat_quantreg_adj_rsq = rursadj,
          stat_quantreg_pc = pct_chg #exports percent change to use in plotting function
        )
        #condenses the informative outputs of the function into a single table that users can export
        stats_tbl <- data.table(matrix(unlist(stats), byrow = TRUE))
        stats_tbl$variables <-names(stats)
        write.csv(stats_tbl, file=paste(save_directory,"/qr_stats_tbl.csv", sep=""));
        
        print("Storing quantile regression.");

        #exports the outputs and the upper quantile subset for plotting
        qr_calc_out <- list(qr_stats_out=stats,qr_upper.quant=upper.quant)
        
      }
    }
  }
}
#closing all the ifs and the function
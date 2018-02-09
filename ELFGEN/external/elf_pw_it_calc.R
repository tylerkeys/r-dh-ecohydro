library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);

#______________________________________________________________________________________________________________

save_directory <- "C:\\Users\\Elaina\\Documents\\HARP\\github\\plots" 

elf_pw_it_calc <- function(inputs, data, startdate, enddate){
  
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
  
  #Statement to convert PWIT breakpoint boundaries for plotting against drainage area [convert cfs (which is roughly equal to mi^2) to km^2]
  # store glo and ghi before converting for use in admincode/properties
  u_input_lo = glo;
  u_input_hi = ghi;
  if(x_metric == "nhdp_drainage_sqkm") {
    glo <- glo * 2.58999
    ghi <- ghi * 2.58999
  }
  
  #must round these boundary values so they fit in admincode 
  glo <- round(glo,digits=0)
  ghi <- round(ghi,digits=0)
  
  #Creates subset of data consisting of only the upper x% of the datapoints
  ##The piecewise function then looks for a breakpoint between the user specified bounding values using only these upper points 
  upper_points <- rq(metric_value ~ log(attribute_value),data = full_dataset, tau = quantile)
  upper_points_newy <- c(log(full_dataset$attribute_value)*coef(upper_points)[2]+coef(upper_points)[1])
  upper_points <- subset(full_dataset, full_dataset$metric_value > upper_points_newy)
  
  x <- upper_points$attribute_value
  y <- upper_points$metric_value
  
  #set initial guess range
  breaks <- x[which(x >= glo & x <= ghi)]
  as.numeric(breaks)
  #print(breaks)
  
  #This is necessary in case no breaks are found
  if(length(breaks) != 0) {
    #Needed in case pwit function only locates a single break in the data    
    if(length(breaks) == 1) {
      breakpt <- breaks
    }else{
      
      #mse <- numeric(length(breaks))
      mse <- as.numeric(length(breaks))
      
      for(n in 1:length(breaks)){
        piecewise1 <- lm(y ~ log(x)*(x < breaks[n]) + log(x)*(x >= breaks[n]))
        mse[n] <- summary(piecewise1)[6]
      }
      mse <- as.numeric(mse)
      #remove any breaks that are NaN
      mse[is.na(mse)] <- 100000
      breakpt <- breaks[which(mse==min(mse))]
      breakpt <- breakpt[1]
    } #end of breaks == 1 loop 
    
    data<-data[!(data$attribute_value >  breakpt),]
    subset_n <- length(data$metric_value)
    
    stat_quantreg_bkpt <-  breakpt
    if(x_metric == "nhdp_drainage_sqkm") {
      # convert the breakpoint found to sqmi from sqkm
      stat_quantreg_bkpt <-  breakpt / 2.58999;
    } else {
      stat_quantreg_bkpt <-  breakpt;
    }
    
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
            stat_quantreg_glo = u_input_lo, #exports the original boundary inputs
            stat_quantreg_ghi = u_input_hi, #exports the original boundary inputs
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
          write.csv(stats_tbl, file=paste(save_directory,"/pw_it_stats_tbl.csv", sep=""));
          
          print("Storing quantile regression.");
          
          #exports the outputs and the upper quantile subset for plotting
          pw_it_calc_out <- list(pw_it_stats_out=stats,pw_it_u.q=upper.quant)
          
        }
      }
    }
  }
}

  


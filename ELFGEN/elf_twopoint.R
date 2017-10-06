library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);

elf_twopoint <- function(inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate){
 
  #Load inputs 
  x_metric <- x_metric_code
  y_metric <- y_metric_code
  Feature.Name <- Feature.Name_code
  Hydroid <- Hydroid_code
  ws_ftype <- ws_ftype_code
  pct_chg <- inputs$pct_chg 
  save_directory <- inputs$save_directory 
  target_hydrocode <- inputs$target_hydrocode
  quantile <- inputs$quantile  
  xaxis_thresh <- inputs$xaxis_thresh
  send_to_rest <- inputs$send_to_rest
  offset <- inputs$offset
  analysis_timespan <- inputs$analysis_timespan
  station_agg <- inputs$station_agg
  site <- inputs$site
  sampres <- inputs$sampres
  
  full_dataset <- data

  x <- data$attribute_value
  y <- data$metric_value
  
  ymax <- max(y) #finds the max y-value
  xmin <- min(x) #finds the minimum x-value
  x.ymax <- subset(data, data$metric_value == ymax) #finds all the x-values associated with that max y-value
  xmin.ymax <- min(x.ymax$attribute_value) #finds the point with the smallest x-value associated with that max y-value
  y.xmin <- subset(data, data$attribute_value == xmin) #finds all the y-values associated with the minimum x-value
  ymax.xmin <- max(y.xmin$metric_value) #finds the point with the highest x-value associated with that min x-value
  
  #This is necessary in case the two points are in the same spot 
  if(xmin != xmin.ymax) {
  
    #creates data frame with the values we just extracted
    twopt <- as.data.frame(matrix(c(xmin,ymax.xmin,xmin.ymax,ymax), ncol=2, byrow = TRUE))  
    
    regline <- lm(V2 ~ log(V1),data = twopt)
    rl <- summary(regline)
    #print (rl)
    ruint <- round(rl$coefficients[1,1], digits = 6) #intercept 
    ruslope <- round(rl$coefficients[2,1], digits = 6) #slope of regression
    rucount <- length(data$metric_value)

      #Set yaxis threshold = to maximum biometric value in database 
      yaxis_thresh <- paste(site,"/femetric-ymax/",y_metric, sep="")
      yaxis_thresh <- read.csv(yaxis_thresh , header = TRUE, sep = ",")
      yaxis_thresh <- yaxis_thresh$metric_value
      print (paste("Setting ymax = ", yaxis_thresh));
      
      #retreive metric varids and varnames
      metric_definitions <- paste(site,"/?q=/fe_metric_export",sep="");
      metric_table <- read.table(metric_definitions,header = TRUE, sep = ",")
      
      biometric_row <- which(metric_table$varkey == y_metric)
      biomeric_name <- metric_table[biometric_row,]
      biometric_title <- biomeric_name$varname                #needed for human-readable plot titles

      flow_row <- which(metric_table$varkey == x_metric)
      flow_name <- metric_table[flow_row,]
      flow_title <- flow_name$varname                         #needed for human-readable plot titles

      #admincode <- paste(Hydroid,"twopoint",x_metric_varid,y_metric_varid,"-9999",statagg,smprs,startdate,enddate, sep='_');
      admincode <-paste(Hydroid,"_fe_twopoint",sep="");
      
      #Store breakpoint in sqmi if plotting for drainage area 
      if (x_metric == 'nhdp_drainage_sqkm') {xmin.ymax <- (xmin.ymax / 2.58999)}
      
      # stash the regression statistics using REST  
      if (send_to_rest == 'YES') {
        
        qd <- list(
          featureid = Hydroid,
          admincode = admincode,
          name = paste( "Two-Point, ", y_metric, ' = f( ', x_metric, ' )', sep=''),
          ftype = 'fe_twopoint',
          site = site,
          x = x_metric,
          y = y_metric,
          stats = list(
            stat_quantreg_m = ruslope,
            stat_quantreg_b = ruint,
            stat_quantreg_n = rucount,
            stat_quantreg_p = "-9999",
            stat_quantreg_rsq = "-9999",
            stat_quantreg_adj_rsq = "-9999",
            stat_quantreg_qu = "-9999",
            stat_quantreg_x = x_metric,
            stat_quantreg_y = y_metric,
            station_agg = station_agg,
            sampres = sampres,
            stat_quantreg_bkpt = xmin.ymax,
            stat_quantreg_glo = 0, #Need to store 0 value in order to query using this property
            stat_quantreg_ghi = 0, #Need to store 0 value in order to query using this property
            analysis_timespan = analysis_timespan
          )
        );
        print("Storing quantile regression.");
        adminid <- elf_store_data(qd, token, inputs, adminid)
      } else {
        adminid <- target_hydrocode #Plot images are stored using watershed hydrocode when NOT performing REST 
      }
      
      #Display only 3 significant digits on plots
      plot_ruslope <- signif(ruslope, digits = 3)
      plot_ruint <- signif(ruint, digits = 3)
      
      #Plot titles
      plot_title <- paste(Feature.Name," (",sampres," grouping)\n",startdate," to ",enddate,"\n\nTwo-Point Model Fit: ",sep=""); #,"\n","\n",search_code,"  (",y_metric,")  vs  (",x_metric,")","\n",sep="");
      xaxis_title <- paste(flow_title,"\n","\n","m: ", plot_ruslope,"    b: ",plot_ruint,"\n    Full Dataset n: ",rucount,sep="");
      yaxis_title <- paste(biometric_title);
      
      print (paste("Plotting ELF"));
      # START - plotting function
      ggplot()+
        ylim(0,yaxis_thresh)+
        geom_point(data = data, aes(x=attribute_value, y=metric_value,colour="blue"))+
        geom_smooth(data = twopt, method = "lm", se = FALSE, aes(x=V1,y=V2,colour="red"))+
        ggtitle(plot_title)+
        theme(plot.title = element_text(size = 12, face = "bold"),axis.text = element_text(colour = "blue"))+
        # theme(plot.title = element_text(size = 12, face = "bold"))+
        labs(x=xaxis_title,y=yaxis_title)+
        scale_x_log10(
          limits = c(0.001,xaxis_thresh),
          breaks = trans_breaks("log10", function(x) {10^x}),
          labels = trans_format("log10", math_format(10^.x))
        ) + 
        annotation_logticks(sides = "b")+
        #Add legend
        scale_color_manual("Legend",values=c("blue","black"),
                           labels=c("Full Dataset                       .","Two-Point Model Fit"))+
        guides(colour = guide_legend(override.aes = list(
          linetype=c(0,1), 
          shape=c(16,NA)),
          label.position = "right")
        ); 
      
      # END plotting function
      filename <- paste(adminid,"elf.png", sep="_")
      ggsave(file=filename, path = save_directory, width=8, height=6)
      
      
      print (paste("Plotting Barplot"));
      print (paste("ELF Slope: ",ruslope,sep="")); 
      print (paste("ELF Y-Intercept: ",ruint,sep="")); 
      if (ruslope >= 0){
        if (ruint >= 0){
          
          #slope barplot  
          pct_inputs<- list(ruslope = ruslope, 
                            ruint = ruint,
                            biometric_title = biometric_title, 
                            flow_title = flow_title,
                            Feature.Name = Feature.Name,
                            pct_chg = pct_chg,
                            sampres = sampres,
                            startdate = startdate,
                            enddate = enddate)
          elf_pct_chg (pct_inputs)
          
          filename <- paste(adminid,"pctchg.png", sep="_")
          ggsave(file=filename, path = save_directory, width=8, height=5)
        } else {
          print (paste("Y-Intercept is negative, not generating barplot"));        
        }  
      } else {
        print (paste("Slope is negative, not generating barplot"));        
      }  
      
  } else { 
    print(paste("... Skipping (xmin == xmin.ymax for ", search_code,")", sep=''));
  }
  
} #close function

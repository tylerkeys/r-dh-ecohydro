library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);

elf_pw_it <- function(inputs = list(glo = 100, ghi = 500), data, x_metric_code, Feature.Name_code, Hydroid_code, search_code, token){

  x_metric <- x_metric_code
  Feature.Name <- Feature.Name_code
  Hydroid <- Hydroid_code
  
  #Load inputs
  pct_chg <- inputs$pct_chg 
  save_directory <- inputs$save_directory 
  y_metric <- inputs$y_metric 
  ws_ftype <- inputs$ws_ftype
  target_hydrocode <- inputs$target_hydrocode
  quantile <- inputs$quantile  
  xaxis_thresh <- inputs$xaxis_thresh
  send_to_rest <- inputs$send_to_rest
  offset <- inputs$offset
  startdate <- inputs$startdate
  enddate <- inputs$enddate
  station_agg <- inputs$station_agg
  site <- inputs$site
  sampres <- inputs$sampres
  glo <- inputs$glo
  ghi <- inputs$ghi
  
        
        dme <- subset(data, attribute_value >= .001 & attribute_value < xaxis_thresh);
        bigger.da <- dme;
        #summary(fit <- lm(dme$metric_value ~ log(dme$attribute_value))) 

        #If statement needed in case there ae fewer than 3 datapoints to the left of x-axis inflection point
        if(nrow(dme) >= 3) {  

        x <- dme$attribute_value
        y <- dme$metric_value

        #set initial guess range
        breaks <- x[which(x >= glo & x <= ghi)]
#        breaks <- x[which(x >= .001 & x <= xaxis_thresh)]
        as.numeric(breaks)
        
      #This is necessary in case no breaks are found
        if(length(breaks) != 0) {
#      breaks <- 10

        mse <- numeric(length(breaks))

        for(n in 1:length(breaks)){
          piecewise1 <- lm(y ~ x*(x < breaks[n]) + x*(x >= breaks[n]))
          mse[n] <- summary(piecewise1)[6]
        }
        mse <- as.numeric(mse)
       
        breakpt <- breaks[which(mse==min(mse))]
      # print(breakpt)
      # print(breakpt[1])
        breakpt <- breakpt[1]
         
        #use breakpt to split data into two parts
        data1 <- subset(dme, attribute_value > 0.001 & attribute_value < breakpt[1])
        data2 <- subset(dme, attribute_value >= breakpt[1])
   
      if(nrow(data1) >= 5) {
      if(nrow(data2) >= 5) {

        #calculate the quantile regression
        #quantile <- .80
        upquant.1 <- rq(metric_value ~ log(attribute_value),data = data1, tau = quantile)
        #find the values of upper quantile values of y for each value of DA based on the quantile regression
        newy.1 <- c(log(data1$attribute_value)*coef(upquant.1)[2]+coef(upquant.1)[1])
        #create a subset of the data that only includes the stations with NT values higher than the y values just calculated
        upper.quant.1 <- subset(data1, data1$metric_value > newy.1)
        
        if(nrow(upper.quant.1) > 6) {

          upquant.2 <- rq(metric_value ~log(attribute_value),data = data2, tau = quantile)
          newy.2 <- c(log(data2$attribute_value)*coef(upquant.2)[2]+coef(upquant.2)[1])
          upper.quant.2 <- subset(data2, data2$metric_value > newy.2)

          if(nrow(upper.quant.2) >= 3) {

          #Regression of uper quantile of LEFT piece 
          regupper <- lm(metric_value ~ log(attribute_value),data = upper.quant.1)
          ru <- summary(regupper)
          #print(ru)
          ruint <- round(ru$coefficients[1,1], digits = 6) #intercept 
          ruslope <- round(ru$coefficients[2,1], digits = 6) #slope of regression
          rurs <- round(ru$r.squared, digits = 6) #r squared of upper quantile
          rursadj <- round(ru$adj.r.squared, digits = 6) #adjusted r squared of upper quantile
          rup <- round(ru$coefficients[2,4], digits = 6) #p-value of upper.quantile
          #rucor <-round(cor.test(log(upper.quant.1$attribute_value),upper.quant.1$metric_value)$estimate, digits = 6) #correlation coefficient of upper quantile
          rucount <- length(upper.quant.1$metric_value)
        
          #regressions of the two piecewise functions
          regfull.1 <- lm(metric_value ~ log(attribute_value),data = data1)
          rf.1 <- summary(regfull.1)
          rfint.1 <- round(rf.1$coefficients[1,1], digits = 6) #intercept 
          rfslope.1 <- round(rf.1$coefficients[2,1], digits = 6) #slope of regression
          rfrs.1 <- round(rf.1$r.squared, digits = 6) #r squared of full dataset linear regression
          rfp.1 <- round(rf.1$coefficients[2,4], digits = 8) #p-value of full dataset
          rfcor.1 <- round(cor.test(log(data1$attribute_value),data1$metric_value)$estimate, digits = 6) #correlation coefficient of full dataset
          rfcount.1 <- length(data1$metric_value)
          
          regfull.2 <- lm(metric_value ~ log(attribute_value),data = data2)
          rf.2 <- summary(regfull.2)
          rfint.2 <- round(rf.2$coefficients[1,1], digits = 6) #intercept 
          rfslope.2 <- round(rf.2$coefficients[2,1], digits = 6) #slope of regression
          rfrs.2 <- round(rf.2$r.squared, digits = 6) #r squared of full dataset linear regression
          rfp.2 <- round(rf.2$coefficients[2,4], digits = 8) #p-value of full dataset
          rfcor.2 <- round(cor.test(log(data2$attribute_value),data2$metric_value)$estimate, digits = 6) #correlation coefficient of full dataset
          rfcount.2 <- length(data2$metric_value)
          
            
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
          y_metric_varid <- biomeric_name$varid                   #needed for admincode
          
          flow_row <- which(metric_table$varkey == x_metric)
          flow_name <- metric_table[flow_row,]
          flow_title <- flow_name$varname                         #needed for human-readable plot titles
          x_metric_varid <- flow_name$varid                       #needed for admincode
            
          #Ensuring uniqueness in submittal admincodes (coding beacuse of limited admincode characters)
          if (station_agg == 'max') {
            statagg <- 1
          } else {
            statagg <- 2
          }
          
          if (sampres == 'species') {
            smprs <- 1
          } else if(sampres == 'maj_fam_gen_spec') {
            smprs <- 2
          } else if(sampres == 'maj_fam_gen') {
            smprs <- 3
          } else if(sampres == 'maj_fam') {
            smprs <- 4
          } else if (sampres == 'maj_spec') {
            smprs <- 5
          }
            
admincode <- paste(Hydroid,"quantreg_pwit",x_metric_varid,y_metric_varid,quantile,statagg,smprs,startdate,enddate, sep='_');
            

#print(paste("ruslope: ",ruslope,sep=""))
#print(paste("ruint: ",ruint,sep=""))
#print(paste("rucount: ",rucount,sep=""))
#print(paste("rup: ",rup,sep=""))
#print(paste("rurs: ",rurs,sep=""))
#print(paste("rursadj: ",rursadj,sep=""))
#print(paste("quantile: ",quantile,sep=""))
#print(paste("x_metric: ",x_metric,sep=""))
#print(paste("y_metric: ",y_metric,sep=""))
#print(paste("station_agg: ",station_agg,sep=""))
#print(paste("sampres: ",sampres,sep=""))
#print(paste("breakpt: ",breakpt,sep=""))

            if (send_to_rest == 'YES') {
            # stash the regression
            qd <- list(
              featureid = Hydroid,
              admincode = admincode,
              name = paste( "Quantile Regression (Piecewise IT), ", y_metric, ' = f( ', x_metric, ' ), upper ',quantile * 100, '%', sep=''),
              ftype = 'fe_quantreg_pwit',
              startdate = startdate,
              enddate = enddate,
	            site = site,
              x = x_metric,
              y = y_metric,
              stats = list(
                stat_quantreg_m = ruslope,
                stat_quantreg_b = ruint,
                stat_quantreg_n = rucount,
                stat_quantreg_p = rup,
                stat_quantreg_rsq = rurs,
                stat_quantreg_adj_rsq = rursadj,
                stat_quantreg_qu = quantile,
                stat_quantreg_x = x_metric,
                stat_quantreg_y = y_metric,
                station_agg =station_agg,
                sampres = sampres,
                stat_quantreg_bkpt = breakpt
              )
            );
            print("Storing quantile regression.");
            qd;
            elf_store_data (qd, token)
            }



            #Display only 3 significant digits on plots
            plot_ruslope <- signif(ruslope, digits = 3)
            plot_ruint <- signif(ruint, digits = 3)
            plot_rurs <- signif(rurs, digits = 3)
            plot_rursadj <- signif(rursadj, digits = 3)
            plot_rup <- signif(rup, digits = 3)

            #Plot titles
            plot_title <- paste(Feature.Name," (",sampres," grouping)\n",startdate," to ",enddate,sep=""); #,"\n","\n",search_code,"  (",y_metric,")  vs  (",x_metric,")","\n",sep="");
            #xaxis_title <- paste(flow_title,"\n","\n","m: ",ruslope,"    b: ",ruint,"    r^2: ",rurs,"    adj r^2: ",rursadj,"    p: ",rup,"    n: ",rucount,sep="");
            xaxis_title <- paste(flow_title,"\n","\n","m: ",plot_ruslope,"    b: ",plot_ruint,"    r^2: ",plot_rurs,"    adj r^2: ",plot_rursadj,"    p: ",plot_rup,"    n: ",rucount,sep="");
            yaxis_title <- paste(biometric_title);
            EDAS_upper_legend <- paste("EDAS Stations (Upper ",((1 - quantile)*100),"%)",sep="");
            Reg_upper_legend <- paste("Regression (Upper ",((1 - quantile)*100),"%)",sep="");       
            Quantile_Legend <- paste(quantile," Quantile (Full Dataset)",sep=""); 
            EDAS_lower_legend <- paste("EDAS Stations (Lower ",(100-((1 - quantile)*100)),"%)",sep="");

           
print (paste("Plotting ELF")); 
          # START - plotting function
          plt <- ggplot(data1, aes(x=attribute_value, y=metric_value))+
              ylim(0,yaxis_thresh)+
                #RIGHT SIDE OF PIECEWISE 
                geom_point(data = data2, show.legend = TRUE, colour = "blue")+
                geom_point(data = upper.quant.2, show.legend = TRUE, colour = "green")+
                geom_quantile(data = data2, show.legend = TRUE,formula = y ~ x, quantiles = quantile, colour = "black")+
                geom_smooth(data = data2, show.legend = TRUE,method = "lm", colour = "red", se = FALSE)+
                geom_smooth(data = upper.quant.2, show.legend = TRUE,method = "lm", formula = y ~ x, colour = "orange", se = FALSE)+
                geom_vline(xintercept = breakpt[1],linetype = "longdash",colour = "black")+
                
                #LEFT SIDE OF PIECEWISE 
                geom_point(data = data1,aes(colour = "blue"))+
                geom_point(data = upper.quant.1, show.legend = TRUE,aes(colour = "black"))+
                stat_smooth(method = "lm",fullrange=FALSE,level = .95, data = upper.quant.1, aes(x=attribute_value,y=metric_value,color = "red")) +
                geom_quantile(data = data1, show.legend = TRUE, formula = y ~ x, quantiles = quantile, aes(colour = "red"))+
                geom_smooth(data = data1, show.legend = TRUE,method = "lm", aes(colour = "yellow"), se = FALSE)+
                geom_smooth(data = upper.quant.1, show.legend = TRUE,method = "lm", formula = y ~ x, aes(colour = "green"), se = FALSE)+
                
                ggtitle(plot_title)+
                theme(plot.title = element_text(size = 12, face = "bold"),axis.text = element_text(colour = "blue"))+
                labs(x=xaxis_title,y=yaxis_title)+
                scale_x_log10(
                  limits = c(0.001,xaxis_thresh),
                  breaks = trans_breaks("log10", function(x) {10^x}),
                  labels = trans_format("log10", math_format(10^.x))) + 
                scale_fill_continuous(guide = "legend")+
                annotation_logticks(sides = "b")+
                theme(legend.key=element_rect(fill='white')) +
                scale_color_manual(
                  "Legend",
                  values=c("green","blue","orange","black","red"),
                  labels=c(EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend,"Regression (Full Dataset)")
                ) + 
                guides(
                  colour = guide_legend(
                    override.aes = list(
                      linetype=c(0,0,1,1,1), 
                      shape=c(16,16,NA,NA,NA)
                    ),
                    label.position = "right"
                  )
                ); 
            
              # END plotting function
              filename <- paste(admincode,"elf.png", sep="_")
              ggsave(file=filename, path = save_directory, width=8, height=5)
print (paste("Plotting Barplot"));
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
              
              filename <- paste(admincode,"barplot.png", sep="_")
              ggsave(file=filename, path = save_directory, width=8, height=5)
              
              
                    } else {
                      print(paste("... Skipping (fewer than 3 datapoints in upper quantile to the right of x-axis breakpoint in ", search_code,")", sep=''));
                    }
              
                  } else {
                    print(paste("... Skipping (fewer than 3 datapoints in upper quantile of ", search_code,")", sep=''));
                  }
              
                } else {
                  print(paste("... Skipping (fewer than 5 datapoints to the right of x-axis inflection point in ", search_code,")", sep=''));
                }
              
              } else {
                print(paste("... Skipping (fewer than 5 datapoints to the left of x-axis inflection point in ", search_code,")", sep=''));
              }
        
              } else {
                print(paste("... Skipping (No breaks are found using this piece-wise method in ", search_code,")", sep=''));
              }
                
            } else {
              print(paste("... Skipping (fewer than 3 datapoints to the left of x-axis threshold in ", search_code,")", sep=''));
            }

} #close function

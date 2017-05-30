library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);

elf_quantreg <- function(inputs, data, x_metric_code, Feature.Name_code, Hydroid_code, search_code, token){

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


  stat_quantreg_bkpt <- 500
  
        dme <- subset(data, attribute_value >= .001 & attribute_value < stat_quantreg_bkpt);
        bigger.da <- dme;
        #summary(fit <- lm(dme$metric_value ~ log(dme$attribute_value))) 
        
        #print(data)
        #print(dme)
        
        #If statement needed in case there are fewer than 3 datapoints to the left of x-axis inflection point
        if(nrow(dme) > 3) {  
          
          up90 <- rq(metric_value ~ log(attribute_value),data = dme, tau = quantile) #calculate the quantile regression
          newy <- c(log(dme$attribute_value)*coef(up90)[2]+coef(up90)[1])            #find the upper quantile values of y for each value of DA based on the quantile regression
          upper.quant <- subset(dme, dme$metric_value > newy)                        #create a subset of the data that only includes the stations with NT values higher than the y values just calculated
          
print(paste("Upper quantile has ", nrow(upper.quant), "values"));
          #If statement needed in case there ae fewer than 3 datapoints in upper quantile of data set
          if (nrow(upper.quant) > 6) {

            regupper <- lm(metric_value ~ log(attribute_value),data = upper.quant)  
            ru <- summary(regupper)                                                  #regression for upper quantile
            print(ru)
            print(ru$coefficients)
            ruint <- round(ru$coefficients[1,1], digits = 6)                         #intercept 
            ruslope <- round(ru$coefficients[2,1], digits = 6)                       #slope of regression
            rurs <- round(ru$r.squared, digits = 6)                                  #r squared of upper quantile
            rursadj <- round(ru$adj.r.squared, digits = 6)                           #adjusted r squared of upper quantile
            rup <- round(ru$coefficients[2,4], digits = 6)                           #p-value of upper quantile
            rucor <-round(cor.test(log(upper.quant$attribute_value),upper.quant$metric_value)$estimate, digits = 6) #correlation coefficient of upper quantile
            rucount <- length(upper.quant$metric_value)
            regfull <- lm(metric_value ~ log(attribute_value),data = dme)            
            rf <- summary(regfull)                                                   #regression for full dataset
            rfint <- round(rf$coefficients[1,2], digits = 6)                         #intercept 
            rfslope <- round(rf$coefficients[2,1], digits = 6)                       #slope of regression
            rfrs <- round(rf$r.squared, digits = 6)                                  #r squared of full dataset linear regression
            rfp <- round(rf$coefficients[2,4], digits = 6)                           #p-value of full dataset
            rfcor <- round(cor.test(log(dme$attribute_value),dme$metric_value)$estimate, digits = 6) #correlation coefficient of full dataset
            rfcount <- length(dme$metric_value) 
            
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

admincode <- paste(Hydroid,"fe_quantreg",x_metric_varid,y_metric_varid,quantile,statagg,smprs,startdate,enddate, sep='_');

        # stash the regression statistics using REST  
           if (send_to_rest == 'YES') {
   
            qd <- list(
              featureid = Hydroid,
              admincode = admincode,
              name = paste( "Quantile Regression, ", y_metric, ' = f( ', x_metric, ' ), upper ',quantile * 100, '%', sep=''),
              ftype = 'fe_quantreg',
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
                stat_quantreg_bkpt = stat_quantreg_bkpt
                
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
            xaxis_title <- paste(flow_title,"\n","\n","m: ",plot_ruslope,"    b: ",plot_ruint,"    r^2: ",plot_rurs,"    adj r^2: ",plot_rursadj,"    p: ",plot_rup,"    n: ",rucount,sep="");
            yaxis_title <- paste(biometric_title);
            EDAS_upper_legend <- paste("EDAS Stations (Upper ",((1 - quantile)*100),"%)",sep="");
            Reg_upper_legend <- paste("Regression (Upper ",((1 - quantile)*100),"%)",sep="");       
            Quantile_Legend <- paste(quantile," Quantile (Full Dataset)",sep=""); 
            EDAS_lower_legend <- paste("EDAS Stations (Lower ",(100-((1 - quantile)*100)),"%)",sep="");

print (paste("Plotting ELF"));
            # START - plotting function
            plt <- ggplot(dme, aes(x=attribute_value,y=metric_value)) + ylim(0,yaxis_thresh) + 
              geom_point(data = bigger.da,aes(colour="blue")) + 
              stat_smooth(method = "lm",fullrange=FALSE,level = .95, data = upper.quant, aes(x=attribute_value,y=metric_value,color = "red")) +
              geom_point(data = upper.quant, aes(x=attribute_value,y=metric_value,color = "black")) + 
              geom_quantile(data = dme, quantiles= quantile,show.legend = TRUE,aes(color="red")) + 
              geom_smooth(data = dme, method="lm",formula=y ~ x,show.legend = TRUE, aes(colour="yellow"),se=FALSE) + 
              geom_smooth(data = upper.quant, formula = y ~ x, method = "lm", show.legend = TRUE, aes(x=attribute_value,y=metric_value,color = "green"),se=FALSE) + 
              ggtitle(plot_title) + 
              theme(
                plot.title = element_text(size = 12, face = "bold"),axis.text = element_text(colour = "blue")
              ) +
              labs(x=xaxis_title,y=yaxis_title) + 
              scale_x_log10(
                limits = c(0.001,15000),
                breaks = trans_breaks("log10", function(x) {10^x}),
                labels = trans_format("log10", math_format(10^.x))
              ) + 
              annotation_logticks(sides = "b")+
              theme(legend.key=element_rect(fill='white')) +
              #Add legend
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
              print(paste("... Skipping (fewer than 6 datapoints in upper quantile of ", search_code,")", sep=''));
          }   
                   
        } else {
          print(paste("... Skipping (fewer than 3 datapoints to the left of x-axis inflection point in ", search_code,")", sep=''));
        }   
        
} #close function

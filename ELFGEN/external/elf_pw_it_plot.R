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
elf_pw_it_plot <- function(inputs,pw_it_calc_out, data, startdate, enddate){
  
  #Load inputs from inputs
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
  
  #Load inputs from pw_it_calc_out
  breakpt <- pw_it_calc_out$pw_it_stats_out$stat_quantreg_bkpt
  rucount <- pw_it_calc_out$pw_it_stats_out$stat_quantreg_n
  yaxis_thresh <- pw_it_calc_out$pw_it_stats_out$stat_quantreg_max
  upper.quant <- pw_it_calc_out$pw_it_u.q
  #Display only 3 significant digits on plots
  plot_ruslope <- signif(pw_it_calc_out$pw_it_stats_out$stat_quantreg_m, digits = 3)
  plot_ruint <- signif(pw_it_calc_out$pw_it_stats_out$stat_quantreg_b, digits = 3)
  plot_rurs <- signif(pw_it_calc_out$pw_it_stats_out$stat_quantreg_rsq, digits = 3)
  plot_rursadj <- signif(pw_it_calc_out$pw_it_stats_out$stat_quantreg_adj_rsq, digits = 3)
  plot_rup <- signif(pw_it_calc_out$pw_it_stats_out$stat_quantreg_p, digits = 3)
  
  full_dataset <- data
  
  #Convert ghi input from sqmi to sqkm, and remove datapoints greater than the ghi DA threashold
  data<-data[!(data$drainage_area > (breakpt * 2.58999)),]
  subset_n <- length(data$metric_value)
  
  
  biometric_title <- y_metric #making the title just the y_metric
  flow_title <- x_metric #making the x-axis title the same name as the x_metric for simplicity
  
  #Plot titles
  plot_title <- paste(target_hydrocode,"\n\n Quantile Regression (Piecewise IT): (breakpoint at DA = ", breakpt, #removed the start and enddates
                      ' sqmi, ', round((breakpt  * 2.58999),digits=0),' sqkm)',
                      sep="");
  xaxis_title <- paste(flow_title,"\n","\n","m: ",plot_ruslope,"    b: ",plot_ruint,"    r^2: ",plot_rurs,"    adj r^2: ",plot_rursadj,"    p: ",plot_rup,"\n","    Upper ",((1 - quantile)*100),"% n: ",rucount,"    Data Subset n: ",subset_n,sep="");
  yaxis_title <- paste(biometric_title);
  EDAS_upper_legend <- paste("Data Subset (Upper ",((1 - quantile)*100),"%)",sep="");
  Reg_upper_legend <- paste("Regression (Upper ",((1 - quantile)*100),"%)",sep="");       
  Quantile_Legend <- paste(quantile," Quantile (Data Subset)",sep=""); 
  EDAS_lower_legend <- paste("Data Subset (Lower ",(100-((1 - quantile)*100)),"%)",sep="");
  
  print (paste("Plotting ELF"));
  # START - plotting function
  plt <- ggplot(data, aes(x=attribute_value,y=metric_value)) + ylim(0,yaxis_thresh) + 
    geom_point(data = full_dataset,aes(colour="aliceblue")) +
    geom_point(data = data,aes(colour="blue")) + 
    stat_smooth(method = "lm",fullrange=FALSE,level = .95, data = upper.quant, aes(x=attribute_value,y=metric_value,color = "red")) +
    geom_point(data = upper.quant, aes(x=attribute_value,y=metric_value,color = "black")) + 
    geom_quantile(data = data, quantiles= quantile,show.legend = TRUE,aes(color="red")) + 
    geom_smooth(data = data, method="lm",formula=y ~ x,show.legend = TRUE, aes(colour="yellow"),se=FALSE) + 
    geom_smooth(data = upper.quant, formula = y ~ x, method = "lm", show.legend = TRUE, aes(x=attribute_value,y=metric_value,color = "green"),se=FALSE) + 
    geom_vline(xintercept = breakpt,linetype = "longdash",colour = "black")+
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
      values=c("gray66","forestgreen","blue","orange","black","red"),
      labels=c("Full Dataset",EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend,"Regression (Data Subset)")
    ) + 
    guides(
      colour = guide_legend(
        override.aes = list(
          linetype=c(0,0,0,1,1,1), 
          shape=c(16,16,16,NA,NA,NA)
        ),
        label.position = "right"
      )
    ); 
  
  # END plotting function
  filename <- paste("example","pw_it.png", sep="_")
  ggsave(file=filename, path = save_directory, width=8, height=6)
  
  
  print (paste("Plotting Barplot"));
  print (paste("ELF Slope: ",plot_ruslope,sep="")); 
  print (paste("ELF Y-Intercept: ",plot_ruint,sep="")); 
  if (plot_ruslope >= 0){
    if (plot_ruint >= 0){
      
      #percent change plot  
      pct_inputs<- list(ruslope = plot_ruslope, 
                        ruint = plot_ruint,
                        biometric_title = biometric_title, 
                        flow_title = flow_title,
                        Feature.Name = target_hydrocode,
                        pct_chg = pct_chg,
                        startdate = startdate,
                        #sampres =  sampres,
                        enddate = enddate)
      elf_pct_chg (pct_inputs)
      
      filename <- paste("pw_it","pctchg.png", sep="_")
      ggsave(file=filename, path = save_directory, width=8, height=5)
    } else {
      print (paste("Y-Intercept is negative, not generating pct_chg plot"));        
    }  
  } else {
    print (paste("Slope is negative, not generating pct_chg plot"));
  }  
} #close function
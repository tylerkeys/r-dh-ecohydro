
#Test of the script I want to insert into the elfplot functions
library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);

DA_Flow <- function(
  inputs, data, x_metric_code, 
  y_metric_code,Feature.Name_code, Hydroid_code, 
  ws_ftype_code, search_code, token, station_agg, 
  startdate, enddate) {
  
  #Load inputs
  x_metric <- x_metric_code
  Feature.Name <- Feature.Name_code
  Hydroid <- Hydroid_code
  ws_ftype <- ws_ftype_code
  save_directory <- inputs$save_directory 
  target_hydrocode <- inputs$target_hydrocode
  xaxis_thresh <- inputs$xaxis_thresh
  station_agg <- inputs$station_agg
  xaxis_title = "Monthly flow" #paste(x_metric," in cfs", sep='')
  #Plot images are stored using watershed hydrocode when NOT performing REST 
  adminid <- paste(search_code,"fe_quantreg",x_metric,"DA",station_agg,"0",ghi, sep='_');
 
  #makes sure all metric values are numeric and not factorial (fixes error with ni, total)
  data$metric_value <- as.numeric(data$metric_value)
  

  plt2 <- ggplot(data, aes(x=data$drainage_area ,y=x_metric))  + 
    geom_point(data = data, aes(colour="blue")) + 
    ggtitle("Drainage Area and Streamflow") + 
    theme(
      plot.title = element_text(size = 12, face = "bold"),axis.text = element_text(colour = "blue")
    ) +
    labs(x= paste("Drainage Area"),y=xaxis_title) + 
    scale_x_log10(
      limits = c(0.001,15000),
      breaks = trans_breaks("log10", function(x) {10^x}),
      labels = trans_format("log10", math_format(10^.x))
    ) + 
    scale_y_log10(
      limits = c(0.001,15000),
      breaks = trans_breaks("log10", function(x) {10^x}),
      labels = trans_format("log10", math_format(10^.x))
    ) + 
    annotation_logticks(sides = "b")+
    theme(legend.key=element_rect(fill='white')) 

  # END plotting function
  filename2 <- paste(adminid,"DA_FlowPlot.png", sep="_")
  ggsave(file=filename2, path = save_directory, width=8, height=6) 
}
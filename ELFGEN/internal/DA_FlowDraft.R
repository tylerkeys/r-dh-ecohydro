
#Draft function that works with one data file
#save a data file for one huc for one month of flow data for example plot
#Jrapp 12/20/2017
#Draft of the DA_flow.R function

save_directory <- "D:\\Jkrstolic\\R\\deqEcoflows\\Breakpoints\\ELFs_PlotsReview\\TestDA\\"
setwd("D:\\Jkrstolic\\R\\deqEcoflows\\Breakpoints\\ELFs_PlotsReview\\TestDA\\")

inputs = read.csv(file="flow_ecology_DATA.csv", header=TRUE) 

data <- inputs
#makes sure all metric values are numeric and not factorial (fixes error with ni, total)
data$metric_value <- as.numeric(data$metric_value)

x_metric <- data$attribute_value
xaxis_thresh = 15000
adminid <- "Huc6_020802_Oct"
xaxis_title = "October monthly flow"


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
filename2 <- paste(adminid,"elf_DA_FlowPlot.png", sep="_")
ggsave(file=paste("DrainageArea", filename2, sep = ""), path = save_directory, width=8, height=6) 


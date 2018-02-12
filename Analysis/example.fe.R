rm(list = ls())  #clear variables
options(timeout=240); # set timeout to twice default level to avoid abort due to high traffic

library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);
#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
#----FOR RUNNING LOCALLY:
source("config.local.private");
source(paste(fxn_vahydro,"rest_functions.R", sep = ""));
source(paste(fxn_locations,"rest.private", sep=""));
source(paste(fxn_locations,"ELFGEN\\internal\\elf_pw_it.R", sep = ""));
mydata <- vahydro_fe_data(
  '030101', "erom_q0001e_mean", "aqbio_nt_total", 
  'watershed',  "nhd_huc6", "species"
);
plot(mydata$drainage_area_sqmi, mydata$qmean_annual );

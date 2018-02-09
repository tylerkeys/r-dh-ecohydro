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

token <- rest_token(site, token, rest_uname, rest_pw);

inputs <- list(
  site = site,
  pct_chg = 10,                             #Percent decrease in flow for barplots (keep at 10 for now)
  save_directory = save_directory, 
  x_metric = 'erom_q0001e_mean', #Flow metric to be plotted on the x-axis
  y_metric = 'aqbio_nt_total',	   #Biometric to be plotted on the y-axis, see "dh variable key" column for options: https://docs.google.com/spreadsheets/d/1PnxY4Rxfk9hkuaXy7tv8yl-mPyJuHdQhrOUWx_E1Y_w/edit#gid=0
  ws_ftype = c('state'),		     #Options: state, hwi_region, nhd_huc8, nhd_huc6, ecoregion_iii, ecoregion_iv, ecoiii_huc6
  target_hydrocode = 'usa_state_virginia',           #Leave blank to process all, individual examples: usa_state_virginia for all of VA, atl_non_coastal_plain_usgs,ohio_river_basin_nhdplus,nhd_huc8_05050001...
  quantile = .80,                  #Specify the quantile to use for quantile regresion plots 
  xaxis_thresh = 15000,            #Leave at 15000 so all plots have idential axis limits 
  analysis_timespan = 'full',      #used to plot for entire timespan 
  send_to_rest = "NO",            #"YES" to push ELF statistic outputs to VAHydro
  station_agg = "max",             #Specify aggregation to only use the "max" NT value for each station or "all" NT values
  sampres = 'species',                  
  glo = 1,  
  ghi = 530,
  dataset_tag = 'ymax75',
  token = token
);

mydata <- vahydro_fe_data(
  '030101', "erom_q0001e_mean", "aqbio_nt_total", 
  'watershed',  "nhd_huc6", "species"
);
inputs$ghi <- max(mydata$attribute_value);

# modify elf_pwit to do analysis and return results (not graph)
elf_pw_it (
  inputs, mydata, inputs$x_metric, 
  inputs$x_metric, inputs$ws_ftype, 
  '020802', '020802', 
  '020802', token, '1900-01-01', '2010-12-31'
);
# add new function
# plot_elf_pwit()
# add new function store_elf_pwit() (if SEND_TO_REST = TRUE)

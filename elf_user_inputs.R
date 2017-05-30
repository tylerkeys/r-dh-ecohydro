rm(list = ls())  #clear variables
#library(httr);   #Load httr package

#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------

#----FOR RUNNING LOCALLY:
fxn_locations <- "C:\\Users\\nrf46657\\Desktop\\FE Batch Processing\\"          #Specify location of supporting function .R files
save_directory <- "C:\\Users\\nrf46657\\Desktop\\FE Batch Processing\\plots"  #Specify location for storing plot images locally

#----FOR RUNNING FROM SERVER:
#fxn_locations <- "/var/www/html/files/fe/elf_codes/fe_master_3.20.2017/"
#save_directory <- "/var/www/html/files/fe/plots"

#------------------------------------------------------------------------------------------------
#User inputs 
inputs <- list(
  site = site,
  offset = 1,                      #Leave at 1 to start from begining of x_metric for-loop
  offset2 =1,                      #Leave at 1 to start from begining of Watershed_Hydrocode for-loop
  pct_chg = 10,                    #Percent decrease in flow for barplots (keep at 10 for now)
  save_directory = save_directory, 
 # x_metric = c('nhdp_drainage_sqkm','erom_q0001e_mean','erom_q0001e_jan','erom_q0001e_feb',
 #               'erom_q0001e_mar','erom_q0001e_apr','erom_q0001e_may',
 #               'erom_q0001e_june','erom_q0001e_july','erom_q0001e_aug',
 #               'erom_q0001e_sept','erom_q0001e_oct','erom_q0001e_nov','erom_q0001e_dec'),		
  x_metric = 'nhdp_drainage_sqkm', #Flow metric to be plotted on the x-axis
  y_metric = 'aqbio_nt_total',	   #Biometric to be plotted on the y-axis, see "dh variable key" column for options: https://docs.google.com/spreadsheets/d/1PnxY4Rxfk9hkuaXy7tv8yl-mPyJuHdQhrOUWx_E1Y_w/edit#gid=0
  ws_ftype = 'hwi_region',		     #Options: state, hwi_region, nhd_huc8, nhd_huc6, ecoregion_iii, ecoregion_iv, ecoiii_huc6
  target_hydrocode = '',           #Leave blank to process all, individual examples: usa_state_virginia for all of VA, atl_non_coastal_plain_usgs,ohio_river_basin_nhdplus,nhd_huc8_05050001...
  quantile = .80,                  #Specify the quantile to use for quantile regresion plots 
  xaxis_thresh = 15000,            #Leave at 15000 so all plots have idential axis limits 
  startdate = '1600-01-01',        #Leave at 1600-01-01 when batch processing to encompass all sample dates (different date-ranges can be used for later analyses)
  enddate = '2100-12-31',          #Leave at 2100-12-31 when batch processing to encompass all sample dates 
  send_to_rest = "YES",             #"YES" to set ELF stats as drupal submittal properties, "NO" otherwise
  station_agg = "max",             #Specify aggregation to only use the "max" NT value for each station or "all" NT values
  sampres = 'species',                  
                                  #--Sample Resolution Grouping Options 
                                   #   species...............Species taxanomic level (Fish metrics only)
                                   #   maj_fam_gen_spec......majority a mix of family/genus/species (Benthics only)
                                   #   maj_fam_gen...........majority a mix of family/genus (Benthics only)
                                   #   maj_fam...............majority family (Benthics only)
                                   #   maj_species..............majority species (Benthics only)
  
  quantreg = "YES",  #Plot using quantile regression method (YES or NO)
  ymax = "YES",      #Plot using breakpoint at x-value corresponding to max y-value (YES or NO)
  pw_it = "YES",     #Plot using breakpoint determined by piecewise iterative function (YES or NO)
  twopoint = "YES"    #Plot using basic two-point ELF method (YES or NO)
) 

#------------------------------------------------------------------------------------------------
#Load Functions               
source(paste(fxn_locations,"elf_retrieve_data.R", sep = ""));  #loads function used to retrieve F:E data from VAHydro

elf_retrieve_data (inputs) 

##############################################################################

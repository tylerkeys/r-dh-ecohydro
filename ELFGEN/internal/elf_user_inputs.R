rm(list = ls())  #clear variables
options(timeout=240); # set timeout to twice default level to avoid abort due to high traffic
 
#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------

#----FOR RUNNING LOCALLY:
#Specify location for storing plot images locally
fxn_locations <- "D:\\Jkrstolic\\R\\deqEcoflows\\GitHub\\r-dh-ecohydro\\ELFGEN\\internal\\"  
save_directory <- "D:\\Jkrstolic\\R\\deqEcoflows\\Breakpoints\\ELFs_PlotsReview\\TestDA\\"

#fxn_locations <- "C:\\Users\\nrf46657\\Desktop\\debugs_12.19.17\\"          #Specify location of supporting function .R files
#save_directory <- "C:\\Users\\nrf46657\\Desktop\\debugs_12.19.17\\plots"    #Specify location for storing plot images locally
#fxn_vahydro <- "C:\\usr\\local\\home\\git\\r-dh-ecohydro\\Analysis\\fn_vahydro-2.0\\"  


#----FOR RUNNING FROM SERVER:
#fxn_locations <- "/var/www/R/r-dh-ecohydro/ELFGEN/"
#save_directory <- "/var/www/html/files/fe/plots"
#fxn_vahydro <- "/var/www/R/r-dh-ecohydro/Analysis/fn_vahydro-2.0/"

#retrieve rest token
source(paste(fxn_locations,"elf_rest_token.R", sep = ""));     #loads function used to generate rest session token
elf_rest_token (site, token)
token <- elf_rest_token(site, token)
print(token)

#------------------------------------------------------------------------------------------------
#User inputs 
inputs <- list(
  site = site,
  offset_x_metric = 1,                      #Leave at 1 to start from begining of x_metric for-loop
  offset_y_metric = 1,                      #Leave at 1 to start from begining of y_metric for-loop
  offset_ws_ftype = 1,                      #Leave at 1 to start from begining of ws_ftype for-loop
  offset_hydrocode = 1,                     #Leave at 1 to start from begining of Watershed_Hydrocode for-loop
  pct_chg = 10,                             #Percent decrease in flow for barplots (keep at 10 for now)
  save_directory = save_directory, 
  not_x_metric = c(
    'nhdp_drainage_sqkm',
    'erom_q0001e_mean',
    'erom_q0001e_jan',
    'erom_q0001e_feb',
    'erom_q0001e_mar',
    'erom_q0001e_apr',
    'erom_q0001e_may',
    'erom_q0001e_june',
    'erom_q0001e_july',
    'erom_q0001e_aug',
    'erom_q0001e_sept',
    'erom_q0001e_oct',
    'erom_q0001e_nov',
    'erom_q0001e_dec'),
 
  x_metric = 'erom_q0001e_mean', #Flow metric to be plotted on the x-axis

  not_y_metric = c(
               'nhdp_drainage_sqkm',
               'aqbio_nt_bival',
               'aqbio_nt_cypr_native'
              ), #this can be used to process by multiple biometrics at once 
  y_metric = 'aqbio_nt_total',	   #Biometric to be plotted on the y-axis, see "dh variable key" column for options: https://docs.google.com/spreadsheets/d/1PnxY4Rxfk9hkuaXy7tv8yl-mPyJuHdQhrOUWx_E1Y_w/edit#gid=0
  not_ws_ftype = c(
    'state',
    'hwi_region',
    'nhd_huc6',
    'nhd_huc8',
    'nhd_huc10',
    'nhd_huc12',
    'ecoregion_iii',
    'ecoregion_iv',
    'ecoiii_huc6'
  ),#this can be used to process by multiple region types at once 

  ws_ftype = c('nhd_huc6'),		     #Options: state, hwi_region, nhd_huc8, nhd_huc6, ecoregion_iii, ecoregion_iv, ecoiii_huc6
  target_hydrocode = '',           #Leave blank to process all, individual examples: usa_state_virginia for all of VA, atl_non_coastal_plain_usgs,ohio_river_basin_nhdplus,nhd_huc8_05050001...
  quantile = .80,                  #Specify the quantile to use for quantile regresion plots 
  xaxis_thresh = 15000,            #Leave at 15000 so all plots have idential axis limits 
  #analysis_timespan = '1990-2000',#used to subset data on date range 
  analysis_timespan = 'full',      #used to plot for entire timespan 

  send_to_rest = "NO",            #"YES" to push ELF statistic outputs to VAHydro
  station_agg = "max",             #Specify aggregation to only use the "max" NT value for each station or "all" NT values
  sampres = 'species',                  
  #sampres = 'maj_fam_gen_spec',                  
                                  #--Sample Resolution Grouping Options 
                                   #   species...............Species taxanomic level (Fish metrics only)
                                   #   maj_fam_gen_spec......majority a mix of family/genus/species (Benthics only)
                                   #   maj_fam_gen...........majority a mix of family/genus (Benthics only)
                                   #   maj_fam...............majority family (Benthics only)
                                   #   maj_species...........majority species (Benthics only)
  
  quantreg = "YES",   #Plot using quantile regression method (YES or NO)
  pw_it = "YES",      #Plot using breakpoint determined by piecewise iterative function (YES or NO)
  ymax = "YES",       #Plot using breakpoint at x-value corresponding to max y-value (YES or NO)
  twopoint = "YES",   #Plot using basic two-point ELF method (YES or NO)
  pw_it_RS = "YES",   #Plot using PWIT *with the regression to the right of the breakpoint included (YES or NO)
  glo = 1,   # PWIT Breakpoint lower guess (sqmi/cfs)
  ghi = 530, # PWIT Breakpoint upper guess (sqmi/cfs) - also used as DA breakpoint for elf_quantreg method
             # ghi values determined from ymax analyses,  q25 = 72 
             #                                            q50 = 205 
             #                                            q75 = 530
  dataset_tag = 'ymax75', # unique indicator of a grouped dataset
  # full_ymax_da75
  # full_ymax_da530
  token = token
) 

#------------------------------------------------------------------------------------------------
#Load Functions               
source(paste(fxn_locations,"elf_retrieve_data.R", sep = ""));  #loads function used to retrieve F:E data from VAHydro
source(paste(fxn_vahydro,"rest_functions.R", sep = "")); 
elf_retrieve_data (inputs) 

##############################################################################

#
#
base_url <- "http://deq1.bse.vt.edu/d.dh/"

#Set location of "fn_dh_elfstats" function
path <- "/Users/nrf46657/Desktop/VAHydro Development/GitHub/r-dh-ecohydro/Analysis/"
source(paste(path,"query_elf_statistics.R", sep = ""))

dataframe <- fn_dh_elfstats(feature_ftype = 'ecoregion_iii', yvar = 'aqbio_benthic_nt_total', sampres = 'maj_fam_gen_spec')
dataframe <- fn_dh_elfstats(feature_ftype = 'ecoregion_iii', stat_quantreg_glo=0, yvar = 'aqbio_nt_total', xvar = 'nhdp_drainage_sqkm', ftype = 'fe_quantreg') 
dataframe$d530 <- dataframe$out_m * log(530.0) + dataframe$out_b
dataframe$d1 <- dataframe$out_m * log(1.0) + dataframe$out_b
dataframe$xatzero <- exp(-1.0 * (dataframe$out_b  / dataframe$out_m))
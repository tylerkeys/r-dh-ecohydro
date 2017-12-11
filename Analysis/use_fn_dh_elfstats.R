#
#
base_url <- "http://deq1.bse.vt.edu/d.dh/"

#Set location of "fn_dh_elfstats" function. Choose one below
path <- "D:/Jkrstolic/R/deqEcoflows/GitHub/r-dh-ecohydro/Analysis/"
#path <- "/Users/nrf46657/Desktop/VAHydro Development/GitHub/r-dh-ecohydro/Analysis/"

#Set location of save path Choose one below
#save_directory <- "C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/stash_plots/"                    
save_directory <- "D:/Jkrstolic/R/deqEcoflows/Breakpoints/"

source(paste(path,"query_elf_statistics.R", sep = ""))

dataframe <- fn_dh_elfstats(feature_ftype = 'ecoregion_iii', yvar = 'aqbio_benthic_nt_total', sampres = 'maj_fam_gen_spec')
dataframe <- fn_dh_elfstats(feature_ftype = 'ecoregion_iii', stat_quantreg_glo=0, yvar = 'aqbio_nt_total', xvar = 'nhdp_drainage_sqkm', ftype = 'fe_quantreg') 

#Benthics options for exporting benthic data from various regions
#dataframe <- fn_dh_elfstats(feature_ftype = 'nhd_huc6', yvar = 'aqbio_benthic_nt_total', sampres = 'maj_fam_gen_spec')
#dataframe <- fn_dh_elfstats(feature_ftype = 'nhd_huc8', yvar = 'aqbio_benthic_nt_total', sampres = 'maj_fam_gen_spec')
#dataframe <- fn_dh_elfstats(feature_ftype = 'ecoregion_iii', yvar = 'aqbio_benthic_nt_total', sampres = 'maj_fam_gen_spec')
#dataframe <- fn_dh_elfstats(feature_ftype = 'hwi_region', yvar = 'aqbio_benthic_nt_total', sampres = 'maj_fam_gen_spec')
#FISH optionos for exporting fish data from various regions
#dataframe <- fn_dh_elfstats(feature_ftype = 'ecoregion_iii', yvar = 'aqbio_nt_total', xvar = 'nhdp_drainage_sqkm') 
#dataframe <- fn_dh_elfstats(feature_ftype = 'ecoregion_iii', yvar = 'aqbio_nt_total', xvar = 'erom_q0001e_mean')

#dataframe <- fn_dh_elfstats(feature_ftype = 'ecoregion_iii', yvar = 'aqbio_nt_total')
#dataframe <- fn_dh_elfstats(feature_ftype = 'nhd_huc6', yvar = 'aqbio_nt_total')
#dataframe <- fn_dh_elfstats(feature_ftype = 'nhd_huc8', yvar = 'aqbio_nt_total')


#commented but not deleted. (JLR)
#dataframe$d530 <- dataframe$out_m * log(530.0) + dataframe$out_b
#dataframe$d1 <- dataframe$out_m * log(1.0) + dataframe$out_b
#dataframe$xatzero <- exp(-1.0 * (dataframe$out_b  / dataframe$out_m))
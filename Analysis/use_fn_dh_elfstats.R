#
#
base_url <- "http://deq1.bse.vt.edu/d.dh/"

#Set location of "fn_dh_elfstats" function
path <- "/Users/nrf46657/Desktop/query_elf_stats/"
source(paste(path,"query_elf_statistics.R", sep = ""))

dataframe <- fn_dh_elfstats(feature_ftype = 'ecoregion_iii', yvar = 'aqbio_benthic_nt_total', sampres = 'maj_fam_gen_spec')
dataframe <- fn_dh_elfstats(feature_ftype = 'ecoregion_iii', yvar = 'aqbio_nt_total', xvar = 'nhdp_drainage_sqkm') 

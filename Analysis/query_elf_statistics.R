#-------------------------------------------------------------------
# Query Input Format: 
# site........................"http://deq1.bse.vt.edu/d.bet"
# ftype.......................fe_quantreg, fe_quantreg_pwit, fe_quantreg_ymax, fe_twopoint
# fstatus.....................active (always set to active for now)
# analysis_timespan...........1990-2000, full (to return all data)
# yvar.............aqbio_nt_total, aqbio_nt_total_benthic...
# sampres.....................species, maj_fam_gen_spec, maj_fam, maj_species
# stat_quantreg_qu............0.80, 0.90...
# station_agg.................max, all
# stat_quantreg_glo...........0, 1, 100...
# stat_quantreg_ghi...........530, 1000...
# feature_ftype...................hwi_region, state, nhd_huc6, nhd_huc8, ecoregion_iii, ecoregion_iv, ecoiii_huc6...
# xvar.............nhdp_drainage_sqkm, erom_q0001e_mean, OR Leave empty to return stats for all x-metrics

# Query Inputs -----------------------------------------------------
fn_dh_elfstats <- function(
  site = "http://deq1.bse.vt.edu/d.dh",
  ftype = "fe_quantreg_pwit",
  fstatus = "active",
  analysis_timespan = "full",
  yvar = "aqbio_nt_total",
  sampres = "species",
  stat_quantreg_qu = 0.80,
  station_agg = "max",
  stat_quantreg_glo = 1,
  stat_quantreg_ghi = 530,
  feature_ftype = "nhd_huc8",
  xvar = "") {
  #------------------------------------------------------------------
  elf_statistics <- paste(site,"export_elf_statistics",ftype,fstatus,analysis_timespan,yvar,sampres,stat_quantreg_qu,station_agg,stat_quantreg_glo,stat_quantreg_ghi,feature_ftype,xvar,sep = "/");
  elf_statistics <- read.table(elf_statistics,header = TRUE, sep = ",");
  return (elf_statistics);
}
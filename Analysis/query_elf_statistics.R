#-------------------------------------------------------------------
# Query Input Format: 
# site........................"http://deq1.bse.vt.edu/d.bet"
# ftype.......................fe_quantreg, fe_quantreg_pwit, fe_quantreg_ymax, fe_twopoint
# fstatus.....................active (always set to active for now)
# analysis_timespan...........1990-2000, full (to return all data)
# stat_quantreg_y.............aqbio_nt_total, aqbio_nt_total_benthic...
# sampres.....................species, maj_fam_gen_spec, maj_fam, maj_species
# stat_quantreg_qu............0.80, 0.90...
# station_agg.................max, all
# stat_quantreg_glo...........0, 1, 100...
# stat_quantreg_ghi...........530, 1000...
# featureid...................hwi_region, state, nhd_huc6, nhd_huc8, ecoregion_iii, ecoregion_iv, ecoiii_huc6...
# stat_quantreg_x.............nhdp_drainage_sqkm, erom_q0001e_mean, OR Leave empty to return stats for all x-metrics

# Query Inputs -----------------------------------------------------
    site <- "http://deq1.bse.vt.edu/d.dh"
    ftype <- "fe_quantreg_pwit"
    fstatus <- "active"
    analysis_timespan <- "full"
    stat_quantreg_y <- "aqbio_nt_total"
    sampres <- "species"
    stat_quantreg_qu <- 0.80
    station_agg <- "max"
    stat_quantreg_glo <- 1
    stat_quantreg_ghi <- 530
    featureid <- "hwi_region"
    stat_quantreg_x <- "erom_q0001e_mean"
#------------------------------------------------------------------
    elf_statistics <- paste(site,"export_elf_statistics",ftype,fstatus,analysis_timespan,stat_quantreg_y,sampres,stat_quantreg_qu,station_agg,stat_quantreg_glo,stat_quantreg_ghi,featureid,stat_quantreg_x,sep = "/");
    elf_statistics <- read.table(elf_statistics,header = TRUE, sep = ",")
    print(head(elf_statistics))

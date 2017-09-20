bundle <- 'watershed';
ftype <- 'nhd_huc8';
metric <- 'aqbio_nt_cent';
selected <- 'all';
quantile <- 0.8;
qftype <- 'fe_quantreg'; # fe_quantreg, fe_quantreg_pwit, fe_quantreg_ymax, fe_twopoint
pmax  <- 0.15;
uri <- paste("http://deq1.bse.vt.edu/d.dh/fe-export-regparms", bundle, ftype, selected, metric, quantile, pmax, qftype, sep='/');
data = read.csv(uri, header = TRUE, sep = "\t");
data.da <- subset(data, x == 'nhdp_drainage_sqkm');
data.mean <- subset(data, x == 'erom_q0001e_mean');
data.jan <- subset(data, x == 'erom_q0001e_jan');
data.feb <- subset(data, x == 'erom_q0001e_feb');
data.mar <- subset(data, x == 'erom_q0001e_mar');
data.apr <- subset(data, x == 'erom_q0001e_apr');
data.may <- subset(data, x == 'erom_q0001e_may');
data.jun <- subset(data, x == 'erom_q0001e_june');
data.jul <- subset(data, x == 'erom_q0001e_july');
data.aug <- subset(data, x == 'erom_q0001e_aug');
data.sep <- subset(data, x == 'erom_q0001e_sept');
data.oct <- subset(data, x == 'erom_q0001e_oct');
data.nov <- subset(data, x == 'erom_q0001e_nov');
data.dec <- subset(data, x == 'erom_q0001e_dec');
n = c('DA', 'Mean', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
title <- paste("R^2 of upper ", 100.*(1.0 - quantile), "% for \n", metric, " = f(DA), f(Qmean), f(Qjan),..., f(Qdec) for ", ftype);
boxplot(
  data.da$Rsq, data.mean$Rsq, data.jan$Rsq, data.feb$Rsq, data.mar$Rsq, 
    data.apr$Rsq, data.may$Rsq, data.jun$Rsq, data.jul$Rsq, data.aug$Rsq, 
    data.sep$Rsq, data.oct$Rsq, data.nov$Rsq, data.dec$Rsq,
  names = n, 
  main = title
);


library(quantreg);

fe_data <- function (
  bundle = 'ecoregion', 
  ftype = 'ecoregion_iii',
  metric = 'aqbio_nt_total',
  selected = 'all',
  quantile = 0.8,
  DA.thresh = 500,
  site = 'd.dh') {
  # load data for select ecoregion
  uri <- paste("http://deq1.bse.vt.edu", site, "multivariate/max", metric, bundle, ftype, selected, sep='/') 
  print(paste('uri: ', uri, sep = '')) 
  data <- read.csv(uri, header = TRUE, sep = ",")
  #makes sure all metric values are numeric and not factorial (fixes error with ni, total)
  data$metric_value <- as.numeric(data$metric_value)
  return(data )
}


bundle <- 'watershed'; # ecoregion, watershed, landunit
ftype <- 'nhd_huc8'; # state, hwi_region, nhd_huc8, nhd_huc6, ecoregion_iii, ecoregion_iv, ecoiii_huc6
metric <- 'aqbio_nt_native';
selected <- 'nhd_huc8_06010205';
quantile <- 0.8;
# breakpoint for DA/Q (~ 500 cfs?)
DA.thresh <- 2852;
site <- 'd.dh';

data <- fe_data(bundle, ftype, metric, selected, quantile, DA.thresh, site);
dme <- subset(data, da_sqkm >= .001 & da_sqkm < DA.thresh);

#calculate the quantile regression
up90 <- rq(metric_value ~ log(annual),data = dme, tau = quantile)
#find the values of upper quantile values of y for each value of DA based on the quantile regression
newy <- c(log(dme$annual)*coef(up90)[2]+coef(up90)[1])
#create a subset of the data that only includes the stations with NT values higher than the y values just calculated
upper.quant <- subset(dme, dme$metric_value > newy)
fit2 <- lm(metric_value ~ log(da_sqkm) + log(annual), data = upper.quant)
summary(fit2);

library(MASS)
fit <- lm(metric_value ~ log(da_sqkm) + log(jan) + log(feb) + 
            log(mar) + log(apr) + log(may)
          + log(jun) + log(jul) + log(aug) 
          + log(sep) + log(oct) + log(nov) + log(dec)
          ,data=upper.quant
)
step <- stepAIC(fit, direction="both")
step$anova # display results
summary(step)

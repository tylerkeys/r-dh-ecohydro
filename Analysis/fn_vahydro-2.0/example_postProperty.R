#-----------------------------------------------------------------------------

base_url <- "http://deq1.bse.vt.edu/d.bet/"

#Set location of "getProperty.R" function and "rest_token.R" function
fxn_locations <- "/Users/nrf46657/Desktop/VAHydro Development/REST_properties/"
source(paste(fxn_locations,"postProperty.R", sep = ""))

#inputs 
inputs <- list(
  varkey = "erom_q0001e_mean",
  featureid = 199720,
  entity_type = "dh_feature",
  proptext = NULL,
  propvalue = 999,
  propcode = NULL,
  startdate = NULL,
  enddate = NULL
)

#property dataframe returned
prop_created <- postProperty(inputs, fxn_locations, base_url, prop)
print(prop_created)
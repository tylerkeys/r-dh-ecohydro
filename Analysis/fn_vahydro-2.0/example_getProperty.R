#-----------------------------------------------------------------------------

base_url <- "http://deq1.bse.vt.edu/d.bet/"

#Set location of "getProperty.R" function and "rest_token.R" function
fxn_locations <- "/Users/nrf46657/Desktop/VAHydro Development/REST_properties/"
source(paste(fxn_locations,"getProperty.R", sep = ""))

#inputs 
varkey <- "erom_q0001e_mean"
featureid <- 199720
entity_type <- "dh_feature"

#property dataframe returned
dataframe <- getProperty(varkey, featureid, entity_type, fxn_locations, base_url, prop)
print(dataframe)
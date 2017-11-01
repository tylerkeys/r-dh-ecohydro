#-----------------------------------------------------------------------------

base_url <- "http://deq1.bse.vt.edu/d.dh/"

#Set location of "getProperty.R" function
path <- "/Users/nrf46657/Desktop/POST/"
source(paste(path,"rest_functions.R", sep = ""))

#User input required to retrieve REST token
token <- rest_token(base_url, token)

#inputs 
inputs <- list(
  varkey = "erom_q0001e_mean",
  featureid = 199720,
  entity_type = "dh_feature"
)

#property dataframe returned
dataframe <- getProperty(inputs, base_url, prop)
print(dataframe)




#-----------------------------------------------------------------------------

base_url <- "http://deq1.bse.vt.edu/d.dh/"

#Set location of "getProperty.R" function
path <- "/usr/local/home/git/r-dh-ecohydro/Analysis/fn_vahydro-2.0/"
source(paste(path,"rest_functions.R", sep = ""))

#User input required to retrieve REST token
token <- rest_token(base_url, token)

#inputs 
xxxinputs <- list(
  varkey = "erom_q0001e_mean",
  featureid = 199720,
  entity_type = "dh_feature"
)
# pid = 3575849
inputs <- list (
  hydrocode = 'vahydrosw_wshed_YP3_6470_6690',
  bundle = 'watershed',
  ftype = 'vahydro'
)
#property dataframe returned
dataframe <- getFeature(inputs, base_url, feature)
print(dataframe)




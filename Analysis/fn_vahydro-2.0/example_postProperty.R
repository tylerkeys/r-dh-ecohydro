#-----------------------------------------------------------------------------

base_url <- "http://deq1.bse.vt.edu/d.dh/"

#Set location of "postProperty.R" function
path <- "/Users/nrf46657/Desktop/VAHydro Development/GitHub/r-dh-ecohydro/Analysis/fn_vahydro-2.0/"
source(paste(path,"rest_functions.R", sep = ""))

#User input required to retrieve REST token
token <- rest_token(base_url, token)

#inputs 
inputs <- list(
  varkey = "erom_q0001e_mean", 
  featureid = 199720,
  entity_type = "dh_feature",
  proptext = NULL,
  propvalue = 10.017,
  propcode = NULL,
  startdate = NULL,
  enddate = NULL
)

#Search for existing property matching supplied *varkey, *featureid, *entity_type 
#--If none exist, proeprty is created 
#--If one exists, property is updated 
#--If more than one exist, execution is halted 
prop_created <- postProperty(inputs,fxn_locations,base_url,prop)
print(prop_created)
#-----------------------------------------------------------------------------

base_url <- "http://deq1.bse.vt.edu/d.bet/"
path <- "C:/usr/local/home/git/r-dh-ecohydro/Analysis/fn_vahydro-2.0/"
#Set location of "getProperty.R" function and "rest_token.R" function
source(paste(path,"rest_functions.R",sep="/"));
# immediately call this
rest_token(base_url, token);
varkey <- "erom_q0001e_mean";
featureid <- 199720;
entity_type <- "dh_feature";

#property dataframe returned
dataframe <- getProperty(varkey, featureid, entity_type, token, base_url, prop);

print(dataframe);


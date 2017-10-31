library(httr);

rest_token <- function(base_url, token){
  
#Cross-site Request Forgery Protection (Token required for POST and PUT operations)
csrf_url <- paste(base_url,"/restws/session/token/",sep="");
# load rest_uname and rest_pw from private file
source("./rest_user.private");
csrf <- GET(url=csrf_url,authenticate(rest_uname,rest_pw));
token <- content(csrf);

} #close function

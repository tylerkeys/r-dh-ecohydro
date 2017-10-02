library(httr);

rest_token <- function(base_url, token){
  
#Cross-site Request Forgery Protection (Token required for POST and PUT operations)
csrf_url <- paste(base_url,"/restws/session/token/",sep="");
csrf <- GET(url=csrf_url,authenticate("tricky_username","tricky_password"));
token <- content(csrf);

} #close function

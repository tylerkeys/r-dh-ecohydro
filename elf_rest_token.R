library(httr);

elf_rest_token <- function(site, token){
  
#Cross-site Request Forgery Protection (Token required for POST and PUT operations)
csrf_url <- paste(site,"/restws/session/token/",sep="");
csrf <- GET(url=csrf_url,authenticate("restws_admin","@dmin123RESTFUL"));
token <- content(csrf);

} #close function
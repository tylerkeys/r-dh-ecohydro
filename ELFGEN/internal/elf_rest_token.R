library(httr);

elf_rest_token <- function(site, token){
  
#Cross-site Request Forgery Protection (Token required for POST and PUT operations)
csrf_url <- paste(site,"/restws/session/token/",sep="");
#source("./rest_user.private");
csrf <- GET(url=csrf_url,authenticate("rest_uname","rest_pw"));
token <- content(csrf);

} #close function

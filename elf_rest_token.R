library(httr);

elf_rest_token <- function(site, token){
  source("./rest_user.private");
  #Cross-site Request Forgery Protection (Token required for POST and PUT operations)
  csrf_url <- paste(site,"/restws/session/token/",sep="");
  csrf <- GET(url=csrf_url,authenticate(rest_uname,rest_pw));
  token <- content(csrf);

} #close function
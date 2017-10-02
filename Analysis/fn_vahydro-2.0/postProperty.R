library(httr);


postProperty <- function(inputs, function_location, base_url, prop){
  
  #retrieve REST token
  source(paste(function_location,"rest_token.R", sep = ""))
  rest_token (base_url, token)
  token <- rest_token(base_url, token)
  
  #Convert varkey to varid - needed for REST operations 
  propdef_url<- paste(base_url,"/?q=vardefs.tsv/",inputs$varkey,sep="")
  propdef_table <- read.table(propdef_url,header = TRUE, sep = "\t")    
  varid <- propdef_table[1][which(propdef_table$varkey == inputs$varkey),]
  #print(paste("varid: ",varid,sep=""))

  pbody = list(
    bundle = 'dh_properties',
    featureid = inputs$featureid,
    varid = varid,
    entity_type = inputs$entity_type,
    proptext = inputs$proptext,
    propvalue = inputs$propvalue,
    propcode = inputs$propcode,
    startdate = inputs$startdate,
    enddate = inputs$enddate
  );
  prop <- POST(paste(base_url,"/dh_properties/",sep=""), 
            add_headers(HTTP_X_CSRF_TOKEN = token),
            body = pbody,
            encode = "json"
  );
  prop <- content(prop);
 # print(prop)
}




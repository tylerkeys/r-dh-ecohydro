library(httr);

rest_token <- function(base_url, token){
  
  #Cross-site Request Forgery Protection (Token required for POST and PUT operations)
  csrf_url <- paste(base_url,"restws/session/token/",sep="");
  if (interactive()) {
    print("Is interactive");
  } else {
    print("Is NOT interactive");
  }
  
  #currently set up to allow infinite login attempts, but this can easily be restricted to a set # of attempts
  token <- c("rest_uname","rest_pw")
  login_attempts <- 1
  while(length(token) == 2  && login_attempts <= Inf){
        print(paste("login attempt #",login_attempts,sep=""))
          
            rest_uname <- readline(prompt="Enter REST user name: ")
            rest_pw <- readline(prompt="Password: ")
            csrf <- GET(url=csrf_url,authenticate(rest_uname,rest_pw));
            token <- content(csrf);
            #print(token)

            if (length(token)==2){
              print("Sorry, unrecognized username or password")
            }
            login_attempts <- login_attempts + 1
  }
  
  if (length(token)==1){
    print("Login attempt successful")
    print(paste("token = ",token,sep=""))
  } else {
    print("Login attempt unsuccessful")
  }
    token <- token
} #close function


getProperty <- function(inputs, base_url, prop){

  #Convert varkey to varid - needed for REST operations 
  propdef_url<- paste(base_url,"/?q=vardefs.tsv/",inputs$varkey,sep="")
  propdef_table <- read.table(propdef_url,header = TRUE, sep = "\t")    
  varid <- propdef_table[1][which(propdef_table$varkey == inputs$varkey),]
  #print(paste("varid: ",varid,sep=""))
  
  pbody = list(
    bundle = 'dh_properties',
    featureid = inputs$featureid,
    varid = varid,
    entity_type = inputs$entity_type
  );
  
  prop <- GET(
    paste(base_url,"/dh_properties.json",sep=""), 
    add_headers(HTTP_X_CSRF_TOKEN = token),
    query = pbody, 
    encode = "json"
  );
  prop_cont <- content(prop);
  
  if (length(prop_cont$list) != 0) {
    print(paste("Number of properties found: ",length(prop_cont$list),sep=""))
    
    prop <- data.frame(proptext=character(),
                       pid=character(),
                       propname=character(), 
                       propvalue=character(),
                       propcode=character(),
                       startdate=character(),
                       enddate=character(),
                       featureid=character(),
                       modified=character(),
                       entity_type=character(),
                       bundle=character(),
                       varid=character(),
                       uid=character(),
                       vid=character(),
                       status=character(),
                       module=character(),
                       stringsAsFactors=FALSE) 
    
    i <- 1
    for (i in 1:length(prop_cont$list)) {
      
      prop_i <- data.frame("proptext" = if (is.null(prop_cont$list[[i]]$proptext)){""} else {prop_cont$list[[i]]$proptext},
                           "pid" = if (is.null(prop_cont$list[[i]]$pid)){""} else {prop_cont$list[[i]]$pid},
                           "propname" = if (is.null(prop_cont$list[[i]]$propname)){""} else {prop_cont$list[[i]]$propname},
                           "propvalue" = if (is.null(prop_cont$list[[i]]$propvalue)){""} else {prop_cont$list[[i]]$propvalue},
                           "propcode" = if (is.null(prop_cont$list[[i]]$propcode)){""} else {prop_cont$list[[i]]$propcode},
                           "startdate" = if (is.null(prop_cont$list[[i]]$startdate)){""} else {prop_cont$list[[i]]$startdate},
                           "enddate" = if (is.null(prop_cont$list[[i]]$enddate)){""} else {prop_cont$list[[i]]$enddate},
                           "featureid" = if (is.null(prop_cont$list[[i]]$featureid)){""} else {prop_cont$list[[i]]$featureid},
                           "modified" = if (is.null(prop_cont$list[[i]]$modified)){""} else {prop_cont$list[[i]]$modified},
                           "entity_type" = if (is.null(prop_cont$list[[i]]$entity_type)){""} else {prop_cont$list[[i]]$entity_type},
                           "bundle" = if (is.null(prop_cont$list[[i]]$bundle)){""} else {prop_cont$list[[i]]$bundle},
                           "varid" = if (is.null(prop_cont$list[[i]]$varid)){""} else {prop_cont$list[[i]]$varid},
                           "uid" = if (is.null(prop_cont$list[[i]]$uid)){""} else {prop_cont$list[[i]]$uid},
                           "vid" = if (is.null(prop_cont$list[[i]]$vid)){""} else {prop_cont$list[[i]]$vid},
                           "status" = if (is.null(prop_cont$list[[i]]$status)){""} else {prop_cont$list[[i]]$status},
                           "module" = if (is.null(prop_cont$list[[i]]$module)){""} else {prop_cont$list[[i]]$module}
      )
      prop  <- rbind(prop, prop_i)
    }
  } else {
    print("This property does not exist")
  }
  prop <- prop
}


postProperty <- function(inputs,fxn_locations,base_url,prop){

  #Search for existing property matching supplied varkey, featureid, entity_type 
  dataframe <- getProperty(inputs, base_url, prop)
  pid <- as.character(dataframe$pid)

  #Convert varkey to varid - needed for REST operations 
  propdef_url<- paste(base_url,"/?q=vardefs.tsv/",inputs$varkey,sep="")
  propdef_table <- read.table(propdef_url,header = TRUE, sep = "\t")    
  varid <- propdef_table[1][which(propdef_table$varkey == inputs$varkey),]
  
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
  
  if (length(dataframe$pid) == 0){
    print("Creating Property...")
    prop <- POST(paste(base_url,"/dh_properties/",sep=""), 
                 add_headers(HTTP_X_CSRF_TOKEN = token),
                 body = pbody,
                 encode = "json"
    );
        if (prop$status == 201){prop <- paste("Status ",prop$status,", Property Created Successfully",sep="")
        } else {prop <- paste("Status ",prop$status,", Error: Property Not Created Successfully",sep="")}
    
  } else if (length(dataframe$pid) == 1){
    print("Single Property Exists, Updating...")
    prop <- PUT(paste(base_url,"/dh_properties/",pid,sep=""), 
                 add_headers(HTTP_X_CSRF_TOKEN = token),
                 body = pbody,
                 encode = "json"
    );
        if (prop$status == 200){prop <- paste("Status ",prop$status,", Property Updated Successfully",sep="")
        } else {prop <- paste("Status ",prop$status,", Error: Property Not Updated Successfully",sep="")}
  } else {
    prop <- print("Multiple Properties Exist, Execution Halted")
  }

}
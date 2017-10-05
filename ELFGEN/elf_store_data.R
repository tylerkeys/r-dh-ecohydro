library(httr)

elf_store_data <- function(qd = list(), token = '', inputs = list(), adminid) {

  if (token == '') {
    x <- list(adminid = FALSE, proplist = list());
    return(x);
  }

  site <- qd$site 
 
  #****************************************************************
  #** Create/Load Submittal Record to attach stats to
  #** search for AdminReg submittal attached to a certain feature
  #** Search on uniqueness (determined by input parameters)
  #** Create submittal if does not exist
  #****************************************************************
  
  elf_submittal_exists <- paste(site,"elf_submittal_exists", 
                                  qd$ftype,
                                  "active",
                                  qd$admincode,
                                  qd$stats$analysis_timespan,
                                  qd$stats$stat_quantreg_x,
                                  qd$stats$stat_quantreg_y,
                                  qd$stats$sampres,
                                  qd$stats$stat_quantreg_qu,
                                  qd$stats$station_agg,
                                  qd$stats$stat_quantreg_glo,
                                  qd$stats$stat_quantreg_ghi,
                                  qd$featureid,
                                sep = "/");
  
  print(paste("Checking submittal by input parameters using: ",elf_submittal_exists, sep=''))
  elf_submittal_exists <- read.table(elf_submittal_exists,header = TRUE, sep = ",")
  adminid <- elf_submittal_exists$s_adminid

  #** Create if does not exist
  if (length(adminid)) {
    print ("Submittal exists");
    print(paste("Submittal adminid = ",adminid,sep=""))
  } else {
    print ("Creating Submittal");
    sq <- POST(
      paste(site,"/dh_adminreg_feature/",sep=""), 
      add_headers(HTTP_X_CSRF_TOKEN = token),
      body = list(
        bundle = 'submittal',
        name = qd$name,
        admincode = qd$admincode,
        ftype = qd$ftype,
        fstatus = 'active',
        dh_link_feature_submittal = list(
          list( 
            'id' = qd$featureid
          )
        )
      ), 
      encode = "json")
    
    sq <- content(sq)
    adminid <- sq$id
    print(paste("New Submittal adminid = ",adminid,sep=""))
  }

  #****************************************************************
  #** Loop through Properties
  #** http://deq1.bse.vt.edu/d.beta/dh-list-variabledefinition/all/quantreg_stats
  #****************************************************************
  propvars <- c(
    'stat_quantreg_m',
    'stat_quantreg_b',
    'stat_quantreg_x',
    'stat_quantreg_y',
    'stat_quantreg_rsq',
    'stat_quantreg_adj_rsq',
    'stat_quantreg_p',
    'stat_quantreg_n',
    'stat_quantreg_qu',
    'station_agg',
    'sampres',
    'stat_quantreg_bkpt',
    'stat_quantreg_glo',
    'stat_quantreg_ghi',
    'analysis_timespan'
  );
  proplist <- list(
    stat_quantreg_m = FALSE,
    stat_quantreg_b = FALSE,
    stat_quantreg_x = FALSE,
    stat_quantreg_y = FALSE,
    stat_quantreg_rsq = FALSE,
    stat_quantreg_adj_rsq = FALSE,
    stat_quantreg_p = FALSE,
    stat_quantreg_n = FALSE,
    stat_quantreg_qu = FALSE,
    station_agg =FALSE,
    sampres = FALSE,
    stat_quantreg_bkpt = FALSE,
    stat_quantreg_glo = FALSE,
    stat_quantreg_ghi = FALSE,
    analysis_timespan = FALSE
  );

#print (propdef_url);
#print(propdef_table);
  for (i in 1:length(propvars)) {
    if(propvars[i] != 'sampres') {  
      propdef_url<- paste(site,"/?q=vardefs.tsv/all/quantreg_stats",sep="");
    } else {
      propdef_url<- paste(site,"/?q=vardefs.tsv/all/aqbio_sampling",sep="");  
    }  
    propdef_table <- read.table(propdef_url,header = TRUE, sep = "\t")    

    varkey <- propvars[i];
    print(varkey); 
    # retrieve varid
    varid <- propdef_table[1][which(propdef_table$varkey == varkey),];
    print(paste("Found varid ", varid));

    #****************************************************************
    #** Retrieve or create properties to attach to the submittal
    # res <- dhCreateProperty(proplist, update = overwrite/append)
    #** POST - create new props
    #** PUT - update existing props

    # format property for POST/PUT
    pf <- list(
      varid = varid,
      propname = varkey,
      propvalue = qd$stats[[varkey]],
      propcode = '',
      featureid = adminid,
      bundle = 'dh_properties',
      entity_type = 'dh_adminreg_feature'
    );
#print(paste("getting pf = ", pf, ''))
    # query first
    sp <- GET(
      paste(site,"/dh_properties.json",sep=""), 
      add_headers(HTTP_X_CSRF_TOKEN = token),
      query = list(
        bundle = 'dh_properties',
        featureid = pf$featureid,
        varid = varid,
        entity_type = 'dh_adminreg_feature'

      ), 
      encode = "json"
    );
    #print(paste("Property Query:",sp,""));
    spc <- content(sp);
    #print(paste("Property Query Result:",spc,""));
    pbody = list(
      bundle = 'dh_properties',
      featureid = pf$featureid,
      varid = pf$varid,
      entity_type = 'dh_adminreg_feature',
      propname = pf$propname,
      propvalue = pf$propvalue,
      propcode = NULL
    );
    if ( (varkey == 'stat_quantreg_x') || (varkey == 'stat_quantreg_y')|| (varkey == 'sampres')|| (varkey == 'station_agg')|| (varkey == 'analysis_timespan') ) {
      pbody$propcode = pf$propvalue;
      pbody$propvalue = NULL;
    }

    if (length(spc$list)) {
      # retrieve submittal
      spe <- spc$list[[1]];
      print ("Property exists - PUT");
      pid <- spe$pid[[1]];
      print(paste("pid: ", pid, "propcode", pbody$propcode, "propvalue", pbody$propvalue));
      #** PUT - Update
      sub <- PUT(paste(site,"/dh_properties/",pid,sep=''), 
        add_headers(HTTP_X_CSRF_TOKEN = token),
        body = pbody, 
        encode = "json"
      );
      #print(paste("PUT result: ", content(sub)));
    } else {
      print ("Property does not exist - POST");
      #** POST - Insert
      x <- POST(paste(site,"/dh_properties/",sep=""), 
        add_headers(HTTP_X_CSRF_TOKEN = token),
        body = pbody,
        encode = "json"
      );
    }
  }
  adminid <- adminid
}

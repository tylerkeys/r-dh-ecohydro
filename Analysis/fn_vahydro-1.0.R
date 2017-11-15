# Automating August Low Flows

library('zoo')
library('IHA')
library(PearsonDS)
options(timeout=240); # set timeout to twice default level to avoid abort due to high traffic


fn_get_rundata <- function(elementid = -1, runid = -1, varname = 'Qout', scenid = 37) {
  if (elementid == -1 ) {
    return(FALSE);
  }
  if (runid == -1 ) {
    return(FALSE);
  }
  # may be obsolete
  #setInternet2(TRUE)
  # Authentication
  hash <- "4291a2a64734da6e0c223a77f4ac5b9f"
  username <- "robertwb"

  # Set up query for batch of model objects
  # Internal variable to construct the query
  urlbase<-"http://deq1.bse.vt.edu/om/remote/get_modelData.php?elementid="
  print(paste("Getting data for run ", runid, " for element ", elementid))      # creates the whole url by pasting the element and run ids into it
  filename<-paste(urlbase, elementid, "&variables=", varname, "&runid=", runid, "&startdate=1984-10-01&enddate=2005-09-30", sep = "")
  print(paste("From ", filename));
  
  dat = try(read.table(filename, header = TRUE, sep = ",")) 
  if (class(dat)=='try-error') { 
    # what to do if file empty 
    print(paste("Error: empty file ", filename))
    return (FALSE);
  } else { 
    #dat<-read.table(filename, header = TRUE, sep = ",")   #  reads the csv-formatted data from the url	
    print(paste("Data obtained, found ", length(dat[,1]), " lines - formatting for IHA analysis"))
    datv<-as.vector(dat)  # stores the data as a vector     
    datv$thisdate <- as.POSIXct(datv$thisdate)
    f3 <- zoo(datv[,paste(varname, runid, sep="_")], order.by = datv$thisdate)
  }
  return(f3);
  
}

fn_get_runfile <- function(elementid = -1, runid = -1, scenid = 37) {
  if (elementid == -1 ) {
    return(FALSE);
  }
  if (runid == -1 ) {
    return(FALSE);
  }
  # may be obsolete
  #setInternet2(TRUE)
  # Authentication
  hash <- "4291a2a64734da6e0c223a77f4ac5b9f"
  username <- "robertwb"

  # just get the run file
  urlbase<-"http://deq1.bse.vt.edu/om/remote/get_modelData.php?operation=11&elementid="
  print(paste("Getting output file for run ", runid, " for element ", elementid))      # creates the whole url by pasting the element and run ids into it
  filename<-paste(urlbase, elementid, "&runid=", runid, "&startdate=1984-10-01&enddate=2005-09-30", sep = "")
  print(paste("From ", filename))
  finfo = try(read.csv(filename, header = TRUE, sep = ",")) ;
  if (class(finfo)=='try-error') { 
    # what to do if file empty 
    print(paste("Error: empty file ", filename))
    return (FALSE);
  }
  filename = as.character(finfo$remote_url);
  if (finfo$compressed == 1) {
    print(paste("Downloading Compressed Run File ", filename));
    download.file(filename,'tempfile',mode="wb");
    filename <-  unzip ('tempfile');
  } else {
    print(paste("Downloading Un-compressed Run File ", filename));
  }
  dat = try(read.table( filename, header = TRUE, sep = ",")) ;
  if (class(dat)=='try-error') { 
    # what to do if file empty 
    print(paste("Error: empty file ", filename))
    return (FALSE);
  } else { 
    #dat<-read.table(filename, header = TRUE, sep = ",")   #  reads the csv-formatted data from the url	
    print(paste("Data obtained, found ", length(dat[,1]), " lines - formatting for IHA analysis"))
    datv<-as.vector(dat)  # stores the data as a vector     
    datv$timestamp <- as.POSIXct(datv$timestamp,origin="1970-01-01")
<<<<<<< HEAD
    #f3 <- zoo(datv, order.by = datv$thisdate)
=======
>>>>>>> 63df34c1d5231b7b8314e5c9599444577af88c91
    f3 <- zoo(datv, order.by = datv$timestamp)
  }
  return(f3);
}

fn_storeprop_vahydro1 = function(){
  # NOT FINISHED - JUST PASTED CODE
  url <- "http://deq1.bse.vt.edu/om/remote/setModelData.php?hash="
  print (paste("Setting 7Q10 for element ", id, " run id ", rid, " to ", x7q10 , sep = "") )
  # building the correct url
  ins_url <- paste(url, hash, "&username=", username, "&elementid=", id, "&runid=", rid, "&dataname=7q10&reporting_frequency=single&dataval=", x7q10, "&starttime=1984-10-01&endtime=2005-09-30&temporal_res=water_year", sep = "")  
  #shell.exec(alf_url)  # opening the webpage
  print(ins_url);
  readLines(ins_url)
}

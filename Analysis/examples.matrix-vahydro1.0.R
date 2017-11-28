library(reshape2)
library('RJSONIO')
library(data.table)
element_id <- '338160' #Aquatic Biota ID of IFIM study reach
# Create URL to read WUA from OM
WUA_url<-paste("http://deq1.bse.vt.edu/om/remote/get_modelData.php?operation=1&variables=wua&elementid=",
               element_id,"&debug=0&view=matrix",sep="") 

# Import data in JSON format
json_data<-fromJSON(WUA_url, handler = NULL,
                    default.size = 100, allowComments = TRUE,data = NULL,
                    maxChar = c(0L, nchar(content)), simplify = Strict,
                    nullValue = NULL, simplifyWithNames = TRUE);

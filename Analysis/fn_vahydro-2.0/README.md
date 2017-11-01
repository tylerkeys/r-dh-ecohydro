functions for vahydro-2.0 entities


1) rest_token()   -function requiring user input to retrieve REST token
2) getProperty()  -function that returns dataframe of properties matching supplied *varkey, *featureid, *entity_type 
3) postProperty() -function that searches for existing properties matching supplied *varkey, *featureid, *entity_type 
                  1. If none exist, proeprty is created 
                  2. If one exists, property is updated 
                  3. If more than one exist, execution is halted

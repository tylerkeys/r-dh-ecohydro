functions for vahydro-2.0 entities


1) rest_token()   -function requiring user input to retrieve REST token
2) getProperty()  -function that returns dataframe of properties matching supplied *varkey, *featureid, *entity_type 
3) postProperty() -function that searches for existing properties matching supplied *varkey, *featureid, *entity_type 
				  --If none exist, proeprty is created 
				  --If one exists, property is updated 
				  --If more than one exist, execution is halted
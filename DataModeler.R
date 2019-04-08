library('revgeo')
library(AotClient)
library('tidyverse')



nodeLocations <- data.frame(ls.nodes(filters=list()))
nodeLocations <- nodeLocations %>% select(location.geometry)
coordinates <- nodeLocations$location.geometry$coordinates

#
# df <- 
#   #revgeo(longitude=-77.0229529, latitude=38.89283435,output='hash', item='zip')
#   revgeo(longitude=--87.627678, latitude=41.878377,output='hash', item='zip')
# df



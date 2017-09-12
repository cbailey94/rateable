rateable <-function(pcd,return='raw'){
  
  #'This function takes a vector of postcodes and returns the rateable values,
  #'split by under/over 51k, within these postcodes.
  #'@param pcd A vector of postcodes (character) of form xxxx xxx/ xxx xxx /...
  #'@param return defaults to 'raw' - returns the raw data for the postcode list, also takes
  #''scatSummary' - provides the sum of rateable values by SCat,
  #''primarySummary' - provides the sum of rateable values by primary code
  #'@keywords chris
  #'@export
  #'@examples
  #'rateable(c('SW15 1SF'),return=scatSummary) 
  #'
  
  library(dplyr)
  library(tidyr)
  
  #read in data 
  if(!exists('base')){
  data(base)
  }
  if(!exists('scat')){
  data(scat)
  }
  if(!exists('primary')){
  data(primary)
  }
  #join to postcode list
  postcode <-data.frame(postcode=pcd,stringsAsFactors = F)
  postcode %>% left_join(base) %>% left_join(scat,by=c('SCAT'='SCat Code')) ->baseRaw
  
  #return raw data if requested
  if(return == 'raw'){
    return(baseRaw)
  }
  
  #summarise by scat if requested
  if(return == 'scatSummary'){
    baseRaw %>% group_by(SCAT,`SCat Description`,category)%>% summarise(SumValue=sum(rateableValue,na.rm=T)) %>% spread(category,SumValue) ->byScat
    return(byScat)
    
  }
  
  #summarise by primary code if requested
  if(return == 'primarySummary'){
    postcode %>% left_join(base) %>% 
      left_join(primary,by=c('primaryAndSecondaryDescriptionCode'='Primary Code')) %>% 
      group_by(primaryAndSecondaryDescriptionCode,`Primary Description`,category) %>% 
      summarise(SumValue=sum(rateableValue,na.rm=T)) %>% spread(category,SumValue)  ->byPrimary
    
    return(byPrimary)
  }
  
  
}


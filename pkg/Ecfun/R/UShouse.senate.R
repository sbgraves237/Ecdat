UShouse.senate <- function(house=readUShouse(), senate=readUSsenate()){
  ##
  ## 0.  RETURN 'NOT AVAILABLE' 
  ##
#  msg <- paste0(
#    'NOTE:  THIS FUNCTION IS CURRENTLY UNAVAILABLE: A CALL TO ', 
#    '"RCurl::getURL" THAT RETURNED INFO ON ALL MEMBERS OF THE ',
#    'US HOUSE IN 2013 AND 2016 ONLY RETURNS 222 CHARACTERS ', 
#    'AS OF 2018-01-11.\n\n', 
#    
#    'If and when a fix for this is found, this function may again:\n\n', 
#    
#    'Read the list of representatives in the United States House of ', 
#    'Representatives.')
#  return(msg)
##
## 1.  reformat house
##
  nh <- nrow(house)
#  cat('hrow(house) = ', nh, '\n')
  Party <- as.character(house$party)
#  Party[Party=='D'] <- 'Democratic'
#  Party[Party=='R'] <- 'Republican'
  np <- length(Party)
  if(np != nh){
      stop('nrow(house) = ', nh, '; length(Party) = ', np,
           ':  NOT equal')
  }
#
  hs <- with(house, data.frame(Chamber=rep('House', nh),
                   State=State, state=toupper(state),
                   district=district, party=Party,
                   surname=surname, givenName=givenName,
#                  nonvoting=nonvoting, 
                   stringsAsFactors=FALSE) )
##
## 2.  reformat senate
##
  ns <- nrow(senate)
#  cat('nrow(senate) =', ns, '\n')
  hS <- rep('Senate', ns)
#
  sn <- with(senate, data.frame(Chamber=hS,
                       State=State, state=state,
                       district=Class, party=party,
                       surname=surname,
                       givenName=givenName,
#                       nonvoting=FALSE,
                       stringsAsFactors=FALSE) )
##
## 3.  rbind
##
  HS <- rbind(hs, sn)
  HS
}


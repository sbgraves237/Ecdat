UShouse.senate <- function(house=readUShouse(), senate=readUSsenate()){
##
## 1.  reformat house
##
  nh <- nrow(house)
#  cat('hrow(house) = ', nh, '\n')
  party <- as.character(house$Party)
  party[party=='D'] <- 'Democratic'
  party[party=='R'] <- 'Republican'
  np <- length(party)
  if(np != nh){
      stop('nrow(house) = ', nh, '; length(party) = ', np,
           ':  NOT equal')
  }
#
  hs <- with(house, data.frame(houseSenate=factor(rep('Rep', nh)),
                   State=State, state=factor(toupper(state)),
                   District=District, Party=factor(party),
                   surname=surname, givenName=givenName,
                   nonvoting=nonvoting, stringsAsFactors=FALSE) )
##
## 2.  reformat senate
##
  ns <- nrow(senate)
#  cat('nrow(senate) =', ns, '\n')
  hS <- rep('Sen', ns)
#
  sn <- with(senate, data.frame(houseSenate=factor(hS),
                       State=State, state=state,
                       District=Class, Party=Party,
                       surname=surname,
                       givenName=givenName,
                       nonvoting=FALSE,
                       stringsAsFactors=FALSE) )
##
## 3.  rbind
##
  rbind(hs, sn)
}


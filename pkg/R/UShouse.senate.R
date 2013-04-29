UShouse.senate <- function(house=readUShouse(), senate=readUSsenate()){
##
## 1.  reformat house
##
  nh <- nrow(house)
#  cat('hrow(house) = ', nh, '\n')
  surnm <- surname(house$Name, TRUE)
  NS <- nrow(surnm)
  if(NS != nh){
      stop('nrow(house) = ', nh, '; nrow(surname(.) = ', NS,
           ':  NOT equal')
  }
  party <- as.character(house$Party)
  party[party=='D'] <- 'Democratic'
  party[party=='R'] <- 'Republican'
  np <- length(party)
  if(np != nh){
      stop('nrow(house) = ', nh, '; length(party) = ', np,
           ':  NOT equal')
  }
  sur. <- surnm[, 'surname']
  nu <- length(sur.)
  if(nu != nh){
      stop('nrow(house) = ', nh, '; length(surnames) = ', sur.,
           ':  NOT equal')
  }
  giv <- surnm[, 'givenName']
  ng <- length(giv)
  if(ng != nh){
      stop('nrow(house) = ', ng, '; length(givenNames) = ', giv,
           ':  NOT equal')
  }
#
  hs <- with(house, data.frame(houseSenate=factor(rep('Rep', nh)),
                   State=State, state=factor(toupper(state)),
                   District=District, Party=factor(party),
                   surname=sur., givenName=giv,
                   nonvoting=nonvoting, stringsAsFactors=FALSE) )
##
## 2.  reformat senate
##
  ns <- nrow(senate)
#  cat('nrow(senate) =', ns, '\n')
  hS <- rep('Sen', ns)
  Surnm <- surname(senate$Name, TRUE)
  nS <- nrow(Surnm)
  if(ns != nS){
      stop('nrow(senate) = ', ns, '; nrow(surname(.)) = ',
           nS, ':  NOT equal')
  }
#
  sn <- with(senate, data.frame(houseSenate=factor(hS),
                       State=State, state=state,
                       District=Class, Party=Party,
                       surname=Surnm[, 'surname'],
                       givenName=Surnm[, 'givenName'],
                       nonvoting=FALSE,
                       stringsAsFactors=FALSE) )
##
## 3.  rbind
##
  rbind(hs, sn)
}


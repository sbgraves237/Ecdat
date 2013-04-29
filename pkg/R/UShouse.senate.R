UShouse.senate <- function(house=readUShouse(), senate=readUSsenate()){
##
## 1.  reformat house
##
  nh <- nrow(house)
  surnm <- surname(house$Name, TRUE)
  hs <- with(house, data.frame(houseSenate=rep('Rep', nh),
                   state=state, District=District,
                   Party=Party, surname=surnm[, "surname"],
                   givenname=surnm[, "givenName"] ) )
##
## 2.  reformat senate
##
  ns <- nrow(senate)

##
## 3.  rbind
##
  "coming soon"
}


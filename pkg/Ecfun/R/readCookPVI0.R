readCookPVI. <- function(url.=
"https://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index",
      UShouse=readUShouse(), USsenate=readUSsenate(), ...){
##
## 1.  readCookPVI()
##
  CookPVI <- readCookPVI(url.)
##
## 2.  merge with UShouse
##
  keyPVI <- with(CookPVI$House, paste(State, District, 
                                      sep='.'))
  houseKey <- with(UShouse, paste(state, district, sep='.'))
  rownames(UShouse) <- houseKey
  House <- cbind(UShouse[keyPVI, ], 
                 CookPVI$House[, c('PVInum', 'PVIchar')])
##
## 3.  merge with USsenate
##
  CookSenate <- CookPVI$Senate
  rownames(CookSenate) <- CookSenate$State
  Senate <- cbind(USsenate, CookSenate[USsenate$State, -1])
  rownames(Senate) <- rownames(USsenate)
##
## 4.  Done
##
  list(House=House, Senate=Senate)
}

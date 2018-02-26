readCookPVI <- function(url.=
"http://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index"){
##
## 1.  download content
##
#  library(RCurl)
  Start <- paste(date(), ': readCookPVI(',
                 url., ')', sep='')
  cat(Start)
  startTime <- proc.time()
  Url. <- try(RCurl::getURL(url., 
                            followlocation = TRUE))
  et <- max(proc.time()-startTime, na.rm=TRUE)
  Read <- paste('|', nchar(Url.), 'bytes read in',
                round(et, 2), 'seconds\n')
  cat(Read)
  if(class(Url.)=='try-error'){
      cat(Url.)
      stop()
  }
##
## 2.  readHTMLTable
##
#  library(XML)
  Wikitbls <- XML::readHTMLTable(Url., 
                      stringsAsFactors=FALSE)
##
## 3.  Find House and Senate tables
##
  len <- sapply(Wikitbls, length)
  Wikitbs <- Wikitbls[len>0]
  ns <- sapply(Wikitbs, dim)
  house <- which((434<ns[1,]) & (ns[1,]<450))
  if((nh <- length(house))!=1){
      stop('URL source changed: Found ', nh,
           ' tables with between 434 and 450 rows.  oops.')
  }
  House <- Wikitbs[[house]]
  district <- sub('st$|nd$|rd|th', '', House$District)
  Dct <- sub('At-large', '0', district)
  House$District <- Dct
#
  senate <- which(ns[1,]==50)
  if((nsen <- length(senate)) != 1){
      stop('URL source changed:  Found ', nsen,
           ' tables with 50 rows.  oops.')
  }
  Senate <- Wikitbs[[senate]]
##
## 4.  Parse PVI columns
##
  pvi <- function(x){
      x. <- strsplit(x, ' !')
      PVIn <- sapply(x., '[', 1)
      PVInum <- as.numeric(PVIn)
      PVIchar <- sapply(x., '[', 2)
      list(PVInum=PVInum, PVIchar=PVIchar)
  }
  sti <- which(tolower(names(House)) == 'state')
#  if(!('state' %in% tolower(names(House)))){
  if(length(sti)<1){
    disti <- which(tolower(names(House)) 
                   == 'district')
    if(length(disti)>0){
#    if('District' %in% names(House)){
      Distr <- House[, disti[1]] 
      H1. <- Distr
      kH1 <- regexpr('!', Distr)
      H1.[kH1>0] <- substring(Distr[kH1>0], 
                1, kH1[kH1>0]-2)
#     On 2018-02-26       
#     the space in "Alabama 1" is NOT a standrd
#     space.  I don't know what it is, but 
#     I need it:  
      Blk.Ala <- substring(Distr[[1]], 8, 8)
      Blk <- paste0(Blk.Ala, '| ')
      H1.. <- strsplit(H1., Blk)
      State. <- sapply(H1.., function(x){
        paste(head(x, -1), collapse=' ')
      })
      State1 <- sub('Nor ', 'North ', State.)
      State <- sub('Sou ', 'South ', State1)
      District <- sapply(H1.., tail, 1)
    } else {
      stop('Neither "State" nor
              names(House)')
    }
  } else {
    State <- House[, sti[1]]
    disti <- which(tolower(names(House)) 
                   == 'district')
    if(length(disti)>0){
      #    if('District' %in% names(House)){
      District <- House[, disti[1]] 
    } else {
      stop('"District" not in names(House)')
    }
  }
#      
  House. <- cbind(data.frame(
    State=State, District=District, 
    stringsAsFactors=FALSE), 
    pvi(House[['PVI']]),
    stringsAsFactors=FALSE)
  ptyi <- which(names(House) ==
      'Party of\nRepresentative')
  if(length(ptyi)<1)
    stop('Party of ... not in names(House)')
  House.$PartyOfRepresentative <- factor(
          House[[ptyi[1]]])
#
  Senate. <- cbind(Senate['State'], 
                   pvi(Senate[['PVI']]),
                   stringsAsFactors=FALSE)
  Senate.$PartyOfGovernor <- factor(
    Senate[['Party of\nGovernor']])
  Senate.$PartyInSenate <- factor(
    Senate[['Party\nin Senate']])
  houseBal <- pvi(Senate[['House\nbalance']])
  Senate.$houseBalanceNum <- houseBal[[1]]/10
  Senate.$houseBalanceChar <- houseBal[[2]]
##
## 8.  Done
##
  list(House=House., Senate=Senate.)
}


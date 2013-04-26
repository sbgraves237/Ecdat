readUShouse <- function(url="http://house.gov/representatives/",
   nonvoting=c('American Samoa', 'District of Columbia',
               'Guam', 'Northern Mariana Islands', 'Puerto Rico',
               'Virgin Islands') ){
##
## 1.  download content
##
  library(RCurl)
  house.gov <- getURL(url)
##
## 2.  find "state"
##
#  st <- gregexpr('state_', house.gov)[[1]] # finds 75
  st <- gregexpr('\t<h2 id=\"', house.gov)[[1]]
#  substring(house.gov, st, st+28)
  st. <- sapply(st, function(x){
      hgi <- substring(house.gov, x)
      x2 <- (x+regexpr('\">', hgi))
  } )
  stCodes <- substring(house.gov, st+9, st.-2)
  st. <- strsplit(stCodes, "_")
  St. <- sapply(st., "[", 2)
##
## 3.  Convert to tables
##
  library(XML)
  House.gov <- readHTMLTable(house.gov, stringsAsFactors=FALSE)
  names(House.gov) <- stCodes
##
## 4.  Rbind tables with the same headers
##
  headers <- lapply(House.gov, names)
  h. <- sapply(headers, paste, collapse=":")
  H. <- table(h.)
  st2 <- St.[1:H.[1]]
#
  ns <- sapply(House.gov, nrow)
  St2 <- rep(st2, ns[1:length(st2)])
#
  nh <- length(H.)
  out <- vector(mode='list', length=nh)
  for(ih in 1:nh){
    sel <- (h.==names(H.)[ih])
    out[[ih]] <- do.call(rbind, House.gov[sel])
  }
  names(out) <- names(H.)
##
## 5.  If 2 tables, combine
##
  if(nh>2){
      warning(nh, " > 2 different tables found;  I'm confused")
      return(out)
  }
  Dist <- out[[2]]$District
  D. <- strsplit(Dist, ' ')
  state <- sapply(D., function(x){
      nx <- length(x)
      Di <- (x[nx]=='District')
      x. <- paste(x[seq(length=nx-Di-1)], collapse=' ')
      x.
  } )
#
  Out <- cbind(data.frame(State=state, state=St2), out[[1]][1:5])
  Out$Party <- factor(Out$Party)
  Out$Committees <- out[[1]][["Committee Assignment"]]
  Out$nonvoting <- (state %in% nonvoting)
#
  Out
}

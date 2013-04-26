readUShouse <- function(url.gov="http://house.gov/representatives/",
            url.wiki="http://en.wikipedia.org/wiki/List_of_current_members_of_the_United_States_House_of_Representatives_by_age"){
    "coming soon"
}
readUShouse.gov <- function(url="http://house.gov/representatives/",
   nonvoting=c('American Samoa', 'District of Columbia',
               'Guam', 'Northern Mariana Islands', 'Puerto Rico',
               'Virgin Islands') ){
##
## 1.  download content
##
  library(RCurl)
  house.gov <- getURL(url)
##
## 2.  Convert to tables
##
  library(XML)
  House.gov <- readHTMLTable(house.gov, stringsAsFactors=FALSE)
##
## 3.  Rbind tables with the same headers
##
  headers <- lapply(House.gov, names)
  h. <- sapply(headers, paste, collapse=":")
  H. <- table(h.)
#
  nh <- length(H.)
  out <- vector(mode='list', length=nh)
  for(ih in 1:nh){
    sel <- (h.==names(H.)[ih])
    out[[ih]] <- do.call(rbind, House.gov[sel])
  }
  names(out) <- names(H.)
##
## 4.  If 2 tables, combine
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
  Out <- cbind(data.frame(State=state), out[[1]][1:5])
  Out$Party <- factor(Out$Party)
  Out$Committees <- out[[1]][["Committee Assignment"]]
  Out$nonvoting <- (state %in% nonvoting)
#
  Out
}
readUShouse.wiki <- function(url="http://en.wikipedia.org/wiki/List_of_current_members_of_the_United_States_House_of_Representatives_by_age",
   nonvoting=c('American Samoa', 'District of Columbia',
               'Guam', 'Northern Mariana Islands', 'Puerto Rico',
               'Virgin Islands') ){
    "coming soon"
}

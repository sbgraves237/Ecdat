readUSsenate <- function(url=
"http://en.wikipedia.org/wiki/List_of_current_United_States_Senators"){
##
## 1.  download content
##
  library(RCurl)
  senate.gov <- getURL(url)
##
## 2.  parse
##
  library(XML)
  senate <- readHTMLTable(senate.gov, stringsAsFactors=FALSE)
#
  len <- sapply(senate, length)
  Senate <- senate[len>0]
  ns <- sapply(Senate, dim)
  ns. <- which(ns[1, ]>=100)
  if(length(ns.)!=1){
      stop('There is not exactly one table with 100 rows in url = ',
           url)
  }
  sen <- Senate[[ns.]]
  dataCols <- sapply(sen, function(x){
      length(unique(x))>1
  } )
  Sen <- sen[dataCols]
  Sen$State <- factor(Sen$State)
  Sen$Party <- factor(Sen$Party)
  nms <- names(Sen)
  Nms <- sub("Prior Experience", "Experience", nms)
  Nms2 <- sub("Assumed Office", "assumedOffice", Nms)
  Nms3 <- sub("Born In", "Born", Nms2)
  Nms4 <- sub("End Office", "endOffice", Nms3)
  names(Sen) <- Nms4
#
  Sen
}


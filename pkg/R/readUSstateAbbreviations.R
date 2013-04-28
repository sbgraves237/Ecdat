readUSstateAbbreviations <- function(url=
"http://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations"){
##
## 1.  download content
##
  library(RCurl)
  abbrev <- getURL(url)
##
## 2.  Find the primary table
##
  library(XML)
  Abbrev <- readHTMLTable(abbrev, stringsAsFactors=FALSE)
#
  len <- sapply(Abbrev, length)
  abbr <- Abbrev[len>0]
  ns <- sapply(abbr, dim)
  ns. <- which(ns[1, ]>=50)
  if(length(ns.)!=1){
      stop('There is not exactly one table with more than 50 rows in url = ',
           url)
  }
  Abbr <- abbr[[ns.]]
##
## 3.  The Wikipedia table has problem headers, so fix ...
##
  colNms <- names(Abbr)
  if(all(substring(colNms, 1, 1) == 'V')){
      colNms. <- make.names(Abbr[1,])
      Abbr <- Abbr[-1,]
      names(Abbr) <- colNms.
  }
##
## 4.  Done
##
  Abbr
}


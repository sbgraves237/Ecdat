readUSsenate <- function(url.=
"https://www.senate.gov/general/contact_information/senators_cfm.xml",
      stateAbbreviations=Ecdat::USstateAbbreviations,
      fixNonStandard=subNonStandardNames, ...){
##
## 1.  download content
##
#  library(RCurl)
  Start <- paste0(date(), ': readUSsenate(', url., ')')
  cat(Start)
  startTime <- proc.time()
#  senate.gov <- try(RCurl::getURL(url.))
  sen_list <- xml2::as_list(xml2::read_xml(url.)) 
  et <- max(proc.time()-startTime, na.rm=TRUE)
#  Read <- paste('|', nchar(senate.gov), 'bytes read in',
#                round(et, 2), 'seconds\n')
#  cat(Read)
#  if(class(senate.gov)=='try-error'){
#      cat(senate.gov)
#      stop()
#  }
##
## 2.  readHTMLTable
##
#  library(XML)
#  senate <- XML::readHTMLTable(senate.gov, stringsAsFactors=FALSE)
##
## 3.  Find table with 100 rows
##
#  len <- sapply(senate, length)
  len <- sapply(sen_list, length)
  senList <- sen_list[len>1]
  len. <- as.integer(len[len>1])
#  Senate <- senate[len>0]
#  ns <- lapply(Senate, dim)
#  NS <- sapply(ns, max)
#  ns. <- which.max(NS)
#  if(NS[ns.]!=100) {
#      warning('Problem with number of senators read at URL = ', url.,
#              ';  max dimension = ', NS[ns.], ' != 100')
#  }
#  ns. <- which(ns[1, ]>=100)
#  if(length(ns.)!=1){
#      stop('There is not exactly one table with 100 rows in url = ',
#           url.)
#  }
#  sen <- Senate[[ns.]]
  Leaders <- which(len.>11)
  colNames <- names(senList[[Leaders[1]]])
  k <- length(colNames)
  N <- length(senList)
  Sen <- matrix('', N, k)
  colnames(Sen) <- colNames
  for(i in 1:N){
    senLi <- unlist(senList[[i]])
    nmsi <- names(senLi)
    Sen[i, nmsi] <- senLi
  }
##
## 5.  parse State
##
  stAb <- stateAbbreviations[stateAbbreviations$USPS!='',]
  USstates <- stAb$USPS
  rownames(stAb) <- USstates
  senState <- stAb[Sen[, 'state'], 'Name']
##
## 6.  assemble
##
  Class. <- sub('Class ', '', Sen[, 'class'])
  Class <- nchar(Class.)
  Name <- paste(Sen[, 'last_name'], Sen[, 'first_name'], sep=', ')
  Senate <- cbind(data.frame(State=factor(senState), 
      state=factor(Sen[, 'state']), Class, Name, 
      party=factor(Sen[, 'party']), stringsAsFactors=FALSE), 
      Sen[, 6:9], surname=Sen[, 'last_name'], 
      givenName=Sen[, 'first_name'], 
      leadership_position=Sen[, 'leadership_position'], 
      stringsAsFactors=FALSE)
  Senate
}


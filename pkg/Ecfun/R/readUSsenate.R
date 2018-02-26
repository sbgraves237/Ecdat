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
  senList0 <- xml2::as_list(xml2::read_xml(url.)) 
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
#  senate <- 
#    XML::readHTMLTable(senate.gov, stringsAsFactors=FALSE)
##
## 3.  Find table with 100 rows
##
#  len <- sapply(senate, length)
  len0 <- sapply(senList0, length)
  senList <- senList0[len0>1]
  for(rpt in 1:9){
    if(length(senList)<2){
      senList <- senList[[1]]
    } else break 
  }
  if(length(senList)<2){
    warning('url. code changed; error likely.')
  }
  len. <- sapply(senList, length)
#  Senate <- senate[len>0]
#  ns <- lapply(Senate, dim)
#  NS <- sapply(ns, max)
#  ns. <- which.max(NS)
#  if(NS[ns.]!=100) {
#      warning('Problem with number of senators ', 
#        'read at URL = ', url., ';  max dimension = ', 
#         NS[ns.], ' != 100')
#  }
#  ns. <- which(ns[1, ]>=100)
#  if(length(ns.)!=1){
#      stop('There is not exactly one table with ', 
#         100 rows in url = ', url.)
#  }
#  sen <- Senate[[ns.]]
  Leaders <- which(len.>11)
  colNm0 <- names(senList[[Leaders[1]]])
  colNm0t <- table(colNm0)
  if(any(colNm0t>1)){
    warning('names(senList[[Leaders[1]]]) not unique', 
            ' url. changes. Error likely.')
  }
  colNames <- make.names(colNm0, unique=TRUE)
  names(senList[[Leaders[1]]]) <- colNames
#  
  k <- length(colNames)
  N <- length(senList)
  Sen <- matrix('', N, k)
  colnames(Sen) <- colNames
  i0 <- 0 
  for(i in 1:N){
    senLi0 <- senList[[i]]
    senLi <- unlist(senList[[i]])
    nmsi <- names(senLi)
    goodi <- (nmsi%in%colNames)
    if(sum(!goodi)>0){
      msg0 <- paste0('Senate member ', i, 
        ' has data element(s) different from', 
        ' Leader[1]; different.name[1]')
      difNm1 <- nmsi[!goodi][1]
      if(difNm1==''){
        msg <- paste(msg0, 'is blank.')
      } else msg <- paste(msg0, '=', difNm1)
      warning(msg)
      i0 <- i
    }
    Sen[i, nmsi[goodi]] <- senLi[nmsi[goodi]]
  }
  if(i0>0){
    cat('\nWarning:  At least one member has', 
        ' at least one data element that ', 
        'a leader does not have.  Last ', 
        'different member = ', i0, ':\n')
    print(Sen[i0,])
  }
##
## 5.  parse State
##
  stAb <- stateAbbreviations[
        stateAbbreviations$USPS!='',]
  USstates <- stAb$USPS
  rownames(stAb) <- USstates
  senState <- stAb[Sen[, 'state'], 'Name']
##
## 6.  assemble
##
  Class. <- sub('Class ', '', Sen[, 'class'])
  Class <- nchar(Class.)
  Name <- paste(Sen[, 'last_name'], 
                Sen[, 'first_name'], sep=', ')
  Senate <- cbind(data.frame(State=factor(senState), 
        state=factor(Sen[, 'state']), Class, Name, 
        party=factor(Sen[, 'party']), 
        stringsAsFactors=FALSE), 
      Sen[, 6:9], surname=Sen[, 'last_name'], 
      givenName=Sen[, 'first_name'], 
      leadership_position=Sen[, 'leadership_position'], 
      stringsAsFactors=FALSE)
  Senate
}


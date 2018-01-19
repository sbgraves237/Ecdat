readUShouse <- function(url.="http://www.house.gov/representatives/",
   nonvoting=c('American Samoa', 'District of Columbia',
               'Guam', 'Northern Mariana Islands', 'Puerto Rico',
               'Virgin Islands'),
   fixNonStandard=subNonStandardNames, ...){
##
## 1.  download content
##
#  library(RCurl)
  Start <- paste(date(), ': readUShouse(', url., ')', sep='')
  cat(Start)
  startTime <- proc.time()
  house.gov <- try(RCurl::getURL(url., followlocation = TRUE))
  et <- max(proc.time()-startTime, na.rm=TRUE)
  Read <- paste('|', nchar(house.gov), 'bytes read in',
                round(et, 2), 'seconds\n')
  cat(Read)
  if(class(house.gov)=='try-error'){
      cat(house.gov)
      stop()
  }
##
## 2.  find "state"
##
  if(length(house.gov)>1){
    stop('Structure of ', url., ' has changed:\n', 
         'getURL(...) no longer returns a single ', 
         'long character string as before.')
  }
#  st <- gregexpr('state_', house.gov)[[1]] # found 75 in 2013
#  but later found nothing.  
# Was replaced by:  
#  st <- gregexpr('\t<h2 id=\"', house.gov)[[1]]
#  which by 2018-01-10 also found nothing.
#  st <- gregexpr('<a href=\"#state-', house.gov)[[1]] # 19 <- not enough 
#  st <- gregexpr('href=\"#state-', house.gov)[[1]] # still only 19
#  st <- gregexpr('#state-', house.gov)[[1]] # still only 19
#  st <- gregexpr('#state', house.gov)[[1]] # still only 19
#  st <- gregexpr('state', house.gov)[[1]] # 81 probably better?
#  st <- gregexpr('state-', house.gov)[[1]] # 76 probably better?
#  substring(house.gov, st, st+28) 
# state-alabama twice: # 2 and 21 
# This has 56 entries starting with # 21 <- what I want.  
# First find the last "Alabama":  
  Alabama <- tail(gregexpr('state-alabama', house.gov)[[1]], 1) 
  stateTable <- substring(house.gov, Alabama)
  st <- gregexpr('state-', stateTable)[[1]] 
# length = 56: What I want, I think.  
  nst <- length(st)
  if(nst != 56){
    warning('house.gov has changed.  readUShouse ', 
            'may not return the desired information.')
  }
  st. <- sapply(st, function(x){
      hgi <- substring(stateTable, x)
      x2 <- (x+regexpr('\">', hgi))
  } )
  st.codes <- substring(stateTable, st+6, st.-2)
# 2018-01-11: alabama ... american-samoa ... 
##
## 3.  Convert to tables
##
#  library(XML)
  House.gov <- XML::readHTMLTable(house.gov, stringsAsFactors=FALSE)
  stNms0 <- names(House.gov)
  stNms1 <- strsplit(stNms0, '\n')
  stNms2 <- sapply(stNms1, '[', 2)
  stNms3 <- trimws(stNms2) 
# 81 = 56 states and territories + 25 letters 
  names(House.gov) <- stNms3
  byState <- (nchar(stNms3)>1)
  State. <- stNms3[byState] # 50 states + DC + 5 territories
#  House.gov <- House.gov0[nchar(stNms3)>1]
##
## 4.  Rbind tables with the same headers
##
  headers <- lapply(House.gov, names)
  h. <- sapply(headers, paste, collapse=":")
  H. <- table(h.)
# st2 <- St.[1:H.[1]]
  ns <- sapply(House.gov, nrow)
  State <- rep(State., ns[byState]) # 441 rows 
#  USPS <- grep('USPS', names(Ecdat::USstateAbbreviations))
  stateAbbr <- Ecdat::USstateAbbreviations[c('Name', 'USPS')]
  rownames(stateAbbr) <- stateAbbr$Name
  state <- stateAbbr[State, 2]
#  St2 <- rep(st2, ns[1:length(st2)])
#
  nh <- length(H.)
  out <- vector(mode='list', length=nh)
  for(ih in 1:nh){
    sel <- (h.==names(H.)[ih])
    out[[ih]] <- do.call(rbind, House.gov[sel])
  }
  names(out) <- names(H.)
  District <- paste(State, out[[1]]$District)
  distr <- sub('At Large|Delegate|Resident Commissioner', 
                  '0', out[[1]]$District)
  distr2 <- sub('st|nd|rd|th', '', distr)
  district <- as.integer(distr2)
  idistr <- which('District' == names(out[[1]]))
  iparty <- which('Party' == names(out[[1]]))
  inm <- which('Name' == names(out[[1]]))
#  
  out[[1]] <- cbind(State=factor(State), 
      state=factor(state), district, 
      out[[1]]['Name'], 
      party=factor(out[[1]]$Party), 
      out[[1]][-c(idistr, inm, iparty)], 
      stringsAsFactors=FALSE)  
  rm <- grep('Office Room', names(out[[1]]))
  names(out[[1]])[rm] <- 'Room'
#  
  cmtes <- grep('Committee', names(out[[1]]))
  if(length(cmtes)==1){
    names(out[[1]])[[cmtes]] <- 'Committees'
  } else {
    warning("length(grep('Committee', names(out[[1]])))", 
            ' != 1:  Name problems')
  }
  rownames(out[[1]]) <- District
#  
  surnm <- parseName(out[[1]]$Name, TRUE)
# fix nonstandard names?
  Surnm <- fixNonStandard(surnm)
  out[[1]] <- cbind(out[[1]], Surnm, 
            stringsAsFactors=FALSE)
##
## 5.  If not 2 tables, issue a warning
##
  if(nh!=2){
      warning(nh, " != 2 different tables found;  I'm confused")
      return(out)
  }
  n. <- sapply(out, nrow)
  if(n.[1] != n.[2]){
    warning('2 tables found with differing numbers of rows:  ', 
            paste(n., collapse=' and '), "; I'm confused:", 
            ' Returning both.')
    return(out)
  }
##
## 8.  Done
##
  out[[1]]
}

asNumericChar <- function(x, leadingChar='^\\$', 
            suppressChar=',', pctChar='%$'){
##
## 1.  Convert factors to character
## 
#  print(x)
  if(length(x)<1)return(x)
  if(all(is.na(x)))return(x)
  X <- x
#  print('local copy made')
  if(is.factor(x))x <- as.character(X)
#  print('if(is.factor(x))...')
##
## 1.  Delete leading blanks and $ 
##
  x[!is.na(x)] <- tis::stripBlanks(x[!is.na(x)])
#  print(('tis::stripBlanks(x)'))
  dol <- grep(leadingChar, x)
#  cat(length(dol), ' $ found: ', 
#      paste(dol, collapse=', '), '\n')
  x[dol] <- sub(leadingChar, '', x[dol])
##
## 2.  find percent
##  
  pct <- grep(pctChar, x)
  x0 <- sub(pctChar, '', x)
##
## 3.  Delete commas (thousand separators) and footnote references
##
  x1 <- gsub(suppressChar, '', x0)
  x2 <- strsplit(x1, ' ')
  x. <- sapply(x2, '[', 1)
# set any blanks to NA so they don't convert to 0  
  xi <- which((!is.na(x1)) & x1=='')
#  cat(length(xi), ' blanks found: ', 
#      paste(xi, collapse=', '), '\n' )
  x.[xi] <- NA
  xo <- as.numeric(x.)
##
## 4.  rescale percents 
##
#  cat(length(pct), ' % found: ', 
#      paste(pct, collapse=', '), '\n')
  xo[pct] <- xo[pct]/100
  xo
}

asNumericDF <- function(x, keep=function(x)any(!is.na(x)), 
              orderBy=NA, ignore=NULL, factors=NULL, 
              Dates=NULL, POSIX=NULL, MSdates=NULL, 
              format., leadingChar='^\\$', 
              suppressChar=',', pctChar='%$'){
##
## 1.  Copy x
##
  Trace <- FALSE
  if(Trace)cat('debug asNumericDF\n')
  X <- as.data.frame(x)
##  
## 2.  Confirm that ignore, factors, Dates, and POSIX
##     all refer to columns of x and do not overlap.  
##
  k <- ncol(x)
  Names <- colnames(x)
#   check for Names in referenceList   
  cols2names <- function(cols=ignore){
    if(length(cols)<1)return(character(0))
    msg <- deparse(substitute(cols))
    if(is.numeric(cols)){
      if(any(cols<1)){
        stop(msg, ' is numeric < 1')
      } 
      if(any(cols>k)){
        stop(msg, ' is numeric > ncol(x)')
      }
      colNames <- Names[cols]
    } else {
      if(is.logical(cols)){
        if((lc <- length(cols))<k){
          stop(msg, ' is logical with length = ', 
              lc, ' < ncol(x) = ', k)
        }
        colNames <- Names[cols]
      } else {
        if(!is.character(cols)){
          stop(msg, ' is class = ', class(cols), 
               '; not allowed')
        }
        if(length(igoops <- which(!(cols %in% Names)))>0){
          stop(msg, ' = ', cols[igoops[1]], 
             ' not in names(x) = ', 
             paste(Names, collapse=', ') )
        }
        colNames <- cols
      }
      if(length(unique(colNames))<length(colNames)){
        stop(msg, ' contains duplicates')
      }
    }
    colNames    
  }
  Ignore <- cols2names(ignore)
  Factors <- cols2names(factors)
  DATES <- cols2names(Dates)
  Posix <- cols2names(POSIX)
  MS.dates <- cols2names(MSdates)
##
## test for intersection 
##  
  if(length(IFoops <- intersect(Ignore, Factors))>0){
    stop(IFoops[1], ' is in both ignore and factors')
  }
  IgFa <- c(Ignore, Factors)
  if(length(IFDoops <- intersect(DATES, IgFa))>0){
    stop(IFDoops[1], ' is in Dates and either ignore', 
         ' or factors')
  }
  IgFaDa <- c(IgFa, DATES)
  if(length(IFDPoops <- intersect(Posix, IgFaDa))>0){
    stop(IFDPoops[1], ' is in POSIX and one of ignore', 
         ', factors, and Dates')
  }
  IgFaDP <- c(IgFaDa, Posix)
  if(length(IFDoops <- intersect(MS.dates, IgFaDP))){
    stop(IFDoops[1], ' is no MSdates and one of ignore', 
         ', factors, Dates, and POSIX')
  }
  dontConvert <- c(IgFaDP, MS.dates)
##  
## 3.  Convert factors, Dates, POSIX, MSdates
##
  if(Trace)cat('factors\n')
  for(f in Factors){
    X[, f] <- factor(x[, f])
  }
  if(Trace)cat('Dates\n')
  for(d in DATES){
    xd <- x[, d]
    if(is.factor(xd))xd <- as.character(xd)
    dd <- try(as.Date(xd, format=format.))
    if(is(dd, 'try-error')){
      dd1 <- try(as.Date(xd, '%m-%d-%Y'))
      dd2 <- try(as.Date(xd, '%m/%d/%Y'))
      if(is(dd1, 'try-error')){
        if(is(dd2, 'try-error')){
          msg <- paste0('Failed to convert date ', 
            d, ' = ', x[1, d], ', ...')
          stop(msg)
        } else {
          X[, d] <- dd2
        } 
      } else {
        if(is(dd2, 'try-error')){
          X[, d] <- dd1 
        } else {
          na1 <- sum(is.na(dd1))
          na2 <- sum(is.na(dd2))
          if(na1<na2){
            X[, d] <- dd1
          } else {
            if(na1>na2){
              X[, d] <- dd2   
            } else {
              d1. <- sum(abs(dd1 - as.Date1970(0)), na.rm=TRUE)
              d2. <- sum(abs(dd2 - as.Date1970(0)), na.rm=TRUE)
              if(d1.<d2.){
                X[,d] <- dd1
              } else X[, d] <- dd2
            }
          }
        }
      }
    } else {
      de1 <- try(as.Date(xd, '%m-%d-%Y'))
      de2 <- try(as.Date(xd, '%m/%d/%Y'))
      dl <- list(dd, de1, de2)
      nad <- sapply(dl, function(x)sum(is.na(x)))
      naMin <- which(nad==min(nad))
      if(length(naMin)<2){
        X[, d] <- dl[[naMin]]
      } else {
        dl. <- dl[naMin]
        del <- sapply(dl., function(x){
          sum(abs(x-as.Date1970(0)), na.rm=TRUE)
        })
        delMin <- which(del==min(del))
        if(length(delMin)<1){
          X[, d] <- NA 
        } else X[, d] <- dl.[[delMin[1]]]
      }
    }
  }
  if(Trace)cat('POSIX\n')
  for(p in Posix){
    xp <- x[, p]
    xpNA <- (xp %in% c('NA', 'NULL'))
    xp[xpNA] <- NA
    if(missing(format.)){
      pp <- try(as.POSIXct(xp))
      if(is(pp, 'try-error')){
        msgP <- paste0('Failed to convert POSIX ', 
                      d, ' = ', x[1, d], ', ...')
        stop(msgP)
      } else X[, p] <- pp 
    } else {
      pp <- try(as.POSIXct(xp, format=format.))
      if(is(pp, 'try-error')){
        msgP <- paste0('Failed to convert POSIX ', 
                       d, ' = ', x[1, d], ', ...')
        stop(msgP)
      } else X[, p] <- pp 
    }
  }
  if(Trace)cat('MSdates\n')
  for(m in MS.dates){
    xm <- x[, m]
    Xm <- as.numeric(xm)
    XM <- as.Date(Xm, origin=as.Date('1899-12-31') )
    X[, m] <- XM
  }
##
## 4.  Apply asNumericChar to all columns 
##     not in ignore, factors, Dates, or POSIX.  
##
  if(Trace)cat('dontConvert\n')
  notNum <- (Names %in% dontConvert)
  numCols <- Names[!notNum]
  for(n in numCols){
    w0 <- options(warn=-1)
#    cat(colnames(x)[n], ":")
#    print(x[,n])
    xn <- asNumericChar(x[, n], leadingChar=leadingChar, 
            suppressChar=suppressChar, pctChar=pctChar)
    options(warn=w0$warn)
    xnNewNA <- which(is.na(xn) & !is.na(x[, n]))
    if(length(xnNewNA)>0){
      msg0 <- paste0('NAs introduced by coercion ', 
         'in asNumericChar(', n, '[' )
      if(length(xnNewNA)<2){
        msg1 <- paste0(msg0, xnNewNA, '])')
      } else {
        if(length(xnNewNA)>4){
          msg1 <- paste0(msg0,  'c(', 
              paste(xnNewNA[1:4], collapse=', '), 
                       ', ...)])')
        } else {
          msg1 <- paste0(msg0, 'c(', 
              paste(xnNewNA, collapse=', '), ')])')
        }
      }
      warning(msg1)
    }
    X[, n] <- xn
  }
##
## 5.  Keep columns specified by keep.  
##
  if(Trace)cat('keep\n')
  kp <- rep(FALSE, k)
  names(kp) <- Names
  kp[notNum] <- TRUE
  if(is.function(keep)){
      Keep <- sapply(X[numCols], keep)
      kp[numCols[Keep]] <- TRUE
  } else {
    if(!is.null(keep)) {
      if(is.logical(keep)) {
        kp[keep] <- TRUE 
      } else {
        kp[keep] <- TRUE 
      }
    }
  }
#
#  if(missing(orderBy)){
#      orderBy <- 1:length(X)
#  }
  if((length(orderBy)>1) && !is.na(orderBy)){
    o <- do.call(order, X[orderBy])
    return(X[o, kp])
  }
  X[, kp]
}


asNumericChar <- function(x, 
    leadingChar='^\\$', 
    suppressChar=',', pctChar='%$', 
    class.=NULL, format.=NULL){
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
  x[!is.na(x)] <- tis::stripBlanks(x[
              !is.na(x)])
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
## 3.  class.='Date'
##
  if(!is.null(class.)){
    cl <- pmatch(class., 
        c('Date', 'POSIXct'))
    if(is.na(cl))
      stop('class. must match either', 
        ' "Date" or "POSIXct";', 
        ' does not; = ', class.)
    if(cl==1){
      dd <- try(if(is.null(format.)){
          as.Date(x) 
        } else {
          as.Date(x, format=format.)
        })
      if(is(dd, 'try-error')){
        dd1 <- try(as.Date(x, '%m-%d-%Y'))
        dd2 <- try(as.Date(x, '%m/%d/%Y'))
        if(is(dd1, 'try-error')){
          if(is(dd2, 'try-error')){
            msg <- paste0(
              'Failed to convert date ', 
              head(x), ', ...')
            stop(msg)
          } else {
            return(dd2)
          } 
        } else {
          if(is(dd2, 'try-error')){
            return(dd1)
          } else {
            na1 <- sum(is.na(dd1))
            na2 <- sum(is.na(dd2))
            if(na1<na2){
              return(dd1)
            } else {
              if(na1>na2){
                return(dd2)   
              } else {
                d1. <- sum(abs(dd1 - 
                   as.Date1970(0)), 
                           na.rm=TRUE)
                d2. <- sum(abs(dd2 - 
                   as.Date1970(0)), 
                           na.rm=TRUE)
                if(d1.<d2.){
                  return(dd1)
                } else return(dd2)
              }
            }
          }
        }
      } else {
        de1 <- try(as.Date(x, '%m-%d-%Y'))
        de2 <- try(as.Date(x, '%m/%d/%Y'))
        dl <- list(dd, de1, de2)
        nad <- sapply(dl, function(x)sum(
          is.na(x)))
        naMin <- which(nad==min(nad))
        if(length(naMin)<2){
          return(dl[[naMin]])
        } else {
          dl. <- dl[naMin]
          del <- sapply(dl., function(x){
            sum(abs(x-as.Date1970(0)), 
                na.rm=TRUE)
          })
          delMin <- which(del==min(del))
          if(length(delMin)<1){
            return(NA) 
          } else return(dl.[[delMin[1]]])
        }
      }
    } else {
##
## 4.  class.='POSIXct'
##      
      xpNA <- (x %in% c('NA', 'NULL'))
      x[xpNA] <- NA
      if(missing(format.)){
        pp <- try(as.POSIXct(x))
        if(is(pp, 'try-error')){
          msgP <- paste0(
            'Failed to convert POSIX ', 
             'x[1] = ', x[1], ', ...')
          stop(msgP)
        } else return(pp)
      } else {
        pp <- try(as.POSIXct(x, 
                    format=format.))
        if(is(pp, 'try-error')){
          msgP <- paste0(
            'Failed to convert POSIX ', 
            'x[1] = ', x[1], ', ...')
          stop(msgP)
        } else return(pp)
      }
    }
  }
##
## 5.  Delete commas (thousand separators) 
##     and footnote references
##
  x1 <- gsub(suppressChar, '', x0)
  x2 <- strsplit(x1, ' ')
  x. <- sapply(x2, '[', 1)
# set any blanks to NA 
#    so they don't convert to 0  
  xi <- which((!is.na(x1)) & x1=='')
#  cat(length(xi), ' blanks found: ', 
#      paste(xi, collapse=', '), '\n' )
  x.[xi] <- NA
  xo <- as.numeric(x.)
##
## 6.  rescale percents 
##
#  cat(length(pct), ' % found: ', 
#      paste(pct, collapse=', '), '\n')
  xo[pct] <- xo[pct]/100
  xo
}

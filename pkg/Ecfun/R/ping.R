Ping <- function(url, pingArgs='', ...){
##
## 1.  construct ping command
##
    pingCmd <- paste('ping', pingArgs, url)
##
## 2.  issue ping command
##
    rawResults <- system(pingCmd, intern=TRUE, ...)
##
## 3.  could not find host?
##
    cnf <- grep('Ping request could not find host', rawResults)
    if(length(cnf)>0){
        return(list(rawResults=rawResults,
                    rawNumbers=numeric(0),
                    counts=c(sent=0, received=0, lost=0),
                    p.lost=NA,
                    stats=c(min=NA, avg=NA, max=NA, mdev=NA) ) )
    }
##
## 4.  raw times
##
    time. <- grep('time=', rawResults, value=TRUE)
    if(length(time.)>0){
        timesel <- strsplit(time., 'time=')
        timechar <- sapply(timesel, '[', 2)
        timech. <- strsplit(timechar, 'ms')
        timech1 <- sapply(timech., '[', 1)
        rawNumbers <- as.numeric(timech1)
        min. <- min(rawNumbers)
        avg. <- mean(rawNumbers)
        max. <- max(rawNumbers)
        mdev <- sd(rawNumbers)
    } else{
        rawNumbers <- numeric(0)
        min. <- avg. <- max. <- mdev <- NA
    }
##
## 5.  timed out
##
    out. <- grep('timed out', rawResults, value=TRUE)
##
## 6.  counts
##
    rcvd <- length(rawNumbers)
    lost <- length(out.)
    counts <- c(sent=rcvd+lost, received=rcvd, lost=lost)
##
## 7.  p.lost
##
    p.lost <- lost/counts[1]
##
## 8.  stats
##
    stats <- c(min=min., avg=avg.,
               max=max., mdev=mdev )
##
## 9.  Done
##
    list(rawResults=rawResults, rawNumbers=rawNumbers,
         counts=counts, p.lost=p.lost, stats=stats)
}

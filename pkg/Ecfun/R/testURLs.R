testURLs <- function(urls=c(
 wiki="http://en.wikipedia.org",
 wiki.PVI="http://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index",
 house="http://house.gov",
 house.reps="http://house.gov/representatives"),
         file.='testURLresults.csv',
         n=10, maxFail=10, ...){
##
## 1. set up
##
  library(RCurl)
  ku <- length(urls)
  uNames <- names(urls)
  if(is.null(uNames))
      uNames <- sub('^http://', '', urls)
  N <- n*maxFail
  elapsed.time <- Time <- urlOut <- rep(NA, N)
  errorMsgs <- character(N)
  Read <- vector('list', ku)
  names(Read) <- uNames
##
## 2.  Do
##
  iout <- 0
  for(i in 1:n){
      cat(i, '\n')
      for(j in 1:ku){
          for(irep in 1:maxFail){
              iout <- iout+1
              Time[iout] <- date()
              urlOut[iout] <- uNames[j]
              cat(Time[iout], uNames[j], '', sep=', ')
#
              pingi <- Ping(urls[i], ...)
              if((i<2) && (irep<2)){
                  kc <- length(pingi$counts)
#                  i.c <- 1:kc
                  ks <- length(pingi$stats)
#                  i.s <- i:ks
                  nameping <- c(names(pingi$counts), names(pingi$stats))
                  pingStats <- matrix(NA, N, kc+ks,
                      dimnames <- list(NULL, nameping) )
#
                  fi <- file.info(file.)
                  if(is.na(fi[1,1])){
                      .Names <- c('Time', 'URL', nameping, 'readTime',
                                  'error')
                      cat(paste(.Names, collapse=','),'\n', file=file.)
                  }
              }
              pingStats[iout,] <- c(pingi$counts, pingi$stats)
#
              start.time <- proc.time()
              readi <- try(getURL(urls[j]))
              et <- max(proc.time() - start.time, na.rm=TRUE)
              elapsed.time[iout] <- et
              si <- (class(readi)!='try-error')
#              success[j] <- si
              if(si){
                  Read[[j]] <- readi
              } else {
                  ri <- gsub('\n', ' ', readi)
                  errorMsgs[iout] <- ri
              }
              outi <- paste(Time[iout], urlOut[iout],
                            paste(pingStats[iout, ], collapse=', '),
                            elapsed.time[iout],
                            errorMsgs[iout], sep=', ')
              cat(outi, '\n', file=file., append=TRUE)
              cat(si, et, '\n')
          }
      }
  }
  jout <- 1:iout
  results1 <- data.frame(Time=Time[jout], URL=urlOut[jout])
  results <- cbind(results1, as.data.frame(pingStats[jout,]),
                   readTime=elapsed.time[jout],
                   error=errorMsgs[jout])
##
## 3.  Done
##
  attr(Read, 'urls') <- urls
  attr(Read, 'testResults') <- results
  Read
}

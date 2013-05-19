mergeUShouse.senate <- function(x, UScongress=UShouse.senate(),
                                newrows='amount0',
        default=list(member=NA, amount=0, vote="notEligible") ){
##
## 1.  keys
##
#  X <- x
#  x$district[x$district=='0'] <- 'At Large'
#
  keyx <- with(x, paste(Office, state, district, sep=":"))
  keyy <- with(UScongress, paste(Office, state, district, sep=":"))
##
## 2.  notx
##
  notx <- !(keyy %in% keyx)
  huh <- !(keyx %in% keyy)
  if((nhuh <- sum(huh))>0){
      cat(nhuh, 'Districts in x not found in UScongress;  the first is:\n')
      print(x[huh,][1,])
      stop('District coding problem in x')
  }
  notx. <- (notx & !UScongress$nonvoting)
#
  Y <- UScongress[notx., ]
##
## 3.  Add default columns to Y
##
  nd <- length(default)
  nmx <- names(x)
  for(id in 1:nd){
      found <- regexpr(names(default)[id], tolower(nmx))
      for(f in nmx[found>0]){
          Y[, f] <- default[id]
      }
  }
##
## 4.  newrows
##
  if(!(newrows %in% names(x))){
      x <- cbind(x, FALSE)
      nx <- ncol(x)
      names(x)[nx] <- newrows
  }
  Y[, newrows] <- TRUE
##
## 4.  rbind
##
  xY <- rbind(x, Y[names(x)])
  xY
}


mergeUShouse.senate <- function(x, UScongress=UShouse.senate(),
                                newrows='contrib.0',
        default=list(member=NA, contrib=0, vote="notEligible") ){
##
## 1.  keys
##
  keyx <- with(x, paste(houseSenate, state, District, sep=":"))
  keyy <- with(UScongress, paste(houseSenate, state, District, sep=":"))
##
## 2.  notx
##
  notx <- !(keyy %in% keyx)
  notx. <- (notx & !UScongress$nonvoting)

  Y <- UScongress[notx & !UScongress$nonvoting, ]
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
  rbind(x, Y[names(x)])
}


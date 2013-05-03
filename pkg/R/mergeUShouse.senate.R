mergeUShouse.senate <- function(x, UScongress=UShouse.senate(),
        default=list(member=FALSE, contrib=0, vote="notEligible") ){
##
## 1.  keys
##
  keyx <- with(x, paste(houseSenate, state, District, sep=":"))
  keyy <- with(UScongress, paste(houseSenate, state, District, sep=":"))
##
## 2.  notx
##
  notx <- !(keyy %in% keyx)
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
## 4.  rbind
##
  rbind(x, Y[names(x)])
}


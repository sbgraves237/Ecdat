rgrep <- function(pattern, x){
##
## 1, 2:  np & g.
##
  np <- length(pattern)
  g. <- rep(NA, np)
##
## 3.  for each pattern
##
  for(i in seq(length=np)){
    g.[i] <- (length(grep(pattern[i], x))>0) 
  }
##
## 3.  done 
##
  return(which(g.))
  
}

subNonStandardCharacters <- function(x,
   standardCharacters=c(letters, LETTERS, ' ','.', ',', 0:9,
      '\"', "\'", '-', '_', '(', ')', '[', ']', '\n'),
   replacement='_', ... ) {
##
## 1.  x. <- strsplit(x, "", ...)
##
  nx <- length(x)
  x. <- strsplit(x, "", ...)
##
## 2.  check each and modify as needed
##
  for(ix in seq(length=nx)){
      gi <- which(!(x.[[ix]] %in% standardCharacters))
      if(length(gi)>0){
          gi. <- range(gi)
          ni <- length(x.[[ix]])
          xi1 <- paste(x.[[ix]][seq(length=gi.[1]-1)], collapse='')
          xie <- paste(x.[[ix]][seq(from=gi.[2]+1, length=ni-gi.[2])],
                       collapse='')
          x[ix] <- paste(xi1, replacement, xie, sep="")
      }
  }
  x
}

subNonStandardCharacters <- function(x,
   standardCharacters=c(letters, LETTERS, ' ','.', ',', 0:9,
      '\"', "\'", '-', '_', '(', ')', '[', ']', '\n'),
   replacement='_',
   gsubList=list(list(pattern='\\\\\\\\|\\\\', replacement='\"')),
   ... ) {
##
## 1.  gsubList
##
  xo <- x
  ng <- length(gsubList)
  for(ig in seq(length=ng)){
      gsLi <- gsubList[[ig]]
      if(!is.list(gsLi)){
          print(deparse(substitute(gsubList)))
          cat(ig, '\n')
          print(gsLi)
          stop('gsubList[[', ig, ']] is NOT a list')
      }
      xo <- gsub(gsLi$pattern, gsLi$replacement, xo)
  }
##
## 2.  x. <- strsplit(x, "", ...)
##
  nx <- length(xo)
  x. <- strsplit(xo, "", ...)
##
## 3.  check each and modify as needed
##
  for(ix in seq(length=nx)){
      gi <- which(!(x.[[ix]] %in% standardCharacters))
      if(length(gi)>0){
          gi. <- range(gi)
          ni <- length(x.[[ix]])
          xi1 <- paste(x.[[ix]][seq(length=gi.[1]-1)], collapse='')
          xie <- paste(x.[[ix]][seq(from=gi.[2]+1, length=ni-gi.[2])],
                       collapse='')
          xo[ix] <- paste(xi1, replacement, xie, sep="")
      }
  }
##
## 4.  Done
##
  xo
}

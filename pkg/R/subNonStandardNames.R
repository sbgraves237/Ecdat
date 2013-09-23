subNonStandardNames <- function(x,
   standardCharacters=c(letters, LETTERS, ' ','.', ',', 0:9,
      '\"', "\'", '-', '_', '(', ')', '[', ']', '\n'),
   replacement='_',
   gsubList=list(list(pattern='\\\\\\\\|\\\\', replacement='\"')),
   removeSecondLine=TRUE,
   nonStandardNames=nonEnglishNames, ...) {
##
## 1.  removeSecondLine
##
  if(removeSecondLine){
      X2 <- strsplit(x, '\n')
      x2 <- sapply(X2, '[', 2)
      x <- sapply(X2, '[', 1)
  } else x2 <- NULL
##
## 2.  x. <- subNonStandardCharacters(x, ...)
##
  x. <- subNonStandardCharacters(x, standardCharacters, replacement,
                                 gsubList, ...)
##
## 3.  loop over rows of nonStandardNames
##
  nSN <- nrow(nonStandardNames)
  for(iSN in seq(length=nSN)){
      x. <- gsub(nonStandardNames[iSN, 1], nonStandardNames[iSN, 2],
                 x.)
  }
##
## 4.  Eliminate leading and trailing blanks
##
  if(require(tis))
      x. <- stripBlanks(x.)
##
## 5.  Done
##
  attr(x., 'secondLine') <- x2
  x.
}

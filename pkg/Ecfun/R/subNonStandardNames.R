subNonStandardNames <- function(x,
   standardCharacters=c(letters, LETTERS, ' ','.', ',', 0:9,
      '\"', "\'", '-', '_', '(', ')', '[', ']', '\n'),
   replacement='_',
   gsubList=list(list(pattern='\\\\\\\\|\\\\', replacement='\"')),
   removeSecondLine=TRUE,
   nonStandardNames=Ecdat::nonEnglishNames, ...) {
##
## 1.  removeSecondLine
##
  x0 <- x
  if(is.data.frame(x0))x <- as.matrix(x0)
  if(removeSecondLine){
    nch0 <- (nchar(x)<1)
    X2. <- strsplit(x, '\n')
    x2 <- sapply(X2., function(xx){
        xx2 <- xx[-1]
        paste(xx2, collapse='\n')
    } ) 
    no2 <- sapply(X2., length)
    x2[no2<2] <- NA
    x <- sapply(X2., '[', 1)
    x[nch0] <- '' 
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
#  if(require(tis)){
      x. <- stripBlanks(x.)
#  } else {
#      warning('need stripBlanks{tis} to delete leading and trailing',
#              ' blanks;  not available')
#  }
##
## 5.  Reformat at matrix or data.frame 
##
  if(is.matrix(x0)){
    attributes(x.) <- attributes(x0)
  } else if(is.data.frame(x0)){
    dim(x.) <- dim(x0)
    colnames(x.) <- colnames(x0)
    x. <- as.data.frame(x.)
  }
##
## 6.  Done
##
  nchx2 <- nchar(x2)
  nchx2[is.na(x2)] <- 0 
  if(any(nchx2>0))attr(x., 'secondLine') <- x2
  x.
}

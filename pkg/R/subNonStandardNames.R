subNonStandardNames <- function(x,
   standardCharacters=c(letters, LETTERS, ' ','.', ',', 0:9,
      '\"', "\'", '-', '_', '(', ')', '[', ']', '\n'),
   replacement='_', nonStandardNames=nonEnglishNames, ...) {
##
## 1.  x. <- subNonStandardCharacters(x, ...)
##
  x. <- subNonStandardCharacters(x, standardCharacters, replacement, ...)
##
## 2.  loop over rows of nonStandardNames
##
  nSN <- nrow(nonStandardNames)
  for(iSN in seq(length=nSN)){
      x. <- gsub(nonStandardNames[iSN, 1], nonStandardNames[iSN, 2],
                 x.)
  }
##
## 3.  done
##
  x.
}

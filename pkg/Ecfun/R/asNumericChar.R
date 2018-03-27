asNumericChar <- function(x, leadingChar='^\\$', 
            suppressChar=',', pctChar='%$'){
##
## 1.  Convert factors to character
## 
#  print(x)
  if(length(x)<1)return(x)
  if(all(is.na(x)))return(x)
  X <- x
#  print('local copy made')
  if(is.factor(x))x <- as.character(X)
#  print('if(is.factor(x))...')
##
## 1.  Delete leading blanks and $ 
##
  x[!is.na(x)] <- tis::stripBlanks(x[!is.na(x)])
#  print(('tis::stripBlanks(x)'))
  dol <- grep(leadingChar, x)
#  cat(length(dol), ' $ found: ', 
#      paste(dol, collapse=', '), '\n')
  x[dol] <- sub(leadingChar, '', x[dol])
##
## 2.  find percent
##  
  pct <- grep(pctChar, x)
  x0 <- sub(pctChar, '', x)
##
## 3.  Delete commas (thousand separators) and footnote references
##
  x1 <- gsub(suppressChar, '', x0)
  x2 <- strsplit(x1, ' ')
  x. <- sapply(x2, '[', 1)
# set any blanks to NA so they don't convert to 0  
  xi <- which((!is.na(x1)) & x1=='')
#  cat(length(xi), ' blanks found: ', 
#      paste(xi, collapse=', '), '\n' )
  x.[xi] <- NA
  xo <- as.numeric(x.)
##
## 4.  rescale percents 
##
#  cat(length(pct), ' % found: ', 
#      paste(pct, collapse=', '), '\n')
  xo[pct] <- xo[pct]/100
  xo
}

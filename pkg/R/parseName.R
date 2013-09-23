parseName <- function(x, surnameFirst=(median(regexpr(',', x))>0),
          suffix=c('Jr.', 'I', 'II', 'III', 'IV', 'Sr.'),
          fixNonStandard=subNonStandardNames, ...){
##
## 1.  length(x)<1?
##
  nx <- length(x)
  if(nx<1){
      warning("length(x) == 0")
      mat0 <- matrix(character(0), nrow=0, ncol=2,
         dimnames=list(NULL, c("surname", "givenName")))
      return(mat0)
  }
##
## 2.  Drop "(AL)", etc.
##
  dropEndParen <- function(x){
      endParen <- grep(')$', x)
      if(length(endParen)>0){
          x.endP <- x[endParen]
          nch <- nchar(x.endP)
          nch3 <- pmax(1, nch-3)
          openP <- substring(x.endP, nch3, nch3)
          nch3. <- ifelse(openP=='(', pmax(1, nch3-1), nch)
          x.woP <- substring(x.endP, 1, nch3.)
          x[endParen] <- x.woP
      }
      if(require(tis)){
          x <- stripBlanks(x)
      }
      x
  }
  x <- dropEndParen(x)
##
## 3.  surnameFirst
##
  if(missing(surnameFirst))
      surnameFirst <- surnameFirst
#
  if(surnameFirst){
      Sep <- regexpr(', ', x)
      oops <- which(Sep<0)
      if((no <- length(oops))>0){
          err <- paste(no, ' of ', length(x), ' elements of x ',
                       'are NOT in surnameFirst format.  ',
                       'The first is ', x[oops[1]],
               '.  Assume they are in (givenName surname) format.')
          warning(err)
          fix0 <- parseName(x[oops])
          fix1 <- paste(fix0[, 2], fix0[, 1], sep=', ')
          x[oops] <- fix1
      }
      sur <- substring(x, 1, Sep-1)
#     drop (AL), etc.
      sur <- dropEndParen(sur)
#
      giv <- substring(x, Sep+2)
      Sur <- fixNonStandard(sur, ...)
      Sur. <- strsplit(Sur, ' ')
      look4suf <- sapply(Sur., tail, n=1)
      suf <- (look4suf %in% suffix)
      surname <- rep(NA, nx)
      lx <- sapply(Sur., length)-suf
      for(ix in 1:nx){
          xi <- Sur.[[ix]][1:lx[ix]]
          surname[ix] <- paste(xi, collapse=' ')
      }
#
      Giv <- fixNonStandard(giv, ...)
      Giv[suf] <- paste(Giv[suf], look4suf[suf], sep=', ')
      out <- cbind(surname, Giv)
      colnames(out) <- c('surname', 'givenName')
      return(out)
  }
##
## 4.  strsplit on either blank or comma
##
  x. <- strsplit(x, ' |,')
##
## 5.  Suffix?
##
  look4suf <- sapply(x., tail, n=1)
  suf <- (look4suf %in% suffix)
##
## 6.  parse
##
  givenName <- surname <- rep(NA, nx)
  lx <- sapply(x., length)-suf
  for(ix in 1:nx){
      xi <- x.[[ix]][1:lx[ix]]
#      ni <- length(xi)
      ni0 <- nchar(xi)
      xi. <- xi[ni0>0]
      surname[ix] <- tail(xi., 1)
      ni <- length(xi.)
      givenName[ix] <- paste(head(xi., ni-1), collapse=' ')
      if(suf[ix])
          givenName[ix] <- paste(givenName[ix], look4suf[ix],
                                 sep=', ')
  }
##
## 7.  fixNonStandard
##
  Sur <- fixNonStandard(surname, ...)
  Giv <- fixNonStandard(givenName, ...)
##
## 8.  Done
##
  out <- cbind(surname=Sur, givenName=Giv)
  out
}


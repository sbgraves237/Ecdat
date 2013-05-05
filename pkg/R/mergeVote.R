mergeVote <- function(x, vote, houseSenate="Rep", vote.x){
##
## 1.  parse vote.x
##
  nx <- nrow(x)
  nv <- nrow(vote)
  votey <- grep('vote', names(vote), value=TRUE)
  if(length(votey)<1)
      stop('No vote column found in vote = ', deparse(substitute(vote)))
#
  if(missing(vote.x)){
      vote.x <- grep('vote', names(x), value=TRUE)
      if(length(vote.x)<1)vote.x <- votey
  }
  if(!(vote.x %in% names(x)))
      x[, vote.x] <- rep('notEligible', nx)
##
## 2.  houseSenate
##
  if(!('houseSenate' %in% names(vote)))
      vote <- cbind(vote, houseSenate=houseSenate)
##
## 3.  keys
##
  nmx <- names(x)
  nmv <- names(vote)
  lnmx <- tolower(nmx)
  lnmv <- tolower(nmv)
  surnmx <- nmx[grep('surname', lnmx)]
  surnmv <- nmv[grep('surname', lnmv)]
  givenx <- nmx[grep('givenname', lnmx)]
  givenv <- nmv[grep('givenname', lnmv)]
  stx <- nmx[grep('state', lnmx)]
  stv <- nmv[grep('state', lnmv)]
  distx <- nmx[grep('district', lnmx)]
  distv <- nmv[grep('district', lnmv)]
  keyx <- paste(x$houseSenate, x[[surnmx]], sep=":")
  keyv <- paste(vote$houseSenate, vote[[surnmv]], sep=":")
  keyx2 <- paste(keyx, x[[givenx]], sep=":")
  keyv2 <- paste(keyv, vote[[givenv]], sep=':')
  keyx. <- paste(x$houseSenate, x[[stx]], x[[distx]], sep=":")
  keyv. <- paste(vote$houseSenate, vote[[stv]], vote[[distv]], sep=":")
##
## 4.   record votes
##
  vote.notFound <- integer(0)
  for(iv in 1:nv){
      jv <- which(keyx == keyv[iv])
      if(length(jv)<1){
          jv <- which(keyx. == keyv.[iv])
          if(length(jv)!=1)
              vote.notFound <- c(vote.notFound, iv)
      }
      if(length(jv)>1){
          jv <- which(keyx2 == keyv2[iv])
          if(length(jv)!=1)
              jv <- which(keyx.==keyv.[iv])
#              vote.notFound <- c(vote.notFound, keyv2[iv])
          if(length(jv)!=1){
              vote.notFound <- c(vote.notFound, iv)
          }
      }
      if(length(jv)==1) {
          x[jv, vote.x] <- as.character(vote[iv, votey])
      }
  }
##
## 5.  Done
##
  x[, vote.x] <- factor(x[, vote.x])
  if((no <- length(vote.notFound))>0){
      cat(no, 'rows of vote not found:\n')
      print(vote[vote.notFound,] )
      stop('Unable to find vote in x')
  }
  x
}


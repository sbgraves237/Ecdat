mergeVote <- function(x, vote, houseSenate="Rep", vote.x){
##
## 1.  parse vote.x
##
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
      x[, vote.x] <- rep('notEligible', nv)
##
## 2.  houseSenate
##
  if(!('houseSenate' %in% names(vote))){
      vote <- cbind(vote, houseSenate=houseSenate)
  }
##
## 3.  keys
##
  surnmx. <- grep('surname', tolower(names(x)))
  surnmx <- names(x)[surnmx.]
  surnmv. <- grep('surname', tolower(names(vote)))
  surnmv <- names(vote)[surnmv.]
  keyx <- paste(x$houseSenate, x[[surnmx]], sep=":")
  keyy <- paste(vote$houseSenate, vote[[surnmv]], sep=":")
##
## 4.   record votes
##
  vote.surnameNotFound <- character(0)
  vote.multipleMatches <- character(0)
  for(iv in 1:nv){
      jv <- which(keyx == keyy[iv])
      if(length(jv)<1){
          vote.surnameNotFound <- c(vote.surnameNotFound, keyy[iv])
      } else {
          if(length(jv)<2){
              x[jv, vote.x] <- as.character(vote[iv, votey])
          } else {
              jv2 <- grep(vote[iv, 'givenName'], x[jv, 'givenName'])
              sgnm <- paste(keyy[iv], vote[iv, 'givenName'], sep=', ')
              if(length(jv2)<1){
                  vote.surnameNotFound <- c(vote.surnameNotFound, sgnm)
              } else {
                  if(length(jv2)<2) {
                      x[jv2, vote.x] <- vote[iv, votey]
                  } else {
                      vote.multipleMatches <- c(vote.multipleMatches, sgnm)
                  }
              }
          }
      }
  }
##
## 5.  Done
##
  x[, vote.x] <- factor(x[, vote.x])
  if((snf <- length(vote.surnameNotFound))>0){
      warning("In ", snf, 'cases, surnames in vote not found in x;  ',
              'the first is ', vote.surnameNotFound[1],
              '; returning as attribute of output')
      attr(x, 'vote.surnameNotFound') <- vote.surnameNotFound
  }
  if((mm <- length(vote.multipleMatches))>0){
      warning("In ", mm, ' cases, multiple matches in vote found in x;  ',
              'the first is ', vote.multipleMatches[1],
              '; returning as attribute of output')
      attr(x, 'vote.multipleMatches') <- vote.multipleMatches
  }
  x
}


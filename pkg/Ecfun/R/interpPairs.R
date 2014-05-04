interpPairs <- function(object, .proportion,
                        pairs=c('1'='\\.0$', '2'='\\.1$', replacement=''),
                        validProportion=0:1, ...){
##
## 1.  find pairs
##
  Names <- names(object)
  suf1 <- grep(pairs[1], Names, value=TRUE)
  suf2 <- grep(pairs[2], Names, value=TRUE)
##
## 2.  Convert identified names to common names
##
  suf1. <- sub(pairs[1], pairs[3], suf1)
  suf2. <- sub(pairs[2], pairs[3], suf2)
  suf. <- unique(c(suf1., suf2.))
##
## 3.  look for pairs
##
#  Matches <- table(c(suf1., suf2.))
#  oops <- names(Matches[Matches<2])
#  numwarn <- ''
#  if(length(oops)>0){
#      un1 <- which(suf1. %in% oops)
#      un2 <- which(suf2. %in% oops)
#      if(length(un1)>0){
#        numwarn <- paste0(suf1[un1[1]], ' found without a matching ',
#                pairs[2], ';  returning ', suf1[un1[1]],
#                ' as ', suf1.[un1[1]])
#        warning(numwarn)
#      }
#      if(length(un2)>0){
#        numwarn <- paste0(suf2[un2[1]], ' found without a matching ',
#                  pairs[1], ';  returning ', suf2[un2[1]],
#                  ' as ', suf2.[un1[1]])
#        warning(numwarn)
#      }
#      el1 <- c(suf1[un1], suf2[un2])
#      Dat <- object[c(suf1[un1], suf2[un2])]
#      names(Dat) <- c(suf1.[un1], suf2.[un2])
#  }
#  match2 <- names(Matches[Matches>1])
##
## 4.  evalObj <- eval(object) 
##
  interpObj <- object
  nel <- length(interpObj)
  interpObj$.proportion <- .proportion 
  for(j in seq(length=nel)){
#   eval(interpObj[[j]])    
    interpObj[[j]] <- eval(interpObj[[j]], interpObj[-j])
#   Is this a pair name? 
    s1 <- which(suf1 == Names[j])
    s2 <- which(suf2 == Names[j])
    k1 <- length(s1)
    k2 <- length(s2)
    k12 <- k1+k2
#   k12 = 0 or 1; can't be 2 
    if(k12>0){
      if(k1>0){
#       suf1[s1];  look for match in suf2 
        j2 <- which(suf2. == suf1.[s1])
        if(length(j2)>0){
          if(suf2[j2] %in% Names[1:j]){
#         Both suf1[s1] and suf2[j2] have been eval'ed
#         Add the interpolation 
#            N12 <- (interpObj[[suf1[s1]]]*() 
#                    + interpObj[[suf2[j2]]]*proportion ) 
            N12 <- interpChar(interpObj[c(suf1[s1], suf2[j2])], 
                              .proportion)
            interpObj[[suf2.[j2]]] <- N12 
          } 
#         match found but not processed yet
          next
        } else {
#         match not found:
#          if(is.numeric(interpObj[[j]])){ 
#           If numeric, store interpObj[[j]] as the match 
#           with a warning           
          N1 <- interpChar(x=interpObj[[j]], .proportion=.proportion) 
          interpObj[[suf1.[s1]]] <- N1
        }
      } else {
#       k2=1, because k1=0         
#       suf2[s2];  look for match in suf1 
        j1 <- which(suf1. == suf2.[s2])
        if(length(j1)>0){
          if(suf1[j1] %in% Names[1:j]){
#         Both suf1[s1] and suf2[j2] have been eval'ed
#         Add the interpolation 
#            N12 <- (interpObj[[suf1[j1]]]*(1-proportion) 
#                    + interpObj[[suf2[s2]]]*proportion ) 
            N12 <- interpChar(interpObj[c(suf1[j1], suf2[s2])], 
                              .proportion)
            interpObj[[suf1.[j1]]] <- N12 
          } 
#         match found but not processed yet
          next
        } else {
#         match not found;  store interpObj[[j]] as the match           
          interpObj[[suf2.[s2]]] <- 
            interpChar(interpObj[j], .proportion) 
          next 
        }     
      }
    }
  }
##
## 5.  Other vectors or data.frames 
##     with the same number of rows?  
##
# length of suf.?  
  if(length(interpObj)>0){
    objLen <- sapply(interpObj, NROW)
  } else objLen <- integer(0)
  nSuf <- length(suf.)
  if(nSuf>0){
    ln <- max(objLen[suf.])
  } else ln <- 1
  lp <- length(.proportion)
  if(lp < ln) {
    .proportion <- rep(.proportion, length=ln)
    N <- ln
  } else {      
    N <- lp 
    if(lp>ln){
      warning('length(.proportion) = ', lp, 
              ' > max length(pairs) = ', ln)      
    }
  }
# Confirm that all suf. have length N 
#  for(s. in suf.){
#    interpObj[[s.]] <- rep(interpObj[[s.]], length=N)
#  }
#  if(length(interpObj)>0){
#    objL. <- sapply(interpObj, NROW)
#  } else objL. <- integer(0)
# Rows to keep 
  In <- ((validProportion[1] <= .proportion) & 
         (.proportion <= validProportion[2]) )
# Cols to trim? 
  cols2trim <- (objLen==N)
# trim   
  for(s. in names(interpObj)[cols2trim]){
#   Retain only "In" in s.
    S. <- interpObj[[s.]]
    ndim <- length(dim(S.))
    if(ndim<2){
        if(!is.null(S.)){
          S. <- S.[In]
        }
    } else {      
      if(is.data.frame(S.)){
            S. <- S.[In,, drop=FALSE]
      } 
    }
    interpObj[[s.]] <- S.
  }
##
## 6.  Delete suf1 and suf2
## 
  interpObj$.proportion <- NULL 
  del <- (names(interpObj) %in% c(suf1, suf2))
  interpObj[!del]
}
interpPairs <- function(object, .proportion, envir=list(), 
                        pairs=c('1'='\\.0$', '2'='\\.1$', replacement=''),
                        validProportion=0:1, Source=character(0), ...){
##
## 1.  Source
##
  if(sum(nchar(Source))<1){
    Source <- deparse(substitute(object), 25)[1]
  }
  if((ns <- length(Source))>1){
    warning('length(Source) = ', ns, "; Source[1:2] = ", 
            paste(Source[1:2], collapse='; '), 
            ';  using the first non-null')
    Source <- Source[nchar(Source)>0][1]
  }  
##
## 2.  find pairs
##
  Names <- names(object)
  suf1 <- grep(pairs[1], Names, value=TRUE)
  suf2 <- grep(pairs[2], Names, value=TRUE)
##
## 3.  Convert identified names to common names
##
  suf1. <- sub(pairs[1], pairs[3], suf1)
  suf2. <- sub(pairs[2], pairs[3], suf2)
  suf. <- unique(c(suf1., suf2.))
##
## 4.  Envir[[..]] <- eval(object) 
##
  Envir <- envir 
#  interpObj <- object
  nel <- length(object)
  Envir$.proportion <- .proportion 
#  ne <- length(Envir)
  for(j in seq(length=nel)){
#   eval(interpObj[[j]])    
    Nj <- eval(object[[j]], Envir) 
    if(is.null(Nj)){
      oj <- object[[j]]
      ojc <- deparse(object[[j]], width.cutoff=25) 
      stop('NULL returned from eval(', Names[j], 
           ' = ', ojc, ', ...)')
    }
    Envir[[Names[j]]] <- Nj 
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
            argNms <- c(suf1[s1], suf2[j2], Source)
            N12 <- interpChar(Envir[c(suf1[s1], suf2[j2])], 
                      .proportion=.proportion, argnames=argNms)
            Envir[[suf2.[j2]]] <- N12 
          } 
#         match found but not processed yet
          next
        } else {
#         match not found:
#          if(is.numeric(interpObj[[j]])){ 
#           If numeric, store interpObj[[j]] as the match 
#           with a warning     
          argNms <- c(Names[j], '.proportion', Source)
          N1 <- interpChar(x=Envir[[Names[j]]], 
                  .proportion=.proportion, argnames=argNms) 
          Envir[[suf1.[s1]]] <- N1
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
            argNms <- c(suf1[j1], suf2[s2], Source)
            N12 <- interpChar(Envir[c(suf1[j1], suf2[s2])], 
                              .proportion=.proportion, argnames=argNms)
            Envir[[suf1.[j1]]] <- N12 
          } 
#         match found but not processed yet
          next
        } else {
#         match not found;  store interpObj[[j]] as the match           
          argNms <- c(Names[j], '.proportion', Source) 
          Envir[[suf2.[s2]]] <- interpChar(Nj, 
                        .proportion=.proportion, argnames=argNms) 
          next 
        }     
      }
    }
  }
##
## 5.  Other vectors or data.frames 
##     with the same number of rows?  
##
  Drop <- (Names %in% c(suf1, suf2))
  Keep. <- c(Names[!Drop], suf.)
  interpOut <- Envir[Keep.]  
  if(length(interpOut)>0){
    objLen <- sapply(interpOut, NROW)
  } else objLen <- integer(0)
#
  nSuf <- length(suf.)
  if(nSuf>0){
    ln <- max(objLen[suf.])
  } else ln <- (-Inf)
  lp <- length(.proportion)
  if(lp < ln) {
    .proportion <- rep(.proportion, length=ln)
    N <- ln
  } else {      
    N <- lp 
    if((lp>ln) && (ln>1)){
      msg <- paste0('length(.proportion) = ', lp, 
              ' > max length(pairs) = ', ln, 
              ';\n  pairs found for ', 
              paste(suf., collapse=', '))
      warning(msg)
    }
  }
# Rows to keep 
  In <- ((validProportion[1] <= .proportion) & 
         (.proportion <= validProportion[2]) )
# Cols to trim? 
  cols2trim <- (objLen==N)
# trim   
  for(s. in names(interpOut)[cols2trim]){
#   Retain only "In" in s.
    S. <- interpOut[[s.]]
    ndim <- length(dim(S.))
    if(ndim<2){
      Subset <- ((!is.null(S.)) && !is.function(S.))
      if(Subset){
          S. <- S.[In]
      }
    } else {     
      sub2 <- (is.data.frame(S.) || (is.matrix(S.)))
      if(sub2){
          S. <- S.[In,, drop=FALSE]
      } 
    }
    interpOut[[s.]] <- S.
  }
##
## 6.  Delete suf1 and suf2
## 
  interpOut
}
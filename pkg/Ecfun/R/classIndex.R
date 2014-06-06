classIndex <- function(x){
  if(is.null(x)) return(1)
  if(is(x, 'logical'))return(2)
  if(is(x, 'integer'))return(3)
  if(is(x, 'numeric'))return(4)
  if(is(x, 'complex'))return(5)
  if(is(x, 'character'))return(6)
  7
}

index2class <- function(i, otherCharacter=TRUE){
##
## 1.  1 <= i <= 7
##
  if(i<1)stop('i = ', i, '; must be positive')
  if(i>7)stop('i = ', i, '; must be at most 7')
##
## 2.  integer?  
##
  if((i%%1)!=0)stop('i = ', i, '; must be an integer')
##
## 3.  other? 
##
  if(i==7){
    if(otherCharacter)return('character')
    return('other')
  }
## 
## 4.  1 <= i <= 6
##
  c('NULL', 'logical', 'integer', 'numeric', 
    'complex', 'character')[i]
}


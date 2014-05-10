compareLengths <- function(x, y, 
          name.x=deparse(substitute(x), width.cutoff, nlines=1, ...), 
          name.y=deparse(substitute(y), width.cutoff, nlines=1, ...), 
          Source='', compFun=c('NROW', 'length'), 
          action=c(compatible='', incompatible='warning'), 
          width.cutoff=20, ...){
##
## 1.  nchar(name.x, name.y)?
## 
  if((nchar(name.x)<1) || (nchar(name.y)<1)){
    Source <- paste0('in compareLengths:')
  }
  if(nchar(name.x)<1) name.x <- 'x'
  if(nchar(name.y)<1) name.y <- 'y'  
##
## 2.  lenx, leny
##
  comp <- match.arg(compFun)
  lenx <- do.call(comp, list(x))
  if(length(lenx)!=1){
    stop('compFun[ = ', comp, '](', name.x, ') has length ', 
         lenx, '; must be 1.')    
  }
  if(!is.numeric(lenx)){
    stop('compFun[ = ', comp, '](', name.x, ') is not numeric;', 
         '  class = ', class(lenx)) 
  }
  if(lenx==0){
    stop('compFun[ = ', comp, '](', name.x, ') == 0')
  }
  leny <- do.call(comp, list(y))
  if(length(leny)!=1){
    stop('compFun[ = ', comp, '](', name.y, ') has length ', 
       leny, '; must be 1.')    
  }
  if(!is.numeric(leny)){
    stop('compFun[ = ', comp, '](', name.y, ') is not numeric;', 
       '  class = ', class(leny)) 
  }
  if(leny==0){
    stop('compFun[ = ', comp, '](', name.y, ') == 0')
  }
  len <- c(lenx, leny)
##
## 3.  lenx==leny?
##  
  if(lenx==leny)return(c('equal', ''))
##
## 4.  Compatible?  
##
  act <- match.arg(action)
  o <- order(len)
  nam <- c(name.x, name.y)
  res <- (len[o[2]] %% len[o[1]])
  if(is.na(res)){
    stop('compFun = ', comp, )
  }

  if((len[o[2]]%%len[o[1]])==0){
    rat <- (len[o[2]] %/% len[o[1]])
    msc <- paste0(Source, ' length(', nam[o[2]], ') = ', 
        len[o[2]], ' is ', rat, ' times length(', 
        nam[o[1]], ') = ', len[o[1]])
    Msc <- c('compatible', msc)
    if(nchar(action[1])<1){ 
      return(Msc)
    } else do.call(action[1], list(Msc))
  }  
##
## 5.  incompatible   
##
  msi <- paste0(Source, ' length(', nam[o[2]], ') = ', 
        len[o[2]], ' is not a multiple of length(', 
        nam[o[1]], ') = ', len[o[1]])
  Msi <- c('incompatible', msi)
  if(nchar(action[2])<1){ 
    return(Msi)
  } else { 
    do.call(action[2], list(Msi[1], ': ', Msi[2]))
  }
  Msi
}

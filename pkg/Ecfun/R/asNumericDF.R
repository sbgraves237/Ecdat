asNumericDF <- function(x, 
    keep=function(x)any(!is.na(x)), 
    orderBy=NA, ignore=NULL, factors=NULL, 
    Dates=NULL, POSIX=NULL, MSdates=NULL, 
    format.=NULL, leadingChar='^\\$', 
    suppressChar=',', pctChar='%$'){
##
## 1.  Copy x
##
  Trace <- FALSE
  if(Trace)cat('debug asNumericDF\n')
  X <- as.data.frame(x)
##  
## 2.  Confirm that ignore, factors, Dates,
##     and POSIX all refer to columns of x
##     and do not overlap.  
##
  k <- ncol(x)
  Names <- colnames(x)
#  Function to check for Names in reference
#  List   
  cols2names <- function(cols=ignore){
    if(length(cols)<1)return(character(0))
    msg <- deparse(substitute(cols))
    if(is.numeric(cols)){
      if(any(cols<1)){
        stop(msg, ' is numeric < 1')
      } 
      if(any(cols>k)){
        stop(msg, ' is numeric > ncol(x)')
      }
      colNames <- Names[cols]
    } else {
      if(is.logical(cols)){
        if((lc <- length(cols))<k){
          stop(msg, ' is logical with',
            ' length = ', lc, 
            ' < ncol(x) = ', k)
        }
        colNames <- Names[cols]
      } else {
        if(!is.character(cols)){
          stop(msg, ' is class = ', 
               class(cols), 
               '; not allowed')
        }
        if(length(igoops <- which(
          !(cols %in% Names)))>0){
            stop(msg, ' = ', 
              cols[igoops[1]], 
             ' not in names(x) = ', 
             paste(Names, collapse=', ') )
        }
        colNames <- cols
      }
      if(length(unique(colNames))<
        length(colNames)){
          stop(msg, ' contains duplicates')
      }
    }
    colNames    
#   end function to check for Names 
#   in referenceList    
  }
# Check names   
  Ignore <- cols2names(ignore)
  Factors <- cols2names(factors)
  DATES <- cols2names(Dates)
  Posix <- cols2names(POSIX)
  MS.dates <- cols2names(MSdates)
##
## test for intersection 
##  
  if(length(IFoops <- intersect(Ignore,
    Factors))>0){
      stop(IFoops[1], 
        ' is in both ignore and factors')
  }
  IgFa <- c(Ignore, Factors)
  if(length(IFDoops <- intersect(DATES, 
    IgFa))>0){
      stop(IFDoops[1], 
        ' is in Dates and either ignore', 
         ' or factors')
  }
  IgFaDa <- c(IgFa, DATES)
  if(length(IFDPoops <- intersect(Posix, 
    IgFaDa))>0){
      stop(IFDPoops[1], 
        ' is in POSIX and one of ignore', 
         ', factors, and Dates')
  }
  IgFaDP <- c(IgFaDa, Posix)
  if(length(IFDoops <- intersect(MS.dates, 
    IgFaDP))){
      stop(IFDoops[1], 
        ' is in MSdates and one of ignore',
         ', factors, Dates, and POSIX')
  }
  dontConvert <- c(IgFaDP, MS.dates)
##  
## 3.  Convert factors, Dates, POSIX, 
##        MSdates
##
  if(Trace)cat('factors\n')
#   convert factors  
  for(f in Factors){
    X[, f] <- factor(x[, f])
  }
  if(Trace)cat('Dates\n')
#  convert Dates  
  for(d in DATES){
    X[, d] <- asNumericChar(x[, d], 
      class.='Date', format.=format.)
  }
  if(Trace)cat('POSIX\n')
#  convert POSIXct   
  for(p in Posix){
    X[, p] <- asNumericChar(x[, p], 
        class.='POSIXct', format.=format.)
  }
# convert MSdates  
  if(Trace)cat('MSdates\n')
  for(m in MS.dates){
    xm <- x[, m]
    Xm <- as.numeric(xm)
    XM <- as.Date(Xm, origin=
              as.Date('1899-12-31') )
    X[, m] <- XM
  }
##
## 4.  Apply asNumericChar to all columns 
##     not in ignore, factors, Dates, or
##     POSIX.  
##
  if(Trace)cat('dontConvert\n')
  notNum <- (Names %in% dontConvert)
  numCols <- Names[!notNum]
  for(n in numCols){
    w0 <- options(warn=-1)
#    cat(colnames(x)[n], ":")
#    print(x[,n])
    xn <- asNumericChar(x[, n], 
      leadingChar=leadingChar, 
      suppressChar=suppressChar, 
          pctChar=pctChar)
    options(warn=w0$warn)
    xnNewNA <- which(is.na(xn) & 
                  !is.na(x[, n]))
    if(length(xnNewNA)>0){
      msg0 <- paste0(
        'NAs introduced by coercion ', 
         'in asNumericChar(', n, '[' )
      if(length(xnNewNA)<2){
        msg1 <- paste0(msg0, xnNewNA, '])')
      } else {
        if(length(xnNewNA)>4){
          msg1 <- paste0(msg0,  'c(', 
              paste(xnNewNA[1:4], 
                collapse=', '), 
                ', ...)])')
        } else {
          msg1 <- paste0(msg0, 'c(', 
              paste(xnNewNA, collapse=
                      ', '), ')])')
        }
      }
      warning(msg1)
    }
    X[, n] <- xn
  }
##
## 5.  Keep columns specified by keep.  
##
  if(Trace)cat('keep\n')
  kp <- rep(FALSE, k)
  names(kp) <- Names
  kp[notNum] <- TRUE
  if(is.function(keep)){
      Keep <- sapply(X[numCols], keep)
      kp[numCols[Keep]] <- TRUE
  } else {
    if(!is.null(keep)) {
      if(is.logical(keep)) {
        kp[keep] <- TRUE 
      } else {
        kp[keep] <- TRUE 
      }
    }
  }
#
#  if(missing(orderBy)){
#      orderBy <- 1:length(X)
#  }
  if((length(orderBy)>1) && all(
    !is.na(orderBy))){
      o <- do.call(order, X[orderBy])
    return(X[o, kp])
  }
  X[, kp]
}


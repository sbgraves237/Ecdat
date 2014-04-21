interpPairs <- function(object, proportion,
                        pairs=c('1'='\\.0$', '2'='\\.1$', replacement=''),
                        validProportion=0:1 ){
##
## 1.  find pairs
##
    suf1 <- grep(pairs[1], names(object), value=TRUE)
    suf2 <- grep(pairs[2], names(object), value=TRUE)
##
## 2.  sub(pairs[1:2], pairs[3], suf*)
##
    suf1. <- sub(pairs[1], pairs[3], suf1)
    suf2. <- sub(pairs[2], pairs[3], suf2)
##
## 3.  look for pairs
##
    Matches <- table(c(suf1., suf2.))
    oops <- names(Matches[Matches<2])
    Dat <- list()
    if(length(oops)>0){
        un1 <- which(suf1. %in% oops)
        un2 <- which(suf2. %in% oops)
        if(length(un1)>0){
            warning(suf1[un1[1]], ' found without a matching ',
                    pairs[2], ';  returning ', suf1[un1[1]],
                    ' as ', suf1.[un1[1]])
        }
        if(length(un2)>0){
            warning(suf2[un2[1]], ' found without a matching ',
                    pairs[1], ';  returning ', suf2[un2[1]],
                    ' as ', suf2.[un1[1]])
        }
        el1 <- c(suf1[un1], suf2[un2])
        Dat <- object[c(suf1[un1], suf2[un2])]
        names(Dat) <- c(suf1.[un1], suf2.[un2])
    }
    match2 <- names(Matches[Matches>1])
##
## 4.  interpolate
##
    for(m in match2){
        m1 <- whichAeqB(suf1., m)
        m2 <- whichAeqB(suf2., m)
        x1 <- object[[suf1[m1]]]
        x2 <- object[[suf2[m2]]]
        xm <- (x1 + proportion * (x2-x1))
        Dat[[m]] <- xm
    }
##
## 5.  Other columns of the same length?
##
#  5.1.  add proportion;  o.w., won't work with no pairs :-(
    Dat$proportion <- proportion
#  5.2  Convert to data.frame to force all to equal length ;-)
    dat <- as.data.frame(Dat)
    if(length(object)>0){
        objLen <- sapply(object, NROW)
    } else objLen <- integer(0)
#
    cols2trim <- ((objLen==nrow(dat)) &
                  !(names(object) %in% c(suf1, suf2)))
    if(any(cols2trim)){
        dat. <- cbind(dat, as.data.frame(object[cols2trim]))
    } else dat. <- dat
##
## 6.  delete undesired rows
##
    out <- ((proportion < validProportion[1]) |
            (validProportion[2] < proportion) )
    dato <- dat.[!out,, drop=FALSE]
##
##
##
## 7.  Combine with the remainder of object
##
    int <- ((names(object) %in% c(suf1, suf2)) | cols2trim)
    Out <- c(dato, object[!int])
##
## 8.  Any elements of class call?
##
    cl <- sapply(Out, class)
    for(jc in which(cl=='call')){
        xj <- eval(Out[[jc]], Out[-jc])
        Out[[jc]] <- xj
    }
##
## 9.  Done
##
    Out$proportion <- NULL
    Out
}

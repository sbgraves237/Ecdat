Extension <- function(filename, split='\\.',
                      noExtensionError=TRUE, ...){
##
## 0.  noExtensionError
##
    err <- function(...)return()
    if(noExtensionError=='warn'){
        err <- "warning"
    } else if(noExtensionError==TRUE){
        err <- "stop"
    }
##
## 1.  Create empty result
##
    nFiles <- length(filename)
    Ext <- rep(NA, nFiles)
##
## 2.  find split in filename
##
    gr <- grep(split, filename, ...)
    if(length(gr)<nFiles){
        oops <- (1:nFiles)[-gr]
        msg <- paste('filename with no extension =',
                     filename[oops[1]])
        do.call(err, list(msg))
    }
    fn <- strsplit(filename[gr], split, ...)
##
## 3.  extract extension
##
    Ext. <- sapply(fn, tail, n=1L)
##
## 4.  split at the end?
##
    nchSplit <- nchar(split)
    spd <- ('$' != substring(split, nchSplit, nchSplit))
    if(spd){
        spD <- paste(split, '$', sep='')
        sp0 <- grep(spD, filename[gr], ...)
        if(length(sp0)>0){
            Msg <- paste('filename with blank extension =',
                         filename[gr][sp0])
            do.call(err, list(Msg))
            Ext.[sp0] <- ''
        }
    }
##
## 5.  done
##
    Ext[gr] <- Ext.
    Ext
}


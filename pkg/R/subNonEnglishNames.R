subNonEnglishNames <- function(x, nonEnglishData=nonEnglishNames, ...) {
    x. <- x
    n.nE <- try(nrow(nonEnglishData))
    if(class(n.nE) == 'try-error'){
        warning('nonEnglishData not found; ',
                'if using default, try data(nonEnglishNames)')
        data(nonEnglishNames)
        n.nE <- nrow(nonEnglishData)
    }
    for(i in seq(length=n.nE)){
        x. <- sub(nonEnglishData$nonEnglish[i],
                  nonEnglishData$English[i], x., ...)
    }
    x.
}

read.testURLsFile <- function(file.='testURLresults.csv', ...){
    dat <- read.csv(file., ...)
    tm <- as.character(dat$Time)
    Tm <- strptime(tm, '%a %b %d %H:%M:%S %Y', tz='GMT')

    class(dat) <- c('testURLsFile', 'data.frame')

    dat
}

read.transpose <- function(file, header=TRUE, sep=',', ...){
##
## 1.  readLines
##
  Txt <- readLines(file)
  Nr <- length(Txt)
  if(Nr<1){
      warning('\nNo data in file ', file)
      attr(Txt, 'headers') <- character(0)
      attr(Txt, 'footers') <- character(0)
      attr(Txt, 'other') <- character(0)
      attr(Txt, 'summary') <- c(headers=0, footers=0, data=0,
                                other=0)
      return(Txt)
  }
##
## 2.  Split into fields
##
  txt <- gsub('\"', '', Txt)
  Split <- strsplit(txt, sep)
  nFields <- sapply(Split, length)
  Nfields <- max(nFields)
  Data <- which(nFields==Nfields)
##
## 3.  headers, footers, desired data, and other
##
  headers <- txt[seq(1, length=Data[1]-1)]
  dat <- Split[Data]
  nv <- nrow(dat)
  nr <- max(Data)
  footers <- txt[seq(nr+1, length=Nr-nr)]
  other <- txt[Data][-(Data-Data[1]+1)]
##
## 4.  Extract column / variable names
##
  if(header){
      h1 <- sapply(dat, '[', 1)
      h2 <- sapply(dat, '[', 2)
      Dat <- sapply(dat, '[', -(1:2))
      rownames(Dat) <- h2
  } else {
      Dat <- dat
      h1 <- rep('', nv)
      h2 <- h1
  }
##
## 5.  Numbers?
##
  dat. <- as.numeric(Dat)
  if(!any(is.na(dat.))){
      attributes(dat.) <- attributes(Dat)
  } else {
      dat. <- Dat
  }
##
## 6.  transpose, done
##
  out <- t(dat.)
  attr(out, 'headers') <- headers
  attr(out, 'footers') <- footers
  attr(out, 'summary') <- c(headers=length(headers),
      footers=length(footers), data=length(dat),
      other=length(other) )
  out
}


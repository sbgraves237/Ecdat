decryptCensusF1 <- function(x, activeCols=7){
  decrypt <- function(x){
# Delete commas (thousand separators) and footnote references
    x1 <- gsub(',', '', x)
    x2 <- strsplit(x1, ' ')
    x. <- sapply(x2, '[', 1)
    x.[x1==''] <- NA
    as.numeric(x.)
  }
  nyrs <- nrow(x)
  aC <- min(ncol(x), activeCols)
  as.data.frame(lapply(x[nyrs:1, 1:aC], decrypt))
}


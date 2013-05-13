specialCharacters <- function(x, standardCharacters =
           c(letters, LETTERS, ' ', '.', ',', '"', "'", '-')){
    x. <- strsplit(x, '')
    x2 <- sapply(x., function(x){
        x0 <- which(!(x %in% standardCharacters))
        if(length(x0)<1) 0 else x0[1]
    } )
    which(x2>0)
}

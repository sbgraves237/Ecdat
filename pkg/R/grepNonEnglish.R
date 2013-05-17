grepNonEnglish <- function(x, value=TRUE,
   standardCharacters=c(letters, LETTERS, ' ','.', ',',
       '\"', "\'", '-', '(', ')', '[', ']') ) {
    x. <- strsplit(x, '')
    x.in.sC <- function(y){
        x0 <- which(!(y %in% standardCharacters))
        if(length(x0)<1) 0 else x0[1]
    }
    x2 <- sapply(x., x.in.sC)
    x1 <- which(x2>0)
    if(value){
        return(x[x1])
    } else return(x1)
}

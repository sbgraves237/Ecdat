Date3to1 <- function(data){
## 
## 1.  check   
##  
  nc <- ncol(data)
  if(is.null(nc)){
    stop('data is not a data.frame')
  }
  if(nc != 3){
    stop('ncol(data) = ', nc, ' != 3')
  }
##
## 2.  Character vector of dates 
##
  Dt <- as.list(data)
  Dt$sep <- "-"
  Dte <- do.call(paste, Dt)
##
## 3.  Finish  
##
  as.Date(Dte)  
}

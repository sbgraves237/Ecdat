getElement2 <- function(object, name=1, default=NA, warn.NULL=TRUE){
#       get element of list;  return default if absent
##
## 1.  is.numeric(name)?
##
  if(is.numeric(name)){
    out <- ((name<1) | (length(object)<name))
  } else {
##
## 2.  name not numeric     
##    
    out <- !(name %in% names(object))
  }
##
## 3.  get object[[name]]
##
  El <- if(out) default else object[[name]]        
##
## 4.  warn.NULL?
##
  if(is.null(El) && warn.NULL && !is.null(default)){
        warning('element ', name, ' is NULL; returning default')
        El <- default
    }
##
## 5.  eval
##
  El. <- eval(El, envir=as.list(object)) 
  eval(El.)
}


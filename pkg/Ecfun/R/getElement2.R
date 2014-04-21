getElement2 <- function(object, name, default=NA, warn.NULL=TRUE){
#       get element of list;  return default if absent
    if(name %in% names(object)){
        El <- object[[name]]
    } else El <- default
#
    if(is.null(El) && warn.NULL && !is.null(default)){
        warning('element ', name, ' is NULL; returning default')
        El <- default
    }
    El
}


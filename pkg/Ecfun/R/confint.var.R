confint.sd <- function(object, parm, level=0.95, ...){
  vCI <-  confint.var(object^2, parm, level, ...)
  sqrt(vCI)
} 
  
confint.var <- function(object, parm, level=0.95, ...){
##
## 1.  is.numeric(object)
##
  if(!is.numeric(object)){
    stop('object is not numeric; is ', 
         class(object))
  }
##
## 2.  parm
##
  if(missing(parm)){
    at <- attributes(object)
    dfi <- pmatch('df', names(at))
    if(is.na(dfi)){
      stop('parm missing without a df attribute', 
           ' of object')
    }
    df. <- at[[dfi]]
  } else df. <- parm
  if(!is.numeric(df.)){
    stop('parm is not numeric; is ', 
         class(df.))
  }
##
## 3.  level
##
  if(!is.numeric(level)){
    stop('level is not numeric;  is ', 
         class(level))
  }
##
## 4.  rep to common length 
##
  no <- length(object)
  np <- length(df.)
  nl <- length(level)
  n. <- c(no, np, nl)
  n <- max(n.)
  oops <- (n%%n.)
  if(any(oops)){
    warning('The longest length of object, parm, and level ', 
            'is not a multiple of a shorter; lengths = ', 
            paste(n., collapse=', '))
  }
  obj <- rep_len(object, n)
  Df <- rep_len(df., n)
  lvl <- rep_len(level, n)
##
## 5.  alpha/2 
##  
  alph2 <- (1-lvl)/2 
##
## 6.  quantiles 
##
  Qntls <- cbind(lower=stats::qchisq(alph2, Df, lower=FALSE), 
                     upper=stats::qchisq(alph2, Df)) 
##
## 7.  CI <- (object*parm/Qntls)
##  
  CI <- (object*Df/Qntls)
  attr(CI, 'level') <- level
  return(CI)  
}
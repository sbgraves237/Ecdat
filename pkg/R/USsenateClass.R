USsenateClass <- function(x, senate=readUSsenate()){
##
## 1.  subset x houseSenate
##
  xSen <- (x$houseSenate=="Sen")
  x.Sen <- x[xSen,]
  nx <- nrow(x.Sen)
##
## 2.  Key
##
  keyx <- with(x.Sen, paste(state, surname, sep=":"))
  keysen <- with(senate, paste(state, surname, sep=":"))
##
## 3.  find
##
  used <- (keysen %in% keyx)
  tabs <- table(keysen[used])
  if(any(tabs>1)){
#   State with both senators having the same last name
      warning("USsenateClass may fail for a state where both senators",
              " have the same last name:", names(tabs)[tabs>1][1])
  }
  senu <- senate[used,]
  keyu <- keysen[used]
  rownames(senu) <- keyu
#
  found <- (keyx %in% keyu)
  Class <- senu[keyx[found], 'Class']
#
  out <- character(nx)
  out[found] <- Class
#
  if(any(!found)){
      oops <- which(!found)
      for(o in oops){
          sto <- as.character(x.Sen$state[o])
          senun <- senate[!used, ]
#
          clo <- senun[senun$state==sto, 'Class']
          out[o] <- paste(clo, collapse=' or ')
      }
  }
##
## 4.  prepare for output
##
  Dist <- as.character(x$District)
  Dist[xSen] <- out
#
  Dist
}


\name{matchName}
\alias{matchName}
\alias{matchName1}
\title{
  Match surname and givenName in a table
}
\description{
  Use \code{\link{parseName}} to split a name into 
  \code{surname} and \code{givenName}, the look for matches 
  in \code{table}.  
}
\usage{
matchName(x, data, Names=1:2, 
          nicknames=matrix(character(0), 0, 2), 
          namesNotFound="attr.replacement", ...)
matchName1(x1, data, name=data[, 1],     
          nicknames=matrix(character(0), 0, 2), ...)
}
\arguments{
  \item{x}{
    One of the following:  
    
    \itemize{
      \item A character matrix or \code{data.frame} with the 
        same number of rows as \code{data}.  The best partial
        match is sought in \code{Names}.  The algorithm 
        stops when a unique match is found;  any remaining 
        columns of \code{x} are then ignored.  Any 
        \code{nicknames} are ignored for the first column 
        but not for subsequent columns.   
      \item A character vector whose length matches the number 
        of rows of \code{data}.  This will be replaced by 
        \code{parseName(x)}.
    }      
  }
  \item{data}{
    a character matrix or a \code{\link{data.frame}}.  If 
    \code{surname} and \code{givenName} are character vectors of 
    names, their length musth match the number of rows of 
    \code{data}.  
  }
  \item{Names}{
    One of the following in which matches for \code{x} will 
    be sought:  

    \itemize{ 
      \item A character vector or matrix or a \code{data.frame} 
        for which \code{NROW(Names) == nrow(data)}.  
% ?     \item A character vector whose length matches the number 
% ?      of rows of \code{data}.  This will be replaced by 
% ?       \code{parseName(Names)}.  
      \item Something to select columns of \code{data} to 
        produce a character vector or matrix or 
        \code{data.frame} via \code{data[, Names]}.  In this 
        case, accents will be stripped using 
        \code{\link{subNonStandardNames}}.  
    }
  }
  \item{nicknames}{
    a character matrix with two columns, each row giving a pair 
    of names like "Pete" and "Peter" that should be regarded as 
    equivalent if no exact match(es) is(are) found.   
  }
  \item{\dots}{ 
    optional arguments passed to \code{subNonStandardNames}
  }
  \item{x1}{ 
    a character vector of names to match \code{name}. 
    
    NOTE:  \code{matchName} calls \code{subNonStandardNames}, 
    but \code{matchName1} does not.  Thus, \code{x1} is 
    assumed to NOT to contain characters not in standard 
    English.  
  }
  \item{name}{
    A character vector or matrix for which \code{NROW(name)}
    == \code{nrow(data)}.  

    NOTE:  \code{matchName} calls \code{subNonStandardNames}, 
    but \code{matchName1} does not.  Thus, \code{name} is 
    assumed to NOT to contain characters not in standard 
    English.  
  }
  \item{namesNotFound}{
    character vector passed to \code{subNonStandardNames} and used 
    to compute any "namesNotFound" attribute of the object returned 
    by \code{parseName}.  
  }
}
\details{
  *** 1.  matchName(x, data, Names, nicknames, ...):  
  
  1.1.  if(length(dim(x)<2))x <- parseName(x, ...)  
  
  1.2.  x1 <- matchName1(x[, 1], cata, Names[1], ...)
  
  1.3.  For any component i of x1 with multiple rows, 
  let x1i <- matchName1(x[i, 2], x1[[i]], Name[-1], 
  nicknames=nicknames, ...).  If nrow(x1i)>0, 
  x1[[i]] <- x1i;  else leave unchanged.  
  
  1.4.  return x1 
  
  ===========
  
  *** 2.  matchName1(x1, data, name, nicknames, ...):  
  
  2.1.  If name indicates a column of data, replace 
  with data[, name].  
  
  2.2.  xsplit <- strsplit(x1, ' ')
  
  2.3.  nx <- length(x1); xlist <- vector(nx, mode='list')
  
  2.4.  for(j in 1:nx):   
  
  2.5.  xj <- xplit[[j]] 
  
  2.6.  let jd = the subset of names that match xj or 
  subNonStandardNames(xj) or nicknames of xj;  xlist[j] <- jd.  
  
  2.7.  return xlist 
}
\value{
  \code{matchName} returns a list of the same length as \code{x}, 
  each of whose components is object obtained as a subset of rows 
  of \code{data} or \code{NULL} if no acceptable matches are found.   
  The list may have an attribute "namesNotFound" as determined per 
  the argument of that name.  
  
  \code{matchNames1} returns a list of vectors of integers for 
  subsets of \code{data} matching \code{x1}.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{parseName}}
  \code{\link{subNonStandardNames}}
}
%\references{}
\examples{
##
## 1.  Names to match exercising many possibile combinations 
##     of surname with 0, 1, >1 matches possibly after 
##     replacing with subNonStandardNames 
##     combined with possibly multiple givenName combinations 
##     with 0, 1, >1 matches possibly requiring replacing with 
##     subNonStandardNames or nicknames 
##
# NOTE:  "-" could also be "e" with an accent;  
#    not included with this documentation, because 
#    non-English characters generate warnings in standard tests.  
Names2mtch <- c("Andr_ Bruce C_rdenas", "Dolores Ella Feinstein",
           "George Homer", "Inez Jane Kappa", "Luke Michael Noel", 
           "Oscar Papa", "Quincy Ra_l Stevens", 
           "Thomas U. Vel_zquez", "William X. Young", 
           "Zebra")
##
## 2.  Data = matrix(..., byrow=TRUE) to exercise the combinations 
##     the combinations from 1 
##
Data1 <- matrix(c("Feld", "Don", "789", 
                  "C_rdenas", "Don", "456", 
                  "C_rdenas", "Andre B.", "123", 
                  "Smith", "George", "aaa", 
                  "Young", "Bill", "369"), 
                ncol=3, byrow=TRUE)
Data1. <- subNonStandardNames(Data1)                
##
## 3.  matchName1
##        
parceNm1 <- parseName(Names2mtch)
match1.1 <- matchName1(parceNm1[, 'surname'], Data1.)

# check
match1.1s <- vector('list', 10)
match1.1s[[1]] <- 2:3
match1.1s[[9]] <- 5
names(match1.1s) <- parceNm1[, 'surname'] 
\dontshow{stopifnot(}
all.equal(match1.1, match1.1s)
\dontshow{)}

##
## 4.  matchName1 with name = multiple columns 
##
match1.2 <- matchName1(c('Cardenas', 'Don'), Data1., 
                       name=Data1.[, 1:2])

# check 
match1.2a <- list(Cardenas=2:3, Don=1:2)
\dontshow{stopifnot(}
all.equal(match1.2, match1.2a)
\dontshow{)}

##
## 5.  matchName 
##
nickNames <- matrix(c("William", "Bill"), 1, byrow=TRUE)

match1 <- matchName(Names2mtch, Data1, nicknames=nickNames)
                  
# check 
match1a <- list("Cardenas, Andre Bruce"=Data1[3,, drop=FALSE ], 
                "Feinstein, Dolores Ella"=NULL, 
                "Homer, George"=NULL, "Kappa, Inez Jane"=NULL, 
                "Noel, Luke Michael"=NULL, "Papa, Oscar"=NULL, 
                "Stevens, Quincy Raul"=NULL, 
                "Velazquez, Thomas U."=NULL, 
                "Young, William X."=Data1[5,, drop=FALSE], 
                "Zebra"=NULL)
\dontshow{stopifnot(}
all.equal(match1, match1a)
\dontshow{)}
##
## 6.  namesNotFound 
##
tstNotFound <- matchName('xx_x', Data1)

# check 
tstNF <- list('xx_x'=NULL)
attr(tstNF, 'namesNotFound') <- 'xx_x'
\dontshow{stopifnot(}
all.equal(tstNotFound, tstNF)
\dontshow{)}

##
## 7.  matchName(NULL) to simplify use 
##
mtchNULL <- matchName(NULL, Data1)
\dontshow{stopifnot(}
all.equal(mtchNULL, NULL)
\dontshow{)}
}
\keyword{manip}

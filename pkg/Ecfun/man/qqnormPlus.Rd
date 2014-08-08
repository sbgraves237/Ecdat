\name{qqnormPlus}
\alias{qqnormPlus}
\title{
  Normal Probability Plot with a histogram, density estimate and boxplot 
}
\description{
  Create a normal probability plot with optionally a histogram, a
  density estimate and a boxplot on the same display.  
}
\usage{
qqnormPlus(y, xlim, ylim, main = "",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       datax = TRUE, histargs, densityargs, boxplotargs, ...)
}
\arguments{
  \item{y}{
    a numeric vector for which a graphical analysis is desired.  
  }
  \item{xlim}{
    The limits for the theoretical quantiles.  If \code{datax = TRUE} 
    (the default for \code{qqnormPlus} but not for \code{qqnorm}), 
    these are the limits for the vertical axis.  These are provided 
    to allow users to expand the space between the qqnorm points and 
    the display of a histogram, density, and boxplot.  
  }
  \item{ylim}{
    The data limits of the plot.  If \code{datax = TRUE} (the default
    for \code{qqnormPlus} but not for \code{qqnorm}), 
    these are the limits for the horizontal axis.  
  }
  \item{main}{
    plot title 
  }
  \item{xlab}{
    axis label corresponding to \code{xlim}. 
  }
  \item{ylab}{
    axis label corresponding to \code{ylim}.    
  }
  \item{datax}{
    The \code{datax} argument of \code{\link{qqnorm}}:  If TRUE, the
    data are displayed on the horizontal rather than the vertical axis.
    (The default value for \code{datax} is the opposite of that for
    \code{\link{qqnorm}}.)
  }
  \item{histargs, densityargs, boxplotargs}{
    lists of optional arguments for \code{\link[MASS]{truehist}}, 
    \code{density}, and \code{boxplot}, respectively, with one 
    optional additional list component:  
    
    
    
    = 0.2 for truehist and density and 0.07 for boxplot???
    
    
    \code{histargs}, \code{densityargs}, and \code{boxplotargs} = 
    NULL or NA to suppress the histogram, density estimate or 
    boxplot.  
  }
  \item{\dots}{
    Optional plot parameters to pass to \code{\link{plot}} with 
    the output of \code{qqnorm(y, plot.it=FALSE, datax=datax)}.      
  }
}
\details{




}
\value{




}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{qqnorm}},
  \code{\link[MASS]{truehist}}
  \code{\link{density}}
  \code{\link{boxplot}}
  \code{\link[TeachingDemos]{subplot}}
}
\references{
  \href{https://en.wikipedia.org/wiki/Iris_flower_data_set}{
    Wikipedia, "Iris flower data set"
  }
  
  Graves, Spencer (2007) "Bayes' Rule of Information and 
  Monitoring in Manufacturing Integrated Circuits", ch. 7 in 
  Bianca M. Colosimo and Enrique del Castillo, \emph{
  Bayesian Process Monitoring, Control and Optimization}, 
  Chapman and Hall / CRC.  [Includes qqnorm plots of 
  yield, defect rate = (-log(yield)), and log(defect 
  rate).  The latter corresponds to the cloglog (complementary
  log-log transformation, corresponding to that Poisson 
  defect model common in the fabrication of integrated 
  circuits;  see \code{\link{family}}.  The image shows a
  mixture of distributions, with a higher varability for the 
  largest defect rates, with cloglog(yield) being more normal 
  than either yield or defect rate.]
}
\examples{
##
## 1.  Simple plot 
##
set.seed(1)
r50 <- rnorm(50)
qqnormPlus(r50)

##
## 2.  datax=FALSE 
##
qqnormPlus(r50, datax=FALSE)

##
## 3.  Turning off histogram, density, boxplot
##     qqnorm should be the same as example 1 
##     without the histogram, density estimate, and boxlplot
##
qqnormPlus(r50, histargs=NULL, densityargs=NA, boxplotargs=NA)

##
## 4.  Swap locations of histogram, density, boxplot 
##
qqnormPlus(r50, histargs=list(

))

##
## 5.  outliers 
##

#   ???


library(MASS)
data(UScrime)


library(BMA)
data(race)
qqnorm(race[, 'Distance'])
qqnorm(race[, 'Climb'])
qqnorm(race[, 'Time'])


library(TeachingDemos)
data(outliers)
qqnorm(outliers)
qqline(outliers)
hist(outliers)


library(DescTools)
Outlier(d.pizza$temperature, na.rm=TRUE)

##
## 6.  OUtliers in residuals 
##
library(car)
qqPlot(lm(prestige ~ income + education + type, data=Duncan), 
       envelope=.99)


outlierTest(lm(prestige ~ income + education, data=Duncan))




##
## 7.  Skewness 
## 


##
## 8.  kurtosis 
##


##
## 9.  Multimodal:  datasets::iris 
##


## 
## 10.  A need for transformations:  dollars -> log 
##
# a stock price dataset???  


##
## 11.  A need for transformations:  
##      See Graves (2007)
##




}
\keyword{plot}

\name{F1.PikettySaez}
\docType{data}
\alias{F1.PikettySaez}
\title{ Income Inequality in the US  }
\description{
  Data on quantiles of the income distribution in the United States.
  This combines US Census Table F-1 for the central qualitiles with
  Piketty and Saez.  The latter are adjusted to 2011 dollars to match
  the former using implicit price deflators from
  \url{MeasuringWorth.com}.
}
\usage{data(F1.PikettySaez)}
\format{A dataframe containing :
  \describe{
    \item{Year}{ numeric year 1947:2011 }
    \item{Number.thousands}{ number of families in the US }
    \item{quintile1, quintile2, median, quintile3, quintile4, p95 }{
      quintile1, quintile2, quintile3, quintile4, and p95 are the
      indicated quantiles of the distribution of family income from US
      Census Table F-1.  The media is computed as the geometric mean of
      quintile2 and quintile3.  This is accurate to the extent that the
      lognormal distribution adequately approximates the central 20
      percent of the income distribution, which it should for most
      practical purposes.
    }
    \item{P90, P95, P99, P99.5, P99.9, P99.99}{
      The indicated quantiles of family income per Piketty and Saez
    }
    \item{realGDP.M, GDP.Deflator, PopulationK, realGDPperCap }{
      real GDP in millions, GDP implicit price deflators, US population
      in thousands, and real GDP per capita, according to
      \url{MeasuringWorth.com}.
    }
    \item{P95IRSvsCensus}{
      ratio of the estimates of the 95th percentile of distributions of
      family income from the Piketty and Saez analyis of data from the
      Internal Revenue Service (IRS) and from the US Census Bureau.

      The IRS has ranged between 72 and 98 percent of the Census Bureau
      figures for the 95th percentile of the distribution, with this
      ratio averaging around 75 percent since the late 1980s.  However,
      this systematic bias is modest relative to the differences between
      the different quantiles of interest in this combined dataset.
    }
    \item{personsPerFamily}{
      average number of persons per family using the number of families
      from US Census Table F-1 and the population from
      \url{MeasuringWorth.com}.
    }
    \item{realGDPperFamily}{
      \code{personsPerFamily * realGDPperCap}
    }
    \item{mean.median}{
      ratio of realGDPperFamily to the median.  This is a measure of
      skewness and income inequality.
    }
  }
}
\details{
  For details on how this \code{data.frame} was created, see
  "F1.PikettySaez.R" in \code{system.file('scripts', package='fda')}.
  This provides links for files to download and R commands to read those
  files and convert them into an updated version of
  \code{F1.PikettySaez}.  This is a reasonable thing to do if it is more
  than 2 years since \code{max(F1.PikettySaez$year)}.
}
\source{
  United States Census Bureau, Table F-1. Income Limits for Each Fifth
  and Top 5 Percent of Families, All Races,
  \url{http://www.census.gov/hhes/www/income/data/historical/families},
  accessed January 9, 2013.

  Thomas Piketty and Emmanuel Saez (2003) "Income Inequality in the
  United States, 1913-1998", Quarterly Journal of Economics, 118(1)
  1-39, \url{http://elsa.berkeley.edu/~saez}, accessed January 9, 2013.

  Louis Johnston and Samuel H. Williamson (2011) "What Was the U.S. GDP
  Then?" MeasuringWorth, \url{http://www.measuringworth.org/usgdp},
  accessed January 9, 2013.
}
%\references{}
%\seealso{}
\examples{
##
## Rato of IRS to census estimates for the 95th percentile
##
data(F1.PikettySaez)
plot(P95IRSvsCensus~Year, F1.PikettySaez, type='b')
# starts ~0.74, trends rapidly up to ~0.97,
# then drifts back to ~0.75
abline(h=0.75)
abline(v=1989)
# check
sum(is.na(F1.PikettySaez$P95IRSvsCensus))
# The Census data runs to 2011;  Pikety and Saez runs to 2010.
quantile(F1.PikettySaez$P95IRSvsCensus, na.rm=TRUE)
# 0.72 ... 0.98

##
## Persons per Family
##

plot(personsPerFamily~Year, F1.PikettySaez, type='b')
quantile(F1.PikettySaez$personsPerFamily)
# ranges from 3.72 to 4.01 with median 3.84
#  -- almost 4

##
## GDP per family
##
plot(realGDPperFamily~Year, F1.PikettySaez, type='b', log='y')

##
## Plot first quintile, median, mean,
##            99th, 99.9th and 99.99th percentiles
##
plotCols <- c(3, 5, 21, 11, 13:14)
kcols <- length(plotCols)
plotColors <- c(1:6, 8:13)[1:kcols] # omit 7=yellow
plotLty <- 1:kcols

matplot(F1.PikettySaez$Year, F1.PikettySaez[plotCols]/1000,
        log='y', type='l', col=plotColors, lty=plotLty)

#*** Growth broadly shared 1947 - 1970, then began diverging
#*** The divergence has been most pronouced among the top 1%
#*** and especially the top 0.01%

##
## Growth rate by quantile 1947-1970 and 1970 - present
##
keyYears <- c(1947, 1970, 2010)
(iYears <- which(is.element(F1.PikettySaez$Year, keyYears)))

(dYears <- diff(keyYears))
kk <- length(keyYears)
(lblYrs <- paste(keyYears[-kk], keyYears[-1], sep='-'))

(growth <- sapply(F1.PikettySaez[iYears,], function(x, labels=lblYrs){
    dxi <- exp(diff(log(x)))
    names(dxi) <- labels
    dxi
} ))

# as percent
(gr <- round(100*(growth-1), 1))

# The average annual income (realGDPperFamily) doubled between
# 1970 and 2010 (increased by 101 percent), while the median household
# income increased only 23 percent.

##
## Income lost by each quantile 1970-2010
## relative to the broadly shared growth 1947-1970
##
(lostGrowth <- (growth[, 'realGDPperFamily']-growth[, plotCols]))
# 1947-1970:  The median gained 20% relative to the mean,
#           while the top 1% lost ground
# 1970-2010:  The median lost 79%, the 99th percentile lost 29%,
#           while the top 0.1% gained

(lostIncome <- (lostGrowth[2, ] *
                F1.PikettySaez[iYears[2], plotCols]))
# The median family lost $39,000 per year in income
# relative to what they would have with the same economic growth
# broadly shared as during 1947-1970.
# That's slightly over $36,500 per year = $100 per day

(grYr <- growth^(1/dYears))
(grYr. <- round(100*(grYr-1), 1))




##
## matplot
##


}
\keyword{datasets}

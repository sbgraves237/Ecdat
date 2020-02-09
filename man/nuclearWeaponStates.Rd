\name{nuclearWeaponStates}
\docType{data}
\alias{nuclearWeaponStates}
\title{Nations with nuclear weapons}
\description{
  Data on the 9 nuclear-weapon states as of 
  April 2019.  
}
\usage{data(nuclearWeaponStates)}
\format{A dataframe containing :
  \describe{
    \item{nation}{
      The name of the country (character).  
      The former USSR is listed here as Russia.  
    }
    \item{ctry}{
      \href{https://en.wikipedia.org/wiki/ISO_3166-1#cite_note-iso3166-info-1}{ISO 31661-} alpha-2 two-letter country codes 
      (character).}
    \item{firstTest}{
      Date of first test of a nuclear weapon.  
      
      For Israel, which has not publicly acknowledged
      that it has nuclear weapons, this uses the Date 
      of the 
      \href{https://en.wikipedia.org/wiki/Vela_Incident}{Vela Incident}.  
    }
    \item{firstTestYr}{
      \code{lubridate::decimal_date(firstTest)}
    }
    \item{yearsSinceLastFirstTest}{
      \code{c(NA, diff(firstTestYr))}
    }
    \item{nuclearWeapons}{
      number of nuclear weapons 
    }
    \item{nYieldNA, nLowYield, nMidYield, nHighYield}{
      number of weapons for which the yield in 
      \code{(nYieldNA)} = unknown or variable, 
      \code{(nLowYield)} = at most 15 kt 
      (kilotons), the size of the Hiroshima 
      bomb, \code{(nMidYield)} = greater than 15 
      but less that 50 kt, and \code{(nHighYield)} 
      = at least 50 kt.  
    }
    \item{popM, popYr}{
      \code{popM} = estimated population in millions 
      for year \code{popYr}, per the Wikipedia 
      article for the indicated country on 2020-02-05.  
    }
    \item{GDP_B,	GDPyr}{
      \code{GDP_B} = nominal Gross Domestic Product 
      in billions of US dollars for year 
      \code{GDPyr}, per the Wikipedia article for 
      the indicated country on 2020-02-05. 
    }
  }
}
\source{
  Overview from \href{https://www.ploughshares.org/world-nuclear-stockpile-report}{World Nuclear Weapon Stockpile}
  
  \code{firstTest} from \href{https://en.wikipedia.org/wiki/List_of_states_with_nuclear_weapons}{Wikipedia, "List of states with nuclear weapons"}

  US from \href{https://www.tandfonline.com/doi/pdf/10.1080/00963402.2018.1438219?needAccess=true}{Hans M. Kristensen & Robert S. Norris (2018) United States nuclear forces,2018, Bulletin of the Atomic Scientists, 74:2, 120-131, DOI: 10.1080/00963402.2018.143821}
  
  Russia from \href{https://www.tandfonline.com/doi/pdf/10.1080/00963402.2019.1580891?needAccess=true}{Hans M. Kristensen & Matt Korda (2019) Russian nuclear forces, 2019, Bulletin of the Atomic Scientists, 75:2, 73-84, DOI: 10.1080/00963402.2019.1580891}
  
  UK from \href{https://journals.sagepub.com/doi/pdf/10.1177/0096340213493260}{Robert S. Norris and Hans M. Kristensen (2013) The British nuclear stockpile, 1953-2013, Bulletin of the Atomic Scientists, 69:4, 69-75s}
  
  France from \href{https://journals.sagepub.com/doi/pdf/10.2968/064004012}{Robert S. Norris & Hans M. Kristensen (2008) French nuclear forces, 2008, Bulletin of the Atomic Scientists, 64:4, 52-54, 57}  
  
  China from \href{https://www.tandfonline.com/doi/pdf/10.1080/00963402.2018.1486620?needAccess=true}{Hans M. Kristensen & Robert S. Norris (2018) Chinese nuclear forces, 2018,Bulletin of the Atomic Scientists, 74:4, 289-295}
  
  India from \href{https://www.tandfonline.com/doi/pdf/10.1080/00963402.2017.1337998?needAccess=true}{ Hans M. Kristensen & Robert S. Norris (2017) Indian nuclear forces, 2017,Bulletin of the Atomic Scientists, 73:4, 205-209}
  
  Israel from \href{https://journals.sagepub.com/doi/pdf/10.1177/0096340214555409}{Hans M. Kristensen and Robert S. Norris (2014) Israeli nuclear weapons, 2014, Bulletin of the Atomic Scientists, 70:6, 97-115}
  
  Pakistan from \href{https://www.tandfonline.com/doi/pdf/10.1080/00963402.2018.1507796?needAccess=true}{Hans M. Kristensen, Robert S. Norris & Julia Diamond (2018)Pakistani nuclear forces, 2018, Bulletin of the Atomic Scientists, 74:5, 348-358}
  
  North Korea from \href{https://www.tandfonline.com/doi/pdf/10.1080/00963402.2017.1413062?needAccess=true}{ Hans M. Kristensen & Robert S. Norris (2018) North Korean nuclear capabilities, 2018, Bulletin of the Atomic Scientists, 74:1, 41-51}
}
\examples{
data(nuclearWeaponStates)
plot(yearsSinceLastFirstTest~firstTest, 
    nuclearWeaponStates, type='h', xlab='', ylab='')
with(nuclearWeaponStates, 
  text(firstTest, yearsSinceLastFirstTest, ctry))
}
\keyword{datasets}

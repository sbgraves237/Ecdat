# to fix: 
# non-ASCII input and no declared encoding

#(USincarc <- dir('man', 'USincarc', full.names = TRUE))
#USinca <- readLines(USincarc)

#(pblms <- which(grepl("[^ -~]", USinca)))
#USinca[pblms]
# It works!!!

# for "Number of redirects hit maximum amount":  
#library(magrittr)
#trace_redirects <- function(url) {
#  httr::GET(url)$all_headers %>%
#    lapply(function(x) x$headers$location) %>%
#    unlist() %>%
#    unique()
#}
#str(CAed <- trace_redirects('https://www.cde.ca.gov'))
# NULL 

# File > "Open Project..." > [Package folder]

# https://r-pkgs.org/release.html

#20.1.  Pick a version number:  
#       in DESCRIPTION: must be later than any version on CRAN
#20.3.  The submission process
#       Store submission comments in cran-comments.md
#20.3.1.  Test environments

# devtools::check_win_*()
dir()

devtools::check()
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()

# check the winbuilder queue

(wbqueue <- foghorn::winbuilder_queue())
(wbqueue2 <- foghorn::winbuilder_queue())

# devtools::check_rhub()
# rhub::check_for_cran()
#This function is deprecated and defunct since rhub v2.
#Please see `?rhubv2` on transitioning to the new rhub functions.

#rhubv2::rhub_setup()
#Error in loadNamespace(x) : there is no package called ‘rhubv2’
#devtools::rhub_setup()
#Error: 'rhub_setup' is not an exported object from 'namespace:devtools'

#Notes:
#  • The workflow file must be added to the default branch of the GitHub repository.
#• GitHub actions must be enabled for the repository. They are disabled for forked
#repositories by default.

#Next steps:
#  • Add the workflow file to git using `git add <filename>`.
#• Commit it to git using `git commit` (if not committed already).
#• Push the commit to GitHub using `git push` (if not pushed already).
#• Call rhub::rhub_doctor() to check that you have set up R-hub correctly.
#• Call rhub::rhub_check() to check your package.

#rhub::rhub_doctor()
# ... WOOT! You are ready to run rhub::rhub_check() on this package.
#rhub::rhub_check()
# 2024-11-10: This produced a list of 26 different platforms
# with no obvious indication of a recommended default other 
# than 0 or 1:26. I chose the former. 

#20.3.3.  Reverse dependencies

# install.packages("revdepcheck")
#devtools::install_github('r-lib/revdepcheck')

revdepcheck::revdep_reset()

revdepcheck::revdep_check(num_workers = 4)

# local on parent to package: 
# R CMD build Ecdat 
# R CMD check --as-cran Ecdat_*.tar.gz

#20.4.  Update README.md and NEWS.md (and cran-comments.md if needed?)

#20.5.  Submit to CRAN

library(devtools)
spell_check()

release()

#20.6.  Update the version number for the future 
#20.7.  Publicise
 
#20.1.  Pick a version number:  in DESCRIPTION > what's on CRAN

#20.2.  Run & document R CMD check 
# 2.1-0 because this is to match the move of 
# the database maintenance to wu.ut.at

#20.3.  Check CRAN policies
#20.4.  Update README.md and NEWS.md 
#20.5.  Submit to CRAN
#20.6.  Update the version number for the future 
#20.7.  Publicise

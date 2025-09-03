For Ecdat 0.4.7: 

CRAN checks on Ecdat_0.4.6.tar.gz identified 39 "(possibly) invalid URLs". I copied all into a text file and asked `browseURL` to open each. They all opened fine in my default browser. 

However, that also identified a problem with the "BugReports" field I added to DESCRIPTION. I fixed that in this version. 

Then I reran all the checks. No errors nor warnings nor substantive notes, apart from "(possibly) invalid URLs".   
Tested on R-release on GitHub Action (Windows, Mac, Ubuntu), devtools::check_win_release _devel, _oldrelease. 

2025-09-03. 
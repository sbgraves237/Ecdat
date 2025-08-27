No errors nor warnings nor substantive notes.  
Tested on R-release on GitHub Action (Windows, Mac, Ubuntu), devtools::check_win_release _devel, _oldrelease, plus rhub. Rhub Fedora Linux, R-devel found 2 possibly invalid URLs that have worked in numerous other tests.  Revdep found no problems.  

devtools::check_win_release _devel, _oldrelease and local R CMD check --as-cran found several "(possibly) invalid URLs". I checked all that were listed and fixed those that were actually invalid. 

These checks continued to find "(possibly) invalid URLs". However, all that remained worked for me. 

2025-08-27. 
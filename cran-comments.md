Please excuse: I fixed the "Lost braces" before I submitted the package but failed to rebuild it, so the version submitted included that error. 

I checked the "(possibly) invalid URLs:" The ones with "Message: Too Many Requests" were all Wikimedia foundation URLs. I manually copied the others into a browser and confirmed that they work. 

Thanks, 
Spencer Graves
 "2025-08-27 16:18:33 CDT"

##########
No errors nor warnings nor substantive notes.  
Tested on R-release on GitHub Action (Windows, Mac, Ubuntu), devtools::check_win_release _devel, _oldrelease, plus rhub. Rhub Fedora Linux, R-devel found 2 possibly invalid URLs that have worked in numerous other tests.  Revdep found no problems.  

devtools::check_win_release _devel, _oldrelease and local R CMD check --as-cran found several "(possibly) invalid URLs". I checked all that were listed and fixed those that were actually invalid. 

These checks continued to find "(possibly) invalid URLs". However, all that remained worked for me. 

2025-08-27. 
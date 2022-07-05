## 2022-07-01: 
* Needed to fix a problem with the Ecfun package that reads a file in Ecdat.
* Winbuilder was unable to access 11 different URLs with R-devel and 9 with R-release and oldrelease;  all "problem URLs" responded fine to a manual check.  
* GitHub Action ran clean on 5 different platforms (Win, maOS, and Ubuntu-release and Ubuntu with oldrel and dev.)
* Rhub said "OK"
* Revdepcheck reported no problems. 

## 2022-06-13: 
* Revdepcheck found 0 new problems.  
* Tested on GitHub Action: Win-latest (release), macOS-latest (release), ubuntu-20.04 (release, devel, and oldrel-1):  All clean. 
* rhub and Winbuilder could not find URLs that I found manually 
  and about which other versions did not complain.  
  Winbuilder (current) issued 'Warning: <table> attribute "width" not allowed for HTML5'.  I asked about this on r-pkg-devel and was told it was a known problem, and I should not worry about it.  
  
* The CRAN check on "r-oldrel-macos-arm64" complained, "checking installed package size ... NOTE:  installed size is 5.3Mb; sub-directories of 1Mb or more: data 4.7Mb".  I manually read and resaved with different compression options all *.rda files.  The "Info" on that data directory now says it is 2,730,913 bytes (3 MB on disk)".  I hope this will continue to be accepted.   

##2020-11-02
* fixed the URL with blanks replaced by "\%20" and checked:  This worked.  

##2020-10-23:  
* Developed primarily on x86_64-apple-darwin17.0 (64-bit) under: macOS Catalina 10.15.7
* Tested on 
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran
*	Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Winbuilder:  x86_64-w64-mingw32 (64-bit)

** ALL complained only about several URLs, which worked for me when I tried them.  

* Revdepcheck found 0 new problems.  

At
https://cran.rstudio.com//web/checks/check_results_Ecdat.html 

https://www.r-project.org/nosvn/R.check/r-devel-linux-x86_64-fedora-clang/Ecdat-00check.html

says:  
checking Rd cross-references ... NOTE
Undeclared packages ‘plm’, ‘gdata’, ‘carData’, ‘wooldridge’ in Rd xrefs

I have eliminated all \code{\link[gdata]{...}} references but retained some \code{gdata} references to document changes.  I've retained links like \code{\link[plm]{...}} \code{\link[carData]{...}} and \code{\link[wooldridge]{...}} to document other potentially related help pages.  I cannot find any documentation that tells me what if anything I should do to differently with these links.  




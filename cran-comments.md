## 2022-06-13: 
* Tested on GitHub Action: Win-latest (release), macOS-latest (release), ubuntu-20.04 (release, devel, and oldrel-1):  All clean. 
* Revdepcheck found 0 new problems.  

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




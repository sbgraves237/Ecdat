##2020-02-08 / -10:  
* All tests and revdepcheck identified "no problems".  
* Included current macOS, rhub x 3 and Win-Builder x 2.

##In more detail:  
* Tested OK on local macOS 10.15.2, R 3.6.2.  
* rhub in Windows, Ubuntu and Fedora all tested OK except for 3 packages unavailable to check Rd xrefs.  
* Win-Builder OK on R 3.6.2 and R-devel.  
* Travis-CI seems not to be functioning, at least for me.    

## Test environments: 
* local macOS 10.15.2, R 3.6.2;  current versions of everything else:  Tested OK 2020-02-09.  
* rhub in Windows, Ubuntu and Fedora all tested OK except for "Packages unavailable to check Rd xrefs: ‘plm’, ‘carData’, ‘wooldridge’".  2020-02-09.  
* Win-builder:  R version 3.5.3 (2019-03-11), 3.6.1 (2019-07-05), and R Under development (unstable) (2019-10-29 r77335), all using platform: x86_64-w64-mingw32 (64-bit)
* Travis-CI:  R version 3.6.1 (2017-01-27), Platform: x86_64-pc-linux-gnu (64-bit), Running under: Ubuntu 16.04.6 LTS

## R CMD check results
There were no ERRORs, WARNINGs, nor NOTES.  






###########

2019-09-27:  Tested with devtools::release() under:  

> sessionInfo()
R version 3.6.1 (2019-07-05)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Mojave 10.14.6

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods  
[7] base     

loaded via a namespace (and not attached):
[1] compiler_3.6.1 tools_3.6.1   

NO ERRORS nor WARNINGS in Ecdat tested locally nor anywhere else that I could see.  

revdepcheck::evdep_check() generated error messages I did not understand and chose to ignore.  
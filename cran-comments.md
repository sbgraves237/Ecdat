##2020-02-08 / -10:  
* All tests and revdepcheck identified "no problems".  
* Included current macOS, rhub x 3 and Win-Builder x 2.
* All CRAN checks OK except "r-release-windows-ix86+x86_64", and that failed, because "there is no package called 'leaps'".
***-->> THAT'S A PROBLEM WITH "r-release-windows-ix86+x86_64", NOT Ecdat.  

revdepcheck:  OK:  16;  BROKEN:  0
BUT the individual checks show E:1 for gamclass, which, however, was not on CRAN when I checked immediately after running "revdepcheck".  

##In more detail:  
* Tested OK on local macOS 10.15.2, R 3.6.2.  
* rhub in Windows, Ubuntu and Fedora all tested OK except for 3 packages unavailable to check Rd xrefs.  
* Win-Builder OK on R 3.6.2 and R-devel.  
* Travis-CI seems not to be functioning, at least for me.    
* revdepcheck results copied below.  

## Test environments: 
* local macOS 10.15.2, R 3.6.2;  current versions of everything else:  Tested OK 2020-02-09.  
* rhub in Windows, Ubuntu and Fedora all tested OK except for "Packages unavailable to check Rd xrefs: ‘plm’, ‘carData’, ‘wooldridge’".  2020-02-09.  
* Win-builder:  R version 3.5.3 (2019-03-11), 3.6.1 (2019-07-05), and R Under development (unstable) (2019-10-29 r77335), all using platform: x86_64-w64-mingw32 (64-bit)
* Travis-CI:  R version 3.6.1 (2017-01-27), Platform: x86_64-pc-linux-gnu (64-bit), Running under: Ubuntu 16.04.6 LTS

## R CMD check results
There were no ERRORs, WARNINGs, nor NOTES.  

## revdepcheck results summary

#  revdepcheck::revdep_check(Ecdat_dir)
Registered S3 method overwritten by 'revdepcheck':
   method            from
   print.maintainers devtools
── INIT ──────────────────────────────────── Computing revdeps ──
── INSTALL ──────────────────────────────────────── 2 versions ──
Installing CRAN version of Ecdat
Installing DEV version of Ecdat
── CHECK ───────────────────────────────────────── 16 packages ──
✓ catdata 1.2.2                          ── E: 0     | W: 0     | N: 0
✓ DCchoice 0.0.15                        ── E: 0     | W: 0     | N: 0
✓ discSurv 1.4.1                         ── E: 0     | W: 0     | N: 0
✓ DStree 1.0                             ── E: 0     | W: 0     | N: 1
✓ DWreg 2.0                              ── E: 0     | W: 0     | N: 0
✓ Ecfun 0.2-2                            ── E: 0     | W: 0     | N: 0
✓ flexmix 2.3-15                         ── E: 0     | W: 0     | N: 0
✓ gamclass 0.58                          ── E: 1     | W: 0     | N: 0
✓ gk 0.5.1                               ── E: 0     | W: 0     | N: 0
✓ micEcon 0.6-14                         ── E: 0     | W: 1     | N: 0
✓ micEconIndex 0.1-6                     ── E: 0     | W: 0     | N: 0
✓ miscTools 0.6-26                       ── E: 0     | W: 0     | N: 0
✓ mssm 0.1.3                             ── E: 0     | W: 0     | N: 0
✓ plm 2.2-0                              ── E: 0     | W: 0     | N: 0
✓ sampleSelection 1.2-6                  ── E: 0     | W: 0     | N: 0
✓ SeleMix 1.0.1                          ── E: 0     | W: 0     | N: 0
OK: 16
BROKEN: 0
Total time: 36 min





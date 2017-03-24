
#Resubmission 
This is the second update of this package. The previous version, 0.3.4, was accepted and published on CRAN on 2016-12-13. 

This version, 0.4.0, is a major re-write, breaking compatibility with previous versions. It is now entirely reliant on parameter inputs, instead of console inputs. These changes ensure greater consistency in syntax across the package, in both the body and formals of the functions. It also includes the ability to use start and end dates as search parameters, and includes a new optional `tidy` parameter that converts variable names into snake_case and removes superfluous characters. Updates have been made to the vignette to reflect this.

There is one note from R CMD check, reproduced below, which believes the words 'API' and 'APIs' are spelling mistakes.


## Test environments

* Ubuntu 14.04 (on Digital Ocean), R 3.3.3
* Ubuntu 12.04.5 (on Travis-CI): R 3.3.2
* local MacOS 10.12.3 installation, R 3.3.3
* local Windows 7 installation, R 3.3.3
* win-builder (devel and release)
 

##R CMD check results
0 errors | 0 warnings | 1 notes


*Note: 
Maintainer: 'Evan Odell <evanodell91@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  API (3:69, 9:143, 9:332)
  APIs (9:105)

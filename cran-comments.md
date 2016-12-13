#Resubmission 
This is the first update of this package. The previous version, 0.2.5, was accepted and published on cran on 2016-11-12. 

This version, 0.3.4, provides four new functions that allow for search parameters to be included in the function call. They are: `lords_vote_record`, `mp_questions`, `mp_vote_record` and `mp_edms`. An example of `mp_vote_record` has been added to the vignette.

This update also includes the new function `hansard_basic()` which uses console input to walk through the various steps of calling to the API. 

There are also bug fixes for `lords_attendance`, deprecation of `lords_ammendments` in favour of `lords_amendments`

Fixed an issue with `lords_written_questions` and `commons_written_questions` returning a 404 error on some requests.

## Test environments

* ubuntu 14.04 (on Digital Ocean), R 3.3.2
* Ubuntu 12.04 (on Travis-CI): R 3.3.1
* local MacOS 10.11.6 installation, R 3.3.2
* win-builder (devel and release)
 

##R CMD check results
0 errors | 0 warnings | 1 notes


*Note: 
Maintainer: 'Evan Odell <evanodell91@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  API (3:69, 9:130, 9:233, 9:292)
  APIs (9:92)

Found the following (possibly) invalid URLs:
  URL: http://api.data.parliament.uk/help/
    From: README.md
    Status: 404
    Message: Not Found

* The data API blocks automated traffic, which caused the issue with the possibly invalid URLs

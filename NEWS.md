# hansard 0.3.4

## Bug fixes

The `lords_attendance` function was not working. It was returning empty data frames when searching by date and failing entirely when trying to retrieve all attendance. This was due to url encoding issues, which have now been identified and fixed. (#4, @meenaparam)

Fixed spelling of amendments, so that `lords_ammendments` is now `lords_amendments`. The `lords_ammendments` function has been deprecated, and will be removed in a future release.

# hansard 0.3.3

## Bug fixes and hansard_basic()

As part of a move towards calling the api through function parameters rather than console input for most functions, I have introduced `hansard_basic()` which uses console input to walk through the various steps of calling to the API. 

# hansard 0.3.2

## Bug Fixes


# hansard 0.3.0

## Requesting data through function parameters

In response to a request, I've added three new functions that allow you to request the voting record of both MPs and members of the House of Lords by using their ID as a function parameter, rather than as a console input. They are:

`lords_vote_record`

`mp_questions`

`mp_vote_record`

`mp_edms`

The option to use console input has been preserved in `commons_divisions`, `lords_divisions`, `commons_written_questions` and `commons_oral_questions`, along with other features not available in the new functions. 

## Bug fixes

This update fixes an issue with `lords_written_questions` and `commons_written_questions` where the functions returned a 404 error on some requests.

# hansard 0.2.5

## Introducing the hansard package

Provides functions to download data from the data.parliament.uk APIs.

Because of the structure of the data.parliament.uk API, there is a named function for each type of available data for ease of use. Functions for each new API will be added as and when they become available on data.parliament.uk. See the package documentation for details on each function and the type of data available.

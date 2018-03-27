

# hansard 0.6.2

## Code changes

If `tidy=TRUE`, URLs are consistently removed from `about` columns in all 
functions.

## Documentation updates

Improvements to some example descriptions


# hansard 0.6.1

## Code changes

`members_search()` now allows custom lucene queries, with or without wildcards.


# hansard 0.6.0

## Code changes

Changed default value of `verbose` parameter to `TRUE`.

Adapted `members()` to fit follow changes in API structure.

## Bug fixes

Remove URL from `about` column from `all_answered_questions` if `tidy=TRUE`.

Fixed error in `members_search()` that produced a 404 error if left empty.

## Testing

Expanded test coverage


# hansard 0.5.9

## Bug fixes

Fixed bugs where capitalised conjunctions in department names were causing 
queries to fail.


# hansard 0.5.8

## New Functions

`bill_publications()` function introduced, to retrieve data on publications
associated with different bills.

## Performance improvements

Transitioned from if and else statements to `dplyr`'s `case_when()` 
and `if_else()` functions for internal use. Also replacing `gsub()` with 
faster `stringi` functions in some cases. These changes are intended to 
increase speed and improve ease of maintenance.


# hansard 0.5.7

Internal rewrites to make maintenance easier and produce a few very small 
increases in speed in several functions.

Fixed some typos in documentation, brought documentation into line with the 
recommended 80 character maximum width for each line.

# hansard 0.5.6

Fixed bug on `constituencies()` so that the `NULL` default value of parameter 
`current` works properly.

# hansard 0.5.5

## Function changes

`lords_attendance()` has been deprecated. Please use `lords_attendance_date()` 
for attendance data for a given date, and `lords_attendance_session()` for 
attendance from a given session ID. (#4, @pssguy)

New function `lords_sessions()` returns a tibble with details of all sessions 
in the House of Lords between two given dates.

## Bug Fixes

`commons_oral_question_times()` now only returns one question ID.

## General improvements

Tidying with `commons_divisions()` now separates vote number from division ID, 
and the function no longer returns two rows to summarise a division when 
only one is needed.

Changes to syntax for `constituencies()`. The `current` parameter can now 
distinguish between current constituencies (`TRUE`), former (`FALSE`) and 
all constituencies (`NULL`).

Simplification of some internal code, which may have a marginal effect on 
speed, and will make maintenance and adding features easier going forward.

Switch from using `stringr` to `stringi` for internal string processing.

`sessions_info()` is now faster.

# hansard 0.5.4

## General improvements

Speeding up of some tests.

Better tidying on `lords_interests()`.

Changes to structure of `lord_vote_record()` to make it slightly faster and 
more consistent with other functions.


# hansard 0.5.3

## New features

`all_answered_questions()`, `commons_answered_questions()`, 
`commons_oral_questions()`, `commons_written_questions()`, 
`lords_written_questions()` now accept arrays of member IDs and department names.

Added `verbose` parameter to all functions. If `verbose`=`TRUE`, messages 
displaying the progress of the API call are sent to the console.

## General improvements

Sped up edm details retrieval in `mp_edms()` where `full_data`==`TRUE` 
and there are multiple MP IDs.

Major increase in speed for `constituencies(current=TRUE)`.

Small increase in speed for all functions.

`all_answered_questions()` can now handle departmental ID numbers 
passed as characters.

Now uses the `stringr` package to remove case sensitivity from queries.

## Bug Fixes

Fixed bug that prevented `lords_written_questions()` from retrieving 
more than 500 results at a time.


# hansard 0.5.2

## New features

`mp_edms()` now accepts lists and character vectors of MP IDs, and 
returns them all in one tibble.

`mp_edms()` now includes `start_date` and `end_date` parameters

## Bug Fixes

Fixed `mp_edms()` to return more useful error if the requested MP does 
not have any Early Day Motions that meet the search parameters.

The `signatory` parameter in `mp_edms()` is now behaving properly.

## Documentation updates

General improvement to documentation

# hansard 0.5.0

## New features

All functions have a wrapper function with the same name, but with 
`hansard_` prefixed. Existing names have remained untouched.

Addition of `house` and `answering_body` parameters to 
`all_answered_questions()` function.

## Bug Fixes

Fixed bug in `lord_vote_record()` which produced an error if requesting 
both lobbies, but a peer had only voted in one of the lobbies.

# hansard 0.4.9

## New features

Party name columns in `election_candidates()` and 
`election_results(all_data=TRUE)` are now in alphabetical order.

## Bug Fixes

Fixed bug in `all_answered_questions()` that wasn't returning results 
when `tabling_mp_id` is a parameter.

## Documentation changes

`lords_interests()` and `bill_stage_types()` seperated from `members` 
and `bills`, respectively, into their own distinct functions.


# hansard 0.4.8

## New features

New `all_data` parameter in `election_results()` to return the number 
of votes cast for each party in each constituency.

New `election_candidates()` function, which returns the name of all 
candidates standing in an election.

New `epetition_tibble()` function, which returns a tibble with all 
epetitions submitted to parliament, subject to parameters. This function 
offers more flexibility for returning basic details about a group of 
epetitions than the existing `epetition()` function, which is designed 
to provide more detailed information on a single epetition.

# hansard 0.4.7

## New features

Additional URL stripping from variable values if `tidy`=TRUE.

Added `tabling_mp_id` parameter to `all_answered_questions()`.

## Bug Fixes

Fixed bug in `members()` where looking up a single member returned a 
tibble with two rows.

Fixed bug in `mp_edms()` where additional data was not being returned 
if `full_data`=TRUE.

Fixed bug in `election_results()` where it did not return data if 
`constit_details`=TRUE.

Fixed bugs where empty queries in `election_results()`, `members()` 
and possibly other functions were returning incorrect errors in some 
environments or circumstances.

# hansard 0.4.6

## New features

Added optional `tidy_style` parameter, allowing users to decide which 
style of "snake_case", "camelCase" and "period.case" they want variable 
names to be in, if `tidy`=TRUE.

The `elections()` function now accepts `start_date` and `end_date` parameters. 
As the API called by the `elections()` function only accepts one additional 
argument not included in the package (the label of the election), changed the 
`extra_args` parameter to `label` to query that argument. 

The `tidy` parameter now changes date values to POSIXct class with 
`as.POSIXct()` when the API returns date information, or to POSIXct with 
the `parse_date_time` from the `lubridate` package where the API returns 
both date and time information.

`tidy` parameter now strips out more unneeded character strings from API 
response.

## Bug fixes

Updated `elections()` to work with new API syntax.

# hansard 0.4.5

## Date Classes

`hansard()` functions with `start_date` and `end_date` parameters now 
accept any input that can be coerced to a date with the `as.Date()` function.

# hansard 0.4.4

## Election types

Added a `type` parameter to `elections()`, to return all elections of a 
particular type.

## Constituency details

Added optional `constit_details` parameter to `election_result()` function. 
If TRUE, `constit_details` retrieves additional constituency details from 
`constituencies()`, most notably including GSS code.

## Bug fixes

Fixed bug on `commons_oral_question_times()` where some calls did not 
return tibbles.

Fixed bug in `lords_divisons()` that did prevented vote summary 
queries from being returned.

Removed superfluous `x` from names in `elections()` when tidy=TRUE.

# hansard 0.4.3

## Voting margins

Added optional `calculate_percent` parameter to `election_result()` function. 
If `TRUE`, `calculate_percent` calculates the turnout percentage for each 
constituency in the tibble and the majority of the winning candidate to one 
decimal place, and includes this information in the tibble in columns labelled 
`turnout_percentage` and `majority_percentage`. Defaults to FALSE.

# hansard 0.4.2

## tibbling

`hansard()` now uses tibbles instead of data frames as the data class returned 
from API calls.

# hansard 0.4.1

Fixes discrepency between output displayed and vignette and actual package 
output (#7, @pssguy).

tidy parameter in `members_search()` now does some additional tidying up.

# hansard 0.4.0

Major re-write, breaks compatibility with previous versions, please check your 
code to ensure it still works with the new package, as console input 
functionality  has been removed from all functions. These changes ensure 
greater consistency insyntax across the package, in both the body and formals 
of the functions.

If you need the old console based functions, they can be accessed through the 
`hansardconsole` package at <https://github.com/EvanOdell/hansard-console>.

Changes have been made to most functions, including:

`all_answered_questions()`: Console interface is gone, now operates as a 
function with the ID for a given MP accepted as the only function parameter. 
Returns all answered questions if blank.

`bills()`: Rewritten, now with the ability to look up bills by ID, as well 
as bill amendments. Added `bill_stage_types()` function.

`commons_answered_questions()`: Changed parameter name `answeredBy` to 
`answered_by`; improved ability to search by date, dropped console inputs.

`commons_divisions()`: Removed console inputs, simplified options.

`commons_oral_question_times()`: Simplified function, removed need for 
console input.

`commons_oral_questions()`: Simpler process, same result, more 
flexibility in requests

`commons_answered_questions()`: Removed need for console input.

`commons_terms()`: Simplified formals.

`commons_written_questions()`: Removed need for console input.

`constituencies()`: Simplified formals.

`early_day_motions()`: Changed all camelCase to snake_case.

`election_results()`: Removed console input, simplified formals.

`elections()`: Removed console input, simplified formals.

`lords_attendance()`: Removed console input, simplified formals.

`lord_vote_record()`: Removed console input, simplified formals.

`lords_divisions()`: Removed console input, simplified formals.

`members()`: Added additional `commons_members()`, `commons_interests()`, 
`lords_members()` and `lords_interests()` functions.

`mp_edms()`: Improved functionality, can now call full text of 
early day motions.

`mp_questions()`: Changed `questionType` to `question_type`

`mp_vote_record()`: Minor re-write

`papers_laid()`: Incorporates new features in the API

`publication_logs()`: Minor re-write, now includes start and end dates.

`sessions_info()`: Added `start_date()` and `end_date()` parameters

`tv_channels()` New function.

`tv_clips()`: New function, previously part of `tv_programmes`.

`tv_programmes()`: Minor re-write, now includes ability to select legislature.

Adding of `research_briefings_lists()` functions.

# hansard 0.3.4

## Bug fixes

The `lords_attendance()` function was not working. It was returning empty 
data frames when searching by date and failing entirely when trying to 
retrieve all attendance. This was due to url encoding issues, which have 
now been identified and fixed. (#4, @meenaparam)

Fixed spelling of amendments, so that `lords_ammendments()` is now 
`lords_amendments()`. The `lords_ammendments()` function has been deprecated, 
and will be removed in a future release.

# hansard 0.3.3

## Bug Fixes

# hansard 0.3.2

## Bug Fixes

# hansard 0.3.0

## Requesting data through function parameters

In response to a request, I've added three new functions that allow you to 
request the voting record of both MPs and members of the House of Lords by 
using their ID as a function parameter, rather than as a console input. 
They are:

`lords_vote_record()`

`mp_questions()`

`mp_vote_record()`

`mp_edms()`

The option to use console input has been preserved in `commons_divisions()`, 
`lords_divisions()`, `commons_written_questions()` and 
`commons_oral_questions()`, along with other features not available in the 
new functions.

## Bug fixes

This update fixes an issue with `lords_written_questions()` and 
`commons_written_questions()` where the functions returned a 404 
error on some requests.

# hansard 0.2.5

## Introducing the hansard package

Provides functions to download data from the data.parliament.uk APIs.

Because of the structure of the data.parliament.uk API, there is a named 
function for each type of available data for ease of use. Functions for 
each new API will be added as and when they become available on 
data.parliament.uk. See the package documentation for details on each 
function and the type of data available.

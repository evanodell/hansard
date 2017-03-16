# hansard 0.4.0

Major re-write, breaks compatibility with previous versions, please check your code to ensure it still works with the new package, as console input functionality has been removed from all functions. These changes ensure greater consistency in syntax across the package, in both the body and formals of the functions.

If you need the old console based functions, they can be accessed through the `hansardconsole` package at <https://github.com/EvanOdell/hansard-console>.

Changes have been made to the following functions:

`all_answered_questions`: Console interface is gone, now operates as a function with the ID for a given MP accepted as the only function parameter. Returns all answered questions if blank.

`bills`: Rewritten, now with the ability to look up bills by ID, as well as bill amendments. Added `bill_stage_types` function.

`commons_answered_questions`: Changed parameter name `answeredBy` to `answered_by`; improved ability to search by date, dropped console inputs.

`commons_divisions`: Removed console inputs, simplified options.

`commons_oral_question_times`: Simplified function, removed need for console input.

`commons_oral_questions`: Simpler process, same result, more flexibility in requests

`commons_answered_questions`: Removed need for console input.

`commons_terms`: Simplified formals.

`commons_written_questions`: Removed need for console input.

`constituencies`: Simplified formals.

`early_day_motions`: Changed all camelCase to snake_case; STILL OUTSTANDING!

`election_results`: Removed console input, simplified formals.

`elections`: Removed console input, simplified formals.

`lords_attendance`: Removed console input, simplified formals.

`lord_vote_record`: Removed console input, simplified formals.
 
`lords_divisions`: Removed console input, simplified formals.

`members`: Added additional `commons_members`, `commons_interests`, `lords_members` and `lords_interests` functions.

`mp_edms`: Improved functionality, can now call full text of early day motions.

`mp_questions`: Changed `questionType` to `question_type`

`mp_vote_record`: Minor re-write

`papers_laid`: NEW FEATURES ADDED TO API, NEED TO INCOPORATE INTO FUNCTION

`publication_logs`: Minor re-write, now includes start and end dates.

`sessions_info`: Added `start_date` and `end_date` parameters

`tv_channels` New function.

`tv_clips`: New function, previously part of `tv_programmes`.

`tv_programmes`: Minor re-write, now includes ability to select legislature.


# hansard 0.3.4

## Bug fixes

The `lords_attendance` function was not working. It was returning empty data frames when searching by date and failing entirely when trying to retrieve all attendance. This was due to url encoding issues, which have now been identified and fixed. (#4, @meenaparam)

Fixed spelling of amendments, so that `lords_ammendments` is now `lords_amendments`. The `lords_ammendments` function has been deprecated, and will be removed in a future release.

# hansard 0.3.3

## hansard_basic()

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

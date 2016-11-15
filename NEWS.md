

# hansard 0.3.0

## Requesting data through function parameters

In response to a request, I've added three new functions that allow you to request the voting record of both MPs and members of the House of Lords by using their ID as a function parameter, rather than as a console input. They are:

`lords_vote_record`

`mp_questions`

`mp_vote_record`

The option to use console input has been preserved in `commons_divisions`, `lords_divisions`, `commons_written_questions` and `commons_oral_questions`, along with other features not available in the new functions. 

## Bug fixes

This update fixes an issue with `lords_written_questions` and `commons_written_questions` where the functions returned a 404 error on some requests.

# hansard 0.2.5

## Introducing the hansard package

Provides functions to download data from the data.parliament.uk APIs.

Because of the structure of the data.parliament.uk API, there is a named function for each type of available data for ease of use. Functions for each new API will be added as and when they become available on data.parliament.uk. See the package documentation for details on each function and the type of data available.

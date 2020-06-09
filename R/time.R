##' Class providing a time construct
##'
##' @description
##' Provides methods for initializing and incrementing internal simulation time. Handles conversions between units.
##'
##' @docType class
##' @export
##' @importFrom R6 R6Class

Time <- R6::R6Class("Time",

    public = list(

        ##' @description
        ##' Create a new time object.
        ##' @param unit The tme resolution to use, currently supports "hour", "day", "week", "month" or "year".
        ##' @param start A Date object describing the date the simulation started. Defaults to the current date.
        initialize = function(unit = "day", start = Sys.Date()) {
            stopifnot(is.character(unit), length(unit) == 1, unit %in% c("hour", "day", "week", "month", "year"))
            stopifnot(class(start) == "Date", length(start) == 1)
            private$unit <- unit
            private$start <- start
            private$now <- start
        },

        ##' @description
        ##' Increment the internal time
        ##' @param amount A numeric value showing how much time passed
        ##' @param unit The time resolution to use, currently supports "hour", "day", "week", "month" or "year".
        increment = function(amount, unit = "day") {
            private$now <<- private$now + amount * self$to_days(unit)
        },

        ##' @description
        ##' Convert other units to days.
        ##' @param unit The time resolution to use, currently supports "hour", "day", "week", "month" or "year".
        to_days = function(unit) {
            return(switch(unit,
                "hour"  = 1.0 / 24.0,
                "day"   = 1.0,
                "week"  = 7.0,
                "month" = 30.0,
                "year"  = 365.0
            ))
        }

    ),

    private = list(

        ## @field unit The internal time resolution
        unit = "",

        ## @field start A Date object describing the date the simulation started.
        start = NULL,

        ## @field now A Date object describing the current internal simulation date.
        now = NULL

    )
)

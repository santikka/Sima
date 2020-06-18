##' Class providing base event functionality.
##'
##' @description
##' Provides methods for evaluating event occurrences. The event mechanism should only be evaluated by the simulator.
##'
##' @docType class
##' @export
##' @importFrom R6 R6Class
##' @import data.table
##' @return Object of \code{\link{R6Class}} Event, with methods to get/set the internal parameter values and to evaluate the event mechanism.

Event <- R6::R6Class("Event",

    public = list(

        ##' @field description A character string giving a more descriptive overview of the event's role.
        description = "",

        ##' @description
        ##' Create a new Event object.
        ##' @param name A character string giving the name of the event. Must be unique.
        ##' @param description A character string giving a more descriptive overview of the event's role.
        ##' @param parameters A numeric vector giving the initial parameter values of the event.
        ##' @param mechanism An expression that defines the event functionality.
        initialize = function(name, description, parameters, mechanism) {
            if (missing(name)) stop("An event must be given a name")
            if (!is.character(name)) stop("Name must be of type 'character'")
            if (missing(description)) description <- ""
            if (!is.character(description)) {
                description <- as.character(description)
            }
            if (missing(parameters)) parameters <- c()
            if (missing(mechanism)) stop("An event must be given a mechanism")
            if (!is.expression(mechanism)) {
                mechanism <- as.expression(mechanism)
            }
            self$description <- description[1]
            private$name <- name[1]
            private$parameters <- parameters
            private$mechanism <- mechanism[1]
        },

        ##' @description
        ##' Apply an event. Not defined for the superclass.
        apply = function() {
            return(NULL)
        },

        ##' @description
        ##' Set the parameter values of the event.
        ##' @param parameters can be one of the following: 
        ##' A numeric vector giving the parameter values to set, or
        ##' A named numeric vector with names corresponding 
        ##' to specific parameters to set, or a list with two elements
        ##' where the first element gives the values and the second
        ##' element gives the positions to set
        set_parameters = function(parameters) {
            if (is.list(parameters)) {
                if (length(parameters[[1]]) > length(parameters[[2]])) stop("Attempted to set less parameters than what were given")
                if (length(parameters[[1]]) < length(parameters[[2]])) stop("Attempted to set more parameters than what were given")
                private$parameters[parameters[[2]]] <- parameters[[1]]
            } else {
                if (is.numeric(parameters)) {
                    if (!is.null(names(parameters))) {
                        if (is.null(names(private$parameters))) stop("Event parameters are not named")
                        if (!all(names(parameters)) %in% names(private$parameters)) stop("Invalid parameter names")
                        private$parameters[names(parameters)] <- parameters
                    } else {
                        private$parameters <- parameters
                    }
                }
            }
        },

        ##' @description
        ##' Get the current parameter values.
        get_parameters = function() {
            return(private$parameters)
        },

        ##' @description
        ##' Assing an identifier for the event. Should not be called by the user directly.
        ##' @param id An integer identifier to set.
        set_id = function(id) {
            private$id <- id
        },

        ##' @description
        ##' Get the event identifier
        get_id = function() {
            return(private$id)
        },

        ##' @description
        ##' Get the name of the event
        get_name = function() {
            return(private$name)
        }

    ),

    private = list(

        ## @field name A character string giving the name of the event.
        name = "",

        ## @field id Identifier of the event.
        id = 0,

        ## @field mechanism An expression that defines the event functionality.
        mechanism = NULL,

        ## @field parameters Parameters controlling the event mechanism.
        parameters = c()

    )

)

##' Class providing manipulation event functionality. 
##' 
##' @description
##' Provides manipulation event functionality. Inherited from Event.
##' @docType class
##' @import data.table
##' @importFrom R6 R6Class
##' @export
##' @return Object of \code{\link{R6Class}} ManipulationEvent.

ManipulationEvent <- R6::R6Class("ManipulationEvent",

    inherit = Event,

    public = list(

        ##' @description
        ##' Apply a manipulation event
        ##' @param status Current status of the simulated population.
        ##' @param living A logical vector indicating the individuals that are still alive.
        apply = function(status, living) {
            status[living, last_event := private$id]
            par <- private$parameters
            eval(private$mechanism)
        }

    )

)

##' Class providing accumulation event functionality. Inherited from Event.
##' 
##' @description
##' Provides accumulation event functionality. Inherited from Event.
##' @docType class
##' @import data.table
##' @importFrom R6 R6Class
##' @export
##' @return Object of \code{\link{R6Class}} ManipulationEvent.

AccumulationEvent <- R6::R6Class("AccumulationEvent",

    inherit = Event,

    public = list(

        ##' @description
        ##' Apply an accumulation event
        ##' @param status Current status of the simulated population.
        ##' @param living A logical vector indicating the individuals that are still alive.
        ##' @param parallel A logical value indicating if parallel computation is being used in simulation.
        ##' @return A \code{\link{data.table}} with the new individuals added.
        apply = function(status, living, parallel) {
            par <- private$parameters
            new <- eval(private$mechanism)
            n_new <- length(new$alive)
            if (n_new > 0) {
                new$last_event <- rep(0, n_new)
                if (!is.null(parallel)) {
                    new$parallelization_index <- rep(parallel, n_new)
                }
                status_new <- data.table::rbindlist(list(status, new))
                setkey(status_new, parallelization_index)
                return(status_new)
            } else return(status)
        }

    )

)

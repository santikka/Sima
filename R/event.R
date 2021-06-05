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
        ##' @param parameters A named list giving the initial parameter values of the event.
        ##' @param derived A list of expression that define additional parameters based on those in \code{parameters}.
        ##' If given, then \code{parameters} should be named, so its elements can be referenced by elements of \code{derived}.
        ##' @param allocated A named list of expression that define objects that persist between iterations, 
        ##' for example to avoid memory reallocation. The initial \code{status} of the simulator can be referenced by these expressions, a
        ##' s well as any \code{parameters} or \code{derived} parameters of the event itself.
        ##' @param mechanism An expression that defines the event functionality.
        initialize = function(name, description = "", parameters = list(), derived = list(), allocated = list(), mechanism) {
            if (missing(name)) {
                stop("An event must be given a name")
            }
            if (!is.character(name)) {
                stop("Name must be of type 'character'")
            }
            if (!is.character(description)) {
                description <- as.character(description)
            }
            if (missing(mechanism)) {
                stop("An event must be given a mechanism")
            }
            if (!is.expression(mechanism)) {
                mechanism <- as.expression(mechanism)
            }
            n_data <- length(parameters)
            n_derived <- length(derived)
            n_allocated <- length(allocated)
            par_names <- names(parameters)
            derived_names <- names(derived)
            allocated_names <- names(allocated)
            private$has_derived <- n_derived > 0
            if (n_data) {
                if (!is.list(parameters)) {
                    stop("Argument 'parameters' must be a list")
                }
                if (is.null(par_names)) {
                    stop("All parameters must be named")
                }
                private$par_names <- par_names
            }
            if (n_derived) {
                if (!is.list(derived)) {
                    stop("Argument 'derived' must be a list")
                }
                if (is.null(derived_names)) {
                    stop("All derived parameters must be named")
                }
                if (any(derived_names %in% par_names)) {
                    stop("A derived parameter cannot share a name with a parameter")
                }
                private$derived_names <- derived_names
            }
            if (n_allocated) {
                if (!is.list(allocated)) {
                    stop("Argument 'allocated' must be a list")
                }
                if (is.null(allocated_names)) {
                    stop("All allocated objects must be named")
                }
                if (any(allocated_names %in% par_names)) {
                    stop("An allocated object cannot share a name with a parameter")
                }
                if (any(allocated_names %in% derived_names)) {
                    stop("An allocated object cannot share a name with a derived parameter")
                }
            }
            self$description <- description[1]
            private$name <- name[1]
            private$data <- parameters
            private$derived <- derived
            private$allocated <- allocated
            private$mechanism <- mechanism[1]
            if (private$has_derived) {
                private$derive_parameters()
            }
        },

        ##' @description
        ##' Apply an event. Not defined for the superclass.
        apply = function() {
            return(NULL)
        },
        
        ##' @description
        ##' Allocates persistent objects for the event.
        ##' @param status The initial status of the \code{Simulator}.
        allocate = function(status) {
            if (n_a <- length(private$allocated)) {
                allocated_names <- names(private$allocated)
                for (i in 1:n_a) {
                    with(private$data[c(private$par_names, private$derived_names)], {
                        private$data[[allocated_names[i]]] <- eval(private$allocated[[i]])
                    })
                }
            }
        },

        ##' @description
        ##' Set the parameter values of the event.
        ##' Derived parameters are computed again based on the new values.
        ##' @param pars A named list where where the names correspond
        ##' to the names of the parameters of whose values are to be set.
        set_parameters = function(pars) {
            if (is.list(pars)) {
                par_names <- names(pars)
                if (is.null(par_names)) {
                    stop("Argument 'pars' must be a named list")
                } else if (!all(par_names %in% names(private$data))) {
                    stop("Attempted to set parameters that do not exist")
                } else if (any(par_names %in% private$derived_names)) {
                    stop("Cannot set the value of derived parameters")
                }
                private$data[par_names] <- pars
                if (private$has_derived) {
                    private$derive_parameters()
                }
            } else {
                stop("Argument 'pars' must be a list")
            }
        },

        ##' @description
        ##' Get the current parameter values.
        ##' @param derived A logical value. If \code{TRUE} (the default), the derived parameters are also returned.
        ##' @return A list of the current parameter values.
        get_parameters = function(derived = TRUE) {
            if (derived) {
                return(private$data[c(private$par_names, private$derived_names)])
            }
            return(private$data[private$par_names])
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

        ## @field data Parameters, derived parameters and allocated objects exposed to the event mechanism.
        data = list(),
        
        ## @field par_names A character vector containing the names of the parameters
        par_names = character(),
        
        ## @field derived A list of expressions to compute derived parameters.
        derived = list(),
        
        ## @field derived_names A character vector containing the names of the derived parameters
        derived_names = character(),
        
        ## @field allocated A list of expression to compute persistent objects.
        allocated = list(),
        
        # @field has_derived A logical value indicating whether there are any derived parameters
        has_derived = FALSE,
        
        ## @description
        derive_parameters = function () {
            for (i in 1:length(private$derived)) {
                with(private$data[private$par_names], {
                    private$data[[private$derived_names[i]]] <- eval(private$derived[[i]])
                })
            }
        }


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
        apply = function(status) {
            with(private$data, eval(private$mechanism))
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
        ##' @param parallel A logical value indicating if parallel computation is being used in simulation.
        ##' @return A \code{\link{data.table}} with the new individuals added.
        apply = function(status, parallel = NULL) {
            new <- NULL
            with(private$data, {new <- eval(private$mechanism)})
            n_new <- new[ , .N]
            if (n_new) {
                if (!is.null(parallel)) {
                    new$parallelization_index <- parallel
                }
                status_new <- data.table::rbindlist(list(status, new))
                setkey(status_new, parallelization_index)
                return(status_new)
            } else return(status)
        }

    )

)

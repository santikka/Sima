##' Class providing the simulation functionality.
##'
##' @description
##' Provides methods for initializing the synthetic population, parallel simulation, calibration and monitoring.
##'
##' @docType class
##' @export
##' @import data.table
##' @import dqrng
##' @import foreach
##' @import iterators
##' @importFrom R6 R6Class
##' @importFrom parallel detectCores
##' @importFrom parallel clusterExport
##' @importFrom doMPI clusterSize
##' @importFrom doMPI exportDoMPI

Simulator <- R6::R6Class("Simulator",

    public = list(

        ##' @description
        ##' Create a new Simulator object.
        ##' @param initializer A function to generate the initial population.
        ##' @param manipulation_events A list of ManipulationEvent objects.
        ##' @param accumulation_events A list of AccumulationEvent objects.
        ##' @param time A Time object.
        ##' @param n Size of the initial population.
        ##' @param seeds Seed values for random number generation.
        ##' @return A new \code{\link{Simulator}} object.
        initialize = function(initializer = NULL, manipulation_events = list(), accumulation_events = list(), time = Time$new("day", Sys.Date()), n, seeds = 123) {
            if (!is.function(initializer)) stop("Argument 'initializer' is not a function")
            n_acc <- length(accumulation_events)
            n_man <- length(manipulation_events)
            if (n_acc == 0 && n_man == 0) stop("There must be at least one accumulation event or manipulation event")
            if (n_acc > 0) {
                not_acc <- which(sapply(lapply(unlist(accumulation_events), class), function(x) !"AccumulationEvent" %in% x))
                if (length(not_acc) > 0) stop("Elements at ", not_acc, " are not of R6Class 'AccumulationEvent'")
            }
            if (n_man > 0) {
                not_man <- which(sapply(lapply(unlist(manipulation_events), class), function(x) !"ManipulationEvent" %in% x))
                if (length(not_man) > 0) stop("Elements at ", not_man, " are not of R6Class 'ManipulationEvent'")
            }
            if (!"Time" %in% class(time)) stop("Argument 'time' must be of R6Class 'Time'")
            if (!is.numeric(seeds)) stop("Seed values must be numeric")
            if (missing(n)) stop("No population size 'n' given")
            if (!is.numeric(n)) stop("Population size 'n' must be numeric")
            private$status <- initializer(as.integer(n))
            private$status[ ,last_event := 0]
            private$accumulation_events <- accumulation_events
            private$manipulation_events <- manipulation_events
            private$settings <- list(cluster_enabled = FALSE, monitor_enabled = FALSE)
            private$unlisted_events <- unlist(list(private$manipulation_events, private$accumulation_events))
            if (length(private$unlisted_events) > 0) {
                mapply(function(e, id) e$set_id(id), private$unlisted_events, 1:length(private$unlisted_events))
                private$settings$event_indices <- setNames(lapply(private$unlisted_events, function(e) e$get_id()), sapply(private$unlisted_events, function(e) e$get_name()))
            }
            private$time <- time
            private$runtime <- 0
            private$seeds <- as.integer(seeds)
            private$reset <- TRUE
            private$history <- list()
        },

        ##' @description
        ##' Start a parallel computation cluster.
        ##' @param cl A cluster object.
        ##' @param nc Size of the cluster (available computing cores).
        ##' @param export A character vector giving the names of objects from the global environment that should be exported to the workers.
        ##' @param packages A character vector giving the names of packages that should be loaded on the workers.
        ##' @param interface Interface to "foreach", must match the cluster type ("doParallel" and "doMPI" are currently supported).
        start_cluster = function(cl, nc, export, packages, interface = "doParallel") {
            if (missing(cl)) stop("No cluster object provided")
            if (missing(nc)) {
                warning("Number of cores not specified, defaulting to available cores")
                if (identical(interface, "doParallel")) nc <- parallel::detectCores()
                else if (identical(interface, "doMPI")) nc <- doMPI::clusterSize(cl)
            }
            if (!interface %in% c("doParallel", "doMPI")) {
                stop("Invalid interface '", interface, "'")
            }
            if (missing(export)) export <- character(0)
            if (missing(packages)) private$settings$packages <- character(0)
            else private$settings$packages <- packages
            private$settings$cluster_enabled <- TRUE
            private$settings$cluster_interface <- interface
            private$settings$cluster <- cl
            private$settings$cluster_ix <- 1:nc
            private$settings$n_cores <- nc
            if (identical(interface, "doParallel")) {
                parallel::clusterExport(private$settings$cluster, 
                    c("accumulation_events", "manipulation_events",
                      "step_accumulation_recursive_parallel", "step_manipulation_recursive_parallel", 
                      "process_accumulation_events_parallel", "process_manipulation_events_parallel"), envir = private)
                parallel::clusterExport(private$settings$cluster, export)
            } else if (identical(interface, "doMPI")) {
                doMPI::exportDoMPI(private$settings$cluster, 
                    c("accumulation_events", "manipulation_events",
                      "step_accumulation_recursive_parallel", "step_manipulation_recursive_parallel", 
                      "process_accumulation_events_parallel", "process_manipulation_events_parallel"), envir = private)
                doMPI::exportDoMPI(private$settings$cluster, export) 
            }
            private$split_status()
            message("Cluster ready: using ", nc, " cores")
        },

        ##' @description
        ##' Stop a parallel computation cluster.
        stop_cluster = function() {
            if (private$settings$cluster_enabled) {
                if (identical(private$settings$cluster_interface, "doParallel")) parallel::stopCluster(private$settings$cluster)
                else if (identical(private$settings$cluster_interface, "doMPI")) doMPI::closeCluster(private$settings$cluster)
                private$combine_status()
                private$settings$cluster_enabled <- FALSE
                private$settings$n_cores <- NULL
                private$settings$cluster <- NULL
                private$settings$cluster_ix <- NULL
                invisible(gc())
                message("Cluster stopped")
            } else {
                message("There is no active cluster")
            }
        },

        ##' @description
        ##' Progress the simulation.
        ##' @param t_sim A numeric value indicating the length of time to process.
        ##' @param unit A character string giving the time unit that \code{t_sim} corresponds to.
        run = function(t_sim) {
            private$time$increment(t_sim)
            if (private$settings$cluster_enabled) {
                tryCatch({
                    # create local copies to avoid exporting 'self' or 'private'.
                    mon <- private$settings$monitor_enabled
                    seeds <- private$seeds
                    reset <- private$reset
                    private$reset <- FALSE
                    runtime <- private$runtime
                    # NULL initialization necessary here due to foreach export
                    mon_ag <- NULL
                    mon_iv <- NULL
                    monitor <- NULL
                    if (mon) {
                        mon_ag <- private$settings$monitor_aggregators
                        mon_iv <- private$settings$monitor_intervals
                        monitor <- private$monitor_factory(mon_ag, mon_iv)
                    }
                    result <- foreach(dt_sub = private$settings$status_split, 
                            .export = c(# These values may change between runs and need to be exported again.
                                "seeds", "reset", "runtime", "t_sim", "mon", "mon_ag", "mon_iv", "monitor"
                            ), 
                            .noexport = c(ls(globalenv()), ls(environment()), ls(self), ls(private)), # Do not export anything extra to avoid possible conflicts.
                            .packages = c("data.table", "dqrng", private$settings$packages), # These packages have to be installed on the workers.
                            .combine = list, 
                            .verbose = FALSE, 
                            .multicombine = TRUE) %dopar% {
                        key <- as.numeric(dt_sub$key)
                        dt <- dt_sub$value
                        new_hist <- NULL
                        if (reset) {
                            n_seeds <- length(seeds)
                            if (n_seeds > 1) dqrng::dqset.seed(seeds[key %% n_seeds + 1], key)
                            else dqrng::dqset.seed(seeds, key)
                        }
                        if (mon) {
                            mon_t <- sort(unique(unlist(lapply(mon_iv, function(x) seq(x, t_sim, by = x)))))
                            mon_ix <- 0
                            new_hist <- vector(mode = "list", length = length(mon_t))
                        }
                        for (t in (runtime + 1):(runtime + t_sim)) {
                            dt <- process_accumulation_events_parallel(dt, accumulation_events, parallel = key)
                            process_manipulation_events_parallel(dt, manipulation_events)
                            if (mon) {
                                temp_hist <- monitor(dt, t)
                                if (!is.null(temp_hist)) {
                                    mon_ix <- mon_ix + 1
                                    new_hist[[mon_ix]] <- temp_hist
                                }
                            }
                        }
                        return(list(value = dt, key = dt_sub$key, history = new_hist))
                    }
                    private$settings$status_split <- lapply(result, "[" , c("value", "key"))
                    if (mon) {
                        new_hist_len <- length(result[[1]]$history)
                        if (new_hist_len > 0) {
                            ag_len <- length(mon_ag)
                            new_history <- vector(mode = "list", length = new_hist_len)
                            for (t in 1:new_hist_len) {
                                new_history[[t]] <- vector(mode = "list", length = ag_len + 1)
                                new_history[[t]][[1]] <- result[[1]]$history[[t]][[1]]
                                for (i in 1:ag_len) {
                                    new_history[[t]][[i+1]] <- private$settings$monitor_combiners[[i]](lapply(lapply(result, function(x) x$history[[t]]), function(y) y[[i+1]]))
                                }
                            }
                            hist_len <- length(private$history)
                            private$history[(hist_len + 1):(hist_len + new_hist_len)] <- new_history
                        }
                    }
                }, warning = function(w) {
                    message(w)
                }, error = function(e) {
                    message(e)
                })
            } else {
                pb <- txtProgressBar(min = 0, max = t_sim, style = 3, width = 50)
                if (private$settings$monitor_enabled) {
                    private$monitor <- private$monitor_factory(private$settings$monitor_aggregators, private$settings$monitor_intervals)
                    mon_t <- sort(unique(unlist(lapply(private$settings$monitor_intervals, function(x) seq(x, t_sim, by = x)))))
                    mon_ix <- 0
                    new_history <- vector(mode = "list", length = length(mon_t))
                }
                if (private$reset) {
                    dqrng::dqset.seed(private$seeds[1])
                    private$reset <- FALSE
                }
                for (t in (private$runtime + 1):(private$runtime + t_sim)) {
                    private$status <- private$process_accumulation_events(private$status, private$accumulation_events)
                    private$process_manipulation_events(private$status, private$manipulation_events)
                    if (private$settings$monitor_enabled) {
                        temp_hist <- private$monitor(private$status, t)
                        if(!is.null(temp_hist)) {
                            mon_ix <- mon_ix + 1
                            new_history[[mon_ix]] <- temp_hist
                        }
                    }
                    setTxtProgressBar(pb, t)
                }
                if (private$settings$monitor_enabled) {
                    new_hist_len <- length(new_history)
                    if (new_hist_len > 0) {
                        hist_len <- length(private$history)
                        private$history[(hist_len + 1):(hist_len + new_hist_len)] <- new_history
                    }
                }
                close(pb)
            }
        },

        ##' @description
        ##' Configure the parameters of Event objects.
        ##' @param p A list of parameter values. The names of the list should match the Event object names to be configured.
        reconfigure = function(p) {
            for (n in names(p)) {
                private$unlisted_events[[private$settings$event_indices[[n]]]]$set_parameters(p[[n]])
            }
            if (private$settings$cluster_enabled) {
                if (identical(private$settings$cluster_interface, "doParallel")) parallel::clusterExport(private$settings$cluster, c("manipulation_events", "accumulation_events"), envir = private)
                else if (identical(private$settings$cluster_interface, "doMPI")) doMPI::exportDoMPI(private$settings$cluster, c("manipulation_events", "accumulation_events"), envir = private)
            }
        },

        ##' @description
        ##' Configure events, run a simulation and compute a summary statistic for calibration. The original status is restored after completion.
        ##' @param t_sim A numeric value indicating the length of time to process.
        ##' @param p A list of parameter values. The names of the list should match the Event object names to be configured.
        ##' @param output A function that computes a summary statistic from the simulation.
        ##' @param seeds Seed values for random number generation.
        ##' @param ... Additional arguments passed to \code{output}.
        ##' @return The output of \code{output_function} evaluated for the resulting population.
        configure = function(t_sim, p, output, seeds = 123, ...) {
            if (length(private$history) > 0) warning("Previous simulation history was overwritten by calibration")
            if (!is.numeric(seeds)) stop("Seed values must be numeric")
            private$history <- list()
            self$reconfigure(p)
            orig_seeds <- private$seeds
            private$seeds <- as.integer(seeds)
            private$reset <- TRUE
            if (private$settings$cluster_enabled) {
                orig_status <- lapply(private$settings$status_split, function(x) {
                    return(list(value = data.table::copy(x$value), key = x$key))
                })
                self$run(t_sim = t_sim)
                status_temp <- data.table::rbindlist(lapply(private$settings$status_split, "[[", "value"))
                out <- output(status_temp, ...)
                status_temp <- NULL
                private$settings$status_split <- orig_status
                private$seeds <- orig_seeds
                private$reset <- TRUE
                private$history <- list()
            } else {
                orig_status <- data.table::copy(private$status)
                self$run(t_sim = t_sim)
                out <- output(private$status, ...)
                private$status <- orig_status
                private$seeds <- orig_seeds
                private$reset <- TRUE
                private$history <- list()
            }
            private$runtime <- 0
            return(out)
        },

        ##' @description
        ##' Get the status of the current population
        ##' @return A data.table of the current status
        get_population = function() {
            if (private$settings$cluster_enabled) {
                temp_status <- data.table::rbindlist(lapply(private$settings$status_split, "[[", "value"))
                temp_status[ ,parallelization_index := NULL]
                return(temp_status)
            } else return(private$status)
        },

        ##' @description
        ##' Get a sample of the current population
        ##' @param sampler A function that constructs the output sample.
        ##' @param ... Additional arguments passed to \code{sampler}.
        ##' @return The sample generated by \code{sampler}.
        get_sample = function(sampler, ...) {
            return(sampler(population = private$status, ...))
        },

        ##' @description
        ##' Initialize monitors to track specific status variables throughout the simulation at certain intervals.
        ##' @param aggregators A list of functions to compute aggregate summary statistics from the simulated population.
        ##' @param intervals A numeric vector giving the time interval after which each aggregator should be evaluated.
        ##' @param combiners If parallel computation is used, this is a list of functions that combine the aggregator results from each parallel worker.
        initialize_monitor = function(aggregators, intervals, combiners = NULL) {
            private$settings$monitor_enabled <- TRUE
            ag_len <- length(aggregators)
            iv_len <- length(intervals)
            if (ag_len %% iv_len != 0) stop("Invalid number of intervals")
            intervals <- rep(intervals, ag_len %/% iv_len)
            private$settings$monitor_aggregators <- aggregators
            private$settings$monitor_intervals <- intervals
            private$settings$monitor_combiners <- combiners
            message("Monitor initialized") 
            # Monitor does not actually exist at this point.
            # It is generated when 'run' is called (and exported to the workers if cluster in enabled).
        }

    ),

    private = list(

        ## @field status The status of the simulated population.
        status = data.table::data.table(),

        ## @field status_split Chunks of the population to be sent to parallel workers.
        status_split = list(),

        ## @field accumulation_events A list of AccumulationEvent objects.
        accumulation_events = list(),

        ## @field manipulation_events A list of ManipulationEvent objects.
        manipulation_events = list(),

        ## @field unlisted_events An unordered vector of both types of events.
        unlisted_events = list(),

        ## @field settings A list containing various settings of the simulator.
        settings = list(),

        ## @field history A list of past statuses.
        history = list(),

        ## @field time A Time object to track internal time.
        time = NULL,

        ## @field monitor A monitor object for recording specific status varibles throughout the simulation.
        monitor = NULL,

        ## @field runtime A numeric value indicating how long the simulation has been run for.
        runtime = 0,

        ## @field seeds Seed values used for random number generation in the simulation. Only the first value is used if parallel computation is not used.
        seeds = 0,

        ## @field reset A logical value indicating whether seeds should be reset
        reset = FALSE,

        ## @description
        ## Split the population into chunks for parallel processing.
        split_status = function() {
            if (is.null(private$status$parallelization_index)) {
                n <- nrow(private$status)
                private$status[ ,parallelization_index := factor(rep(1:private$settings$n_cores, ceiling(n / private$settings$n_cores))[1:n])]
                setkey(private$status, parallelization_index)
                private$settings$status_split <- as.list(data.table_isplit(private$status, levels(private$status$parallelization_index)))
            }
            private$status <- data.table()
            invisible(gc()) # We can release the memory taken by the original status
        },

        ## @description
        ## Reintegrate population chunks when parallel computation finishes.
        combine_status = function() {
            private$status <- data.table::rbindlist(lapply(private$settings$status_split, "[[", "value"))
            private$status[ ,parallelization_index := NULL]
            private$settings$status_split <- list()
            invisible(gc()) # We can release the memory taken by the parts of the data.table
        },

        ## @description
        ## A function to handle recursive event structures for accumulation events (nested & ordered/unordered subgroups of events)
        ## @param dt The data.table to update
        ## @param event_group List of events or a single event to apply to 'dt'
        step_accumulation_recursive = function(dt, event_group, parallel) {
            if ("AccumulationEvent" %in% class(event_group)) {
                living <- dt$alive == 1
                if (any(living)) {
                    dt <- event_group$apply(dt, living, parallel)
                }
            } else {
                e_size <- length(event_group)
                if (e_size > 0) {
                    if (isTRUE(attr(event_group, "ordered"))) group_order <- 1:e_size # isTRUE takes NULL into account
                    else group_order <- dqrng::dqsample(e_size)
                    for (e in group_order) {
                        dt <- private$step_accumulation_recursive(dt, event_group[[e]], parallel)
                    }
                }
            }
            return(dt)
        },

        step_accumulation_recursive_parallel = function(dt, event_group, parallel) {
            if ("AccumulationEvent" %in% class(event_group)) {
                living <- dt$alive == 1
                if (any(living)) {
                    dt <- event_group$apply(dt, living, parallel)
                }
            } else {
                e_size <- length(event_group)
                if (e_size > 0) {
                    if (isTRUE(attr(event_group, "ordered"))) group_order <- 1:e_size # isTRUE takes NULL into account
                    else group_order <- dqrng::dqsample(e_size)
                    for (e in group_order) {
                        dt <- step_accumulation_recursive_parallel(dt, event_group[[e]], parallel)
                    }
                }
            }
            return(dt)
        },

        ## @description
        ## A function to handle recursive event structures for manipulation events (nested & ordered/unordered subgroups of events)
        ## @param dt The data.table to update
        ## @param event_group List of events or a single event to apply to 'dt'
        step_manipulation_recursive = function(dt, event_group) {
            if ("ManipulationEvent" %in% class(event_group)) {
                living <- dt$alive == 1
                if (any(living)) {
                    event_group$apply(dt, living)
                }
            } else {
                e_size <- length(event_group)
                if (e_size > 0) {
                    if (isTRUE(attr(event_group, "ordered"))) group_order <- 1:e_size # isTRUE takes NULL into account
                    else group_order <- dqrng::dqsample(e_size)
                    for (e in group_order) {
                        private$step_manipulation_recursive(dt, event_group[[e]])
                    }
                }
            }
        },

        step_manipulation_recursive_parallel = function(dt, event_group) {
            if ("ManipulationEvent" %in% class(event_group)) {
                living <- dt$alive == 1
                if (any(living)) {
                    event_group$apply(dt, living)
                }
            } else {
                e_size <- length(event_group)
                if (e_size > 0) {
                    if (isTRUE(attr(event_group, "ordered"))) group_order <- 1:e_size # isTRUE takes NULL into account
                    else group_order <- dqrng::dqsample(e_size)
                    for (e in group_order) {
                        step_manipulation_recursive_parallel(dt, event_group[[e]])
                    }
                }
            }
        },

        ## @description
        ## Function to process events that introduce new individuals into the population
        ## @param dt The data.table to update
        ## @param events List of accumulation events of the simulator
        ## @param parallel A logical value indicating if parallel computation is used
        process_accumulation_events = function(dt, events, parallel) {
            if (any(dt$alive == 1)) {
                dt <- private$step_accumulation_recursive(dt, events, parallel)
            }
            return(dt)
        },

        process_accumulation_events_parallel = function(dt, events, parallel) {
            if (any(dt$alive == 1)) {
                dt <- step_accumulation_recursive_parallel(dt, events, parallel)
            }
            return(dt)
        },

        ## @description
        ## Function to process events that modify the status variables
        ## @param dt The data.table to update
        ## @param event The list of manipulation events of the simulator
        process_manipulation_events = function(dt, events) {
            if (any(dt$alive == 1)) {
                private$step_manipulation_recursive(dt, events)
            }
        },

        process_manipulation_events_parallel = function(dt, events) {
            if (any(dt$alive == 1)) {
                step_manipulation_recursive_parallel(dt, events)
            }
        },

        ## @description
        ## A generator function used to create monitors based on the input aggregators and intervals
        ## @param aggregators A list of functions with a single argument that is the status data.table
        ## @param intervals An integer vector describing the time interval to apply each aggregator
        monitor_factory = function(aggregators, intervals) {
            force(aggregators)
            force(intervals)
            return(function(dt, t) {
                ix <- which(t %% intervals == 0)
                ix_len <- length(ix)
                if (ix_len > 0) {
                    new_hist <- vector(mode = "list", length = length(aggregators) + 1)
                    new_hist[[1]] <- t
                    for (i in (ix + 1)) {
                        new_hist[[i]] <- aggregators[[i-1]](dt)
                    }
                    return(new_hist)
                }
                return(NULL)
            })
        }

    )

)

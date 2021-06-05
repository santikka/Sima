## Utility functions used by the Simulator

## @description
## data.table iterator used in parallel computation
## The data.table is split to each process (no shared memory)
## @param x The data.table to be split
## @param vals A squence 1:nc where nc is the number of parts to split the table into
data.table_isplit <- function(x, vals) {
    ival <- iterators::iter(vals)
    el <- function() {
        val <- iterators::nextElem(ival)
        list(value = x[val], key = val)
    }
    obj <- list(nextElem = el)
    class(obj) <- c("abstractiter", "iter")
    return(obj)
}

is_event <- function(x, type) {
    sapply(unlist(x), inherits, what = type)
}
EstimateTime <-
function (inputs) 
{
    sample.size <- 50
    if (nrow(inputs$data) < sample.size) {
        message(paste("Timing estimate unavailable when n <", 
            sample.size))
        return(NULL)
    }
    sample.index <- sample(nrow(inputs$data), size = sample.size)
    small.inputs <- inputs
    small.inputs$data <- small.inputs$data[sample.index, ]
    small.inputs$data <- droplevels(small.inputs$data)
    small.inputs$regimes <- small.inputs$regimes[sample.index, 
        , , drop = FALSE]
    small.inputs$observation.weights <- small.inputs$observation.weights[sample.index]
    small.inputs$uncensored <- small.inputs$uncensored[sample.index, 
        , drop = FALSE]
    small.inputs$intervention.match <- small.inputs$intervention.match[sample.index, 
        , , drop = FALSE]
    small.inputs$combined.summary.measures <- small.inputs$combined.summary.measures[sample.index, 
        , , , drop = FALSE]
    if (!is.null(small.inputs$id)) 
        small.inputs$id <- small.inputs$id[sample.index]
    if (is.numeric(inputs$gform)) 
        small.inputs$gform <- small.inputs$gform[sample.index, 
            , , drop = FALSE]
    if (length(dim(inputs$msm.weights)) == 3) 
        small.inputs$msm.weights <- small.inputs$msm.weights[sample.index, 
            , , drop = FALSE]
    start.time <- Sys.time()
    try.result <- suppressWarnings(try(MainCalcs(small.inputs), 
        silent = TRUE))
    if (inherits(try.result, "try-error")) {
        message("Timing estimate unavailable")
    }
    else {
        elapsed.time <- Sys.time() - start.time
        est.time1 <- round(sqrt(as.double(elapsed.time, units = "mins") * 
            nrow(inputs$data)/sample.size), digits = 0)
        est.time2 <- round(as.double(elapsed.time, units = "mins") * 
            nrow(inputs$data)/sample.size, digits = 0)
        if (est.time2 == 0) {
            est.time.str <- "< 1 minute"
        }
        else if (est.time2 == 1) {
            est.time.str <- "1 minute"
        }
        else {
            est.time.str <- paste(est.time1, "to", est.time2, 
                "minutes")
        }
        message("Estimate of time to completion: ", est.time.str)
    }
    return(NULL)
}

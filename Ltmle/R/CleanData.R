CleanData <- function(data,
                      nodes,
                      deterministic.Q.function,
                      survivalOutcome,
                      showMessage = TRUE){
    is.nan.df <- function(x) {
             if (length(x)) {
                 do.call("cbind", lapply(x, "is.nan"))
             }
             else {
                 matrix(FALSE, length(row.names(x)), 0)
             }
    }
    is.na.strict <- function(x) is.na(x) & !is.nan.df(x)
    changed <- FALSE
    ua <- rep(TRUE, nrow(data))
    if (ncol(data) == 1)
        return(data)
    deterministic.Q.function.depends.on.called.from.estimate.g <- !is.null(deterministic.Q.function) &&
        length(grep("called.from.estimate.g", as.character(body(deterministic.Q.function)))) >
        0
    ## the first variable may not have missing values
    ## during the for loop the vector ua is updated
    ## so that ua is FALSE after censoring or survival event
    for (i in 1:(ncol(data) - 1)) {
        if (anyNA(data[ua, i])){
            stop(paste0("Missing values in variable ",names(data)[i],".\n",
                        "NA values are not permitted in data except after censoring or a survival event"))
        }
        ## stop("NA values are not permitted in data except after censoring or a survival event")
        if (i %in% c(nodes$D, nodes$L, nodes$Y, nodes$AC)) {
            is.deterministic <- ua & IsDeterministic(data, cur.node = i +
                                                               1, deterministic.Q.function = deterministic.Q.function,
                                                     nodes = nodes, called.from.estimate.g = TRUE,
                                                     survivalOutcome = survivalOutcome)$is.deterministic
            if (deterministic.Q.function.depends.on.called.from.estimate.g) {
                is.deterministic.Q <- ua & IsDeterministic(data,
                                                           cur.node = i + 1, deterministic.Q.function = deterministic.Q.function,
                                                           nodes = nodes, called.from.estimate.g = FALSE,
                                                           survivalOutcome = survivalOutcome)$is.deterministic
                if (any(is.deterministic[ua] & !is.deterministic.Q[ua]))
                    stop("Any row set deterministic by deterministic.Q.function(..., called.from.estimate.g=TRUE) must imply that the row is also set deterministic by deterministic.Q.function(..., called.from.estimate.g=FALSE)")
            }
            ua[ua] <- !is.deterministic[ua]
            if (anyNA(ua))
                stop("internal ltmle error - ua should not be NA in CleanData")
            if (!all(is.na.strict(faildata <- data[is.deterministic, setdiff((i + 1):ncol(data), nodes$Y), drop = FALSE]))) {
                failcolumns <- colnames(faildata)[sapply(faildata,function(x){!all(is.na.strict(x))})]
                warning(paste0("The following nodes should be NA because a competing risk has occurred\nor deterministic.Q.function is TRUE for other reasons\nbut they are not: ",
                               paste0(failcolumns,collapse=", ")))
                data[is.deterministic, setdiff((i + 1):ncol(data), nodes$Y)] <- NA
                changed <- TRUE
            }
            if (i %in% nodes$C) {
                censored <- data[, i] == "censored" & ua
                if (!all(is.na.strict(data[censored, (i + 1):ncol(data), drop = FALSE]))) {
                    data[censored, (i + 1):ncol(data)] <- NA
                    changed <- TRUE
                }
                ua[ua] <- !censored[ua]
                if (anyNA(ua))
                    stop("internal ltmle error - ua should not be NA in CleanData")
            }
            if (changed && showMessage) {
                message("Note: for internal purposes, all nodes after a censoring event are set to NA and \n all nodes (except Ynodes) are set to NA after Y=1 if survivalOutcome is TRUE (or if specified by deterministic.Q.function).\n Your data did not conform and has been adjusted. This may be relevant if you are \n writing your own deterministic function(s) or debugging ltmle.")
            }
        }
    }
    return(data)
}

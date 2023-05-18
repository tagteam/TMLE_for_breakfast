IsDeterministicG <-
function (data, cur.node, deterministic.g.function, nodes, using.newdata) 
{
    stopifnot(cur.node %in% nodes$AC)
    default <- list(is.deterministic = rep(FALSE, nrow(data)), 
        prob1 = NULL)
    if (is.null(deterministic.g.function)) 
        return(default)
    det.list <- deterministic.g.function(data = data, current.node = cur.node, 
        nodes = nodes)
    if (is.null(det.list)) 
        return(default)
    if (!is.list(det.list) || !setequal(names(det.list), c("is.deterministic", 
        "prob1")) || length(det.list) != 2) 
        stop("deterministic.g.function should return a list with names: is.deterministic, prob1")
    if (!length(det.list$prob1) %in% c(1, length(which(det.list$is.deterministic)))) 
        stop("the length of the 'prob1' element of deterministic.g.function's return argument should be either 1 or length(which(det.list$is.deterministic))")
    inconsistent.rows <- (det.list$prob1 %in% c(0, 1)) & (det.list$prob1 != 
        data[det.list$is.deterministic, cur.node]) & !is.na(data[det.list$is.deterministic, 
        cur.node])
    if (any(inconsistent.rows)) {
        err.msg <- paste("At node:", names(data)[cur.node], "deterministic.g.function is inconsistent with data - prob1 is either 0 or 1 but this does not match the node value.\nCheck data rows:", 
            paste(head(rownames(data)[det.list$is.deterministic][inconsistent.rows]), 
                collapse = " "))
        if (using.newdata) {
            err.msg <- paste(err.msg, "\n This error occured while calling deterministic.g.function on data where Anodes are set to abar.")
            cat("deterministic.g.function is inconsistent with data.\nAfter setting Anodes to abar, the data looks like this:\n")
            print(head(data[det.list$is.deterministic[inconsistent.rows], 
                ]))
        }
        stop(err.msg)
    }
    return(det.list)
}

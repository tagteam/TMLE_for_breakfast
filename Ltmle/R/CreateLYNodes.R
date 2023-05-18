CreateLYNodes <-
function (data, nodes, check.Qform, Qform) 
{
    LYnodes <- sort(c(nodes$L, nodes$Y))
    SuppressGivenWarnings(nodes.to.remove <- LYnodes[LYnodes < 
        min(nodes$AC)], "no non-missing arguments to min; returning Inf")
    if (length(LYnodes) > 1) {
        for (i in 1:(length(LYnodes) - 1)) {
            cur.node <- LYnodes[i]
            next.node <- LYnodes[i + 1]
            if (!any(cur.node:next.node %in% nodes$AC)) {
                nodes.to.remove <- c(nodes.to.remove, next.node)
            }
        }
    }
    new.LYnodes <- setdiff(LYnodes, nodes.to.remove)
    if (check.Qform) {
        removed.Qform.index <- NULL
        for (i in nodes.to.remove) {
            index <- which(names(Qform) == names(data)[i])
            if (length(index) > 0) {
                removed.Qform.index <- c(removed.Qform.index, 
                  index)
            }
        }
        if (!is.null(removed.Qform.index)) {
            message("L/Y nodes (after removing blocks)  : ", 
                paste(names(data)[new.LYnodes], collapse = " "), 
                "\n")
            message("Qform names                        : ", 
                paste(names(Qform), collapse = " "), "\n")
            message(paste("The following nodes are not being considered as L/Y nodes because they are part of a block\nof L/Y nodes. They are being dropped from Qform:\n"), 
                paste(names(Qform)[removed.Qform.index], "\n", 
                  collapse = " "))
            Qform <- Qform[-removed.Qform.index]
        }
        return(list(LYnodes = new.LYnodes, Qform = Qform))
    }
    return(new.LYnodes)
}

NodeToIndex <-
function (data, node) 
{
    if (is.numeric(node) || is.null(node)) 
        return(node)
    if (!is.character(node)) 
        stop("nodes must be numeric, character, or NULL")
    index <- match(node, names(data))
    if (anyNA(index)) {
        stop(paste("\nnamed node(s) not found:", node[is.na(index)]))
    }
    return(index)
}

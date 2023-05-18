GetDefaultForm <-
function (data, nodes, is.Qform, stratify, survivalOutcome, showMessage) 
{
    if (is.Qform) {
        lhs <- rep("Q.kplus1", length(nodes$LY))
        node.set <- nodes$LY
    }
    else {
        lhs <- names(data)[nodes$AC]
        node.set <- nodes$AC
    }
    if (stratify) {
        stratify.nodes <- c(nodes$C, nodes$A)
    }
    else {
        stratify.nodes <- c(nodes$C)
    }
    if (survivalOutcome) {
        stratify.nodes <- c(stratify.nodes, nodes$Y)
    }
    form <- NULL
    for (i in seq_along(node.set)) {
        cur.node <- node.set[i]
        if (cur.node == 1) {
            form[i] <- paste(lhs[i], "~ 1")
        }
        else {
            parent.node.names <- names(data)[setdiff(1:(cur.node - 
                1), stratify.nodes)]
            if (length(parent.node.names) == 0) {
                form[i] <- paste(lhs[i], "~ 1")
            }
            else {
                form[i] <- paste(lhs[i], "~", paste(parent.node.names, 
                  collapse = " + "))
            }
        }
        names(form)[i] <- names(data)[cur.node]
    }
    if (showMessage) {
        message(ifelse(is.Qform, "Qform", "gform"), " not specified, using defaults:")
        lapply(seq_along(form), function(i, names) {
            message("formula for ", names[i], ":")
            message(capture.output(print(as.formula(form[i]), 
                showEnv = FALSE)))
        }, names = names(form))
        message("")
    }
    return(form)
}

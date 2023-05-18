ReorderFits <-
function (l1) 
{
    if (length(l1) == 0) 
        l1 <- list("no fit due to no A/C nodes")
    num.regimes <- length(l1[[1]])
    num.nodes <- length(l1)
    l2 <- vector("list", num.regimes)
    for (i in 1:num.regimes) {
        l2[[i]] <- vector("list", num.nodes)
        names(l2[[i]]) <- names(l1)
        for (j in 1:num.nodes) {
            l2[[i]][[j]] <- l1[[j]][[i]]
        }
    }
    return(l2)
}

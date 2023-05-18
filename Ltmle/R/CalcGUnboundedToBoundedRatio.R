CalcGUnboundedToBoundedRatio <-
function (g.list, nodes, final.Ynodes) 
{
    CalcForFinalYNode <- function(num.AC.nodes) {
        if (num.AC.nodes == 0) 
            return(1)
        if (!anyNA(g.list$cum.g)) 
            return(AsMatrix(g.list$cum.g.unbounded[, num.AC.nodes, 
                ]/g.list$cum.g[, num.AC.nodes, ]))
        g.ratio1 <- matrix(NA, n, num.regimes)
        for (i in 1:num.regimes) {
            g.ratio.temp <- cbind(g.list$cum.g.meanL.unbounded[, 
                num.AC.nodes, i, ]/g.list$cum.g.meanL[, num.AC.nodes, 
                i, ], g.list$cum.g.unbounded[, num.AC.nodes, 
                i]/g.list$cum.g[, num.AC.nodes, i])
            index <- max.col(!is.na(g.ratio.temp), "last")
            g.ratio1[, i] <- g.ratio.temp[sub2ind(1:n, col = index, 
                num.rows = n)]
        }
        return(g.ratio1)
    }
    n <- dim(g.list$cum.g)[1]
    num.regimes <- dim(g.list$cum.g)[3]
    num.final.Ynodes <- length(final.Ynodes)
    g.ratio <- array(dim = c(n, num.regimes, num.final.Ynodes))
    for (j in 1:num.final.Ynodes) {
        num.AC.nodes <- sum(nodes$AC < final.Ynodes[j])
        g.ratio[, , j] <- CalcForFinalYNode(num.AC.nodes)
    }
    return(g.ratio)
}

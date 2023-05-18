CalcG <-
function (prob.A.is.1, cur.abar, deterministic.newdata) 
{
    g <- matrix(NA_real_, nrow(prob.A.is.1), ncol(prob.A.is.1))
    g[!is.na(cur.abar) & cur.abar == 1] <- prob.A.is.1[!is.na(cur.abar) & 
        cur.abar == 1]
    g[!is.na(cur.abar) & cur.abar == 0] <- 1 - prob.A.is.1[!is.na(cur.abar) & 
        cur.abar == 0]
    g[deterministic.newdata] <- 1
    return(g)
}

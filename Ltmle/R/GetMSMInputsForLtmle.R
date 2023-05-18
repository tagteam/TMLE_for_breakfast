GetMSMInputsForLtmle <-
function (data, abar, rule, gform) 
{
    if ((!missing(abar) && is.list(abar)) || is.list(rule)) {
        if (is.list(rule)) {
            if (length(rule) != 2) 
                stop("If rule is a list, it must be of length 2")
            regimes1 <- RegimesFromAbar(data, abar, rule[[1]])
            regimes0 <- RegimesFromAbar(data, abar, rule[[2]])
        }
        else {
            if (length(abar) != 2) 
                stop("If abar is a list, it must be of length 2")
            regimes1 <- RegimesFromAbar(data, abar[[1]], rule)
            regimes0 <- RegimesFromAbar(data, abar[[2]], rule)
        }
        if (ncol(regimes1) != ncol(regimes0)) 
            stop("If abar or rule is a list, both elements must give a matrix with the same number of columns")
        if (nrow(regimes1) != nrow(regimes0)) 
            stop("If abar or rule is a list, both elements must give a matrix with the same number of rows")
        regimes <- c(regimes1, regimes0)
        dim(regimes) <- c(nrow(regimes1), ncol(regimes1), 2)
        summary.measures <- array(1:0, dim = c(2, 1, 1))
        colnames(summary.measures) <- "A"
        working.msm <- "Y ~ A"
        msm.weights <- matrix(1, nrow = 2, ncol = 1)
    }
    else {
        regimes <- RegimesFromAbar(data, abar, rule)
        working.msm <- "Y ~ 1"
        msm.weights <- matrix(1, nrow = 1, ncol = 1)
        summary.measures <- array(dim = c(1, 0, 1))
    }
    msm.inputs <- list(regimes = regimes, working.msm = working.msm, 
        summary.measures = summary.measures, gform = gform, final.Ynodes = NULL, 
        msm.weights = msm.weights)
    return(msm.inputs)
}

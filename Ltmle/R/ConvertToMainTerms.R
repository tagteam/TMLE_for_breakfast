ConvertToMainTerms <-
function (data, msm, summary.measures, nodes) 
{
    baseline.column.names <- names(data)[nodes$baseline]
    summary.column.names <- colnames(summary.measures)
    rhs.vars <- RhsVars(msm)
    if (length(intersect(baseline.column.names, summary.column.names)) > 
        0) 
        stop("Baseline covariate columns of data and columns of summary.measures may not have the same name")
    if (!all(rhs.vars %in% c(baseline.column.names, summary.column.names))) 
        stop("All right hand side variables in working.msm must be either column names of summary.measures or column names of baseline covariates")
    baseline.column.names <- intersect(baseline.column.names, 
        rhs.vars)
    baseline.data <- data[, baseline.column.names, drop = FALSE]
    num.regimes <- dim(summary.measures)[1]
    num.summary.measures <- dim(summary.measures)[2]
    num.final.Ynodes <- dim(summary.measures)[3]
    n <- nrow(data)
    for (j in 1:num.final.Ynodes) {
        for (i in 1:num.regimes) {
            combined.summary.measures <- model.matrix(as.formula(msm), 
                data.frame(Y = 1, baseline.data, matrix(summary.measures[i, 
                  , j], nrow = n, ncol = num.summary.measures, 
                  byrow = TRUE, dimnames = list(NULL, colnames(summary.measures)))))
            if (i == 1 && j == 1) {
                main.terms.summary.measures <- array(dim = c(n, 
                  ncol(combined.summary.measures), num.regimes, 
                  num.final.Ynodes))
                beta.names <- colnames(combined.summary.measures)
            }
            main.terms.summary.measures[, , i, j] <- combined.summary.measures
        }
    }
    colnames(main.terms.summary.measures) <- paste("S", seq_len(ncol(main.terms.summary.measures)), 
        sep = "")
    main.terms.msm <- paste("Y ~ -1 +", paste(colnames(main.terms.summary.measures), 
        collapse = " + "))
    return(list(msm = main.terms.msm, summary.measures = main.terms.summary.measures, 
        beta.names = beta.names, baseline.column.names = baseline.column.names))
}

CreateInputs <- function(data, Anodes, Cnodes, Dnodes, Lnodes, Ynodes, survivalOutcome,
    Qform, gform, gbounds, Yrange, deterministic.g.function,
    SL.library, SL.cvControl, regimes, working.msm, summary.measures,
    final.Ynodes, stratify, msm.weights, estimate.time, gcomp,
    iptw.only, deterministic.Q.function, variance.method, observation.weights,
    id,verbose)
{
    if (is.list(regimes)) {
        if (!all(sapply(regimes, is.function)))
            stop("If 'regimes' is a list, then all elements should be functions.")
        regimes <- simplify2array(lapply(regimes, function(rule) drop3(RegimesFromAbar(data,
            rule = rule))), higher = TRUE)
    }
    if (!(is.null(regimes) || length(dim(regimes)) == 3)) {
        stop("regimes must be an array with 3 dimensions (unless Anodes is NULL, in which case regimes can be NULL)")
    }
    if (is.null(regimes) || dim(regimes)[3] == 0) {
        if (length(Anodes) != 0) {
            stop("regimes must not be NULL (or have dim(regimes)[3]==0) unless Anodes is also NULL")
        }
        regimes <- array(numeric(0), dim = c(nrow(data), 0, 1))
    }
    num.regimes <- dim(regimes)[3]
    if (is.logical(regimes)) {
        regimes <- regimes * 1
        message("abar or regimes was passed as logical and was converted to numeric")
    }
    all.nodes <- CreateNodes(data, Anodes, Cnodes, Dnodes, Lnodes, Ynodes)
    Qform <- CreateLYNodes(data, all.nodes, check.Qform = TRUE,
        Qform = Qform)$Qform
    data <- ConvertCensoringNodes(data, Cnodes, has.deterministic.functions = !is.null(deterministic.g.function) &&
        is.null(deterministic.Q.function))
    if (is.null(final.Ynodes)) {
        final.Ynodes <- max(all.nodes$Y)
    }
    else {
        final.Ynodes <- NodeToIndex(data, final.Ynodes)
    }
    if (identical(SL.library, "default"))
        SL.library <- get("Default.SL.Library")
    SL.library.Q <- GetLibrary(SL.library, "Q")
    SL.library.g <- GetLibrary(SL.library, "g")
    if (is.null(summary.measures)) {
        summary.measures <- matrix(nrow = num.regimes, ncol = 0)
    }
    if (length(dim(summary.measures)) == 2) {
        num.final.Ynodes <- length(final.Ynodes)
        summary.measures <- array(repmat(summary.measures, m = 1,
            n = num.final.Ynodes), dim = c(nrow(summary.measures),
            ncol(summary.measures), num.final.Ynodes), dimnames = list(rownames(summary.measures),
            colnames(summary.measures), NULL))
    }
    if (is.null(observation.weights))
        observation.weights <- rep(1, nrow(data))
    if (is.matrix(gform)) {
        if (num.regimes > 1 && variance.method != "ic")
            stop("If there is more than one regime (using ltmle with list abar or ltmleMSM) 
                 and numeric gform and variance.method != 'ic', then gform must be an array, not a matrix.")
        gform <- array(gform, dim = c(nrow(gform), ncol(gform),
            num.regimes))
    }
    check.results <- CheckInputs(data, all.nodes, survivalOutcome,
        Qform, gform, gbounds, Yrange, deterministic.g.function,
        SL.library, SL.cvControl, regimes, working.msm, summary.measures,
        final.Ynodes, stratify, msm.weights, deterministic.Q.function,
        observation.weights, gcomp, variance.method, id)
    survivalOutcome <- check.results$survivalOutcome
    if (!isTRUE(attr(data, "called.from.estimate.variance", exact = TRUE)) &&
        !isTRUE(attr(data, "skip.clean.data", exact = TRUE))) {
        data <- CleanData(data, all.nodes, deterministic.Q.function,
            survivalOutcome)
    }
    transform.list <- TransformOutcomes(data, all.nodes, Yrange)
    data <- transform.list$data
    transformOutcome <- transform.list$transformOutcome
    binaryOutcome <- check.results$binaryOutcome
    if (length(Qform) == 0)
        Qform <- GetDefaultForm(data, all.nodes, is.Qform = TRUE,
            stratify, survivalOutcome, showMessage = TRUE)
    if (length(gform) == 0)
        gform <- GetDefaultForm(data, all.nodes, is.Qform = FALSE,
            stratify, survivalOutcome, showMessage = TRUE)
    main.terms <- ConvertToMainTerms(data, working.msm, summary.measures,
        all.nodes)
    intervention.match <- CalcInterventionMatchArray(data, regimes,
        all.nodes$A)
    inputs <- list(data = data, all.nodes = all.nodes, survivalOutcome = survivalOutcome,
        Qform = Qform, gform = gform, gbounds = gbounds, Yrange = Yrange,
        deterministic.g.function = deterministic.g.function,
        SL.library.Q = SL.library.Q, SL.library.g = SL.library.g,
        SL.cvControl = SL.cvControl, regimes = regimes, working.msm = main.terms$msm,
        combined.summary.measures = main.terms$summary.measures,
        final.Ynodes = final.Ynodes, stratify = stratify, msm.weights = msm.weights,
        estimate.time = estimate.time, gcomp = gcomp, iptw.only = iptw.only,
        deterministic.Q.function = deterministic.Q.function,
        binaryOutcome = binaryOutcome, transformOutcome = transformOutcome,
        variance.method = variance.method, observation.weights = observation.weights,
        baseline.column.names = main.terms$baseline.column.names,
        beta.names = main.terms$beta.names, uncensored = check.results$uncensored,
        intervention.match = intervention.match, id = id,verbose=verbose)
    class(inputs) <- "ltmleInputs"
    if (length(all.nodes$AC) == 0)
        inputs$variance.method <- "ic"
    if (inputs$variance.method != "ic" && !is.null(VarianceAvailableWarning(inputs)))
        inputs$variance.method <- "ic"
    return(inputs)
}

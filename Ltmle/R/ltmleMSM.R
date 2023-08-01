ltmleMSM <- function(data, Anodes, Cnodes = NULL, Dnodes = NULL, Lnodes = NULL, Ynodes, 
    survivalOutcome = NULL, Qform = NULL, gform = NULL, gbounds = c(0.01, 
        1), Yrange = NULL, deterministic.g.function = NULL, SL.library = "glm", 
    SL.cvControl = list(), regimes, working.msm, summary.measures, 
    final.Ynodes = NULL, stratify = FALSE, msm.weights = "empirical", 
    estimate.time = TRUE, gcomp = FALSE, iptw.only = FALSE, deterministic.Q.function = NULL, 
    variance.method = "tmle", observation.weights = NULL, id = NULL) {
    data <- CheckData(data)
    inputs <- CreateInputs(data, Anodes, Cnodes, Dnodes, Lnodes, Ynodes, 
        survivalOutcome, Qform, gform, gbounds, Yrange, deterministic.g.function, 
        SL.library, SL.cvControl, regimes, working.msm, summary.measures, 
        final.Ynodes, stratify, msm.weights, estimate.time, gcomp, 
        iptw.only, deterministic.Q.function, variance.method, 
        observation.weights, id)
    result <- LtmleMSMFromInputs(inputs)
    result$call <- match.call()
    class(result) <- "ltmleMSM"
    return(result)
}

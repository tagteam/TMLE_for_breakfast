Ltmle_working_horse <- function (data, Anodes, Cnodes = NULL, Dnodes = NULL, Lnodes = NULL, Ynodes,
                                 survivalOutcome = NULL, Qform = NULL, gform = NULL, abar,
                                 rule = NULL, gbounds = c(0.01, 1), Yrange = NULL, deterministic.g.function = NULL,
                                 deterministic.Q.function = NULL,
                                 stratify = FALSE, SL.library = "glm", SL.cvControl = list(),
                                 estimate.time = TRUE, gcomp = FALSE, iptw.only = FALSE, 
                                 variance.method = "tmle", observation.weights = NULL, id = NULL,info = NULL,verbose=FALSE)
{
    require(matrixStats)
    for (f in list.files("z:/Workdata/706582/R-packages/Ltmle/R/",pattern = ".R$",full.names = TRUE)) {
        source(f)
    }
    for (f in list.files("z:/Workdata/706582/R-packages/Ltmle/Augmentation/",pattern = ".R$",full.names = TRUE)) {
        source(f)
    }
    data <- CheckData(data)
    msm.inputs <- GetMSMInputsForLtmle(data, abar, rule, gform)
    
    inputs <- CreateInputs(data = data, Anodes = Anodes, Cnodes = Cnodes, Dnodes = Dnodes,
                           Lnodes = Lnodes, Ynodes = Ynodes, survivalOutcome = survivalOutcome,
                           Qform = Qform, gform = msm.inputs$gform, Yrange = Yrange,
                           gbounds = gbounds, deterministic.g.function = deterministic.g.function,
                           SL.library = SL.library, SL.cvControl = SL.cvControl,
                           regimes = msm.inputs$regimes, working.msm = msm.inputs$working.msm,
                           summary.measures = msm.inputs$summary.measures, final.Ynodes = msm.inputs$final.Ynodes,
                           stratify = stratify, msm.weights = msm.inputs$msm.weights,
                           estimate.time = estimate.time, gcomp = gcomp, iptw.only = iptw.only,
                           deterministic.Q.function = deterministic.Q.function,
                           variance.method = variance.method, observation.weights = observation.weights,
                           id = id, verbose = verbose)
    result <- LtmleFromInputs(inputs)
    result$call <- match.call()
    result$info <- result$call$info
    class(result) <- "Ltmle"
    return(result)
}

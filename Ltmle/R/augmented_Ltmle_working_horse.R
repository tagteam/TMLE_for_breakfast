Ltmle_working_horse <- function(data,
                                Anodes,
                                Cnodes = NULL,
                                Dnodes = NULL,
                                Lnodes = NULL,
                                Ynodes,
                                survivalOutcome = NULL,
                                Qform = NULL,
                                gform = NULL,
                                abar,
                                rule = NULL,
                                gbounds = c(0, 1),
                                Yrange = NULL,
                                deterministic.g.function = NULL,
                                deterministic.Q.function = NULL,
                                stratify = FALSE,
                                SL.library = "glm",
                                SL.cvControl = list(),
                                estimate.time = FALSE,
                                gcomp = FALSE,
                                iptw.only = FALSE,
                                variance.method = "tmle",
                                observation.weights = NULL,
                                id = NULL,
                                info = NULL,
                                reduce = TRUE,
                                verbose=FALSE)
{
    require(matrixStats)
    if (NROW(data) == 0) stop("Argument 'data' seems empty.") 
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
    if (reduce){
        result$call <- NULL
        result$cum.g <- result$cum.g.used <- result$cum.g.unbounded <- NULL
        result$Qstar <- NULL
        result$fit$Qstar <- NULL        
    }
    class(result) <- "Ltmle"
    return(result)
}

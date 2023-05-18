ltmle <- function (data, Anodes, Cnodes = NULL, Lnodes = NULL, Ynodes,
                   survivalOutcome = NULL, Qform = NULL, gform = NULL, abar,
                   rule = NULL, gbounds = c(0.01, 1), Yrange = NULL, deterministic.g.function = NULL,
                   stratify = FALSE, SL.library = "glm", SL.cvControl = list(),
                   estimate.time = TRUE, gcomp = FALSE, iptw.only = FALSE, deterministic.Q.function = NULL,
                   variance.method = "tmle", observation.weights = NULL, id = NULL,
                   verbose=FALSE)
{
    stop("Use Ltmle instead.")
    require(matrixStats)
    if (verbose) message("Loading ltmle functions ...")
    for (f in list.files("z:/Workdata/706582/Andrew Mertens/targets_diabetes_dementia/functions/Ltmle/R/",pattern = ".R$",full.names = TRUE)) {
        source(f)
    }
    if (verbose) message("Checking data ...")
    data <- CheckData(data)
    if (verbose) message("Creating inputs ...")
    msm.inputs <- GetMSMInputsForLtmle(data, abar, rule, gform)
    inputs <- CreateInputs(data = data, Anodes = Anodes, Cnodes = Cnodes,
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
                           id = id,verbose=verbose)
    if (verbose) message("Running ltmle ...")
    result <- LtmleFromInputs(inputs)
    result$call <- match.call()
    return(result)
}

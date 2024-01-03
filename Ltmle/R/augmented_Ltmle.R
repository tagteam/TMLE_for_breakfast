##' Convenience wrapper for ltmle
##'
##' This wrapper runs the function Ltmle_working_horse
##' @title Augmented Longitudinal Minimum Loss Estimation
#'@param data data frame following the time-ordering of the nodes. See 'Details'.
#' @param Anodes column names or indicies in \code{data} of treatment nodes
#' @param Cnodes column names or indicies in \code{data} of censoring nodes
#' @param Lnodes column names or indicies in \code{data} of time-dependent
#' covariate nodes
#' @param Ynodes column names or indicies in \code{data} of outcome nodes
#' @param survivalOutcome If \code{TRUE}, then Y nodes are indicators of an
#' event, and if Y at some time point is 1, then all following should be 1.
#' Required to be \code{TRUE} or \code{FALSE} if outcomes are binary and there
#' are multiple Ynodes.
#' @param Qform character vector of regression formulas for \eqn{Q}. See
#' 'Details'.
#' @param gform character vector of regression formulas for \eqn{g} or a
#' matrix/array of prob(A=1). See 'Details'.
#' @param abar binary vector (numAnodes x 1) or matrix (n x numAnodes) of
#' counterfactual treatment or a list of length 2. See 'Details'.
#' @param time_horizon sequence of target time horizons. The update step of
#' the Ltmle analysis is performed at each time horizon separately.
#' @param rule a function to be applied to each row (a named vector) of
#' \code{data} that returns a numeric vector of length numAnodes. See 'Details'.
#' @param gbounds lower and upper bounds on estimated cumulative probabilities
#' for g-factors. Vector of length 2, order unimportant.
#' @param Yrange NULL or a numerical vector where the min and max of
#' \code{Yrange} specify the range of all Y nodes. See 'Details'.
#' @param deterministic.g.function optional information on A and C nodes that
#' are given deterministically. See 'Details'. Default \code{NULL} indicates no
#' deterministic links.
#' @param stratify if \code{TRUE} stratify on following \code{abar} when
#' estimating Q and g. If \code{FALSE}, pool over \code{abar}.
#' @param SL.library optional character vector of libraries to pass to
#' \code{\link[SuperLearner:SuperLearner]{SuperLearner}}. \code{NULL} indicates
#' \link{glm} should be called instead of
#' \code{\link[SuperLearner:SuperLearner]{SuperLearner}}. '\code{default}'
#' indicates a standard set of libraries. May be separately specified for
#' \eqn{Q} and \eqn{g}. See 'Details'.
#' @param SL.cvControl optional list to be passed as \code{cvControl} to \code{\link[SuperLearner:SuperLearner]{SuperLearner}}
#' @param estimate.time if \code{TRUE}, run an initial estimate using only 50
#' observations and use this to print a very rough estimate of the total time
#' to completion. No action if there are fewer than 50 observations.
#' @param gcomp if \code{TRUE}, run the maximum likelihood based G-computation
#' estimate \emph{instead} of TMLE
#' @param regimes binary array: n x numAnodes x numRegimes of counterfactual
#' treatment or a list of 'rule' functions
#' @param working.msm character formula for the working marginal structural
#' model
#' @param summary.measures array: num.regimes x num.summary.measures x
#' num.final.Ynodes - measures summarizing the regimes that will be used on the
#' right hand side of \code{working.msm} (baseline covariates may also be used
#' in the right hand side of \code{working.msm} and do not need to be included
#' in \code{summary.measures})
#' @param final.Ynodes vector subset of Ynodes - used in MSM to pool over a set
#' of outcome nodes
#' @param msm.weights projection weights for the working MSM. If "empirical",
#' weight by empirical proportions of rows matching each regime for each
#' final.Ynode, with duplicate regimes given zero weight. If \code{NULL}, no
#' weights. Or an array of user-supplied weights with dimensions c(n,
#' num.regimes, num.final.Ynodes) or c(num.regimes, num.final.Ynodes).
#' @param iptw.only by default (\code{iptw.only = FALSE}), both TMLE and IPTW
#' are run in \code{ltmle} and \code{ltmleMSM}. If \code{iptw.only = TRUE},
#' only IPTW is run, which is faster.
#' @param deterministic.Q.function optional information on Q given
#' deterministically. See 'Details'. Default \code{NULL} indicates no
#' deterministic links.
#' @param observation.weights observation (sampling) weights. Vector of length
#' n. If \code{NULL}, assumed to be all 1.
#' @param variance.method Method for estimating variance of TMLE. 
#' One of "ic", "tmle", "iptw". If "tmle", compute both the robust variance
#' estimate using TMLE and the influence curve based variance estimate (use the
#' larger of the two). If "iptw", compute both the robust variance
#' estimate using IPTW and the influence curve based variance estimate (use the
#' larger of the two). If "ic", only compute the influence curve based
#' variance estimate. "ic" is fastest, but may be substantially
#' anti-conservative if there are positivity violations or rare outcomes. "tmle" is
#' slowest but most robust if there are positivity violations or rare outcomes. 
#' "iptw" is a compromise between speed and robustness.
#' variance.method="tmle" or "iptw" are not yet available with non-binary outcomes, 
#' gcomp=TRUE, stratify=TRUE, or deterministic.Q.function.
#' @param id Household or subject identifiers. Vector of length n or \code{NULL}. 
#' Integer, factor, or character recommended, but any type that can be coerced 
#' to factor will work. \code{NULL} means all distinct ids.
##' @param info Unclear
##' @param verbose Shut up the messages and warnings
##' @param ... Unclear
##' @return See ltmle
##' @seealso ltmle
##' @examples
##' library(lava)
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
Ltmle <- function(data,
                  Anodes,
                  Cnodes = NULL,
                  Dnodes = NULL,
                  Lnodes = NULL,
                  Ynodes,
                  survivalOutcome = NULL,
                  Qform = NULL,
                  gform = NULL,
                  abar,
                  time_horizon,
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
                  variance.method = "ic",
                  B = 20,
                  m = 0.632,
                  observation.weights = NULL,
                  id = NULL,
                  info = NULL,
                  verbose=FALSE,
                  subsampling=FALSE,
                  ...){#ltmle_dir = NULL,...){
    requireNamespace("matrixStats")
    requireNamespace("foreach")
    requireNamespace("data.table")
    if ("glmnet" %in% SL.library)
        if (length(SL.cvControl)==0)
            SL.cvControl=list(selector="undersmooth",alpha=0.5)
    name_competing_risk = gsub("_[^_]*$", "", Dnodes[[1]])
    if(length(Dnodes)>0){
        survivalOutcome=TRUE
        if (length(deterministic.Q.function)>0){
            stop("Cannot both specify deterministic.Q.function and Dnodes.")
        }
        deterministic.Q.function <- function(data, current.node, nodes, called.from.estimate.g){
            death.index <- grep(paste0(name_competing_risk, "_"),names(data))
            if(length(death.index)==0)stop("No death/terminal event node found")
            hist.death.index <- death.index[death.index < current.node]
            if(length(hist.death.index)==0)
                return(NULL)
            else{
                is.deterministic <- Reduce("+",lapply(data[,hist.death.index,drop=FALSE],
                                                      function(dd){x=dd;x[is.na(dd)] <- 0;x}))>=1
                # should be unnecessary to exclude those who readily
                # have a missing value for death, but it does not hurt either
                is.deterministic[is.na(is.deterministic)] <- FALSE
                list(is.deterministic=is.deterministic, Q.value=0)
            }
        }
    } 
    result <- foreach(time = time_horizon, .export = "ShowGlmMessage")%do%{
        cut <- cut_Ltmle(data = data, Anodes = Anodes, Cnodes = Cnodes, Dnodes = Dnodes, Lnodes = Lnodes, Ynodes = Ynodes,
                         survivalOutcome = survivalOutcome, Qform = Qform, gform = gform, abar = abar, time_horizon = time,
                         rule = rule, gbounds = gbounds, Yrange = Yrange, deterministic.g.function = deterministic.g.function,
                         stratify = stratify, SL.library = SL.library, SL.cvControl = SL.cvControl, estimate.time = estimate.time, 
                         gcomp = gcomp, iptw.only = iptw.only, variance.method = variance.method, 
                         observation.weights = observation.weights, id = id, info = info, verbose = verbose,...)
        do.call(Ltmle_working_horse, c(cut, list(deterministic.Q.function = deterministic.Q.function))) 
    }
    if(length(time_horizon)==1){result <- result[[1]]}
    else{names(result) <- paste0("Time horizon ", time_horizon)}
    if (subsampling){
        if (m <= 0 | m >= 1) stop("m must be between 0 and 1 for subsampling")
        res_boot <- foreach(b = 1:B) %do% {
            if (verbose) message("Bootstrap ", b, " of ", B, "...")
            temp_boot <- NULL
            while (inherits(temp_boot,"simpleError") || is.null(temp_boot)){
                temp_boot <- tryCatch({foreach(time = time_horizon)%do%{
                    dat_subsample <- data[sample(1:nrow(data), size = floor(m*nrow(data)), replace = FALSE),]
                    cut <- cut_Ltmle(data = dat_subsample, Anodes = Anodes, Cnodes = Cnodes, Dnodes = Dnodes, Lnodes = Lnodes, Ynodes = Ynodes,
                                     survivalOutcome = survivalOutcome, Qform = Qform, gform = gform, abar = abar, time_horizon = time,
                                     rule = rule, gbounds = gbounds, Yrange = Yrange, deterministic.g.function = deterministic.g.function,
                                     stratify = stratify, SL.library = SL.library, SL.cvControl = SL.cvControl, estimate.time = estimate.time, 
                                     gcomp = gcomp, iptw.only = iptw.only, variance.method = variance.method, 
                                     observation.weights = observation.weights, id = id, info = info, verbose = FALSE,...)
                    do.call(Ltmle_working_horse, c(cut, list(deterministic.Q.function = deterministic.Q.function))) 
                }}, error = function(e) e)
                if (inherits(temp_boot,"simpleError")){
                    message("Error in bootstrap ", b, " of ", B, ": ", temp_boot$message)
                    message("Trying a new subsample")
                    message("If you see a lot of these messages, consider increasing m")
                }
            }
            if(length(time_horizon)==1){temp_boot <- temp_boot[[1]]}
            else{names(temp_boot) <- paste0("Time horizon ", time_horizon)}
            temp_boot
        }
        result <- list(result = result, res_boot = res_boot, m = round(m*nrow(data)), n = nrow(data))
        class(result) <- "Ltmle_boot"
    }
    return(result)
}

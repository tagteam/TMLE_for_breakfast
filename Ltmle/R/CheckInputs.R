CheckInputs <-
    function (data, nodes, survivalOutcome, Qform, gform, gbounds,
              Yrange, deterministic.g.function, SL.library, SL.cvControl,
              regimes, working.msm, summary.measures, final.Ynodes, stratify,
              msm.weights, deterministic.Q.function, observation.weights,
              gcomp, variance.method, id)
{
    stopifnot(length(dim(regimes)) == 3)
    num.regimes <- dim(regimes)[3]
    if (!is.glm(GetLibrary(SL.library, "Q")) || !is.glm(GetLibrary(SL.library,
                                                                   "g"))) {
        if (!requireNamespace("SuperLearner"))
            stop("SuperLearner package is required if SL.library is not NULL or 'glm'")
    }
    if (!is.list(SL.cvControl)) {
        stop("SL.cvControl must be a list")
    }
    ## if (!(all(names(SL.cvControl) %in% c("V", "stratifyCV", "shuffle")))) {
    ## stop("The valid names for SL.cvControl are V, stratifyCV, shuffle. validRows is not currently supported. See ?SuperLearner.CV.control")
    ## }
    if (is.unsorted(nodes$A, strictly = TRUE))
        stop("Anodes must be in increasing order")
    if (is.unsorted(nodes$C, strictly = TRUE))
        stop("Cnodes must be in increasing order")
    if (is.unsorted(nodes$D, strictly = TRUE))
        stop("Dnodes must be in increasing order")
    if (is.unsorted(nodes$L, strictly = TRUE))
        stop("Lnodes must be in increasing order")
    if (is.unsorted(nodes$Y, strictly = TRUE))
        stop("Ynodes must be in increasing order")
    if (is.unsorted(final.Ynodes, strictly = TRUE))
        stop("final.Ynodes must be in increasing order")
    if (length(nodes$L) > 0) {
        if (max(nodes$L) > max(nodes$Y))
            stop("Lnodes are not allowed after the final Y node")
    }
    if (length(nodes$D) > 0) {
        if (max(nodes$D) > max(nodes$Y))
            stop("Dnodes are not allowed after the final Y node")
    }
    all.nodes <- c(nodes$A, nodes$C, nodes$D, nodes$L, nodes$Y)
    if (length(all.nodes) > length(unique(all.nodes)))
        stop("A node cannot be listed in more than one of Anodes, Cnodes, Dnodes, Lnodes, Ynodes",
             "\nThe following variables are listed in more than one Xnodes list:\n",
             paste0(names(data)[all.nodes[duplicated(all.nodes)]],collapse=", "))
    if (is.null(nodes$Y))
        stop("Ynodes cannot be null")
    if (length(nodes$AC) > 0 && !all(sseq(min(nodes$AC), ncol(data)) %in%
                                     all.nodes)) {
        tmp=sseq(min(nodes$AC), ncol(data))
        stop(paste0("The following variables are not listed as A-, C-, D-, L-, or Y-nodes:\n",
                    paste(names(data)[tmp[!(tmp%in%all.nodes)]],collapse=", "),
                    "\n",
                    "All nodes after the first A/C node must be in A-, C-, L-, or Ynodes"))
        ## stop("All nodes after the first A/C node must be in A-, C-, L-, or Ynodes")
    }
    for (reserved.name in c("observation.weights", "Q.kplus1",
                            "Qstar")) {
        if (reserved.name %in% names(data))
            stop(paste(reserved.name, "is reserved and may not be used as a column name of data"))
    }
    if (length(variance.method) != 1 || !(variance.method %in%
                                          c("ic", "tmle", "iptw"))) {
        stop("variance.method must be one of 'ic', 'tmle', 'iptw'")
    }
    if (length(gform) > 0) {
        if (is.character(gform)) {
            if (length(gform) != length(nodes$AC))
                stop("length(gform) != length(c(Anodes, Cnodes))")
            for (i in seq_along(gform)) {
                if (LhsVars(gform[i]) != names(data)[nodes$AC[i]]) {
                    stop("The LHS of gform[", i, "] should be the name of the ",
                         i, "th A or C node.")
                }
                parents <- if (nodes$AC[i] > 1) {
                               names(data)[1:(nodes$AC[i] - 1)]
                           }
                           else {
                               NULL
                           }
                if (!all(rout <- RhsVars(gform[i]) %in% parents)) {
                    stop("Some nodes in gform[", i, "] are not parents of ",
                         LhsVars(gform[i]),".\nParents: ",
                         paste0(parents,collapse=","),
                         "\nUnmatched nodes:" ,
                         paste0(RhsVars(gform[i])[!rout],collapse=","))
                }
                if (any(RhsVars(gform[i]) %in% names(data)[nodes$C]))
                    stop("Cnodes should not be used as RHS variables in gform (regressions are only run on uncensored observations so including a Cnode has no effect and slows down regressions)")
            }
        }
        else {
            if (!is.numeric(gform))
                stop("gform should be a character vector or numeric")
            if (nrow(gform) != nrow(data))
                stop("if gform is numeric, it should have the same number of rows as data")
            if (ncol(gform) != length(nodes$AC))
                stop("if gform is numeric, it should have the same number of columns as length(c(Anodes, Cnodes))")
            if (length(dim(gform)) != 3 || dim(gform)[3] != num.regimes)
                stop("if gform is numeric, dim[3] should be num.regimes (gform can also be a matrix if variance.method == 'ic')")
            if (!is.null(deterministic.g.function))
                stop("if gform is numeric, deterministic.g.function must be NULL")
            if (max(gform, na.rm = TRUE) > 1 || min(gform, na.rm = TRUE) <
                0)
                stop("if gform is numeric, all values should be probabilities")
            if (!is.null(deterministic.Q.function) && !isTRUE(attr(data,
                                                                   "called.from.estimate.variance", exact = TRUE)))
                warning("If gform is numeric and deterministic.Q.function is not NULL, 
                        deterministic.Q.function will only affect g based on the observed values of the Anodes, 
                        not the counterfactual values. If your deterministic.Q.function depends on the values of the Anodes,
                        it is recommended to not use numeric gform.")
        }
    }
    if (length(Qform) > 0) {
        if (!is.character(Qform))
            stop("Qform should be a character vector")
        if (length(Qform) != length(nodes$LY))
            stop("length of Qform is not equal to number of D/L/Y nodes")
        for (i in seq_along(Qform)) {
            if (length(names(Qform[i])) == 0)
                stop("Each element of Qform must be named. The name must match the name of the corresponding D/L/Y node in data.")
            if (names(Qform[i]) != names(data)[nodes$LY[i]])
                stop("The name of each element of Q must match the name of the corresponding D/L/Y node in data.")
            if (Qform[i] != "IDENTITY") {
                if (LhsVars(Qform[i]) != "Q.kplus1")
                    stop("LHS of each Qform should be Q.kplus1")
                parents <- names(data)[1:(nodes$LY[i] - 1)]
                if (!all(RhsVars(Qform[i]) %in% parents)) {
                    stop("Some nodes in Qform[", i, "] are not parents of ",
                         names(Qform[i]))
                }
                if (any(RhsVars(Qform[i]) %in% names(data)[nodes$C]))
                    stop("Cnodes should not be used as RHS variables in Qform 
                         (regressions are only run on uncensored observations so including a Cnode has no effect and slows down regressions)")
            }
        }
    }
    if (length(gbounds) != 2)
        stop("gbounds should have length 2")
    if (!(is.null(deterministic.g.function) || is.function(deterministic.g.function)))
        stop("deterministic.g.function should be a function or NULL")
    if (!all(unlist(data[, nodes$A]) %in% c(0, 1, NA)))
        stop("in data, all Anodes should be binary")
    if (!all(sapply(data[, c(nodes$A, nodes$Y), drop = FALSE], is.numeric)))
        stop("in data, all Anodes and Ynodes should be numeric (not, for instance, logical)")
    if (any(sapply(data, is.infinite)))
        stop("infinite values are not supported in data")
    all.Y <- unlist(data[, nodes$Y])
    binaryOutcome <- all(all.Y %in% c(0, 1, NA))
    if (binaryOutcome) {
        if (is.null(survivalOutcome)) {
            if (length(nodes$Y) == 1) {
                survivalOutcome <- FALSE
            }
            else {
                stop("All Ynodes are 0, 1, or NA; the outcome is treated as binary. The 'survivalOutcome' argument must be specified if there are multiple Ynodes.")
            }
        }
        if (!is.null(Yrange) && !is.equal(Yrange, c(0L, 1L))) {
            stop("All Ynodes are 0, 1, or NA, but Yrange is something other than NULL or c(0, 1)")
        }
    }
    else {
        if (is.null(survivalOutcome))
            survivalOutcome <- FALSE
        if (survivalOutcome)
            stop("When survivalOutcome is TRUE, all Ynodes should be 0, 1, or NA")
    }
    uncensored.array <- CalcUncensoredMatrix(data, nodes$C)
    for (Ynode in nodes$Y) {
        uncensored <- IsUncensored(uncensored.array, nodes$C,
                                   cur.node = Ynode)
        deterministic <- IsDeterministic(data, cur.node = Ynode,
                                         deterministic.Q.function = NULL, nodes, called.from.estimate.g = FALSE,
                                         survivalOutcome)$is.deterministic
        if (anyNA(data[deterministic, Ynode]))
            stop("For survival outcomes, Ynodes can never have missing values, they have to be either 0 or 1:\n0=(no event, competing event or censored)\n1=(event has occurrred).")
        if (length(nodes$D)>0){
            # PLAUSIBILITY CHECK MISSING 
                ## stop("For survival outcomes, once a Ynode jumps to 1 (e.g. death), all subsequent Ynode values should be 1")
        }else{
            if (!all(data[deterministic, Ynode] == 1)){
                stop("For survival outcomes, once a Ynode jumps to 1 (e.g. death), all subsequent Ynode values should be 1")
            }
        }
        if (anyNA(data[uncensored, Ynode]))
            stop("Ynodes may not be NA except after censoring")
    }
    if (!is.equal(dim(regimes)[1:2], c(nrow(data), length(nodes$A))))
        stop("Problem with abar or regimes:\n   In ltmleMSM, regimes should have dimensions n x num.Anodes x num.regimes\n   In ltmle, abar should be a matrix with dimensions n x num.Anodes or a vector with length num.Anodes")
    stopifnot(num.regimes == nrow(summary.measures))
    if (!all(regimes %in% c(0, 1, NA)))
        stop("all regimes should be binary")
    for (Anode.index in seq_along(nodes$A)) {
        first.LYnode <- min(nodes$LY)
        cur.node <- nodes$A[Anode.index]
        uncensored <- IsUncensored(uncensored.array, Cnodes = nodes$C,
                                   cur.node = cur.node)
        deterministic <- IsDeterministic(data, cur.node, deterministic.Q.function,
                                         nodes, called.from.estimate.g = TRUE, survivalOutcome)$is.deterministic
        if (anyNA(regimes[uncensored & !deterministic, Anode.index,
                          ])) {
            stop("NA in regimes/abar not allowed (except after censoring/death)")
        }
        if (cur.node < first.LYnode && anyNA(regimes[!deterministic,
                                                     Anode.index, ])) {
            warning("NA in regimes/abar before the first L/Y node will probably cause an error")
        }
    }
    num.final.Ynodes <- length(final.Ynodes)
    if ((length(dim(summary.measures)) != 3) || !is.equal(dim(summary.measures)[c(1,
                                                                                  3)], c(num.regimes, num.final.Ynodes)))
        stop("summary.measures should be an array with dimensions num.regimes x num.summary.measures x num.final.Ynodes")
    if (class(working.msm) != "character")
        stop("class(working.msm) must be 'character'")
    if (LhsVars(working.msm) != "Y")
        stop("the left hand side variable of working.msm should always be 'Y' [this may change in future releases]")
    if (!is.vector(observation.weights) || length(observation.weights) !=
        nrow(data) || anyNA(observation.weights) || any(observation.weights <
                                                        0) || max(observation.weights) == 0)
        stop("observation.weights must be NULL or a vector of length nrow(data) with no NAs, no negative values, and at least one positive value")
    if (!(is.null(msm.weights) || is.equal(msm.weights, "empirical") ||
          is.equal(dim(msm.weights), c(nrow(data), num.regimes,
                                       num.final.Ynodes)) || is.equal(dim(msm.weights),
                                                                      c(num.regimes, num.final.Ynodes)))) {
        stop("msm.weights must be NULL, 'empirical', or an array with dim(msm.weights) = c(n, num.regimes, num.final.Ynodes) or c(num.regimes, num.final.Ynodes)")
    }
    if (!(is.null(id) || ((is.factor(id) || is.vector(id)) &&
                          length(id) == nrow(data))))
        stop("id must be a vector with length nrow(data) or be NULL")
    return(list(survivalOutcome = survivalOutcome, binaryOutcome = binaryOutcome,
                uncensored = uncensored.array))
}

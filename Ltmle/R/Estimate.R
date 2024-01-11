# called from EstimateG
Estimate <- function(inputs,
                     form,
                     subs,
                     family,
                     type,
                     nodes,
                     Qstar.kplus1,
                     cur.node,
                     calc.meanL,
                     called.from.estimate.g,
                     regimes.meanL,
                     regimes.with.positive.weight)
{
    FitAndPredict <- function() {
        if (length(Y.subset) < 2)
            stop("Estimation failed because there are fewer than 2 observations to fit")
        Y.subset.range <- range(Y.subset)
        if (anyNA(Y.subset.range))
            stop("Internal error - NA in Y during Estimate")
        if (Y.subset.range[1] < -0.0001 || Y.subset.range[2] >
            1.0001)
            stop("Internal error - Y negative or greater than 1 in Estimate")
        if (Y.subset.range[2] - Y.subset.range[1] < 0.0001) {
            Y.value <- Y.subset.range[2]
            m <- list("no estimation occured because all Y values are the same",
                      Y.value = Y.value)
            predicted.values <- ValuesByType(rep(Y.value, nrow(newdata)))
            class(m) <- "no.Y.variation"
        }
        else {
            if (use.glm) {
                if (inputs$verbose){ message("Estimate: calling ltmle.glm.fit ...")}
                SuppressGivenWarnings({
                    m <- ltmle.glm.fit(y = Y.subset, x = X.subset,
                                       family = family, weights = observation.weights.subset,
                                       offset = offst, intercept = intercept)
                    if (inputs$verbose){
                        cat(paste("Sample size: ",m$n),"\n")
                    }
                    m$terms <- tf
                    predicted.values <- predict(m,newdata = newdata,type = type)
                }, GetWarningsToSuppress())
            }
            else {
                newX.list <- GetNewX(newdata)
                SetSeedIfRegressionTesting()
                if (!is.list(SL.library) && SL.library[[1]]=="glmnet"){
                    if (inputs$verbose){ message("Estimate: calling ltmle.glmnet ...")}
                    try.result <- try({
                        m <- ltmle.glmnet(Y=Y.subset,
                                          X=X.subset,
                                          newX=newX.list$newX,
                                          family=family,
                                          obsWeights=observation.weights.subset,
                                          id =id.subset,
                                          alpha=inputs$SL.cvControl$alpha,
                                          selector=inputs$SL.cvControl$selector
                                          )
                    })
                    predicted.values <- ProcessSLPrediction(pred=m$predicted.values,
                                                            new.subs=newX.list$new.subs,
                                                            try.result=try.result)
                    m <- m$fit
                }else{
                    try.result <- try({
                        SuppressGivenWarnings(
                            m <- SuperLearner::SuperLearner(Y = Y.subset,
                                                            X = X.subset, SL.library = SL.library, cvControl = inputs$SL.cvControl,
                                                            verbose = FALSE, family = family, newX = newX.list$newX,
                                                            obsWeights = observation.weights.subset,
                                                            id = id.subset, env = environment(SuperLearner::SuperLearner)),
                            c("non-integer #successes in a binomial glm!",
                              "prediction from a rank-deficient fit may be misleading"))
                    })
                    predicted.values <- ProcessSLPrediction(m$SL.predict,
                                                            newX.list$new.subs, try.result)
                }
            }
        }
        return(list(m = m, predicted.values = predicted.values))
    }
    GetSLStopMsg <- function(Y) {
        ifelse(all(Y %in% c(0, 1, NA)), "", "\n Note that some SuperLeaner libraries crash when called with continuous dependent variables, as in the case of initial Q regressions with continuous Y or subsequent Q regressions even if Y is binary.")
    }
    ProcessSLPrediction <- function(pred, new.subs, try.result) {
        if (inherits(try.result, "try-error")) {
            stop(paste("\n\nError occured during call to SuperLearner:\n",
                       form, GetSLStopMsg(Y.subset), "\n The error reported is:\n",
                       try.result))
        }
        if (all(is.na(pred))) {
            stop(paste("\n\n Unexpected error: SuperLearner returned all NAs during regression:\n",
                       form, GetSLStopMsg(Y.subset)))
        }
        predicted.values <- rep(NA, nrow(newdata))
        predicted.values[new.subs] <- pred
        if (max(predicted.values, na.rm = TRUE) > 1 || min(predicted.values,
                                                        na.rm = TRUE) < 0) {
            msg <- paste("SuperLearner returned predicted.values > 1 or < 0: [min, max] = [",
                         min(predicted.values, na.rm = TRUE), ",", max(predicted.values,
                                                                    na.rm = TRUE), "]. Bounding to [0,1]")
            warning(msg)
            predicted.values <- Bound(predicted.values, bounds = c(0,
                                                                   1))
        }
        return(ValuesByType(predicted.values))
    }
    PredictOnly <- function(newdata1) {
        if (class(m)[1] == "no.Y.variation")
            return(rep(m$Y.value, nrow(newdata1)))
        if (use.glm) {
            SuppressGivenWarnings(pred <- predict(m, newdata1,
                                                  type), "prediction from a rank-deficient fit may be misleading")
        }
        else {
            if (inputs$verbose) print(SL.library)
            if  (SL.library[[1]]=="glmnet"){
                newX.list <- GetNewX(newdata1)
                pred <- ProcessSLPrediction(pred=predict(m,newX=newX.list$newX),
                                            new.subs=newX.list$new.subs,
                                            try.result = NULL)

            }else{
                newX.list <- GetNewX(newdata1)
                pred <- ProcessSLPrediction(predict(m,newX.list$newX,X.subset,Y.subset,onlySL = TRUE)$pred,
                                            newX.list$new.subs,
                                            try.result = NULL)
            }
        }
        return(pred)
    }
    ValuesByType <- function(x) {
        if (type == "link") {
            stopifnot(family$family %in% c("binomial", "quasibinomial"))
            qlogis(Bound(x, bounds = c(0.0001, 0.9999)))
        }
        else {
            x
        }
    }
    GetNewX <- function(newdata1) {
        new.mod.frame <- model.frame(f, data = newdata1, drop.unused.levels = TRUE,
                                     na.action = na.pass)
        newX.temp <- model.matrix(terms(f), new.mod.frame)
        Xnames=colnames(newX.temp)
        if (!use.glm) {
            colnames(newX.temp) <- paste0("Xx.", seq_len(ncol(newX.temp)))
        }
        new.subs <- !matrixStats::rowAnyMissings(newX.temp)
        newX <- as.data.frame(newX.temp[new.subs, , drop = FALSE])
        if (ncol(X) == 1) {
            X.subset <<- cbind(X.subset, ltmle.added.constant = 1)
            newX <- cbind(newX, ltmle.added.constant = 1)
        }
        attr(newX,"Xnames") <- Xnames
        return(list(newX = newX, new.subs = new.subs))
    }
    PredictProbAMeanL <- function() {
        probAis1.meanL <- matrix(NaN, nrow(inputs$data), length(nodes$LY) -
                                                         1)
        if (ncol(probAis1.meanL) == 0)
            return(probAis1.meanL)
        all.LY.nodes <- sort(union(nodes$L, nodes$Y))
        LYindex <- length(nodes$LY)
        for (i in rev(seq_along(all.LY.nodes))) {
            regression.node <- all.LY.nodes[i]
            L <- data[single.subs, regression.node]
            if (is.numeric(L) && !IsBinary(L)) {
                meanL <- mean(L, na.rm = TRUE)
            }
            else {
                meanL <- Mode(L, na.rm = TRUE)
            }
            newdata.meanL[, regression.node] <- meanL
            if (regression.node %in% nodes$LY[seq_along(nodes$LY) -
                                              1]) {
                LYindex <- LYindex - 1
                probAis1.meanL[, LYindex] <- PredictOnly(newdata = newdata.meanL)
            }
        }
        if (anyNA(probAis1.meanL[, 1]))
            stop("NA in probAis1.meanL[, 1]")
        return(probAis1.meanL)
    }
    #
    # Body of function Estimate starts here
    #
    stopifnot(type %in% c("link", "response"))
    num.regimes <- dim(inputs$regimes)[3]
    if (form == "IDENTITY") {
        stopifnot(is.vector(Qstar.kplus1) == 1)
        predicted.values <- ValuesByType(matrix(Qstar.kplus1,
                                                nrow = nrow(inputs$data), ncol = num.regimes))
        fit <- as.list(rep("no fit because form == IDENTITY",
                           num.regimes))
        deterministic.list.olddata <- IsDeterministic(inputs$data,
                                                      cur.node, inputs$deterministic.Q.function, nodes,
                                                      called.from.estimate.g, inputs$survivalOutcome)
        is.deterministic <- matrix(deterministic.list.olddata$is.deterministic,
                                   nrow = nrow(inputs$data), ncol = num.regimes)
        deterministic.Q <- matrix(NA, nrow(inputs$data), num.regimes)
        deterministic.Q[is.deterministic, ] <- deterministic.list.olddata$Q
        return(list(predicted.values = predicted.values, fit = fit,
                    is.deterministic = is.deterministic, deterministic.Q = deterministic.Q,
                    prob.A.is.1.meanL = NULL))
    }
    data <- inputs$data
    if (cur.node %in% nodes$C) {
        data[, cur.node] <- ConvertCensoringNodeToBinary(data[, cur.node])
    }
    f <- as.formula(form)
    SL.library <- if (called.from.estimate.g)
                      inputs$SL.library.g
                  else inputs$SL.library.Q
    use.glm <- (is.glm(SL.library) || length(RhsVars(f)) == 0)
    first.regime <- min(regimes.with.positive.weight)
    if (is.null(Qstar.kplus1)) {
        data.with.Qstar <- data
    }
    else {
        if (is.matrix(Qstar.kplus1)) {
            data.with.Qstar <- cbind(data, Q.kplus1 = Qstar.kplus1[,
                                                                   first.regime])
        }
        else {
            data.with.Qstar <- cbind(data, Q.kplus1 = Qstar.kplus1)
        }
    }
    if (inputs$verbose){ message("Estimate: framing formula ",form," into Y and X...")}
    ## if(length(grep("Censored_2",form)>0))    browser(skipCalls=1L)
    mod.frame <- model.frame(f, data = data.with.Qstar, drop.unused.levels = TRUE,
                             na.action = na.pass)
    Y <- mod.frame[[1]]
    tf <- terms(f)
    X <- model.matrix(tf, mod.frame)
    offst <- model.offset(mod.frame)
    intercept <- attributes(tf)$intercept
    if (!use.glm) {
        if (is.equal(family, quasibinomial()))
            family <- binomial()
        if (!is.null(offst))
            stop("offset in formula not supported with SuperLearner")
        colnames(X) <- paste0("Xx.", seq_len(ncol(X)))
        X <- as.data.frame(X)
    }
    fit <- vector("list", num.regimes)
    predicted.values <- deterministic.Q <- matrix(NA, nrow(data),
                                                  num.regimes)
    is.deterministic <- matrix(FALSE, nrow(data), num.regimes)
    fit.and.predict <- NULL
    multiple.subs <- is.matrix(subs)
    multiple.Qstar <- is.matrix(Qstar.kplus1)
    if (calc.meanL) {
        prob.A.is.1.meanL <- array(NaN, dim = c(nrow(inputs$data),
                                                num.regimes, length(nodes$LY) - 1))
    }
    else {
        prob.A.is.1.meanL <- NULL
    }
    for (regime.index in regimes.with.positive.weight) {
        newdata <- SetA(data = data.with.Qstar, regimes = inputs$regimes,
                        Anodes = nodes$A, regime.index = regime.index, cur.node = cur.node)
        deterministic.list.newdata <- IsDeterministic(newdata,
                                                      cur.node, inputs$deterministic.Q.function, nodes,
                                                      called.from.estimate.g, inputs$survivalOutcome)
        if (called.from.estimate.g && !is.null(inputs$deterministic.g.function)) {
            newdata.with.current <- newdata
            stopifnot(cur.node %in% nodes$AC)
            if (cur.node %in% nodes$A) {
                newdata.with.current[, cur.node] <- inputs$regimes[,
                                                                   which(nodes$A == cur.node), regime.index]
            }
            else {
                newdata.with.current <- newdata
            }
            deterministic.g.list.newdata <- IsDeterministicG(newdata.with.current,
                                                             cur.node, inputs$deterministic.g.function, nodes,
                                                             using.newdata = TRUE)
        }
        else {
            deterministic.g.list.newdata <- list(is.deterministic = rep(FALSE,
                                                                        nrow(data)), prob1 = NULL)
        }
        if (regime.index > first.regime && multiple.Qstar) {
            Y <- Qstar.kplus1[, regime.index]
        }
        if (regime.index == first.regime || multiple.subs) {
            single.subs <- if (multiple.subs)
                               subs[, regime.index]
                           else subs
            X.subset <- X[single.subs, , drop = FALSE]
            id.subset <- inputs$id[single.subs]
            ## if (any(is.na(single.subs))) browser()
            if (any(single.subs))
                X.subset[, colAlls(X.subset == 0)] <- 1
            observation.weights.subset <- inputs$observation.weights[single.subs]
        }
        if (regime.index == first.regime || multiple.subs ||
            multiple.Qstar) {
            Y.subset <- Y[single.subs]
            if (anyNA(Y.subset)){
                stop("NA in Estimate")
            }
        }
        if (!all(deterministic.list.newdata$is.deterministic |
                 deterministic.g.list.newdata$is.deterministic)) {
            if (is.null(fit.and.predict) || multiple.Qstar || multiple.subs) {
                ## print("fit.and.predict");print(is.null(fit.and.predict))
                ## print("multiple.Qstar");print(multiple.Qstar)
                ## print("multiple.subs");print(multiple.subs)
                fit.and.predict <- FitAndPredict()
                m <- fit.and.predict$m
                predicted.values[, regime.index] <- fit.and.predict$predicted.values
            }
            else {
                predicted.values[, regime.index] <- PredictOnly(newdata)
            }
            if (calc.meanL)
                prob.A.is.1.meanL[, regime.index, ] <- PredictProbAMeanL()
        } else {
            m <- "all rows are deterministic, no estimation took place"
        }
        predicted.values[deterministic.g.list.newdata$is.deterministic,
                         regime.index] <- deterministic.g.list.newdata$prob1
        if (calc.meanL)
            prob.A.is.1.meanL[deterministic.g.list.newdata$is.deterministic,
                              regime.index, ] <- deterministic.g.list.newdata$prob1
        is.deterministic[, regime.index] <- deterministic.list.newdata$is.deterministic
        if (!called.from.estimate.g)
            deterministic.Q[deterministic.list.newdata$is.deterministic,
                            regime.index] <- deterministic.list.newdata$Q
        if (isTRUE(attr(SL.library, "return.fit", exact = TRUE))) {
            fit[[regime.index]] <- m
        }
        else {
            if (use.glm) {
                if (class(m)[1] %in% c("speedglm", "glm")) {
                    fit[[regime.index]] <- summary(m)$coefficients
                        ## list(logistic_regression = summary(m)$coefficients,predicted=table(cut(predicted.values,include.lowest=TRUE,breaks=c(1,.75,0.5,.25,10^{-(1:8)}))))
                }
                else {
                    stopifnot(class(m)[1] %in% c("no.Y.variation",
                                                 "character"))
                    fit[[regime.index]] <- m
                }
            }
            else {
                if (inherits(m,"ltmle.glmnet")){
                    fit[[regime.index]] <- m$selected_beta
                    ## list(beta=m$selected_beta,predicted=table(cut(predicted.values,include.lowest=TRUE,breaks=c(1,.75,0.5,.25,10^{-(1:8)}))))
                } else{
                    capture.output(print.m <- print(m))
                    fit[[regime.index]] <- print.m
                    ## list(superlearner = print.m, predicted=table(cut(predicted.values,include.lowest=TRUE,breaks=c(1,.75,0.5,.25,10^{-(1:8)}))))
                }
            }
        }
    }
    return(list(predicted.values = predicted.values, fit = fit,
                is.deterministic = is.deterministic, deterministic.Q = deterministic.Q,
                prob.A.is.1.meanL = prob.A.is.1.meanL))
}

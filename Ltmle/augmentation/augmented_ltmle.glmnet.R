ltmle.glmnet <- function(Y,
                         X,
                         newX,
                         family,
                         obsWeights=NULL,
                         id,
                         alpha = 1,
                         nfolds = 10,
                         selector="undersmooth",
                         nlambda = 100,
                         useMin = TRUE,
                         intercept = TRUE,
                         loss = "deviance",
                         ...){
    requireNamespace("glmnet")
    if (!is.matrix(X)) {
      X <- model.matrix(~-1 + ., X)
    }
    if (missing(newX)) newX=NULL
    if (length(newX)>0){
        Xnames=attr(newX,"Xnames") # This does not make any sense to me?
        if (!is.matrix(newX)) {
          newX <- model.matrix(~-1 + ., newX)
        }
    }
    FAM <- ifelse(length(unique(Y))>2,"gaussian","binomial")
    if (length(selector)>0&&selector=="undersmooth")
        uoh <- try(fit <- glmnet::glmnet(X,Y,weights = obsWeights,lambda = NULL,alpha = alpha,nlambda = nlambda,
                                         trace.it = 0L,family=FAM, intercept = intercept,...))
    else{
        # make sure that
        if (length(id)>0 && any(duplicated(id))){
            id_data=data.table(id=id)
            foldid_data=data.table(id=unique(id),
                                   foldid=sample(1:nfolds,
                                                 size=length(unique(id)),
                                                 replace=TRUE))
            foldid=foldid_data[id_data,on="id"]$foldid
            uoh <- try(fit <- glmnet::cv.glmnet(x = X,
                                                y = Y,
                                                weights = obsWeights,
                                                lambda = NULL,
                                                type.measure = loss,
                                                nfolds = nfolds,
                                                foldid=foldid,
                                                family = FAM,
                                                alpha = alpha,
                                                nlambda = nlambda,
                                                intercept = intercept,
                                                ...))
        } else{
            uoh <- try(fit <- glmnet::cv.glmnet(x = X,
                                                y = Y,
                                                weights = obsWeights,
                                                lambda = NULL,
                                                type.measure = loss,
                                                nfolds = nfolds,
                                                family = FAM,
                                                alpha = alpha,
                                                nlambda = nlambda,
                                                intercept = intercept,
                                                ...))
        }
    }
    if (inherits(uoh,"try-error")) #browser()
        stop("ltmle.glmnet could not fit")
    if (length(selector)>0&&selector=="undersmooth"){
        selected.lambda <- fit$lambda[length(fit$lambda)]
        if (length(newX)>0){
            pred <- c(predict(fit, newx = newX, type = "response", s = fit$lambda[length(fit$lambda)]))
        }
    } else{
        ## pred <- c(predict(fit$glmnet.fit, newx = newX, type = "response", s = ifelse(useMin,"lambda.min","lambda.1se")))
        if (useMin)
            selected.lambda <- fit$lambda.1se
        else
            selected.lambda <- fit$lambda.min
        if (length(newX)>0){
            pred <- c(predict(fit$glmnet.fit, newx = newX, type = "response", s = selected.lambda))
        }
    }
    class(fit) <- c("ltmle.glmnet")
    if (length(selector)>0&&selector=="undersmooth"){
        beta <- data.table::data.table(beta=fit$beta[,NCOL(fit$beta),drop=TRUE])
    } else{
        bmat <- fit$glmnet.fit$beta
        beta <- data.table::data.table(beta=bmat[,match(fit$lambda.min,fit$lambda)])
    }
    if (length(newX)>0){
        if (length(Xnames)==NROW(beta)) beta=cbind(X=Xnames,beta)
        # If length(newX)>0 but Xnames = NULL, we do not save any names for X?
    }else{
        if (length(selector)>0&&selector=="undersmooth"){
            beta=cbind(X=rownames(fit$beta),beta)
        }else{
            beta=cbind(X=rownames(fit$glmnet.fit$beta),beta)
        }
    }
    attr(fit,"selector") <- selector
    fit$selected_beta <- beta
    fit$selected.lambda <- selected.lambda
    if (length(newX)>0)
        out <- list(predicted.values = pred, fit = fit)
    else
        out <- list(predicted.values = NULL, fit = fit)
    return(out)
}

predict.ltmle.glmnet <- function(object,newX,...){
    selector <- attr(object,"selector")
    if (!is.matrix(newX)){
      newX <- model.matrix(~-1 + ., newX)
    }
    if (length(selector)>0&&selector=="undersmooth"){
        class(object)="glmnet"
        uha <- try(pred <- c(predict(object,
                                     newx = newX,
                                     type = "response",
                                     s = object$lambda[length(object$lambda)],
                                     ...)))
        if (inherits(uha,"try-error"))
            stop("Prediction of glmnet object failed in ltmle.glmnet.")
        else
            pred
    } else{
        pred <- c(predict(object$glmnet.fit,
                          newx = newX,
                          type = "response",
                          s = object$selected.lambda,
                          ... ))
    }
}

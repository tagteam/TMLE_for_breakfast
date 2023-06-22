ltmle.glmnet <- function(Y,
                         X,
                         newX,
                         family,
                         obsWeights,
                         id,
                         alpha = 1,
                         nfolds = 10,
                         selector="undersmooth",
                         nlambda = 100,
                         useMin = TRUE,
                         loss = "deviance",
                         ...){
    requireNamespace("glmnet")

    Xnames=attr(newX,"Xnames")
    if (!is.matrix(X)) {
        X <- model.matrix(~-1 + ., X)
        newX <- model.matrix(~-1 + ., newX)
    }
    FAM <- ifelse(length(unique(Y))>2,"gaussian","binomial")
    if (length(selector)>0&&selector=="undersmooth")
        uoh <- try(fit <- glmnet::glmnet(X,Y,weights = obsWeights,lambda = NULL,alpha = alpha,nlambda = nlambda,trace.it = 0L,family=FAM,...))
    else{
        # make sure that 
        if (any(duplicated(id))){
            id_data=data.table(id=id)
            foldid_data=data.table(id=unique(id),foldid=sample(1:nfolds,size=length(unique(id)),replace=TRUE))
            foldid=foldid_data[id_data,on="id"]
        }else{
            foldid=sample(1:nfolds,size=length(id),replace=TRUE)
        }
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
                                            ...))
    }
    if (inherits(uoh,"try-error")) #browser()
        stop("ltmle.glmnet could not fit")
    if (length(selector)>0&&selector=="undersmooth"){
        selected.lambda <- fit$lambda[length(fit$lambda)]
        pred <- c(predict(fit, newx = newX, type = "response", s = fit$lambda[length(fit$lambda)]))
    } else{
        ## pred <- c(predict(fit$glmnet.fit, newx = newX, type = "response", s = ifelse(useMin,"lambda.min","lambda.1se")))
        if (useMin)
            selected.lambda <- fit$lambda.1se
        else
            selected.lambda <- fit$lambda.min
        pred <- c(predict(fit$glmnet.fit, newx = newX, type = "response", s = selected.lambda))
    }
    class(fit) <- c("ltmle.glmnet")
    if (length(selector)>0&&selector=="undersmooth"){
        beta <- data.table::data.table(beta=fit$beta[,NCOL(fit$beta),drop=TRUE])
    } else{
        bmat <- fit$glmnet.fit$beta
        beta <- data.table::data.table(beta=bmat[,match(fit$lambda.min,fit$lambda)])
    }
    if (length(Xnames)==NROW(beta)) beta=cbind(X=Xnames,beta)
    attr(fit,"selector") <- selector
    fit$selected_beta <- beta
    fit$selected.lambda <- selected.lambda
    out <- list(predicted.values = pred, fit = fit)
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

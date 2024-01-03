#
# Function called from Estimate (via EstimateG and FixedTimeTMLE)
#
ltmle.glm.fit <- function (x, y, weights, family, offset, intercept) {
    ## print("ltmle.glm.fit")
    ## print(table(y))
    ## print(unlist(apply(x,2,function(u)length(unique(u)))))
    if (all(weights==1)||is.null(weights)){
        try.speedglm <- try({
            m <- speedglm::speedglm.wfit(y = y,
                                         X = x,
                                         family = family,
                                         offset = offset,
                                         intercept = intercept,
                                         maxit = 100)
            class(m) <- c("speedglm", "speedlm")
        }, silent = TRUE)
        if (inherits(try.speedglm, "try-error")) {
            ShowGlmMessage()
            try.glm <- try({m <- glm.fit(x = x,
                                         y = y,
                                         family = family,
                                         offset = offset,
                                         intercept = intercept,
                                         control = glm.control(maxit = 100))
            })
            if (inherits(try.glm,"try-error")){
                stop("Could not fit glm")
            }
            class(m) <- c("glm", "lm")
        }
    }else{
        try.glm <- try({
            m <- glm.fit(x = x,
                         y = y,
                         family = family,
                         weights = weights,
                         offset = offset,
                         intercept = intercept,
                         control = glm.control(maxit = 100))})
        if (inherits(try.glm,"try-error")) stop("Could not fit glm")
        class(m) <- c("glm", "lm")
    }
    return(m)
}

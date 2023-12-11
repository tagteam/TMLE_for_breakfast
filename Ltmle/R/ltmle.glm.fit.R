#
# Function called from Estimate (via EstimateG and FixedTimeTMLE)
#
ltmle.glm.fit <- function (x, y, weights, family, offset, intercept) {
    ## print(table(y))
    ## print("fit")
    if (all(weights==1)||is.null(weights)){
        try.fastglm <- try({
          m <- fastglm::fastglm(y = y,
                                x = x,
                                family = family,
                                offset = offset,
                                intercept = intercept,
                                maxit = 100, method = 2)
          # m <- speedglm::speedglm.wfit(y = y,
          #                             X = x,
          #                             family = family,
          #                             offset = offset,
          #                             intercept = intercept,
          #                             maxit = 100)
          
        }, silent = TRUE)
        if (inherits(try.fastglm, "try-error")) {
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

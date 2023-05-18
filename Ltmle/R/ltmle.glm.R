#
# Function called from CalcIPTW, FitPooledMSM, UpdateQ (via FixedTimeTMLE)
# with Q* as outcome
#
ltmle.glm <- function (formula, family, data, weights){
    #    print(formula)
    if (length(weights)==NROW(data)||is.null(weights)) {
        try.result <- try(m <- speedglm::speedglm(formula = formula,
                                                  family = family,
                                                  data = data,
                                                  maxit = 100),
                          silent = TRUE)
        if (inherits(try.result, "try-error")) {
            ShowGlmMessage()
            m <- glm(formula = formula,
                     family = family,
                     data = data,
                     control = glm.control(maxit = 100))
        }
    }
    else {
        m <- glm(formula = formula,
                 family = family,
                 data = data.frame(data,weights),
                 weights = weights,
                 control = glm.control(maxit = 100))
    }
    return(m)
}

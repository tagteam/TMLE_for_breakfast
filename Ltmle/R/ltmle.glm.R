#
# Function called from CalcIPTW, FitPooledMSM, UpdateQ (via FixedTimeTMLE)
# with Q* as outcome
#
ltmle.glm <- function (formula, family, data, weights){
    ## print("ltmle.glm")
    ## print(NROW(data))
    if (length(weights)!=NROW(data)||is.null(weights)) {
      ## Why do we fit speedglm without weights if length(weights)==NROW(data)? (Emilie)
      ## In Joshua's ltmle.glm, they fit glm without weights if is.null(weights)
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

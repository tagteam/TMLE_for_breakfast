#
# Function called from CalcIPTW, FitPooledMSM, UpdateQ (via FixedTimeTMLE)
# with Q* as outcome
#
ltmle.glm <- function (formula, family, data, weights){
    #    print(formula)
    if (length(weights)==NROW(data)||is.null(weights)) {
        ## get outcome from formula
        outcome <- as.character(formula[[2]])
        ## get outcome from data
        if (outcome %in% colnames(data)) {
            y <- data[,outcome]
        } else {
            stop(paste("Outcome",outcome,"not found in data"))
        }
        model_mat <- model.matrix(formula, data)
        ## remove outcome from model matrix
        if (outcome %in% colnames(model_mat)) { ## sometimes when outcome is constant, it won't appear in the model_mat
            x <- model_mat[, -which(colnames(model_mat) == outcome)]
        }
        else {
          x <- model_mat
        }
        # print(formula)
        m <- glm(formula = formula,
                 family = family,
                 data = data,
                 control = glm.control(maxit = 100))
        
        # try.result <- try({
        #                    m <- fastglm::fastglm(x=x,
        #                                         y=y,
        #                                         family = family,
        #                                         data = data,
        #                                         maxit = 100)},
        #                   silent = TRUE)
        # if (inherits(try.result, "try-error")) {
        #   ShowGlmMessage()
          # m <- glm(formula = formula,
          #          family = family,
          #          data = data,
          #          control = glm.control(maxit = 100))
        # }
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

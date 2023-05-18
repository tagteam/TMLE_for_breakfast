GetWarningsToSuppress <-
function (update.step = FALSE) 
{
    warnings.to.suppress <- c("glm.fit: fitted probabilities numerically 0 or 1 occurred", 
        "prediction from a rank-deficient fit may be misleading")
    if (update.step) {
        warnings.to.suppress <- c(warnings.to.suppress, "glm.fit: algorithm did not converge")
    }
    return(warnings.to.suppress)
}

print.ltmleEffectMeasures <-
function (x, ...) 
{
    PrintCall(x$call)
    cat("Use summary(...) to get estimates, standard errors, p-values, and confidence intervals for treatment EYd, control EYd, additive effect, relative risk, and odds ratio.\n")
    invisible(x)
}

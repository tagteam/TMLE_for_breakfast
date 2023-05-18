print.ltmle <-
function (x, ...) 
{
    PrintCall(x$call)
    if (x$gcomp) {
        cat("GCOMP Estimate: ", x$estimates["gcomp"], "\n")
    }
    else {
        cat("TMLE Estimate: ", x$estimates["tmle"], "\n")
    }
    invisible(x)
}

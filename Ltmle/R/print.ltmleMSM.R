print.ltmleMSM <-
function (x, ...) 
{
    PrintCall(x$call)
    if (x$gcomp) {
        cat("GCOMP Beta Estimates: \n")
    }
    else {
        cat("TMLE Beta Estimates: \n")
    }
    print(x$beta)
    if (x$transformOutcome) {
        Yrange <- attr(x$transformOutcome, "Yrange")
        cat("NOTE: The MSM is modeling the transformed outcome ( Y -", 
            min(Yrange), ")/(", max(Yrange), "-", min(Yrange), 
            ")")
    }
    invisible(x)
}

SuppressGivenWarnings <-
function (expr, warningsToIgnore) 
{
    h <- function(w) {
        if (w$message %in% warningsToIgnore) 
            invokeRestart("muffleWarning")
    }
    withCallingHandlers(expr, warning = h)
}

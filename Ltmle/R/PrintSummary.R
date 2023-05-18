PrintSummary <-
function (x) 
{
    if (!is.null(x$long.name)) 
        cat(x$long.name, ":\n", sep = "")
    cat("   Parameter Estimate: ", signif(x$estimate, 5), "\n")
    if (x$log.std.err) {
        if (x$long.name == "Relative Risk") {
            param.abbrev <- "RR"
        }
        else if (x$long.name == "Odds Ratio") {
            param.abbrev <- "OR"
        }
        else {
            stop("unexpected x$long.name")
        }
        cat("  Est Std Err log(", param.abbrev, "):  ", sep = "")
    }
    else {
        cat("    Estimated Std Err:  ")
    }
    cat(signif(x$std.dev, 5), "\n")
    cat("              p-value: ", ifelse(x$pvalue <= 2 * 10^-16, 
        "<2e-16", signif(x$pvalue, 5)), "\n")
    cat("    95% Conf Interval:", paste("(", signif(x$CI[1], 
        5), ", ", signif(x$CI[2], 5), ")", sep = ""), "\n\n")
    invisible(x)
}

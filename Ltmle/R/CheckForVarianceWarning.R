CheckForVarianceWarning <-
function (inputs, g.ratio)
{
    if (inputs$variance.method == "ic") {
        positivity <- mean(g.ratio < 1, na.rm = TRUE) > 0.0001
        rare.events <- inputs$binaryOutcome && any(colMeans(inputs$data[,
            inputs$final.Ynodes, drop = FALSE], na.rm = TRUE) <
            0.0003)
        if (positivity || rare.events) {
            variance.available.warning <- VarianceAvailableWarning(inputs)
            warning.msg <- "Variance estimate is based on influence curve only, which may be significantly anticonservative because your data appears to contain"
            if (positivity)
                warning.msg <- paste(warning.msg, "positivity violations")
            if (positivity && rare.events)
                warning.msg <- paste(warning.msg, "and")
            if (rare.events)
                warning.msg <- paste(warning.msg, "rare events")
            if (is.null(variance.available.warning)) {
                warning.msg <- paste0(warning.msg, ". It is recommended to use variance.method='tmle' or variance.method='iptw' to obtain a more robust variance estimate (but run time may be significantly longer). See variance.method details in ?ltmle")
            }
            else {
                warning.msg <- paste0(warning.msg, ". ", variance.available.warning,
                  " but this will be addressed in a future release.")
            }
            warning(warning.msg)
        }
    }
    invisible(NULL)
}

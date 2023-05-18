TransformOutcomes <-
function (data, nodes, Yrange) 
{
    all.Y <- unlist(data[, nodes$Y])
    transformOutcome <- FALSE
    if (!is.null(Yrange)) {
        rng <- range(all.Y, na.rm = TRUE)
        if (min(rng) < min(Yrange) || max(rng) > max(Yrange)) {
            message("Some Ynodes are not in [Yrange[1], Yrange[2]], Y values are truncated")
            data[, nodes$Y][data[, nodes$Y] < min(Yrange)] <- min(Yrange)
            data[, nodes$Y][data[, nodes$Y] > max(Yrange)] <- max(Yrange)
        }
        transformOutcome <- TRUE
    }
    else {
        Yrange <- range(all.Y, na.rm = TRUE)
        if (min(Yrange) < 0 || max(Yrange) > 1) {
            transformOutcome <- TRUE
            message("Some Ynodes are not in [0, 1], and Yrange was NULL, so all Y nodes are being\ntransformed to (Y-min.of.all.Ys)/range.of.all.Ys")
        }
    }
    if (transformOutcome) {
        attr(transformOutcome, "Yrange") <- Yrange
        data[, nodes$Y] <- (data[, nodes$Y] - min(Yrange))/diff(Yrange)
    }
    return(list(data = data, transformOutcome = transformOutcome))
}

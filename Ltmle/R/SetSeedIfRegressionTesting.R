SetSeedIfRegressionTesting <-
function () 
{
    seed <- Sys.getenv("LTMLE.REGRESSION.TESTING.SEED")
    stopifnot(length(seed) == 1)
    if (seed != "") {
        seed <- as.numeric(seed)
        stopifnot(is.finite(seed))
        set.seed(seed)
    }
    invisible(NULL)
}

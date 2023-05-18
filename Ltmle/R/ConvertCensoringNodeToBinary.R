ConvertCensoringNodeToBinary <-
function (x) 
{
    stopifnot(is.factor(x) && all(levels(x) %in% c("censored", 
        "uncensored")))
    b <- rep(NA_integer_, length(x))
    b[x == "censored"] <- 0L
    b[x == "uncensored"] <- 1L
    return(b)
}

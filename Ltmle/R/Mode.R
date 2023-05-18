Mode <-
function (x, na.rm = FALSE) 
{
    if (na.rm) 
        x <- x[!is.na(x)]
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

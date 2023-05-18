CalcCumG <-
function (g, gbounds) 
{
    cum.g <- rowCumprods(g)
    return(list(unbounded = cum.g, bounded = Bound(cum.g, gbounds)))
}

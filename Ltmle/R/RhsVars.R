RhsVars <-
function (f) 
{
    f <- as.formula(f)
    return(all.vars(f[[3]]))
}

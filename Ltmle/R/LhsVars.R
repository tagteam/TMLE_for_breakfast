LhsVars <-
function (f) 
{
    f <- as.formula(f)
    return(as.character(f[[2]]))
}

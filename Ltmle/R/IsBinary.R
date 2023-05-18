IsBinary <-
function (mat) 
{
    is.equal(mat, as.numeric(as.logical(mat)))
}

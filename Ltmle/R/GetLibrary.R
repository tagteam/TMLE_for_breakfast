GetLibrary <-
function (SL.library, estimate.type) 
{
    NullToGlm <- function(libr) if (is.null(libr)) 
        "glm"
    else libr
    if (is.null(names(SL.library))) 
        return(NullToGlm(SL.library))
    if (!identical(sort(names(SL.library)), sort(c("Q", "g")))) 
        stop("If SL.library has names, it must have two names: Q and g")
    if (!estimate.type %in% c("Q", "g")) 
        stop("bad estimate.type")
    if (length(setdiff(names(attributes(SL.library)), c("names", 
        "return.fit"))) > 0) 
        stop("If SL.library has attributes, the only valid attributes are name and return.fit")
    lib <- SL.library[[estimate.type]]
    attr(lib, "return.fit") <- attr(SL.library, "return.fit", 
        exact = TRUE)
    return(NullToGlm(SL.library[[estimate.type]]))
}

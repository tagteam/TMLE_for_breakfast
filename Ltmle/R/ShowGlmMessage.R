ShowGlmMessage <-
function () 
{
    message("speedglm failed, using glm instead. If you see a lot of this message and you have large absolute values in data[, Lnodes], you may get a speed performance improvement by rescaling these values.")
}

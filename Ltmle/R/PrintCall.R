PrintCall <-
function (cl) 
{
    cat("Call:\n", paste(deparse(cl), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")
}

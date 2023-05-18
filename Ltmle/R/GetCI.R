GetCI <-
function (estimate, std.dev, n) 
{
    if (n < 100) {
        x <- qt(0.97499999999999998, df = n - 1) * std.dev
    }
    else {
        x <- qnorm(0.97499999999999998) * std.dev
    }
    CI <- cbind(`2.5%` = estimate - x, `97.5%` = estimate + x)
    return(CI)
}

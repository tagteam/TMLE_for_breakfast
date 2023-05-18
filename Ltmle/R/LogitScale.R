LogitScale <-
function (x) 
{
    qlogis(Scale(x, 0.01, 0.98999999999999999))
}

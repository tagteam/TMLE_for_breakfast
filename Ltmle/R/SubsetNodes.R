SubsetNodes <-
function (nodes, final.Ynode) 
{
    return(lapply(nodes, function(x) x[x <= final.Ynode]))
}

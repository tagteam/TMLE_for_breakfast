rexpit <-
function (x) 
rbinom(n = length(x), size = 1, prob = plogis(x))

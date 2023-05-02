library(riskRegression)
library(data.table)
library(Publish)
library(ltmle)
d <- sampleData(200,outcome = "binary")
setnames(d,"X1","A")
fit <- glm(Y~A+X6,data = d,family = "binomial")
publish(fit)
d0 <- copy(d[,.(A,X6)])
d1 <- copy(d[,.(A,X6)])
d0[,A := "0"]
d1[,A := "1"]
risk0 <- mean(predictRisk(fit,newdata = d0))
risk1 <- mean(predictRisk(fit,newdata = d1))
risk0-risk1

ate(event = Y~A+X6,treatment = "A",data = d)
# todo
ltmle(event = Y~A+X6,treatment = "A",data = d)

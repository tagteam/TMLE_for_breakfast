try(setwd("~/research/Methods/TMLE_for_breakfast/simulation/randomized_treatment/"))
for (f in list.files("functions/",pattern = "R$",full.names = TRUE)){source(f)}
library(targets)
library(lava)
library(drtmle)
library(data.table)
library(ltmle)
library(augmentedLtmle)
library(Publish)
library(lmtp)
d <- simulate_data(n = 200,treatment_effect = 0.1,covariate_effect = c(.7,0))
refresh(augmentedLtmle)

## publish(glm(Y_treated~W,data = d,family = "binomial"))
## publish(glm(Y_untreated~W,data = d,family = "binomial"))
## publish(glm(Y~A+W,data = d,family = "binomial"))
d[,.(Estimate = c("True","Naive"),
     Risk_treated = c(mean(Y_treated),mean(Y[A == 1])),
     Risk_untreated = c(mean(Y_untreated),mean(Y[A == 0])),
     ATE = c(mean(Y_treated)-mean(Y_untreated),
              mean(Y[A == 1])-mean(Y[A == 0])))]

a <- with(d,summary(suppressWarnings(ltmle(data = data.frame(W,A,Y),estimate.time = FALSE,gbounds = c(0,1),Lnodes = "W",gform = "A~1",Anodes = "A",Ynodes = "Y",abar = list(0,1))))$effect.measures$ATE$estimate)

refresh(augmentedLtmle)
A <- Ltmle(data = d[,.(W,A,Y)],time_horizon = 1,estimate.time = FALSE,gbounds = c(0,1),Lnodes = "W",survivalOutcome = FALSE,Qform = c("Y" = "Q.kplus1 ~ W + A"),gform = "A~1",Anodes = "A",Ynodes = "Y",verbose = FALSE,abar = list(0,1))
B <- Ltmle(data = d[,.(W,A,Y)],time_horizon = 1,estimate.time = FALSE,gbounds = c(0,1),Lnodes = "W",survivalOutcome = FALSE,Qform = c("Y" = "Q.kplus1 ~ W + A"),gform = "A~W",Anodes = "A",Ynodes = "Y",verbose = FALSE,abar = list(0,1))
a <- summary.Ltmle(A)
b <- summary.Ltmle(B)

u <- with(d,summary(suppressWarnings(ltmle(data = data.frame(W,A,Y),estimate.time = FALSE,iptw.only = TRUE,gbounds = c(0,1),Lnodes = "W",gform = "A~1",Anodes = "A",Ynodes = "Y",abar = list(0,1))))$effect.measures$ATE$estimate)
v <- with(d,summary(suppressWarnings(ltmle(data = data.frame(W,A,Y),estimate.time = FALSE,iptw.only = TRUE,gbounds = c(0,1),Lnodes = "W",gform = "A~W",Anodes = "A",Ynodes = "Y",abar = list(0,1))))$effect.measures$ATE$estimate)
c(a,b,u,v)



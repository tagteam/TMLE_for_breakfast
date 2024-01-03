library(targets)
library(data.table)
library(lava)
library(pander)
try(setwd("~/research/Methods/TMLE_for_breakfast/"))
tar_source("Ltmle")
prob_baseline <- c("Male" = 0.5985464)
source("~/research/Methods/TMLE_for_breakfast/meeting/data/coefs.R")

m <- get_lava_model(time_horizon = 2, name_baseline_covariates = "V", name_time_covariates = "L", 
                    name_regimen = "A", name_outcome = "Y", name_censoring = NULL, name_comp.event = NULL, 
                    censoring_model = censoring_model, comp.event_model = comp.event_model,
                    outcome_model = outcome_model, prop_model = prop_model, prob_baseline = prob_baseline, cov_model = cov_model)

d <- simulate_rdata(n = 10000)
head(d)


## EXAMPLE 1: One time point, no comp. event, no censoring

d0 <- copy(d[,grep("V|L|A",names(d)),with = FALSE])
d1 <- copy(d[,grep("V|L|A",names(d)),with = FALSE])
d0[,names(d0[,grep("A",names(d0)), with = FALSE]):=rep(0,d0[,.N])]
d1[,names(d1[,grep("A",names(d1)), with = FALSE]):=rep(1,d1[,.N])]

fit_glm <- glm(Y_1 ~ V + L_0 + A_0, data = d, family = binomial)

pl <- prepare_Ltmle(outcome_data = list("Y" = d[,c("pnr",grep("Y",names(d), value = TRUE)), with = FALSE]),
                   regimen_data = list("A" = d[,c("pnr",grep("A",names(d), value = TRUE)), with = FALSE]),
                   baseline_data = d[,.(pnr,V)],
                   timevar_data = d[,c("pnr",grep("L",names(d), value = TRUE)), with = FALSE],
                   time_horizon = 1, deterministic.Q.function = NULL,
                   name_outcome = "Y", name_regimen = "A", name_censoring = NULL, name_comp.event = NULL,
                   gcomp = TRUE,
                   abar = list(1,0))
pl$gcomp = TRUE ## Implies no tmle update in the ltmle sequential regression
fit_ltmle <- do.call(Ltmle, pl)
ATE_ex1 <- c(mean(predict(fit_glm, newdata = d1, type = "response")) - mean(predict(fit_glm, newdata = d0, type = "response")))
summary(fit_ltmle)
summary.ltmleEffectMeasures(fit_ltmle)$effect.measures$treatment$estimate

## EXAMPLE 2: Two time points, no comp. event, no censoring

fit_glm_2 <- glm(Y_2 ~ V + L_0 + L_1 + A_0 + A_1, data = d[Y_1 == 0], family = binomial)
Qhat2_1 <- predict(fit_glm_2, newdata = d1, type = "response")
Qhat2_0 <- predict(fit_glm_2, newdata = d0, type = "response")
d[,Q2_1 := ifelse(Y_1==1, Y_1, Qhat2_1)]
d[,Q2_0 := ifelse(Y_1==1, Y_1, Qhat2_0)]
fit_Q2_1 <- glm(Q2_1 ~ V + L_0 + A_0, data = d, family=quasibinomial(link = logit))
Q1_1 <- predict(fit_Q2_1, newdata = d1, type = "response")
Q0_1 <- mean(Q1_1)
fit_Q2_0 <- glm(Q2_0 ~ V + L_0 + A_0, data = d, family=quasibinomial(link = logit))
Q1_0 <- predict(fit_Q2_0, newdata = d0, type = "response")
Q0_0 <- mean(Q1_0)

pl <- prepare_data(outcome_data = list("Y" = d[,c("pnr",grep("Y",names(d), value = TRUE)), with = FALSE]),
                   regimen_data = list("A" = d[,c("pnr",grep("A",names(d), value = TRUE)), with = FALSE]),
                   baseline_data = d[,.(pnr,V)],
                   timevar_data = d[,c("pnr",grep("L",names(d), value = TRUE)), with = FALSE],
                   time_horizon = 2, deterministic.Q.function = NULL,
                   name_outcome = "Y", name_regimen = "A",
                   name_censoring = NULL, censored_label = "0", name_comp.event = NULL,
                   abar = list(c(1,1),c(0,0)))
pl$gcomp = TRUE ## Implies no tmle update in the ltmle sequential regression
fit <- do.call(Ltmle, pl)
ATE_ex2 <- c(Q0_1 - Q0_0,summary(fit)$effect.measures$ATE$estimate)

unique(fit$Qstar)
unique(Q1_1)
unique(Q1_0)

## EXAMPLE 3: Two time points WITH censoring

m <- get_lava_model(time_horizon = 2, name_baseline_covariates = "V", name_time_covariates = "L", 
                    name_regimen = "A", name_outcome = "Y", name_censoring = "C", name_comp.event = NULL, 
                    censoring_model = censoring_model, comp.event_model = comp.event_model,
                    outcome_model = outcome_model, prop_model = prop_model, prob_baseline = prob_baseline, cov_model = cov_model)
d <- get_sim_data(m, 10000)

pl <- prepare_data(outcome_data = list("Y" = d[,c("pnr",grep("Y|C",names(d), value = TRUE)), with = FALSE]),
                   regimen_data = list("A" = d[,c("pnr",grep("A",names(d), value = TRUE)), with = FALSE]),
                   baseline_data = d[,.(pnr,V)], order_YC = FALSE,
                   timevar_data = d[,c("pnr",grep("L",names(d), value = TRUE)), with = FALSE],
                   time_horizon = 2, deterministic.Q.function = NULL,
                   name_outcome = "Y", name_regimen = "A",
                   name_censoring = "C", censored_label = "0", name_comp.event = NULL,
                   abar = list(c(1,1),c(0,0)))
pl$gcomp = TRUE ## Implies no tmle update in the ltmle sequential regression
fit <- do.call(Ltmle, pl)

d0 <- copy(d[,grep("V|L|A",names(d)),with = FALSE])
d1 <- copy(d[,grep("V|L|A",names(d)),with = FALSE])
d0[,names(d0[,grep("A",names(d0)), with = FALSE]):=rep(0,d0[,.N])]
d1[,names(d1[,grep("A",names(d1)), with = FALSE]):=rep(1,d1[,.N])]

fit_glm_2 <- glm(Y_2 ~ V + L_0 + L_1 + A_0 + A_1, data = d[Y_1 == 0 & C_1 == 1 & C_2 == 1], family = binomial)
Qhat2_1 <- predict(fit_glm_2, newdata = d1, type = "response")
Qhat2_0 <- predict(fit_glm_2, newdata = d0, type = "response")
d[,Q2_1 := ifelse(Y_1==1, Y_1, Qhat2_1)]
d[,Q2_0 := ifelse(Y_1==1, Y_1, Qhat2_0)]
fit_Q2_1 <- glm(Q2_1 ~ V + L_0 + A_0, data = d[C_1 == 1], family=quasibinomial(link = logit))
Q1_1 <- predict(fit_Q2_1, newdata = d1, type = "response")
Q0_1 <- mean(Q1_1)
fit_Q2_0 <- glm(Q2_0 ~ V + L_0 + A_0, data = d[C_1 == 1], family=quasibinomial(link = logit))
Q1_0 <- predict(fit_Q2_0, newdata = d0, type = "response")
Q0_0 <- mean(Q1_0)

ATE_ex3 <- c(Q0_1 - Q0_0,summary(fit)$effect.measures$ATE$estimate)

## EXAMPLE 4: Two time points WITH censoring and comp.event

m <- get_lava_model(time_horizon = 2, name_baseline_covariates = "V", name_time_covariates = "L", 
                    name_regimen = "A", name_outcome = "Y", name_censoring = "C", name_comp.event = "D", 
                    censoring_model = censoring_model, comp.event_model = comp.event_model,
                    outcome_model = outcome_model, prop_model = prop_model, prob_baseline = prob_baseline, cov_model = cov_model)
d <- get_sim_data(m, 10000)

pl <- prepare_data(outcome_data = list("Y" = d[,c("pnr",grep("Y|C|D",names(d), value = TRUE)), with = FALSE]),
                   regimen_data = list("A" = d[,c("pnr",grep("A",names(d), value = TRUE)), with = FALSE]),
                   baseline_data = d[,.(pnr,V)], order_YC = FALSE,
                   timevar_data = d[,c("pnr",grep("L",names(d), value = TRUE)), with = FALSE],
                   time_horizon = 2, deterministic.Q.function = NULL,
                   name_outcome = "Y", name_regimen = "A",
                   name_censoring = "C", censored_label = "0", name_comp.event = "D",
                   abar = list(c(1,1),c(0,0)))
pl$gcomp = TRUE ## Implies no tmle update in the ltmle sequential regression
fit <- do.call(Ltmle, pl)

d0 <- copy(d[,grep("V|L|A",names(d)),with = FALSE])
d1 <- copy(d[,grep("V|L|A",names(d)),with = FALSE])
d0[,names(d0[,grep("A",names(d0)), with = FALSE]):=rep(0,d0[,.N])]
d1[,names(d1[,grep("A",names(d1)), with = FALSE]):=rep(1,d1[,.N])]

fit_glm_2 <- glm(Y_2 ~ V + L_0 + L_1 + A_0 + A_1, data = d[Y_1 == 0 & C_1 == 1 & C_2 == 1 & D_1 == 0], family = binomial)
Qhat2_1 <- predict(fit_glm_2, newdata = d1, type = "response")
Qhat2_0 <- predict(fit_glm_2, newdata = d0, type = "response")
d[,Q2_1 := ifelse(Y_1==1|D_1==1, Y_1, Qhat2_1)]
d[,Q2_0 := ifelse(Y_1==1|D_1==1, Y_1, Qhat2_0)]
fit_Q2_1 <- glm(Q2_1 ~ V + L_0 + A_0, data = d[C_1 == 1], family=quasibinomial(link = logit))
Q1_1 <- predict(fit_Q2_1, newdata = d1, type = "response")
Q0_1 <- mean(Q1_1)
fit_Q2_0 <- glm(Q2_0 ~ V + L_0 + A_0, data = d[C_1 == 1], family=quasibinomial(link = logit))
Q1_0 <- predict(fit_Q2_0, newdata = d0, type = "response")
Q0_0 <- mean(Q1_0)

ATE_ex4 <- c(Q0_1 - Q0_0,summary(fit)$effect.measures$ATE$estimate)

res <- as.table(c(ATE_ex1,ATE_ex2,ATE_ex3,ATE_ex4))
names(res) <- unlist(lapply(1:4, function(x){paste0(c("Manual seq. regression ex.", "LTMLE fit (gcomp=TRUE) ex."), x, sep = " ")}))
pander(res)

library(dplyr)
library(gt)

# res <- tibble(
#   name = unlist(lapply(1:3, function(x){paste0(c("Manual seq. regression ex.", "LTMLE fit (gcomp=TRUE) ex."), x, sep = " ")})),
#   value = c(ATE_ex1,ATE_ex2,ATE_ex3)
# )

res <- data.frame(c("Logistic sequential regression", "Ltmle no update (gcomp = TRUE)"), ATE_ex1,ATE_ex2,ATE_ex3,ATE_ex4)
names(res) <- c(" ",paste0(paste0("Example ", 1:4)))
t(res)
t(res) %>% 
  gt() %>%
  fmt_number(
    columns = 2:5,
    decimals = 6
  )


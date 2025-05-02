get_test_rtmle <- function(dummy_data){
    if (FALSE){
        tar_load(dummy_data)
    }
    tau <- 4
    x <- rtmle_init(intervals = tau,
                    name_id = "ID",
                    name_outcome = "Y",
                    name_competing = "Dead",
                    name_censoring = "Censored",
                    censored_label = "censored")


    x$long_data <- dummy_data[c("outcome_data","censored_data","competing_data","timevar_data")]

    add_baseline_data(x) <- dummy_data$baseline_data


    ## PREPARE THE DATA 
    ## Intervals as 4 intervals, so from 0:930
    x <- long_to_wide(x, intervals = seq(0, 930, 30.45*6))

    prepare_data(x) <- list()

    #x$prepared_data |> View()

    protocol(x) <- list(name = "Always_Degludec_Never_Glargine",
                        intervention = data.frame("Degludec" = factor(1,levels = c(0,1)),
                                                  "Glargine" = factor(0,levels = c(0,1))))
    protocol(x) <- list(name = "Always_Glargine_Never_Degludec",
                        intervention = data.frame("Degludec" = factor(0,levels = c(0,1)),
                                                  "Glargine" = factor(1,levels = c(0,1))))
    prepare_data(x) <- list()


    target(x) <- list(name = "Outcome_risk",
                      strategy = "additive",
                      estimator = "tmle",
                      protocols = c("Always_Degludec_Never_Glargine", 
                                    "Always_Glargine_Never_Degludec"))

    refProtocol <- list(Outcome_risk = "Always_Glargine_Never_Degludec")

    x <- run_rtmle(x,
                        refit = TRUE,
                        learner = "learn_glmnet",
                        time_horizon = 1:4)

    summary(x,
            targets = "Outcome_risk",
            reference = refProtocol) 
}

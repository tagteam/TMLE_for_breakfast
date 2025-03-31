library(rtmle)
library(data.table)
setwd('~/github/tmle_for_breakfast/help_tag')
baseline_data <- readRDS('./baseline_data.rds')
outcome_dt <- readRDS('./outcome_dt.rds')
timevar_data <- readRDS('./treatment_dt.rds')

x <- rtmle_init(intervals=4,name_id='ID',name_time='period',name_outcome='outcome',
                name_competing='compete',name_censoring='censor',
                censored_levels=c('1','0'),censored_label="1")
x$data <- list(baseline_data = baseline_data[,.(ID,Age)],
               outcome_data = outcome_dt,
               timevar_data = treatment_dt[,grep('^(ID|A_)',names(treatment_dt)),with=FALSE]
)
prepare_data(x) <- list()
protocol(x) <- list(name = "treat",treatment_variables = "A",intervention = 1)
protocol(x) <- list(name = "placebo",treatment_variables = "A",intervention = 0)
target(x) <- list(name = "Risk",strategy = "additive",estimator = "tmle",protocols = c("treat","placebo"))
x <- run_rtmle(x,time_horizon = 2)
summary(x)
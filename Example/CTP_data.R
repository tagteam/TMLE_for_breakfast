set.seed(42)

# Number of patients
n_patients <- 1000

# Assigning treatment (0/1)
treatment_assignment <- sample(c(0, 1), size = n_patients, replace = TRUE)

# Generating ages
age <- sample(c('<60', '60-70', '>70'), size = n_patients, replace = TRUE)

# Generating time from start of trial
time <- rexp(n_patients, rate = 1/5)

# Generating persistent treatment periods for half of the patients
persistent_treatment_periods <- list()
for (i in 1:(n_patients / 2)) {
  if (treatment_assignment[i] == 1) {
    n_periods <- sample(1:3, 1)  # Random number of consecutive periods
    for (j in 1:n_periods) {
      treatment_start <- runif(1, 0, 2) + (j - 1) * 2.5  # Consecutive periods
      treatment_end <- treatment_start + runif(1, 0.5, 2)
      persistent_treatment_periods[[length(persistent_treatment_periods) + 1]] <- c(i, treatment_start, treatment_end)
    }
  }
}

# Generating non-persistent treatment periods for the other half of the patients
non_persistent_treatment_periods <- list()
for (i in ((n_patients / 2) + 1):n_patients) {
  if (treatment_assignment[i] == 1) {
    n_periods <- sample(2:5, 1)  # Random number of non-overlapping periods
    for (j in 1:n_periods) {
      treatment_start <- runif(1, 0, 2) + (j - 1) * 2.5  # Consecutive periods
      treatment_end <- treatment_start + runif(1, 0.1, 0.5)  # Non-overlapping periods
      non_persistent_treatment_periods[[length(non_persistent_treatment_periods) + 1]] <- c(i, treatment_start, treatment_end)
    }
  }
}

# Combining persistent and non-persistent treatment periods
all_treatment_periods <- c(persistent_treatment_periods, non_persistent_treatment_periods)

# Generating event type (censoring=0, event=1, death=2)
event_type <- sample(c(0, 1, 2), size = n_patients, prob = c(0.2, 0.7, 0.1), replace = TRUE)

# Creating dataframe for baseline dataset
baseline_data <- data.frame(ID = 1:n_patients,
                            Age = age,
                            Time = time,
                            Event_Type = event_type)

# Add a variable indicating if the patient started on treatment
baseline_data$Started_On_Treatment <- ifelse(baseline_data$ID %in% unlist(lapply(all_treatment_periods, `[`, 1)), 1, 0)

# Adjusting event outcomes based on treatment and duration of treatment
for (i in 1:nrow(baseline_data)) {
  if (baseline_data$Started_On_Treatment[i] == 1) {
    # Risk reduction for those on treatment
    event_type_prob <- c(0.2, 0.35, 0.45)
  } else {
    event_type_prob <- c(0.25, 0.65, 0.1)  # Higher risk for those not on treatment
  }
  baseline_data$Event_Type[i] <- sample(c(0, 1, 2), size = 1, prob = event_type_prob)
}

# Creating dataframe for treatment periods dataset
treatment_data <- data.frame(matrix(unlist(all_treatment_periods), ncol = 3, byrow = TRUE))
colnames(treatment_data) <- c('ID', 'Treatment_Start', 'Treatment_End')

# Output
cat("Baseline Dataset:\n")
print(head(baseline_data))
cat("\nTreatment Periods Dataset:\n")
print(head(treatment_data))
setDT(baseline_data)
setDT(treatment_data)
fit <- prodlim(Hist(Time,Event_Type)~Started_On_Treatment,data=baseline_data)
plot(fit)

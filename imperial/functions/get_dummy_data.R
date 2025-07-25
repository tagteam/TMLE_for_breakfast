get_dummy_data <- function(filename){
    if (FALSE){
        library(tidyverse)
        library(data.table)
        #filename <- "data/01-Dummy_data_to_share_DATES.xlsx"
        filename <- "data/UPDATEDATA.xlsx"

    }
    dummydf1 <- readxl::read_xlsx(path = filename, sheet = 1) |> 
        ## FILTER TO GET TRIAL POPULATION
        ## FILTER FOR PRIMARY OUTCOME ONLY
        filter(Include_or_not == "Include", 
               CVD_related_death != "Not CVD-related") 
    
    HBC <- readxl::read_xlsx(path = filename, sheet = 2) %>% 
      mutate(HBC_date = ymd(HBC_date))
    names(HBC) <- c("ID","date","value")
    
    BMI <- readxl::read_xlsx(path = filename, sheet = 3) %>% 
      mutate(BMI_date = ymd(BMI_date))
    names(BMI) <- c("ID","date","value")
    
    ## PREPARE & PIVOT TABLES SO THAT DATES ARE ALL IN ONE COLUMN
    ## BASELINE DATASET  ===========
    BaselineDataset <- dummydf1 |> 
        select(ID, Age, clean_sex, Date) |> 
        mutate(clean_sex = case_when(clean_sex == 1 ~ 0,
                                     clean_sex == 2 ~ 1),
              start_followup_date = as.Date(Date)
               ) |> 
        setDT()


    ## TREATMENT DATASET  ===========
    BaseDrugs <- dummydf1 |> 
        select(ID, date = Date, Treatment = Drug_grouping) |> 
        mutate(Treatment = str_replace(Treatment, " only", ""))


    Glargine_0 <- BaseDrugs |> 
        filter(Treatment == "Glargine")

    Degludec_0 <- BaseDrugs |> 
        filter(Treatment == "Degludec")

    FUDrugs <- dummydf1 |> 
        select(ID, starts_with("Deglud_"), starts_with("glarg_")) |> 
      mutate(
        across(.cols = contains("_date"),
               .fns = ~as.numeric(.x)),
        across(.cols = contains("_date"),
               .fns = ~as.Date(.x, origin = "1900-01-01"))) |>  
            # across(.cols = ends_with("_date"),
            #        .fns = ~ifelse(is.na(.), 0, 1))
       # ) |> 
        pivot_longer(cols = -c("ID"),
                     names_to = "Treatment",
                     values_to = "date") |> 
        filter(!is.na(date)) |> 
        mutate(Treatment = case_when(
                   str_detect(Treatment, "Deglud") ~ "Degludec",
                   str_detect(Treatment, "glarg_") ~ "Glargine"
               ))  
    
    
    
    Glargine_FU <- FUDrugs |> 
        filter(Treatment == "Glargine")

    Degludec_FU <- FUDrugs |> 
        filter(Treatment == "Degludec")    
    

    Glargine <- 
        Glargine_FU |> 
        #bind_rows(Glargine_0, Glargine_FU) |> 
        select(-Treatment) |> 
        setDT()

    Degludec <- 
        Degludec_FU |> 
        #bind_rows(Degludec_0, Degludec_FU) |> 
        select(-Treatment) |> 
        setDT()

    timevar_data <- list(Degludec = Degludec, 
                         Glargine = Glargine,
                         HBC = HBC,
                         BMI = BMI)

    
    ## CENSORING & OUTCOME & COMPETING EVENTS DATASET ===========

    ### CENSORING DATASET 

    CensoredData <- dummydf1 |> 
        select(ID) |> 
        mutate(date = ymd("2024-12-31")) |> 
        setDT()
    
    ### OUTCOME & COMPETING EVENTS DATASET

    Outcome_6 <- dummydf1 |> 
        select(ID,
               starts_with("Primary_"),
               starts_with("CVD_date"),
               contains("cause_mort_"),
               death_date) |>
        select(ID, ends_with("_6months"), death_date) |> 
        PrepOutComp()


    Outcome_12 <- dummydf1 |> 
        select(ID,
               starts_with("Primary_"),
               starts_with("CVD_date"),
               contains("cause_mort_"),
               death_date) |>
        select(ID, ends_with("_12months"), death_date) |> 
        PrepOutComp()


    Outcome_18 <- dummydf1 |> 
        select(ID,
               starts_with("Primary_"),
               starts_with("CVD_date"),
               contains("cause_mort_"),
               death_date) |>
        select(ID, ends_with("_18months"), death_date) |> 
        PrepOutComp()


    Outcome_24 <- dummydf1 |> 
        select(ID,
               starts_with("Primary_"),
               starts_with("CVD_date"),
               contains("cause_mort_"),
               death_date) |>
        select(ID, ends_with("_24months"), death_date) |> 
        PrepOutComp()

    
    ACMOutcomeData <- bind_rows(Outcome_6,
                                Outcome_12,
                                Outcome_18,
                                Outcome_24) 

    OutcomeData <- ACMOutcomeData |> 
        filter(EventType == "PrimaryOutcome") |> 
        group_by(ID) |>
        mutate(date = min(date)) |> 
        ungroup() |> 
        distinct(ID, date) |> 
        setDT()


    ### COMPETING EVENTS DATASET

    CompetingData <- ACMOutcomeData |> 
        filter(EventType == "ACM") |> 
        group_by(ID) |>
        mutate(date = min(date)) |> 
        ungroup() |> 
        distinct(ID, date) |> 
        setDT()


    ## LIST DATATABLES FOR RTMLE  ===========

    ldd <- list(
        baseline_data = BaselineDataset,
        timevar_data = timevar_data,
        outcome_data = OutcomeData,
        competing_data = CompetingData,
        censored_data = CensoredData
    )
    return(ldd)
}

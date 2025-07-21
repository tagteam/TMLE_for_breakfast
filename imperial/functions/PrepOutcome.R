## ORIGINAL EXAMPLE 


PrepOutComp <- function(FUdataset){
  
  FUdataset |> 
    select(ID,
           Primary_outcome = starts_with("Primary_"),
           CVD_date = starts_with("CVD_date"),
           Allcause_mort = contains("cause_mort_"),
           death_date) |>
    # select(ID, ends_with("_6months"), death_date) |> 
    mutate(EventType = 
             case_when(
               ## DEFINITION OF PRIMARY OUTCOME
               Primary_outcome == 1 &
                 CVD_date != "NULL" ~ "PrimaryOutcome",
               Primary_outcome == 1 &
                 CVD_date != "NULL" &
                 Allcause_mort == 1 ~ "PrimaryOutcome",
               # DEFINITION OF ALL-CAUSE MORTALITY 
               Primary_outcome == 1 &
                 CVD_date == "NULL" &
                 Allcause_mort == 1 ~ "ACM")) |> 
    
    mutate(
      across(.cols = contains("_date"),
             .fns = ~as.numeric(.x)),
      across(.cols = contains("_date"),
             .fns = ~as.Date(.x, origin = "1900-01-01"))
      ## using the second script for now 
      # across(.cols = contains("_date"),
      #        .fns = ~if_else(is.na(.), 0, 1))
    )  |> 
    select(ID,
           EventType, 
           CVD_date,
           death_date) |> 
    pivot_longer(cols = -c("ID", "EventType"),
                 names_to = "Outcome",
                 values_to = "date") |> 
    filter(!is.na(date), !is.na(EventType)) |> 
    select(-Outcome) 
} 





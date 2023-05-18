LtmleMSMFromInputs <-
function (inputs)
{
    if (inputs$estimate.time)
        EstimateTime(inputs)
    if (inputs$verbose){ message("LtmleMSMFromInputs: starting main calculations ...")}
    result <- MainCalcs(inputs)
    result$gcomp <- inputs$gcomp
    result$formulas <- list(Qform = inputs$Qform, gform = inputs$gform)
    result$binaryOutcome <- inputs$binaryOutcome
    result$transformOutcome <- inputs$transformOutcome
    result$survivalOutcome <- inputs$survivalOutcome
    return(result)
}

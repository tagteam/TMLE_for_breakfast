specify_analysis <- function(intervals,
                             regimen,
                             dual_studypop,
                             icd_data,
                             medicine){
    spec <- list(Anodes=names(regimen),
                 Lnodes=)
    grid <- dual_studypop[,.(date=start+intervals),by=pnr]
    # each interval has a number starting at zero
    grid[,interval:=0:(length(intervals)-1),by=pnr]
    #
    # map to intervals
    #
    ## wide=map_intervals(grid=grid,data=treatment,name="Statins",rollforward=6*30.45)
    ## for (d in unique(comed$drug)){
        ## drug_data=comed[drug==d][,drug:=NULL]
        ## w=map_intervals(grid=grid,data=drug_data,name=d,rollforward=6*30.45)
        ## wide=wide[w]
    ## }
    # see tmle_spec_TCP1 = readRDS("z:/Workdata/706582/Andrew Mertens/targets_diabetes_dementia/_targets/objects/tmle_spec_TCP1")
    # data: wide format, one line per patient, first baseline variables
    #       then time-dependent variables evaluated at baseline: _0
    #       then treatment_1 (WHY NO treatment_0?), censor_1 (uncensored/censored), event_1
    #       then time-dependent variables evaluated at time _1
    ## tmle_spec_TCP1$node_names = union of all the following
    ## tmle_spec_TCP1$Anodes
    ## [1] "glp1_1"  "glp1_2"  "glp1_3"  "glp1_4"  "glp1_5"  "glp1_6"  "glp1_7"
    ## [8] "glp1_8"  "glp1_9"  "glp1_10"
    ## $Cnodes
    ## [1] "censor_dem_1"  "censor_dem_2"  "censor_dem_3"  "censor_dem_4"
    ## [5] "censor_dem_5"  "censor_dem_6"  "censor_dem_7"  "censor_dem_8"
    ## [9] "censor_dem_9"  "censor_dem_10"
    ## $Ynodes
    ## [1] "event_dementia_1"  "event_dementia_2"  "event_dementia_3"
    ## [4] "event_dementia_4"  "event_dementia_5"  "event_dementia_6"
    ## [7] "event_dementia_7"  "event_dementia_8"  "event_dementia_9"
    ## [10] "event_dementia_10"
    ## $Lnodes
    ##   [1] "age_base"                    "sex"
    ##   [3] "code5txt"                    "quartile_income"
    ##   [5] "metformin_dur"               "year_2nd_line_start"
    ##   [7] "code5txt_miss"               "quartile_income_miss"
    ##   [9] "insulin_0"                   "chronic.pulmonary.disease_0"
    ##  [11] "hypertension_0"              "myocardial.infarction_0"
    ##  [13] "ischemic.heart.disease_0"    "heart.failure_0"
    ##  [15] "renal.disease_0"             "stroke_0"
    ##  [17] "any_second_line_0"           "insulin_1"
    ##  [19] "chronic.pulmonary.disease_1" "hypertension_1"
    ##  [21] "myocardial.infarction_1"     "ischemic.heart.disease_1"
    ## [103] "heart.failure_9"             "renal.disease_9"
    ## [105] "event_death_9"               "stroke_9"
    ## [107] "any_second_line_9"
}


baseline_covariates <- function(dual_studypop,
                                heart_failure,
                                reread_income){
    bsl= dual_studypop[,.(pnr,sex,start,birth_date,education,treat,start_diabetes,start_2ndline,first_2ndline)]
    # duration of diabetes
    bsl[,diabetes_duration:=Publish::acut(round(as.numeric(start-start_diabetes)/365.25,1),
                                          breaks=c(-Inf,5,10,Inf),format="%l-%u",format.low="below %u",format.high="above %l")]
    bsl[,start_diabetes:=NULL]
    # duration of secondline diabetes
    bsl[,secondline_duration:=Publish::acut(round(as.numeric(start-start_2ndline)/365.25,1),
                                            breaks=c(-Inf,1,3,Inf),format="%l-%u",format.low="below %u",format.high="above %l")]
    bsl[,start_2ndline:=NULL]
    bsl[,first_2ndline:=factor(first_2ndline)]
    levels(bsl$first_2ndline) <- list("glp1"="glp1","sglt2_inhib"="sglt2_inhib","other"=c("dpp4_inhib","sulfonylurea","thiazolodinidion"))
    # age
    bsl[,age:=round(as.numeric(start-birth_date)/365.25,1)]
    bsl[,birth_date:=NULL]
    bsl[,AgeGroups:=cut(age,breaks=c(-Inf,45,65,Inf),labels=c("<45","45-65",">65"))]
    bsl[,agegroups:=Publish::acut(age,breaks=c(-Inf,45,50,55,60,65,70,75,80,85,Inf),format="%l-%u",format.low="below %u",format.high="above %l")]
    bsl[,age:=NULL]
    # education
    levels(bsl$education) <- list(Basic="Basic","Medium"=c("Upper secondary","Vocational training"),"High"=c("Bachelor","Higher education"))
    message(paste0("Imputed ",bsl[is.na(education),.N]," missing education values as lowest education"))
    bsl[is.na(education),education:="Basic"]
    # income
    if(reread_income){
        income <- heaven::importSAS('X:/Data/Rawdata_Hurtig/706582/husstandsindk.sas7bdat',
                                    filter=bsl[,.(pnr)], obs=Inf, keep=c('pnr','year','indiv_indk_index'))
        income <- averageIncome(bsl[,.(pnr,start)],income,c("pnr","start"),c("pnr","year","indiv_indk_index"))
        fwrite(income,file="data/income_dual_studypop.csv")
    }else{
        income <- fread(file="data/income_dual_studypop.csv",colClasses=c("character","character","numeric"))
    }
    income[,tertile_income:=cut(income,quantile(income,probs = 0:3/3),include.lowest = TRUE,
                                labels=c("Income_q1","Income_q2","Income_q3"))]
    setkey(income,pnr)
    setkey(bsl,pnr)
    bsl <- income[,.(pnr,tertile_income)][bsl]
    message(paste0("Imputed ",bsl[is.na(tertile_income),.N]," missing income values as lowest quartile"))
    bsl[is.na(tertile_income),tertile_income:="Income_q1"]
    # index heart failure
    hf0=heart_failure[,.(pnr,start=inddto,index_heart_failure=1)]
    setkey(hf0,pnr,start)
    setkey(bsl,pnr,start)
    bsl <- hf0[bsl,roll=10*365.25]
    bsl[is.na(index_heart_failure),index_heart_failure:=0]
    bsl[,start:=NULL]
    bsl[,index_heart_failure:=factor(index_heart_failure,levels=c("0","1"),labels=c("No","Yes"))]
    bsl[]
}

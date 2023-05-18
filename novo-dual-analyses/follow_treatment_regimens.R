follow_treatment_regimens <- function(clean_dual_studypop,
                                      intervals=seq(0,10*6*30.45,6*30.45),
                                      rollforward=6*30,
                                      secondline_drugs,
                                      outcome,
                                      como,
                                      endfup_date=as.Date("2021-12-31")){
    clean_dual_studypop[,slut:=pmin(death_date, emigration_date ,endfup_date,na.rm=TRUE)]
    grid <- clean_dual_studypop[,.(date=start+intervals),by="pnr"]
    grid[,interval:=0:(length(intervals)-1),by=pnr]
    grid <- clean_dual_studypop[,.(pnr,slut)][grid,on="pnr"]
    length_interval=unique(round(diff(intervals),2))
    grid <- grid[date<=slut+length_interval]
    setkey(grid,pnr,date)
    # map drug prescriptions to personalized intervals
    x=copy(grid)
    setkey(x,pnr,date)
    for (d in levels(secondline_drugs$drug)){
        print(d)
        drug_data=secondline_drugs[drug==d][,.(pnr,date=start,X=1)]
        setnames(drug_data,"X",d)
        w <- drug_data[grid,roll=rollforward,on=c("pnr","date")]
        set(w,i=which(is.na(w[[d]])),j=d,value=0)
        x=x[w[,.SD,.SDcols=c("pnr","date",d)],on=c("pnr","date")]
    }
    # death and emigration and end-of-followup
    x <- clean_dual_studypop[!is.na(death_date),.(pnr,date=death_date,death=1)][x,roll=TRUE,on=c("pnr","date")]
    x[is.na(death),death:=0]
    x <- clean_dual_studypop[!is.na(emigration_date),.(pnr,date=emigration_date,emigration=1)][x,roll=TRUE,on=c("pnr","date")]
    x[is.na(emigration),emigration:=0]
    x <- clean_dual_studypop[,.(pnr,date=endfup_date,endfup=1)][x,roll=TRUE,on=c("pnr","date")]
    x[is.na(endfup),endfup:=0]
    # outcome
    if (length(outcome[[1]])==1){
        outcome <- outcome[[1]]
        outcome_data=como[[outcome]][,list(pnr=pnr,date=inddto,discharge=uddto)]
    }else{
        outcome_data <- rbindlist(lapply(outcome[[1]],function(oo){
            como[[oo]][,list(pnr,date=inddto,discharge=uddto)]
        }))
        outcome=names(outcome)
    }
    setkey(outcome_data,pnr,date)
    setkey(outcome_data,pnr)
    setkey(clean_dual_studypop,pnr)
    outcome_data=clean_dual_studypop[,.(pnr,start)][outcome_data]
    # -----------------------------------------------------------------------
    # only interested in new outcomes with onset after index
    # but want to tag patients who are in hospital with the outcome
    # at the index date, in order to use this as a baseline variable
    outcome_data=outcome_data[date>start]
    ## only interested in first new outcome
    setkey(outcome_data,pnr,date)
    outcome_data=outcome_data[outcome_data[,.I[1],by=c("pnr")]$V1]
    ## hospital diagnoses overlapping start
    ## admitted_index=outcome_data[date<=start & discharge>start,unique(pnr)]
    outcome_data <- outcome_data[,.(pnr,date=date)]
    outcome_data[[outcome]] <- 1
    x <- outcome_data[x,roll=TRUE,on=c("pnr","date")]
    set(x,j=outcome,i=which(is.na(x[[outcome]])),value=0)
    # define dual combination treatments
    x[,GS:=1*(glp1==1&sglt2_inhib==1)]
    x[,GO:=1*(glp1==1&(dpp4_inhib==1|sulfonylurea==1|thiazolodinidion==1))]
    x[,SO:=1*(sglt2_inhib==1&(dpp4_inhib==1|sulfonylurea==1|thiazolodinidion==1))]
    x[,OO:=1*((dpp4_inhib+sulfonylurea+thiazolodinidion)>1)]
    x[,GO:=1*(glp1==1&(dpp4_inhib==1|sulfonylurea==1|thiazolodinidion==1))]
    x[,SO:=1*(sglt2_inhib==1&(dpp4_inhib==1|sulfonylurea==1|thiazolodinidion==1))]
    x[,OO:=1*((dpp4_inhib+sulfonylurea+thiazolodinidion)>1)]
    ## three drugs
    x[,GSO:=1*(glp1==1&sglt2_inhib==1&(dpp4_inhib==1|sulfonylurea==1|thiazolodinidion==1))]
    x[,OOS:=1*(sglt2_inhib==1&OO==1)]
    x[,OOG:=1*(glp1==1&OO==1)]
    ## single drugs
    x[,glp1_only:=1*(glp1==1&sglt2_inhib==0&dpp4_inhib==0&sulfonylurea==0&thiazolodinidion==0)]
    x[,sglt2_only:=1*(sglt2_inhib==1&glp1==0&dpp4_inhib==0&sulfonylurea==0&thiazolodinidion==0)]
    x[,dpp4_only:=1*(dpp4_inhib==1&sglt2_inhib==0&glp1==0&sulfonylurea==0&thiazolodinidion==0)]
    x[,sulfonylurea_only:=1*(sulfonylurea==1&sglt2_inhib==0&glp1==0&dpp4_inhib==0&thiazolodinidion==0)]
    x[,thiazolodinidion_only:=1*(thiazolodinidion==1&sglt2_inhib==0&glp1==0&dpp4_inhib==0&sulfonylurea==0)]
    ## no drugs
    x[,holidays:=1*(glp1==0&sglt2_inhib==0&dpp4_inhib==0&sulfonylurea==0&thiazolodinidion==0)]
    second_line_drugs=c("glp1","sglt2_inhib","dpp4_inhib","sulfonylurea","thiazolodinidion")
    duals <- list("GS"=c("glp1","sglt2_inhib"),
                  "GO"=c("glp1","dpp4_inhib","sulfonylurea","thiazolodinidion"),
                  "SO"=c("sglt2_inhib","dpp4_inhib","sulfonylurea","thiazolodinidion"),
                  "OO"=c("dpp4_inhib","sulfonylurea","thiazolodinidion"))
    triples=c("GSO","OOS","OOG")
    singles=c("glp1_only","sglt2_only","dpp4_only","sulfonylurea_only","thiazolodinidion_only")
    setkey(x,pnr,interval)
    states <- foreach (d=names(duals))%do%{
        # restrict to patients who start this dual treatment
        d.id <- x[interval==0&x[[d]]==1,.(pnr)]
        setkey(d.id,pnr)
        xd=x[d.id]
        # count transitions of patients to all other possible states
        other_states <- c("death",outcome,"emigration","endfup",triples,names(duals),singles,"holidays")
        state_matrix <- data.table(data.frame(matrix(numeric(length(intervals)*length(other_states)),ncol=length(other_states))))
        names(state_matrix) <- other_states
        state_matrix <- cbind(interval=0:(length(intervals)-1),state_matrix)
        # initialize first row
        set(state_matrix,i=1L,j=d,value=NROW(xd[interval==0]))
        xd_next=copy(xd)
        for (k in (1:(length(intervals)-1))){
            for (s in other_states){
                enter_state_s <- xd_next[interval==k & xd_next[[s]]==1,.(pnr)]
                set(state_matrix,i=as.integer(k+1),j=s,value=NROW(enter_state_s))
                # flag subjects according to state hierachy
                if (s %in% c("death","emigration","endfup"))
                    xd_next=xd_next[!(pnr%in%enter_state_s$pnr)]
            }
        }
        state_matrix
    }
    names(states) <- names(duals)
    states
}

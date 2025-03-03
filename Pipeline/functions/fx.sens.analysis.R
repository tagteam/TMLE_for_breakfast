#Sensitivity analysis

# tar_load(dt_sgl_dpp4)
# tar_load(pnrlist_EMPAREG)
# tar_load(death)
# tar_load(immiemi)
# tar_load(primary_outcomes)
# tar_load(dt_baseline_allvar_LTMLEprep)
# tar_load(dt_widencovar_LTMLEprep)
# tar_load(names_baseline_covariates)

# path1="Z:/Workdata/703740/Puriya/SGLT2/output"
# output1="output/ltmle_mace_plot_sens_2014.jpeg"

# lapply(list.files("functions", pattern=".R$",full.names = TRUE), source)
#time1, time2, time3, path1, path2, path3, output1, output2, output3

fx.sensitivity.analysis <- function(time1, time2, path1, path2, output1, output2, dt_sgl_dpp4, pnrlist_EMPAREG, 
                                    death, immiemi, primary_outcomes, dt_baseline_allvar_LTMLEprep, 
                                    dt_widencovar_LTMLEprep, names_baseline_covariates) {


  
  dt <- dt_sgl_dpp4[pnr%in%pnrlist_EMPAREG$pnr]
  setnames(dt, "index_date", "index.date")
    
    
    dt1 <- dt[year(index.date) >=time1]
    dt1 <- death[, .(pnr, doddato)][dt1, on = .(pnr) ]
    ud <- unique(immiemi[indud_kode=="U",.(pnr,emi.date=haend_dato,joindate=haend_dato)])
    setkey(ud,pnr,joindate)
    dt1[,joindate:=index.date]
    setkey(dt1,pnr,joindate)
    dt1 <- ud[dt1,roll=-Inf] #roll forward
    dt1 <- dt1[start>=index.date]
    dt1[,':='(joindate=NULL)]
    
    dt2 <- dt[year(index.date) >=time2]
    dt2 <- death[, .(pnr, doddato)][dt2, on = .(pnr) ]
    ud <- unique(immiemi[indud_kode=="U",.(pnr,emi.date=haend_dato,joindate=haend_dato)])
    setkey(ud,pnr,joindate)
    dt2[,joindate:=index.date]
    setkey(dt2,pnr,joindate)
    dt2 <- ud[dt2,roll=-Inf] #roll forward
    dt2 <- dt2[start>=index.date]
    dt2[,':='(joindate=NULL)]

  
  sens1 <- fx.widen.regimens.LTMLEprep(regime_data=dt1,
                                        intervals=seq(0,10*6*30.45,6*30.45))
  
  sens2 <- fx.widen.regimens.LTMLEprep(regime_data=dt2,
                                       intervals=seq(0,10*6*30.45,6*30.45))
  
  
  primary_analysis_sens1 <- {
    
    list(lapply(1:9,function(x){
      get_primary_analysis(
        primary_outcomes=primary_outcomes,
        primary_treatment_regimens=sens1,
        primary_baseline_covariates=dt_baseline_allvar_LTMLEprep[,names_baseline_covariates,with=FALSE],
        primary_time_covariates=dt_widencovar_LTMLEprep,
        SL.library="glmnet",
        time_horizon=x
      )
    }))
  }
  
  
  primary_analysis_sens2 <- {
    
    list(lapply(1:9,function(x){
      get_primary_analysis(
        primary_outcomes=primary_outcomes,
        primary_treatment_regimens=sens2,
        primary_baseline_covariates=dt_baseline_allvar_LTMLEprep[,names_baseline_covariates,with=FALSE],
        primary_time_covariates=dt_widencovar_LTMLEprep,
        SL.library="glmnet",
        time_horizon=x
      )
    }))
  }
  
  ltmle_plot1 <- get_ltmle_plot(primary_analysis_sens1,
                                       pathname=path1,
                                       output=output1)
  
  ltmle_plot2 <- get_ltmle_plot(primary_analysis_sens2,
                               pathname=path2,
                               output=output2)
}

  
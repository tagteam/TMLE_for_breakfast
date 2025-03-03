fx.get_baseline_covar <- function(data,income,education,sexage,kom){

# tar_load(sexage)
# tar_load(education)
# tar_load(income)
# tar_load(kom)
# tar_load(dt_index)
# data=dt_index
  
  
data[,min.start:=index.date]
data[,min.start.year:=year(min.start)]
data <- unique(data[,.(pnr,min.start,min.start.year)])
  
# Add sex and age ----
data <-  unique(sexage[,.(pnr,foed_dag,koen)])[data,on=.(pnr)]
setnames(data,c("foed_dag","koen"),c("fdato","sex"))
data[,sex:=factor(sex,levels=c(1,0),labels=c("male","female"))]

# Degree of urbanization ----
kom[,year:=year(referencetid)]
kom[,referencetid:=NULL]
kom[,joiny:=year]
setkey(kom,pnr,joiny)

data[,joiny:=min.start.year]
setkey(data,pnr,joiny)

data <- kom[data,roll=T]

#Fix degree of urbanization

# Income ---- 
inc <- averageIncome(data,income,
                        c("pnr","min.start"),
                        c("pnr","year","aekvivadisp_13"),
                        numyears=5)
data <- inc[,.(pnr,income)][data,on=.(pnr=pnr)]


# Education ----
edu <- education[,c("isced_code","year","pnr")]
edu[,joiny:=year]
setkey(edu,pnr,joiny)

data[,joiny:=min.start.year]
setkey(data,pnr,joiny)

data <- edu[data,roll=T]

#fix edu categorization 
data[isced_code =="1"|isced_code =="2",edu:="group1"]
data[isced_code =="3",edu:="group2"]
data[isced_code =="5"|isced_code =="6",edu:="group3"]
data[isced_code =="7"|isced_code =="8",edu:="group4"]
#what about 9??? 

data <- data[,.SD[1],pnr] # make sure that there is only one obs per individual - some have moved during the calendar year 

return(unique(data))
}
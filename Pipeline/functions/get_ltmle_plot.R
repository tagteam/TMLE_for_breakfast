get_ltmle_plot <- function(primary_analysis, pathname, output) {
  
  library(readxl)
  #tar_load(primary_analysis)
  
  filelist <- list.files(pathname,pattern="^res",full.names=T)
  
  dat <- rbindlist(lapply(1:length(filelist),function(x){
      dat <- readxl::read_excel(filelist[x]) 
    }))
  
  dat[,arm:=c(rep("DPP4",9),rep("SGLT2",9))]
  
  p <- ggplot(dat,aes(x=time_horizon,y=estimate,color=arm))+
    geom_line()+
    geom_point()+
    xlab("Time (months)")+
    ylab("Probability of an event of type 'Outcome within t months")+
    geom_ribbon(aes(ymin=lower,ymax=upper,fill=arm),alpha=0.1,show.legend = F,colour=NA)+
    theme_classic()+
    scale_x_continuous(breaks=seq(0,9,1),labels=seq(0,9*6,6))+
    scale_y_continuous(breaks=seq(0,0.26,0.02),limits=c(0,0.20))+
    labs(color="Treatment arm")
  
  p
  ggsave(output,scale=0.5,height=9,width=14)
  p
}

#output/ltmle_mace_plot.jpeg
#Z:/Workdata/703740/Puriya/SGLT2/output
### show_performance.R ---
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 10 2023 (07:01)
## Version:
## Last-Updated: May 17 2024 (15:12) 
##           By: Thomas Alexander Gerds
##     Update #: 15
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
### Code:
show_performance <- function(print=TRUE){
    m = tar_meta()
    data.table::setDT(m)
    m = m[type == "stem"]
    m[,Memory := utils:::format.object_size(bytes,units="GB")]
    m[,Minutes := seconds%/%(60)]
    m[,Hours := seconds%/%(60*60)]
    m = m[,.(Target = name,Hours,Minutes,Seconds = seconds,Memory,Storage = format)]
    if (all(m$Hours == 0)) m$Hours = NULL else m$Seconds = NULL
    if (print) print(m)
    invisible(m)
}
######################################################################
### show_performance.R ends here



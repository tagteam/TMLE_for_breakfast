### augmented_show_errors.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 17 2024 (15:14) 
## Version: 
## Last-Updated: May 17 2024 (15:20) 
##           By: Thomas Alexander Gerds
##     Update #: 2
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
show_errors <- function(){
    m = tar_meta()
    data.table::setDT(m)
    m = m[!is.na(warnings)|!is.na(error)]
    un = unique(m$n)
    if (length(un) == 0) {
        cat("No errors or warnings\n")
        invisible(NULL)
    }else{
        for (n in un){
            cat("Target", n,":\n")
            if (!is.na(en <- m[name == n]$error))
                cat("   Errors: ",en,"\n")
            if (!is.na(wn <- m[name == n]$warning))
                cat("   Warnings: ",wn,"\n")
        }
        invisible(m)
    }
}


######################################################################
### augmented_show_errors.R ends here

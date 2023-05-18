

package_stub<-function (package_name, function_name, stubbed_value, expr){
  if (!is.element(package_name, utils::installed.packages()[,
                                                            1]) && !is.element(package_name, loadedNamespaces())) {
    stop(gettextf("Could not find package %s for stubbing %s",
                  sQuote(package_name), dQuote(function_name)))
  }
  stopifnot(is.character(function_name))
  if (!is.function(stubbed_value))
    warning(gettextf("Stubbing %s::%s with a %s instead of a function",
                     package_name, function_name, sQuote(class(stubbed_value)[1])))
  namespaces <- list(as.environment(paste0("package:",
                                           package_name)), getNamespace(package_name))
  if (!exists(function_name, envir = namespaces[[1]], inherits = FALSE))
    namespaces <- namespaces[-1]
  if (!exists(function_name, envir = utils::tail(namespaces,
                                                 1)[[1]], inherits = FALSE))
    stop(gettextf("Cannot stub %s::%s because it must exist in the package",
                  package_name, function_name))
  lapply(namespaces, unlockBinding, sym = function_name)
  previous_object <- get(function_name, envir = utils::tail(namespaces,
                                                            1)[[1]])
  on.exit({
    lapply(namespaces, function(ns) {
      tryCatch(error = function(.) NULL, assign(function_name,
                                                previous_object, envir = ns))
      lockBinding(function_name, ns)
    })
  })
  lapply(namespaces, function(ns) assign(function_name, stubbed_value,
                                         envir = ns))
  eval.parent(substitute(expr))
}






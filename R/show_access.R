
#' Access values stored in the data slot of an object of class \code{predictive}
#' @usage \\method{$}{lilikelihood}(object, ...)
#' @noRd
#' @export
setMethod("$",
  signature = "predictive",
  function(x, name) {
    returnval <- x@data[[name]]
    return(returnval)
  }
)

#' Access values stored in the data slot of an object of class \code{likelihood}
#' @usage \\method{$}{lilikelihood}(object, ...)
#' @noRd
#' @export
setMethod("$",
  signature = "likelihood",
  function(x, name) {
    returnval <- x@data[[name]]
    return(returnval)
  }
)

#' Access values stored in the data slot of an object of class \code{prior}
#' @usage \\method{$}{lilikelihood}(object, ...)
#' @noRd
#' @export
setMethod("$",
          signature = "prior",
          function(x, name) {
            returnval <- x@data[[name]]
            return(returnval)
          }
)

#' Get names of the data slot of an object of class \code{likelihood}
#' @usage \\method{names}{likelihood}(object, ...)
#' @noRd
#' @export
setMethod("names",
  signature = "likelihood",
  function(x) {
    return(names(x@data))
  }
)

#' Get names of the data slot of an object of class \code{prior}
#' @usage \\method{names}{prior}(object, ...)
#' @noRd
#' @export
setMethod("names",
          signature = "prior",
          function(x) {
            return(names(x@data))
          }
)



#' Summary for an object of class \code{likelihood}
#' @noRd
#' @export
setMethod(
  "show",
  "likelihood",
  function(object) {
    cat("Object of class", class(object), "\n")
    cat("Likelihood type:", object@data$likelihood_type, "\n")
    cat(object@desc, "\n")
  }
)


#' Summary for an object of class \code{prior}
#' @noRd
#' @export
setMethod(
  "show",
  "prior",
  function(object) {
    cat("Object of class", class(object), "\n")
    cat("Prior type:", object@data$distribution, "\n")
    cat(paste0(object@desc, collapse = ""))
  }
)



#' Get names of the data slot of an object of class \code{predictive}
#' @usage \\method{names}{predictive}(object, ...)
#' @noRd
#' @export
setMethod("names",
  signature = "predictive",
  function(x) {
    return(names(x@data))
  }
)

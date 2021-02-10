
prior_class <- setClass(
  Class = "prior",
  slots = list(
    data = "list",
    theta_range = "numeric",
    func = "function",
    desc = "character",
    type = "character",
    dist_type = "character",
    plot = "list",
    parameters = "list",
    function_text = "character"
  )
)


likelihood_class <- setClass(
  Class = "likelihood",
  slots = list(
    data = "list",
    # theta_range = "",
    func = "function",
    desc = "character",
    # type = "character",
    dist_type = "character",
    plot = "list",
    # parameters = "",
    marginal = "character"
  )
)




predictive_class <- setClass(
  Class = "predictive",
  slots = list(
    data = "list",
    desc = "character",
    K = "numeric",
    lik = "function",
    prior = "function",
    theta_range = "numeric",
    likelihood_obj = "likelihood",
    prior_obj = "prior"
  )
)

setClassUnion("bayesplay", c("likelihood", "prior", "predictive"))

#' Summary for an object of class \code{bayesplay}
#' @noRd
#' @export
setMethod(
  "show",
  "bayesplay",
  function(object) {
    cat(object@desc, "\n")
  }
)

#' Access values stored in the data slot of an object of class \code{bayesplay}
#' @usage \\method{$}{bayesplay}(object, ...)
#' @noRd
#' @export
setMethod("$",
  signature = "bayesplay",
  function(x, name) {
    returnval <- x@data[[name]]
    return(returnval)
  }
)


#' Get names of the data slot of an object of class \code{bayesplay}
#' @usage \\method{names}{bayesplay}(object, ...)
#' @noRd
#' @export
setMethod("names",
  signature = "bayesplay",
  function(x) {
    return(names(x@data))
  }
)

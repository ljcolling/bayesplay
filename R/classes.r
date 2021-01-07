
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



setClassUnion("bayesplay", c("likelihood", "prior"))

predictive_class <- setClass(
  Class = "predictive",
  slots = list(
    data = "list",
    K = "numeric",
    lik = "function",
    prior = "function",
    theta_range = "numeric",
    likelihood_obj = "likelihood",
    prior_obj = "prior"
  )
)

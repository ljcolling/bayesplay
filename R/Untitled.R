# #' @export
# setClass(Class = "student_t",
#         list(family = "character", fun = "function"),
#         prototype = list(family = "student_t",
#                          fun = function(x, mean, sd) {
#                          dnorm(x = x, mean = mean, sd = sd)})
# )
# 
# #' @export
# normal <- setClass(Class = "normal", list(family = "character"))
# 
# 
# setClassUnion("family", c("normal",
#                              "student_t"))
# #' likelihood
# #' @export
# setGeneric("lik", signature = "family", function(family, ...) NextMethod("lik"))
# 
# setMethod("lik", signature(family = "normal"), function(family, mean, sd) {
#   lik.normal(family, mean, sd)
# })
# 
# #' @export
# l <- function(family, ...) {
#   if (is.character(family))
#     f <- tryCatch(new(family), error = function(e) stop("invalid family", call. = FALSE))
#   lik(family = f, ...)
# }
# 
# 
# setMethod("lik", signature(family = "student_t"), function(family, mean, sd, df) {
#   lik.student_t(family, mean, sd, df)
# })
# 
# 
# lik.default <- function(family) {
#   stop("no method for family ", class(family))
# }
# lik.normal <- function(family, mean, sd) {
#   mean + sd
#   return("normal")
# }
# 
# lik.student_t <- function(family, mean, sd, df) {
#   mean + sd + df
#   return("student_t")
# }
# 


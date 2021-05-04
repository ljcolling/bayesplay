#' @export
student_t <- setClass(Class = "student_t", list(family = "character"))

#' @export
normal <- setClass(Class = "normal", list(family = "character"))


setClassUnion("family", c("normal",
                             "student_t"))
#' likelihood
#' @export
setGeneric("lik", signature = "family", function(family, ...) NextMethod("lik"))

setMethod("lik", signature(family = "normal"), function(family, mean, sd) {
  lik.normal(family, mean, sd)
})

setMethod("lik", signature(family = "student_t"), function(family, mean, sd, df) {
  lik.student_t(family, mean, sd, df)
})

lik.normal <- function(family, mean, sd) {
  mean + sd
  return("normal")
}

lik.student_t <- function(family, mean, sd, df) {
  mean + sd + df
  return("student_t")
}



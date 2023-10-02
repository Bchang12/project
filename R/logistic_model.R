#' logistic model function
#'
#' @param data
#' @param predictors
#'
#' @return summary of logistic model
#' @export
#'
#' @examples
logistic_model = function(data=heart, predictors) {
  formula = paste("output", "~", paste(predictors, collapse = " + "))
  model = glm(formula, family=binomial, data = data)

  return(summary(model))
}



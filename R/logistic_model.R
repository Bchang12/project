#' logistic model function
#'
#' Fit a logistic regression model to find a heart attack probability. Please refer to "heart" data document for variable names.
#'
#' @param data
#' @param predictors possible predictors in heart attack data. Please use the original "heart" data variable name.
#'
#' @return summary of logistic model
#' @export
#'
#'
#' @examples logistic_model(data=heart, c("age","cp"))
#' @examples logistic_model(c("age","cp"))

logistic_model = function(data=heart, predictors) {
  formula = paste("output", "~", paste(predictors, collapse = " + "))
  model = glm(formula, family=binomial, data = data)

  return(summary(model))
}



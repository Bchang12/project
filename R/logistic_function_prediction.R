#' prediction for logistic regression
#'
#' @param data
#' @param predictors
#' @param new_data
#'
#' @return probability of new observation's heart attack based on the predictors
#' @export
#'
#' @examples
predict_regression_model = function(data, predictors, new_data) {
  formula = paste("output", "~", paste(predictors, collapse = " + "))
  model = glm(formula, family=binomial, data = data)
  predictions = predict(model, newdata = new_data,type="response")

  return(predictions)
}

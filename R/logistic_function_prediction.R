#' prediction for logistic regression
#'
#' a function return the predicted probability of a fitted logisitc regression model.
#'
#' @param data
#' @param predictors possible predictors in heart attack data. Please use the original "heart" data variable name.
#' @param new_data a data frame for new observations
#'
#' @return probability of new observation's heart attack based on the predictors
#' @export
#'
#' @examples new_data=data.frame(age=50, sex="Male")
#' @examples predict_regression_model(data=heart, predictors=c("age","sex"), new_data=new_data)
#' @examples predict_regression_model(predictors=c("age","sex"), new_data=new_data)
#'
predict_regression_model = function(data, predictors, new_data) {
  formula = paste("output", "~", paste(predictors, collapse = " + "))
  model = glm(formula, family=binomial, data = data)
  predictions = predict(model, newdata = new_data,type="response")

  return(predictions)
}

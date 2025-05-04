#' @keywords internal
"_PACKAGE"

## usethis namespace: start

#' @title Function to choose predictors in model
#' @description Gives original model of data with all predictors, model of data with only best predictors
#' @param data A data frame containing the variables.
#' @param response_var Variable to model.
#' @param predictor_vars Predictor variables.
#' @export
#' @seealso \link{vignette("Choose_best_model", package = "bim")}
#' @examples
#' choose_best_model <- function(data, response_var, predictor_vars)

#Packages needed: MuMIn

choose_best_model <- function(data, response_var, predictor_vars) {
  options(na.action = "na.fail")  # Required for dredge

  # Build full model formula
  predictors_str <- paste(predictor_vars, collapse = " + ")
  formula_str <- paste(response_var, "~", predictors_str)
  full_formula <- as.formula(formula_str)

  # Fit full model
  full_model <- lm(full_formula, data = data)

  # Model selection using dredge
  all_models <- dredge(full_model)
  top_models <- get.models(all_models, subset = delta < 4)

  # Compute variable importance
  importance_df <- as.data.frame(sw(model.avg(top_models)))
  importance_df$predictor <- rownames(importance_df)
  importance_df$N_models <- sapply(importance_df$predictor, function(var) {
    sum(sapply(top_models, function(m) var %in% names(coef(m))))
  })

  # Find predictors in all top models
  n_models_total <- length(top_models)
  predictors_in_all_models <- importance_df$predictor[importance_df$N_models == n_models_total]

  # If no predictors appear in all models, fall back to those in >50%
  if (length(predictors_in_all_models) == 0) {
    predictors_in_all_models <- importance_df$predictor[importance_df$N_models > n_models_total / 2]
  }

  # Create reduced model
  if (length(predictors_in_all_models) > 0) {
    reduced_formula_str <- paste(response_var, "~", paste(predictors_in_all_models, collapse = " + "))
    reduced_formula <- as.formula(reduced_formula_str)
    reduced_model <- lm(reduced_formula, data = data)
  } else {
    reduced_model <- NULL
  }

  return(list(
    full_model = full_model,
    reduced_model = reduced_model,
    top_predictors = predictors_in_all_models
  ))
}

## usethis namespace: end
NULL

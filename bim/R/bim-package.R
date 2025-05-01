#' @keywords internal
"_PACKAGE"

## usethis namespace: start

#Function to choose predictors in model
#Inputs: dataframe, variable to model, predictor variables
#Outputs: original model of data with all predictors, model of data with only best predictors
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

#Function to make tables of summary of the model
#Inputs: model
#Outputs: table of Estimate, SE, F, and p for the given model
#Packages needed: car, dplyr
make_model_table <- function(model) {
  # Extract model summary
  model_summary <- summary(model)$coefficients

  # Convert to data frame and select relevant columns
  model_df <- as.data.frame(model_summary)[, c("Estimate", "Std. Error", "Pr(>|t|)")]

  # Run Type II ANOVA using car::Anova
  anova_table <- Anova(model, type = "II")

  # Extract F values and match with row names
  f_values <- anova_table[, "F value"]
  model_df$F_value <- f_values[match(rownames(model_df), rownames(anova_table))]

  # Clean and rename columns
  model_df <- model_df %>%
    filter(rownames(model_df) != "(Intercept)") %>%
    rename(
      SE = `Std. Error`,
      p = `Pr(>|t|)`,
      F = F_value
    )

  return(model_df)
}

#Function to make plot of predicted values, split between groups
#Inputs: model, dataframe, y_var, x_var, group_var
#Outputs: plot of variable and predictor with CI, split by group
#Packages needed: ggplot2, car
plot_model_with_ci <- function(model, data, y_var, x_var,
                               group_var = NULL,
                               x_label = NULL,
                               y_label = NULL,
                               color_vals = NULL,
                               color_labels = NULL) {

  # Predict with confidence intervals
  preds <- predict(model, newdata = data, se.fit = TRUE)
  z_val <- 1.96
  data$fit <- preds$fit
  data$se_fit <- preds$se.fit
  data$lower_CI <- data$fit - z_val * data$se_fit
  data$upper_CI <- data$fit + z_val * preds$se.fit

  # Sort data to help smoothness if needed
  data <- data[order(data[[x_var]]), ]

  # Tidy variable references
  x_sym <- sym(x_var)
  group_sym <- if (!is.null(group_var)) sym(group_var) else NULL

  # Base plot
  p <- ggplot(data, aes(x = !!x_sym, y = fit))

  # Add smoothed line and confidence ribbon only
  if (!is.null(group_var)) {
    p <- p +
      geom_smooth(aes(color = !!group_sym, fill = !!group_sym), method = "lm", se = TRUE, formula = y ~ x)

    if (!is.null(color_vals)) {
      p <- p +
        scale_color_manual(values = color_vals, labels = color_labels) +
        scale_fill_manual(values = color_vals, labels = color_labels)
    }
  } else {
    p <- p +
      geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "blue", fill = "blue", alpha = 0.2)
  }

  # Add labels and theme
  p <- p +
    labs(
      x = ifelse(is.null(x_label), x_var, x_label),
      y = ifelse(is.null(y_label), paste("Predicted", y_var), y_label),
      color = ifelse(!is.null(group_var), group_var, NULL),
      fill = ifelse(!is.null(group_var), group_var, NULL)
    ) +
    theme_minimal()

  return(p)
}




## usethis namespace: end
NULL

#' @keywords internal
"_PACKAGE"

## usethis namespace: start

#' @title Function to make plot of predicted values, split between groups
#' @description Plot of variable and predictor with CI, split by group
#' @param model A statistical model of data.
#' @param dataframe A data frame containing the variables.
#' @param y_var A string specifying the name of the outcome variable.
#' @param x_var A string specifying the name of the predictor variable.
#' @param group_var A string specifying the name of the grouping variable to split lines by.
#' @export
#' @seealso \link{vignette("bim")}
#' @examples
#' plot_model_with_ci(model = brain_CD8_model_reduced, data = combined_data, y_var = "Brain CD8+ T Cell Counts", x_var = "Meninges CD8+ T Cell Counts", group_var = "Group")
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

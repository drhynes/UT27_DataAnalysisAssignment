#' @title Creates Table Summary
#' @name make_model_table
#' @description
#' This function generates a table of estimates, standard errors (SE), F-values, and p-values for the specified model.
#' @param model A statistical model of data.
#' @keywords statistical summary
#' @examples
#' make_model_table(brain_CD8_model)
#' @seealso 
#'   [Make Model Table Vignette](../doc/Make_model_table.html)
#' @import car dplyr
#' @export

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

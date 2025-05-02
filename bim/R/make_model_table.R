#' @keywords internal
"_PACKAGE"

## usethis namespace: start

#' @title Function to make tables of summary of the model
#' @description Table of Estimate, SE, F, and p for the given model
#' @param model A statistical model of data.
#' @export
#' @examples
#' make_model_table(brain_CD8_model)


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

## usethis namespace: end
NULL

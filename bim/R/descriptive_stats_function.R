#' @title Provides Descriptive Statistics
#' @name descriptive_data
#' @description
#' This function can be used to calculate different types of descriptive statistics (e.g., number of subjects (n), median, interquartile range (IQR), mean, standard deviation (SD), and the standard error of the mean (SEM)).
#' These descriptive statistics are calculated for any group and for any numerical variables of your choosing.
#' @param data A data frame or tibble containing a dataset.
#' @param group_variables A character vector of your column names that is written as "text". This will tell the function how to group the data (e.g., group_variables(c("Sex", "Genotype"))).
#' @param cell_variables A character vector of column names that is written as "text". This should have the numerical data that you want to summarize (e.g., cell_variables(c("T_Cells", "B_Cells", "Microglia"))).
#' @param stats A character vector of which descriptive statistics to calculate. Options include: "n", "median", "q1", "q3", "mean", "sd", "sem" and will defaults to all of them if no stats are chosen.
#' @return A data frame where each row represents a group and each column contains the summary statistics that were calculated.
#' Each column of descriptive statistics will contain the name of the numerical variable and have the name of the statistic (e.g., T_Cells_mean ).
#' @keywords descriptive statistics summary
#' @examples
#' \dontrun{
#' descriptive_data(
#'   data = data_frame,
#'   group_variables = c("Sex", "Genotype"),
#'   cell_variables = c("T_Cells", "B_Cells", "Microglia"),
#'   stats = c("n", "mean", "sd")
#' )}
#' @seealso
#'   [Descriptive Stats Function Vignette](../doc/Descriptive_stats_function.html)
#' @import dplyr
#' @export

descriptive_data <- function(data, group_variables, cell_variables, stats = c("n", "median", "q1", "q3", "mean", "sd", "sem")) {

  stats_formulas <- list( # ~ is needed to create a temporary function ; .x is just a place holder for the column that is being summarized
    n = ~sum(!is.na(.x)), # counts the number of subjects per group and does not count NAs
    median = ~median(.x, na.rm = TRUE), # calculates the median
    q1 = ~quantile(.x, 0.25, na.rm = TRUE), # calculates the 25th percentile
    q3 = ~quantile(.x, 0.75, na.rm = TRUE),# calculates the 75th percentile
    mean = ~mean(.x, na.rm = TRUE), # calculates the mean
    sd = ~sd(.x, na.rm = TRUE), # calculates the standard deviation
    sem = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))) # calculates the standard error of the mean

  selected_formulas <- stats_formulas[stats] # this helps apply each of the stats that were selected or all of them if no selection was made below

  data |> # data table
    group_by(across(all_of(group_variables))) |> # first groups by categorical variables; across() - applies to all columns listed ; all_of() - turns the character string list into column names
    summarise(across( #across() - apply to all columns listed
      all_of(cell_variables), #all_of() - turns the character string list into the column names
      .fns = selected_formulas, # Applies the selected list of formulas
      .names = "{.col}_{.fn}")) # creates a new column with the name of the variable and what summary statistics was done
}




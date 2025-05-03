#' @description 
#' This function can be used to check for outliers in your dataset for any group and for any numerical variables of your choosing. 
#' It finds outliers that fall below the first quartile or above the third quartile by more than 1.5 times the interquartile range (IQR).
#' The method is based on the standard Tukey outlier rule, though the multiplier can be changed to be more conservative or more lenient (e.g., 1, 1.5, 2, 2.5, or 3)
#' @param data A data frame containing a dataset. 
#' @param group_variables A character vector of your column names that is written as "text". This will tell the function how to group the data (e.g., group_variables(c("Sex", "Genotype"))).
#' @param cell_variables A character vector of column names that is written as "text". This should have the numerical data you want to check for outliers (e.g., cell_variables(c("T_Cells", "B_Cells", "Microglia"))). 
#' @param outlier_condition A numerical value that sets how far from the IQR a value must be to be considered an outlier. Default is 1.5 (Tukey's rule), but it can be adjusted.
#' @return A data frame containing categorical data, the same columns that were given, and new columns next to each given variable showing which values exactly are ouliers displaying TRUE or FALSE. 
#' The data frame also only contains the only rows where outliers were detected.                                                                                                                                   
#' @keywords outliers                                                                                                                                      
#' @examples
#' check_for_outliers(
#'   data = my_data,
#'   group_variables = c("Sex", "Genotype"),
#'   cell_variables = c("T_Cells", "B_Cells"), 
#'   outlier_condition = 1.5
#' )
#' @export

check_for_outliers <- function(data, group_variables, cell_variables, outlier_condition = 1.5) {
  outlier_data <- data |> #creating a data frame that is going to check is there is a outlier and add a column that says TRUE or FALSE for each numerical variables given 
    group_by(across(all_of(group_variables))) |> # first groups by categorical variables; across() - applies to all columns listed ; all_of() - turns the character string list into the column names
    mutate( #creating a new column
      across( #across() - applies to all columns listed
      .cols = all_of(cell_variables), #grabbing all the numerical variables 
      .fns = ~ (. < quantile(., 0.25, na.rm = TRUE) - outlier_condition * IQR(., na.rm = TRUE)) | (. > quantile(., 0.75, na.rm = TRUE) + outlier_condition * IQR(., na.rm = TRUE)), # outlier test being applied to each column listed 
      .names = "{.col}_outliers")) #creating the new column to show its an outlier or not
    
  outlier_columns <- paste0(cell_variables, "_outliers") # you need to rewrite columns otherwise it wont let you filter them later
  
  ordered_cell_columns <- as.vector(rbind(cell_variables, outlier_columns)) # this allows for new outlier_column to be next to the original variable
  
  categorical_variables <- names(select(data, where(~ is.factor(.) || is.character(.)))) # keeps all the categorical variables to know all of the information about these outliers 
  
  # this creates a new data frame that shows only the outliers 
  outlier_data |> # data frame that was created above 
    filter(if_any(all_of(outlier_columns), ~ .)) |> # keeps only rows where outliers are present
    select(all_of(categorical_variables), all_of(ordered_cell_columns))  # keeps only the categorical variables, cell variables, and the new outlier columns
}

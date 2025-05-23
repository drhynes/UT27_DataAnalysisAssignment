% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_function.R
\name{check_for_outliers}
\alias{check_for_outliers}
\title{Identify Outliers}
\usage{
check_for_outliers(
  data,
  group_variables,
  cell_variables,
  outlier_condition = 1.5
)
}
\arguments{
\item{data}{A data frame containing a dataset.}

\item{group_variables}{A character vector of your column names that is written as "text". This will tell the function how to group the data (e.g., group_variables(c("Sex", "Genotype"))).}

\item{cell_variables}{A character vector of column names that is written as "text". This should have the numerical data you want to check for outliers (e.g., cell_variables(c("T_Cells", "B_Cells", "Microglia"))).}

\item{outlier_condition}{A numerical value that sets how far from the IQR a value must be to be considered an outlier. Default is 1.5 (Tukey's rule), but it can be adjusted.}
}
\value{
A data frame containing categorical data, the same columns that were given, and new columns next to each given variable showing which values exactly are ouliers displaying TRUE or FALSE.
The data frame also only contains the only rows where outliers were detected.
}
\description{
This function can be used to check for outliers in your dataset for any group and for any numerical variables of your choosing.
It finds outliers that fall below the first quartile or above the third quartile by more than 1.5 times the interquartile range (IQR).
The method is based on the standard Tukey outlier rule, though the multiplier can be changed to be more conservative or more lenient (e.g., 1, 1.5, 2, 2.5, or 3)
}
\examples{
\dontrun{
check_for_outliers(
  data = my_data,
  group_variables = c("Sex", "Genotype"),
  cell_variables = c("T_Cells", "B_Cells"),
  outlier_condition = 1.5
)}
}
\seealso{
\href{../doc/Outlier_function.html}{Outlier Function Vignette}
}
\keyword{outliers}

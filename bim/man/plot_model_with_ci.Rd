% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_model_with_ci.R
\name{plot_model_with_ci}
\alias{plot_model_with_ci}
\title{Plot Predicted Values}
\usage{
plot_model_with_ci(
  model,
  data,
  y_var,
  x_var,
  group_var = NULL,
  x_label = NULL,
  y_label = NULL,
  color_vals = NULL,
  color_labels = NULL
)
}
\arguments{
\item{model}{A statistical model of data.}

\item{y_var}{A string specifying the name of the outcome variable.}

\item{x_var}{A string specifying the name of the predictor variable.}

\item{group_var}{A string specifying the name of the grouping variable to split lines by.}

\item{dataframe}{A data frame containing the variables.}
}
\description{
This function creates a plot of predicted values with confidence intervals (CI), displaying the relationship between a predictor and outcome variable, separated by group.
}
\examples{
\dontrun{
plot_model_with_ci(model = brain_CD8_model_reduced, data = combined_data, y_var = "Brain CD8+ T Cell Counts", x_var = "Meninges CD8+ T Cell Counts", group_var = "Group")
}
}
\seealso{
\href{../doc/Plot_model_with_ci.html}{Plot Model With CI Vignette}
}
\keyword{boxplot}
\keyword{plotting}
\keyword{visualization}

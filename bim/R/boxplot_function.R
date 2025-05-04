#' @title Creates Boxplot
#' @name graph_boxplots
#' @description
#' This function can be used to create a multiple boxplots for one or more numerical y-variables and can be grouped by any categorical x-variables.
#' This function can create boxplots with the option to color fill, include raw data points, and facet by any variable.
#' @param data A data frame or tibble containing a dataset that will be used to make plots.
#' @param x_variable The name of the categorical variable to use on the x-axis, written as a string or as "text" (e.g., x_variable("Age")).
#' @param y_variables The names of the numerical variables to use on the y-axis, written as character vector or as "text" (e.g., y_variable(c("T_Cells", "B_Cells", "Microglia""))).
#' A separate plot will be created for each y-variable.
#' @param fill_variable (Optional) A variable to use for fill color and point color, written as a string or as "text." If NULL, then no fill will be used.
#' @param facet_variable (Optional) A variable to use for faceting the plots, written as a string or as "text." If NULL, then no fill will be used.
#' @param add_points (TRUE or FALSE). If TRUE, individual data points will be added to the boxplots. The default is TRUE. If FALSE, then no facet wrap will occur.
#' @param coef A numerical value that controls the length of the whiskers in the boxplot. This will be multiplied by the IQR to determine whisker size.
#' Use (coef = 1.5) for Tukey’s standard whiskers.
#' Use (coef = Inf) to extend whiskers to the min/max values.
#' Use (coef = 0) to remove whiskers.
#' @return One or more boxplots will be made for each of y-variables that were given.
#' @keywords visualization plotting boxplot
#' @examples
#' p <- graph_boxplots(
#'   data = data_frame,
#'   x_variable = "Genotype",
#'   y_variables = c("T_Cells", "B_Cells", "Microglia"),
#'   fill_variable = "Time_of_day",
#'   facet_variable = "Sex",
#'   add_points = TRUE
#' )
#' @seealso 
#'   [Boxplot Function Vignette](../doc/Boxplot_function.html)
#' @import ggplot2 stringr
#' @export

graph_boxplots <- function(data, x_variable, y_variables, fill_variable = NULL, facet_variable = NULL, add_points = TRUE, coef = 1.5) {

  plots <- list() # This stores the plots in a list

  for (y in y_variables) { # loops through the list of variables given
  y_label <- str_to_title(str_replace_all(y, "_", " ")) # cleans the up the y-variables for a title
  x_label <- str_to_title(str_replace_all(x_variable, "_", " ")) # cleans the up the x-variable for a title
  legend_title <- str_to_title(str_replace_all(fill_variable, "_", " ")) # Cleans the fill_variable for a figure legend title

  # this if and else statement for when you do have or don't have a fill_variable()
  if (!is.null(fill_variable)) { # only does this block of code if the fill_variable() is given
    plot_aes <- aes( # it will plot these aesthetics
      x = .data[[x_variable]], # X variable ; .data is a place holder for the data frame and [[]] gets a specific column
      y = .data[[y]], # y variable
      fill = .data[[fill_variable]], # fill_variable for the boxplots
      color = .data[[fill_variable]]) # fill_variable for the points
  }

  else { # only does this block of code if the fill_variable() is NOT given
    plot_aes <- aes(x = .data[[x_variable]],
                    y = .data[[y]])
  }

  # This creates the actual boxplot
  p <- ggplot(data, plot_aes) + # uses your data frame and plot aesthetics from above depending on if there is  fill_variable or not
    geom_boxplot(outlier.shape = NA, #creates boxplot and stops adding the identical black outliers
                 color = "black", # oulines the boxplot in black
                 coef = coef) +  # aplplies what the length of whiskers will be
    labs(title = str_to_title(y_label), #plot title
         x = x_label, # x-variable title
         y = y_label) + # y-variable title
    guides(color = "none") # helps to remove the extra legend it was creating

  # this if statement is for when you want or do not want to add points to graph
  if (add_points) {
    # this if and else statement is for when you have fill_variable ;
    # this if adds points if there is a fill_variable
    if (!is.null(fill_variable)) {
      p <- p + geom_point(position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.7), shape = 21, color = "black" )
    }

    # this adds plain points if no fill_variable is given
    else {
      p <- p + geom_point(position = position_jitter(width = 0.05, height = 0))
    }
  }

  # this if statement is for when you want to facet wrap by a specific variable
  if (!is.null(facet_variable)) {
    p <- p + facet_wrap(vars(.data[[facet_variable]])) #vars lets you pass the variable to facet_wrap()
  }

  # This is saving the plot to the plots list
  plots[[y]] <- p
  }

# this returns back the list of plots that were created
return(plots)
}






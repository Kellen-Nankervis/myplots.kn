#' Create a ggplot box plot
#' 
#' This function generates a ggplot box plot for a given set of values and optional categories.
#' 
#' @param values A numeric vector of data values.
#' @param categories (Optional) A factor vector specifying categories for grouping the values.
#' @param color The color of the box plot outlines (default is "black").
#' @param fill The color to fill the box plots (default is "lightblue").
#' @param title The title of the plot (default is "Box Plot").
#' @param value_lab The label for the y-axis (default is "Values").
#' @param cat_lab The label for the x-axis (default is "Categories").
#' @param horizontal Logical; if TRUE, the box plots are horizontal; if FALSE (default), they are vertical.
#' 
#' @return A ggplot object representing the box plot.
#' 
#' @examples
#' # Box plot without categories
#' gbox(c(1, 2, 3, 4, 5))
#' 
#' # Box plot with categories
#' gbox(c(1, 2, 3, 4, 5), categories = c("A", "A", "B", "B", "B"))
#' 
#' @import ggplot2
#' @export

gbox <- function(values, categories = NULL, color = "black", fill = "lightblue", title = "Box Plot", value_lab = "Values", cat_lab = "Categories", horizontal = FALSE) {
  require(ggplot2)
  if (is.null(categories)) {
    if (horizontal) {
      ggplot(data.frame(values), aes(y = values)) + 
        geom_boxplot(color = color, fill = fill) + 
        labs(title = title, y = ifelse(horizontal, value_lab, ""), x = ifelse(horizontal, "", value_lab)) + 
        theme_minimal() +
        coord_flip()
    } else {
      ggplot(data.frame(values), aes(y = values)) + 
        geom_boxplot(color = color, fill = fill) + 
        labs(title = title, y = value_lab) + 
        theme_minimal()
    }
  } else {
    if (horizontal) {
      ggplot(data.frame(values, categories), aes(x = categories, y = values, fill = categories)) + 
        geom_boxplot(color = color) + 
        labs(title = title, x = cat_lab, y = value_lab, fill = cat_lab) + 
        theme_minimal() + 
        coord_flip()
    } else {
      ggplot(data.frame(values, categories), aes(x = categories, y = values, fill = categories)) + 
        geom_boxplot(color = color) + 
        labs(title = title, x = cat_lab, y = value_lab, fill = cat_lab) + 
        theme_minimal()
    }
  }
}

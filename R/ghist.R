#' Create a ggplot histogram
#' 
#' This function creates a ggplot histogram of a numeric vector.
#' 
#' @param x A numeric vector for which the histogram will be created.
#' @param point_color The color of the points (default is "black").
#' @param line_color The color of the lines (default is "black").
#' @param fill_color The color of the fill (default is "lightblue").
#' @param title The title of the plot (default is "Histogram").
#' @param xlab The x-axis label (default is "X").
#' @param ylab The y-axis label (default is "Frequency").
#' @param bins The number of bins in the histogram (default is 0, which auto-computes bins).
#' 
#' @return A ggplot histogram object.
#' 
#' @examples
#' data(mtcars)
#' ghist(mtcars$mpg, bins = 0)
#' ghist(mtcars$mpg, bins = 3)
#' 
#' @import ggplot2
#' @export

ghist <- function(x, point_color = "black", line_color = "black", fill_color = "lightblue", title = "Histogram", xlab = "X", ylab = "Frequency", bins = 0) {
  require(ggplot2)
  if (bins == 0) {
    ggplot(data.frame(x), aes(x = x)) + 
      geom_histogram(color = line_color, fill = fill_color) + 
      labs(title = title, x = xlab, y = ylab) + 
      theme_minimal()
  } else {
    ggplot(data.frame(x), aes(x = x)) + 
      geom_histogram(color = line_color, fill = fill_color, bins = bins) + 
      labs(title = title, x = xlab, y = ylab) + 
      theme_minimal()
  }
}

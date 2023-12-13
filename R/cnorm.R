#' Check Normality of Data
#'
#' This function checks the normality of a given numeric vector using visual
#' inspection (histogram and Q-Q plot) and a statistical test (Shapiro-Wilk).
#'
#' @param x A numeric vector for which normality is to be checked.
#' @return A list containing the following elements:
#'   \itemize{
#'     \item \code{histogram}: A histogram of the data.
#'     \item \code{qq_plot}: A Q-Q plot of the data.
#'     \item \code{shapiro_test}: Results of the Shapiro-Wilk test.
#'   }
#' @examples
#' data <- rnorm(100)
#' cnorm(data)
#'
#' @export
cnorm <- function(x) {
  # Visual inspection
  par(mfrow = c(1, 2))  # Set up a 1x2 plotting grid

  # Histogram
  hist(x, main = "Histogram", col = "lightblue", border = "black")

  # Q-Q plot
  qqnorm(x)
  qqline(x)

  par(mfrow = c(1, 1))  # Reset plotting layout

  # Statistical test
  shapiro_test <- shapiro.test(x)


  # Check p-value
  p_value <- shapiro_test$p.value
  cat("Shapiro-Wilk Test p-value:", p_value, "\n")

  # Interpretation
  if (p_value < 0.05) {
    cat("Reject the null hypothesis: The data does not follow a normal distribution.\n")
  } else {
    cat("Cannot reject the null hypothesis: There is not enough evidence to conclude that the data does not follow a normal distribution.\n")
  }

  return(shapiro_test)
}

#####################################################################
#*copying the data set 100 times;
#' @export
#' @import formattable
#' @import mefa

replicate_data <- function(start, end, Before, After, data) {
  mu <- seq(start * 100, end * 100, by = (end - start))
  mu <- rep(mu, each = nrow(data))
  
  before <- data[[Before]] - mu / 100
  after <- data[[After]]
  
  mee_chua <- data.frame(mu = mu, before = before, after = after)
  return(mee_chua)
}
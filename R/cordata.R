################## Correlation and Effect Sizes ##############################
#' @export
#' @import stats
#' @import effsize
#' @import formattable
#' @import htmlwidgets

cordata <- function(Before, After, within = TRUE, data = NULL) {
  if (!is.null(data)) {
    Before <- data[[Before]]
    After <- data[[After]]
  }
  
  Correlation <- round(cor(Before, After), 2)
  
  if (within) {
    ef_pol <- round(cohen.d(After, Before, paired = TRUE)$estimate, 2)
    ef_std <- round(cohen.d(After, Before, paired = TRUE, pooled = FALSE)$estimate, 2)
  } else {
    ef_pol <- round(cohen.d(After, Before, paired = TRUE, within = FALSE)$estimate, 2)
    ef_std <- round(cohen.d(After, Before, paired = TRUE, pooled = FALSE, within = FALSE)$estimate, 2)
  }
  
  cor_table <- cbind(Correlation, ef_pol, ef_std)
  second <- as.data.frame(cor_table)
  names(second) <- c("Correlation", "Effect size (pooled sd)", "Effect size (based on treatment sd)")
  
  formattable_table <- formattable(second,
                                   align = c("l", "c", "r"),
                                   list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")))
  )
  
  html_widget <- as.htmlwidget(formattable_table)
  
  # Öffne temporäre Dateien nur im interaktiven Modus
  if (interactive()) {
    temp_file <- tempfile(fileext = ".html")
    on.exit(unlink(temp_file), add = TRUE)
    htmlwidgets::saveWidget(html_widget, temp_file, selfcontained = TRUE)
    viewer <- getOption("viewer")
    if (!is.null(viewer)) {
      viewer(temp_file)
    } else {
      utils::browseURL(temp_file)
    }
  }
  
  return(second)
}
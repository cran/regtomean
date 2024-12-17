########################################################################################
# Print the summary of each model
#' @export
#' @import graphics
#' @import sjPlot
#' @import sjmisc

meechua_plot <- function(models = NULL, env = regtomean_env) {
  # Modelle aus der Umgebung abrufen, falls sie nicht direkt übergeben wurden
  if (is.null(models)) {
    if (exists("models", envir = env)) {
      models <- get("models", envir = env)
    } else {
      stop("No models found. Please provide a list of linear models or ensure 'models' exists in the specified environment.")
    }
  }
  
  # Überprüfen, ob models eine Liste von lm-Objekten ist
  if (!is.list(models) || !all(sapply(models, inherits, "lm"))) {
    stop("Input must be a list of linear models (lm).")
  }
  
  # Plots nur im interaktiven Modus erstellen
  if (interactive()) {
    lapply(models, plot)
  } else {
    message("Plots are only generated in an interactive session.")
  }
  
  return(invisible(NULL))
}
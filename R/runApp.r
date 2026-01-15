# R/runApp.R
#' Launch MS2 Database Builder Shiny Application
#'
#' This function launches the Shiny application for building MS2 databases
#' from compound lists and MS2 data files.
#'
#' @param launch.browser Logical, should the application be opened in the
#'   default browser? Defaults to TRUE in interactive sessions.
#' @param port The TCP port that the application should listen on. Defaults
#'   to a random available port.
#' @param host The IPv4 address that the application should listen on.
#'   Defaults to "127.0.0.1".
#'
#' @return This function does not return anything; the Shiny app is run
#'   as a side effect.
#'
#' @examples
#' \dontrun{
#' # Launch the application
#' runMS2DatabaseBuilder()
#'
#' # Launch on a specific port
#' runMS2DatabaseBuilder(port = 4242)
#' }
#'
#' @export
runMS2DatabaseBuilder <- function(launch.browser = NULL,
                                   port = getOption("shiny.port"),
                                   host = "127.0.0.1") {
  
  # Check for required packages
  required_packages <- c("shiny", "DT", "plotly", "shinyjs", "mzR")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(
      "The following required packages are not installed:\n",
      paste("  -", missing_packages, collapse = "\n"),
      "\n\nPlease install them using:\n",
      "install.packages(c('", 
      paste(setdiff(missing_packages, "mzR"), collapse = "', '"), 
      "'))\n",
      if ("mzR" %in% missing_packages) {
        "if (!requireNamespace('BiocManager', quietly = TRUE))\n  install.packages('BiocManager')\nBiocManager::install('mzR')"
      }
    )
  }
  
  # Get app directory
  app_dir <- system.file("shinyapp", package = "MS2DatabaseBuilder")
  
  if (app_dir == "") {
    stop("Could not find the Shiny application directory. ",
         "Try re-installing `MS2DatabaseBuilder`.")
  }
  
  # Set launch.browser if not specified
  if (is.null(launch.browser)) {
    launch.browser <- getOption("shiny.launch.browser", interactive())
  }
  
  # Print welcome message
  message("╔══════════════════════════════════════════════════════╗")
  message("║        MS2 Database Builder v1.0                    ║")
  message("║        Launching Shiny Application...               ║")
  message("╚══════════════════════════════════════════════════════╝")
  message("")
  message("📁 Expected input files:")
  message("  • Compound list: CSV file with 'name', 'mz', 'rt' columns")
  message("  • MS2 data: mzML or mzXML format")
  message("")
  message("⚙️  Features:")
  message("  • EIC extraction and visualization")
  message("  • MS2 spectrum matching and scoring")
  message("  • Multiple export formats (CSV, RDS, MSP)")
  message("")
  
  # Run the app
  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    port = port,
    host = host
  )
}
# MS2 Database Builder

An R package providing a Shiny application for building MS2 databases from compound lists and MS2 data files.

## Installation

### From GitHub (development version)

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install the package
devtools::install_github("Zhujiamin97/MS2DatabaseBuilder")

# Run shiny
MS2DatabaseBuilder::runMS2DatabaseBuilder()


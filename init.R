# init.R
#
# Example R code to install packages if not already installed
#
my_packages = c("RSQLite", "RMySQL", "DBI", "ggplot2", "dplyr", "tidyr", "Shiny", "plotly", "knitr", "CRAN", "Caret", "RColorBrewer", "shinyWidgets", "shinydashboard", "shinythemes", "shinyjs", "lubridate", "tibble", "sodium", "EMSC", "readr", "mdatools", "corrplot", "MASS", "ROCR", "qrcode", "shinyFiles", "fs", "shinyAce", "mailR", "rJava", "shinylogs","googledrive","DT","blastula","cairo","firebase","shinyalert","shinyWidgets","data.table")
install_if_missing = function(p) {
if (p %in% rownames(installed.packages()) == FALSE) {
install.packages(p)
}
}
invisible(sapply(my_packages, install_if_missing))

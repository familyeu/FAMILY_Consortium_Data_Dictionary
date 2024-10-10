FAMILY Consortium Data Dictionary 


-Overview-
The FAMILY Consortium Data Dictionary is a web application designed to provide researchers within the FAMILY consortium with access to metadata information related to the data collected. It serves as a centralized platform for checking sample sizes and metadata within each familial high risk cohort belonging to the consortium.


-Prerequisites-
Run the following code to make sure you have all the packages needed to use the app:

required_packages <- c(
  "shiny", "shinyWidgets", "shinyTree", "shinydashboard", "shinydashboardPlus",
  "openxlsx", "DT", "stringr", "tidyverse", "conflicted", "plotly",
  "dplyr", "patchwork", "scales", "RColorBrewer", "formattable"
)
installed_packages <- installed.packages()
packages_to_install <- required_packages[!(required_packages %in% installed_packages)]
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, dependencies = TRUE)
}
invisible(lapply(required_packages, library, character.only = TRUE))

-Roadmap-
Future development plans include:
-Enhancing metadata details.
-Improving methods for selecting sample sizes.

-Reporting issues or support-
To report bugs, provide feedback or ask for support, please email family@erasmusmc.nl.
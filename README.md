<p align="center">
  <img src="https://mmortuno.shinyapps.io/family_shinny_app_final/_w_2f889e4342b4466589d51fdfb4c87b98/data_dict_logo.png" alt="FAMILY Project Logo" height="100">
</p>

<h2 align="center">FAMILY Consortium Data Dictionary</h2>

<p align="center">
  🧠 Centralized metadata and sample size overview for familial high-risk cohorts participating in the consortium.
</p>

---

## 📘 Overview

This repository hosts the **FAMILY Consortium Data Dictionary**, a Shiny-based web application designed to support research within the FAMILY project.

The tool provides:

- 🗂️ **Central access to metadata** across all FAMILY high-risk cohorts  
- 🔍 **Sample size overviews** for participating sites  
- 📊 **Interactive UI** for browsing variable groups and cohort-level data  

> Part of the **FAMILY project**, funded by the European Union’s Horizon Europe programme.  
> Learn more: [family-project.eu](https://family-project.eu)

---

## 🚀 Getting Started

### 🔧 Requirements

Before launching the app, make sure you have R and the following packages installed:

```r
# Run in R
packages <- c(
  "shiny", "shinyWidgets", "shinyTree", "shinydashboard", "shinydashboardPlus",
  "openxlsx", "DT", "stringr", "tidyverse", "conflicted", "plotly",
  "dplyr", "patchwork", "scales", "RColorBrewer", "formattable"
)

install.packages(setdiff(packages, installed.packages()[, "Package"]))

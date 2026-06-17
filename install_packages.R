# =========================
# GAM Key Driver Analysis
# Package Requirements
# =========================

# Core modelling
install.packages("mgcv")

# Data manipulation
install.packages(c(
  "dplyr",
  "tidyr",
  "forcats"
))

# Data import
install.packages("readxl")

# Visualisation
install.packages("ggplot2")

# Model diagnostics + explanation
install.packages(c(
  "gratia",
  "car",
  "gam.hp"
))

# Reporting
install.packages(c(
  "rmarkdown",
  "pagedown"
))

# File path management
install.packages("here")

# Optional but used in some workflows
install.packages("tibble")
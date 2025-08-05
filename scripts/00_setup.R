# 00_setup.R
# Package installation script for SPRUCE Protist Traits Analysis

# Install and load required packages

# CRAN packages
packages <- c(
  "tidyr", "dplyr", "ggplot2", "cluster", "plyr", "ggsignif", "psych", 
  "fmsb", "RColorBrewer", "scales", "mclust", "moments", "plotly", 
  "lme4", "ggstatsplot", "tidyverse", "scam", "extrafont", "cowplot", 
  "GGally", "ggpubr", "readxl", "remotes","qiime2R", "phyloseq", "viridis", 
  "car", "rcompanion", "FSA", "vegan", "microViz", "microbiome", "pals", "ggh4x", 
  "data.table", "stringr", "stargazer", "DDoutlier", "ggthemes", "devtools",
  "gtsummary", "ggpmisc", "FD", "fundiversity", "progress", 
  "lavaan", "semTools", "semPlot"
)

# Install missing packages
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# GitHub packages
if (!require(devtools)) install.packages("devtools")

if (!require(ecoflux)) {
  remotes::install_github("jpshanno/ecoflux")
  library(ecoflux)
}

if (!require(ggcorrplot)) {
  devtools::install_github("kassambara/ggcorrplot")
  library(ggcorrplot)
}

if (!require(ggradar)) {
  devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
  library(ggradar)
}

# Load fonts (Windows only, as specified in original code)
if (.Platform$OS.type == "windows") {
  extrafont::loadfonts(device = "win")
}
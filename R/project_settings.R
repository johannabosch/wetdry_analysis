library(RMark)
library(dplyr)
library(tidyverse)
library(here)
library(knitr)
library(grid)
library(patchwork)
library(ggplot2)
library(plotly)
library(flexdashboard)
library(htmlwidgets)
library(readr)
library(lubridate)

#packages for clipping deg files
library(pryr) #tracks memory usage
library(profvis)
library(purrr)
library(furrr)
library(future)

options(scipen = 999, digits=4)

# Set up folders
(proj.dir <- file.path("C:/Users/BoschJ/Desktop/wet-dry_analysis"))
(data.dir <- file.path(proj.dir, "data"))
(R.dir <- file.path(proj.dir, "R"))
(plot.dir <- file.path(proj.dir, "plots"))

(origin.dir <- file.path(data.dir,"DEG"))

(deg.dir <- file.path(data.dir, "DEG_clean"))

(output.dir <- file.path(data.dir, "clipped"))

(log_file <- file.path(output.dir, "clip_summary.txt"))

(downsampled.dir <- file.path(data.dir, "downsampled"))


sessionInfo()

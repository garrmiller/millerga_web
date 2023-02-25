#### Compiler
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(survminer)
library(survival)

data_dir <- './data'
file_name <- 'lung.csv'
dd_raw <- read.csv(file.path(data_dir, file_name))

(dim(dd_raw))

source("data_prep.R")
kToday <- Sys.Date()
report_title <- paste0(paste("lung_survival_example",
                      gsub("\\-", "_", kToday),
                      sep = "_"), '.pdf')
rmarkdown::render("lung_survival.Rmd",
                  output_file = report_title)

library(tidyverse)

library(here)

raw_files <- list.files(path = 'projects/synpufs/data/raw/')[1:2]


load_files <- function(files, dir) {
  # mapply(files, function(file){
  for (file in files) {
  
  x <- read.csv(file = file.path(dir, file))
  assign(file, x)
  rm(x)
  

  }
}

load_files(raw_files, dir  = 'projects/synpufs/data/raw/')


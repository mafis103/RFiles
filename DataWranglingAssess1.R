library(dslabs)
library(tidyverse)    # includes readr
library(readxl)


url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat <- read_csv(url,col_names=FALSE)
ncol(dat)
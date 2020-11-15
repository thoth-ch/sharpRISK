# Title     : TODO
# Objective : TODO
# Created by: joao
# Created on: 15/11/2020

# Load packages
library(tidyverse)
library(lubridate)
library(reticulate)

# Set python environment
use_condaenv("/home/joao/anaconda3/envs/Rpython")
source_python("rpy_prototype.py")

risks_rpy <- unlist(risks_py4)
risks_rpy


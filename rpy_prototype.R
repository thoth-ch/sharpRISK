# Title     : TODO
# Objective : TODO
# Created by: joao
# Created on: 15/11/2020

# Load packages and set environments
library(tidyverse)
library(lubridate)
library(reticulate)
use_condaenv("/home/joao/anaconda3/envs/Rpython")


source_python("rpy_prototype.py")
action_responsibles <- unlist(action_responsibles_py) %>%
  as_tibble_col(column_name = "Responsible")

action_responsibles %>%
  mutate(Responsible = fct_infreq(Responsible)) %>%
  ggplot(aes(x = Responsible)) +
  geom_bar(width = 0.5) +
  theme_light() +
  labs(title = "Count of actions by responsible")
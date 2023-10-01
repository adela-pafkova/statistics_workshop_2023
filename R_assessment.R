# EES 4th year fieldcourse, University of Edinburgh
# Statistics assessment
# Adela Pafkova s1978469

#LIBRARIES ----
library(tidyverse)
library(ggplot2)

#LOAD DATA ----
inga <- read.csv('data/Inga_traits.csv')

# Exercise 1: histograms and  normality (100 marks)
## a) Leaf area histogram
hist(inga$Leaf_Area)

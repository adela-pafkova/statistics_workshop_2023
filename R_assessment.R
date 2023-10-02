# EES 4th year fieldcourse, University of Edinburgh
# Statistics assessment
# Adela Pafkova s1978469

#LIBRARIES ----
library(tidyverse)
library(ggplot2)
#install.packages("colourpicker")
library(colourpicker)

#LOAD DATA ----
inga <- read.csv('data/Inga_traits.csv')

# Exercise 1: histograms and  normality (100 marks) ----
## a) Leaf area histogram
hist(inga$Leaf_Area, breaks=20,col="grey",xlab="Soil pH",main="")
abline(v=median(inga$Leaf_Area)) 
abline(v=mean(inga$Leaf_Area)) 

# Make it nicer in ggplot
ggplot(inga, aes(x=Leaf_Area)) + 
  geom_histogram(binwidth=4, colour = 'darkseagreen4', fill = 'darkseagreen3') +
  xlab('Leaf area') +
  ylab('Frequency') +
  geom_vline(aes(xintercept=mean(Leaf_Area)),
           color="darkslategray", linetype="dashed", size=1) +
  annotate("text", x=45, y=3.5, label="mean", angle=90, size =3) +
  theme_bw()

#Answer: Leaf size does not appear to be normally distributed (maybe poisson?),
# there is an outlier

# b) Log transform leaf area and plot
inga$log_leaf_A <- log(inga$Leaf_Area)

ggplot(inga, aes(x=log_leaf_A)) + 
  geom_histogram(colour = 'darkseagreen4', fill = 'darkseagreen3') +
  xlab('Log leaf area') +
  ylab('Frequency') +
  geom_vline(aes(xintercept=mean(log_leaf_A)),
             color="darkslategray", linetype="dashed", size=1) +
  annotate("text", x=3.62, y=3.5, label="mean", angle=90, size =3) +
  theme_bw()
# c) Describe what leaf sizes across trees in this region are like to a non-scientist?

# Exercise 2: Box plots and ANOVA (100 marks) ----

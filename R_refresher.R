# R refresher
# Download Barbie theme
install.packages("rstudioapi")
library(rstudioapi)
barbie_light <- "https://raw.githubusercontent.com/emhogg/barbie_r_studio_themes/main/Barbie_Light.rstheme"
rstudioapi::addTheme(barbie_light, apply = TRUE)

# Basic ANOVA
A <- c(115,120,135,155,160,170,175,200,205,220)
B <- c(115,145,75,60,95,95,170,105,130,120) 
C <- c(155,75,110,145,85,105,140,75,140,110) 
height <- c(A,B,C)
fertiliser <- c(rep("A",length(A)),rep("B",length(B)),rep("C",length(C))) 
alldata <- data.frame(fertiliser,height) 

#Construct a model and do anova

height_lm <- lm(height~fertiliser, data=alldata)
anova(height_lm)
summary(height_lm)

# Test assumptions 
height_resids <- resid(height_lm) 
shapiro.test(height_resids) # Test normality of residuals
bartlett.test(height~fertiliser,data=alldata) # Test equality of variances

# Plot the model
plot(height_lm)
  
(height_aov <- aov(height~fertiliser,data=alldata))
TukeyHSD(height_aov) 

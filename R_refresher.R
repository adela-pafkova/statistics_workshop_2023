# R refresher
# Download Barbie theme
install.packages("rstudioapi")
library(rstudioapi)
barbie_light <- "https://raw.githubusercontent.com/emhogg/barbie_r_studio_themes/main/Barbie_Light.rstheme"
rstudioapi::addTheme(barbie_light, apply = TRUE)
# LIBRARIES----
library(readxl)

# EXERCISE 1 ----
# EXCERCISE 2 -----
## load data
soils <- read_excel("data/Peru_Soil_Data(1) (1).xlsx")
## quick summary
dim(soils) 
names(soils) 
soils$Soil_pH 
soils[,c(4,7)] 
head(soils) 
summary(soils) 

## histograms
hist(soils$Soil_pH)
hist(soils$Soil_pH,breaks=10) 
hist(soils$Soil_pH,breaks=10,col="grey") 
hist(soils$Soil_pH,breaks=10,col="grey",xlab="Soil pH",main="")
abline(v=median(soils$Soil_pH)) 
abline(v=mean(soils$Soil_pH)) 

## plot diff relationships
boxplot(Soil_pH~Habitat,data=soils) 
boxplot(Potassium~Habitat,data=soils) 

## anovas
lm_pH <- lm(Soil_pH~Habitat,data=soils) 
anova(lm_pH) 
lm_K <- lm(Potassium~Habitat,data=soils) 
anova(lm_K) 

## check assumptions
lm_pH_resids <- resid(lm_pH) 
shapiro.test(lm_pH_resids) 
bartlett.test(Soil_pH ~Habitat,data=soils) 
lm_K_resids <- resid(lm_K) 
shapiro.test(lm_K_resids) 
bartlett.test(Potassium ~Habitat,data=soils) #Not all groups have the same variance

plot(lm_pH) 
plot(lm_K) 

### boxplots to examine why assumptions are not met
hist(soils$Soil_pH,breaks=10) 
hist(soils$Potassium,breaks=10) 
boxplot(Soil_pH~Habitat,data=soils) 
boxplot(Potassium~Habitat,data=soils) 

##Log transform data 
soils$log_K <- log(soils$Potassium)
lm_log_K <- lm(log_K ~Habitat,data=soils) 
anova(lm_log_K) 

lm_log_K_resids <- resid(lm_log_K) 
shapiro.test(lm_log_K_resids) 
bartlett.test(log_K ~Habitat,data=soils) 
plot(lm_log_K) #scale/location is now nearly perfectly horizontal

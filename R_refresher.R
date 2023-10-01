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
plot(Soil_pH~Habitat,data=soils) 
plot(Potassium~Habitat,data=soils) 

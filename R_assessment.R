# EES 4th year fieldcourse, University of Edinburgh
# Statistics assessment
# Adela Pafkova s1978469

### REMINDER - make your own ggplot format ###

#LIBRARIES ----
library(tidyverse)
library(ggplot2)
#install.packages("colourpicker")
library(colourpicker)

#LOAD DATA ----
inga <- read.csv('data/Inga_traits.csv', stringsAsFactors=T, row.names=1)
## Check data
head(inga)
dim(inga) 
names(inga)
summary(inga) 

#EXCERCISE 1: histograms and  normality (100 marks) ----
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

mean(inga$Leaf_Area)
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
## ?? Mostly between like 10 and 100, mean at 47.33173

#EXCERCISE 2: Box plots and ANOVA (100 marks) ----

# a) Boxplot leaf P vs habitat
boxplot(P_Leaf~Habitat,data=inga) 

# b) ANOVA
lm_P <- lm(P_Leaf~Habitat, data = inga)
anova(lm_P)
anova(lm_P) %>%
  as.data.frame() %>%
  write.csv(file = "Phosphorus~Habitat_anova_table.csv")

# c) Validate model. Assumptions?
lm_P_resids <- resid(lm_P) 
shapiro.test(lm_P_resids) # checks out
bartlett.test(P_Leaf ~Habitat,data=inga) # nope (p < 0.05); variances are not equal across samples

plot(lm_P) # not great, esp. scale/location

# d) Improve model: log transform?

lm_log_P <- lm(log_leaf_A~Habitat, data = inga)
anova(lm_log_P) %>%
  as.data.frame() %>%
  write.csv(file = "Log_transformed_P~Habitat_anova_table.csv")

plot(lm_log_P) #looks better but worse results? p is large
lm_log_P_resids <- resid(lm_log_P)
shapiro.test(lm_log_P_resids) # checks out
3
bartlett.test(log_leaf_A~Habitat,data=inga) # checks out

# Tukeys test to see what is driving significance ( what groups are significantly different)
P_Leaf_aov <- aov(P_Leaf~Habitat,data=inga)
Tukey <- TukeyHSD(P_Leaf_aov)

#generalist and floodplain are nearly but not significantly different, 0.06
# upland and floodplain are significantly different, 0.0012
# upland and generalist are very much not significantly different, 0.78


#EXERCISE 3: Multiple explanatorz variables (100 marks)

# a) Plot P and C as y and x respectively, differentiate symbols for diff habitats, fit trendline

#preliminary plot
plot(P_Leaf ~ C_Leaf, data = inga) # looks like an intercept only relationship

ggplot(inga, aes(x=C_Leaf,y=P_Leaf))
inga$Habitat <-as.factor(inga$Habitat)
floodplain <- filter(inga, Habitat=="floodplain")
generalist <- filter(inga, Habitat=="generalist")
upland <- filter(inga, Habitat=="upland")

ggplot(inga, aes(x=C_Leaf,y=P_Leaf))+
  geom_point(aes(col=Habitat, shape=Habitat))+
  geom_smooth(method="lm",se=F,data=floodplain,aes(col=Habitat))+
  geom_smooth(method="lm",se=F,data=generalist,aes(col=Habitat))+
  geom_smooth(method="lm",se=F,data=upland,aes(col=Habitat))+
  labs(x="Leaf Carbon Concentration (mg/g)", y="Leaf Phosphorous Concentration (mg/g)", title="Relationship of Carbon and Phosphorus in Leaves")+
  theme(plot.title = element_text(hjust=0.5))+
  theme_classic()


inga <- read.csv("C:/Users/User/Downloads/Inga_traits.csv",stringsAsFactors=T, row.names=1)
install.packages("lme4")
library(lme4)
View(inga)

install.packages("MuMIn")
library(MuMIn)

install.packages("ggplot2")
library("ggplot2")
install.packages("dplyr")
library("dplyr")

install.packages("tidyverse")
library("tidyverse")



#Question One 
hist(inga$Leaf_Area)
hist(inga$Leaf_Area,breaks=10,col="green",xlab=expression("Leaf Area (cm^2)"),main="Histogram of Leaf Area")
abline(v=mean(inga$Leaf_Area))
mean(inga$Leaf_Area)

log_L <- log(inga$Leaf_Area)
hist(inga$log_L,breaks=10,col="red",xlab="Log Transformed Leaf Area",main="Histogram of Log Transformed Leaf Area")
abline(v=mean(inga$log_L))

range(log_L)
mean(log_L)

AH_model <- lm(Leaf_Area~SuperHabitat, data=inga)
plot(AH_model)

anova(AH_model)
#Question Two 
 
#part a) 
boxplot(P_Leaf~Habitat ,data=inga,xlab= "Habitat Types", ylab="Leaf Phosphorous Concentration (mg/g)", cex.lab=0.75)
title(main="Leaf Phosphorous Concentration vs Habitat Types" , cex.main=0.75)

#part b), testing to see which model is better
P_model <- lm((P_Leaf)~Habitat,data=inga)
Plog_model <- lm(log(P_Leaf)~Habitat,data=inga)
AICc(P_model, Plog_model)
summary.aov(P_model)
summary(P_model)
# base linear model with no log is best, AICc, -117.17, log model is -5.6 


#part c, checking anova assumptions test 

P_resids <- resid(P_model)
# need residuals (?) to do this 
shapiro.test(P_resids)
#null hypothesis accepted and population is normally distributed 
bartlett.test(P_Leaf~Habitat,data=inga)
# null hypothesis is rejected, not all groups have the same variance, p=0.005795 
plot(P_resids)

plot(P_model) # first residuals vs fitted graph, the graph goes of on a line in the end 




# Tukeys test to see what is driving significance ( what groups are significantly different)
P_Leaf_aov <- aov(P_Leaf~Habitat,data=inga)
TukeyHSD(P_Leaf_aov)

#generalist and floodplain are nearly but not significantly different, 0.06
# upland and floodplain are significantly different, 0.0012
# upland and generalist are very much not significantly different, 0.78

inga2_no <- inga[complete.cases(inga[,("P_Leaf")]),]
View(inga2_no)




#part d, making the model better
# remove tomentosa and cinnamomea as a strategy?
Pg_model <- lm(log(P_Leaf)~Habitat, data=inga)
plot(Pg_model) # first two looking good

#diagnostic tests
Pg_resids <- resid(Pg_model)
shapiro.test(Pg_resids)# p value 0.6036 null hypothesis accepted pop normally distributed
bartlett.test(log(P_Leaf)~Habitat,data=inga)# p value 0.1232 null hypothesis accepted variances is similar

# logs should reduce skewedness
summary(Pg_model)
summary.aov(Pg_model)

Pg_aov <- aov(log(P_Leaf)~Habitat,data=inga)
TukeyHSD(Pg_aov)

Pg_model <- lm(log(P_Leaf)~Habitat, data=inga2_no)
P_model <- lm((P_Leaf)~Habitat,data=inga2_no)



AICc(P_model, Pg_model)

#Question Three 

#3 a) 
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

#3b
#going to fuse upland and generalist together as habitats? THis is becaue upland and generalist have higher p value so less different 
#Fusing upland and generalist together 

# creating the new column 
(inga <- inga %>% 
  mutate(SuperHabitat = case_when(Habitat == "floodplain" ~ 'NeedFlood',
                                  TRUE ~ "NoFlood")))
View(inga)

#viewing the new category

inga$SuperHabitat <-as.factor(inga$SuperHabitat)
Flood<- filter(inga, SuperHabitat=="NeedFlood")
NoFlood <- filter(inga, SuperHabitat=="NoFlood")

ggplot(inga, aes(x=C_Leaf,y=P_Leaf))+
  geom_point(aes(col=SuperHabitat, shape=SuperHabitat))+
  geom_smooth(method="lm",se=F,data=Flood,aes(col=SuperHabitat))+
  geom_smooth(method="lm",se=F,data=NoFlood,aes(col=SuperHabitat))+
  labs(x="Leaf Carbon Concentration", y="Leaf Phosphorous Concentration")

plot (C_Leaf~SuperHabitat, data=inga)



 # should this be a binomial glm - should not be, binomial is not the dependent 
PC_model<-lm(P_Leaf~SuperHabitat + C_Leaf,data=inga)
PCM_model <-lm(P_Leaf ~SuperHabitat * C_Leaf, data=inga)
#cracked out the log alex did bartlett and shaprio and they were fine with logged data remember to do your model tests
# using a multiply term as i think floodplain affects nutrients cycling so would affect carbon use a ref

inga$C_Leaf

#tests for the lm
PCM_resids <- resid(PCM_model)
shapiro.test(PCM_resids) # fine, 0.0003528
HCinteraction <- interaction(inga$SuperHabitat, inga$C_Leaf )
bartlett.test(P_Leaf ~ C_Leaf , data=inga)
plot(PCM_model)
#leverage in the last plot? 
# do the log on the last plot 

plot(PC_model)
summary(PCM_model)
anova(PCM_model)
summary(PC_model)
AICc(PC_model,PCM_model)

summary.aov(PCM_model)
TukeyHSD(PCM_model)




#3c
#tomentosa falls in the lines of Cook's distance in residuals vs leverage plot, meaning it has leverage
#removing tomentosa ( check on Saturday if any of the other ones fall in here)
ingaNO <- inga[-35,]
View(ingaNO)

PCNO_model<-lm(P_Leaf~SuperHabitat + C_Leaf,data=ingaNO)
PCMNO_model<-lm(P_Leaf~SuperHabitat *C_Leaf,data=ingaNO)

PCNO_resids <- resid(PCNO_model)
shapiro.test(PCNO_resids)#fine 0.03249

plot(PCMNO_model)
# residuals vs fitted plot is still not looking great so doing the log transform 
# log trasform does not do anything - looks like a light negative parabola for the +
# if using the  * interaction term the log does fix it 
#deciding to use the * interaction term 

PCNOQ_model<-lm(P_Leaf~ SuperHabitat + C_Leaf + I(C_Leaf^2) ,data=ingaNO)
plot(PCNOQ_model)
PCMNOL_model<-lm(log(P_Leaf)~ SuperHabitat *C_Leaf,data=ingaNO)

plot(PCMNOL_model)
anova(PCMNOL_model)

#3d testing stuff

boxplot(C_Leaf ~ SuperHabitat, data=inga) # visually about more carbon in noflood but
# carbon difference may only be significant when interacting with habitat
boxplot(P_Leaf ~ SuperHabitat, data=inga) #phosphorus def higher in the need flood

inga$SuperHabitat <-as.factor(ingaNO$SuperHabitat)
NeedFlood <- filter(ingaNO, SuperHabitat=="NeedFlood")
NoFlood <- filter(ingaNO, SuperHabitat=="NoFlood")

testmod <- (lm(P_Leaf ~ C_Leaf , data=NeedFlood))
plot(testmod)




#Question Four
#4 a) - does affect the leaf expansion 
bimodE <- glm(Mevalonic_Acid~Expansion, data=inga4_no,family=binomial)
summary(bimodE)
plot(Mevalonic_Acid,data=inga)
inga$Mevalonic_Acid <-as.factor(inga$Mevalonic_Acid)
ggplot(data=inga, aes(x=Mevalonic_Acid,y=Expansion))
bimodT<- glm(Mevalonic_Acid~Trichome_Density, data=inga4_no,family=binomial)
summary(bimodT)

#4b)

testmod <- lm(Expansion ~ log(Trichome_Density), data=inga)
summary(testmod)

inga4_no <-filter(inga,!is.na(inga$Expansion))
inga4_no <-filter(inga,!is.na(inga$Trichome_Density))

inga4_no <- inga[complete.cases(inga[,c("Expansion","Trichome_Density")]),]
View(inga4_no)

dualmod <- glm(Mevalonic_Acid~Expansion + Trichome_Density, data=inga4_no, family=binomial)
dualmod2 <- glm(Mevalonic_Acid~ Expansion * Trichome_Density , data=inga4_no, family=binomial)
AICc(dualmod, dualmod2) # dualmod lower AICc, think the right one is + anyway as I want to see how Mevalonic Acid react to each not them interacting together
# found paper that says that trichome density effects leaf expansion so going to use a multiplier?
# not using mixed effect no significant result and doesn't explain variance well
plot(dualmod2)
plot(dualmod)
summary(dualmod2)# very low overdispersion 22.717/22, not sig
summary(dualmod)#no overdispersion, 22.865/23, sig
anova(dualmod, test ="Chisq")

dualmod_null <- glm(Mevalonic_Acid~1,data=inga4_no,family=binomial)
AICc(dualmod,dualmod_null,bimodE,bimodT,dualmod2)# AICc dualmod 29.95639, dualmod_null,40.39670, bimodE 35.0495,bimodT 34.52118, 32.62190

plot(Mevalonic_Acid~Expansion)

# making model better - not making the diagnostic plot 
ingaNO2 <- inga[-c(25,7,14),]# multiplier
ingaNO2 <-inga[-c(25,7),]# + 
View(ingaNO2)
dualmodO <- glm(Mevalonic_Acid~ Expansion + Trichome_Density , data=ingaNO2, family=binomial)

dualmod2O <- glm(Mevalonic_Acid~ Expansion * Trichome_Density , data=ingaNO2, family=binomial)
plot(dualmodO)

summary(dualmod2L)
summary(dualmodO)# more significant remove outliers, 0.0251 p value
anova(dualmod)
anova(d)
# the + glm is the correct model 

#logging?
dualmodOL <- glm(Mevalonic_Acid~ log(Expansion) + Trichome_Density , data=ingaNO2, family=binomial)
plot(dualmodOL)# doesn't fix it a lot 

#4 c)

# don't do this need to use the equation alex sent 
NEw <-(data= data.frame ( Expansion= 32.16, Trichome_Density=0.2))
predict(dualmod2,newdata=NEw,type="response")

predict(dualmod)
# check with everyone but i think here this is right! Predicted a 1.3% probability of a 1


#4 e)
#should this be data = ingaNO
# concept, make a variable that is the interaction(?) to deal with the interaction


boxplot(Expansion~ Mevalonic_Acid, data=inga)

dualmod <-glm(Mevalonic_Acid~Expansion + Trichome_Density , data=inga, family=binomial)
summary(dualmod)

[Expansion$inga]

Dline = function(inga){-3.9669 +0.1064*inga$Expansion-0.1528*inga$Trichome_Density}

worm_colour <- c("#8BB26C","#E16B7B")

ggplot(data=inga, aes(x=Expansion,y=Mevalonic_Acid, color=Trichome_Density))+
  geom_point() +
  stat_smooth( formula= y ~ x, method="glm", 
              method.args = list(family = "binomial"),se=FALSE)+
  scale_colour_gradient(low="green",high="red",name="Trichome Density (hairs/cm^2)")+
  labs(x="Leaf Expansion Rate (%/day)", y="Mevalonic Acid Presence", 
       title="Expansion and Trichome Density Influences on Mevalonic Acid")+
  theme_classic()

# looking at means

Present <- filter(inga, Mevalonic_Acid=="1")
Not <-filter(inga, Mevalonic_Acid=="0")

View(Present)
View(Not)

mean(Present$Expansion)#43.30605 overall mean?,  actual 49.83667
Not <-Not[-c(11,12),]
mean(Not$Expansion)#37.70837 
  


stat_smooth(formula=(Mevalonic_Acid~Expansion+Trichome_Density), method="glm", method.args = list(family = "binomial"),se=F,data=inga )+
    
formula=(Mevalonic_Acid~Expansion+Trichome_Density)
stat_smooth(method="glm", method.args = list(family = "binomial"),se=F,data=inga )



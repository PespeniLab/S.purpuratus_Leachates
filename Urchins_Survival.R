########################################################################
#############Survival Data Analysis####################################

library(ggbeeswarm)
library(lmerTest)
library(ggplot2)
library(car)
library(rlang)
library(ggpubr)
library(nlme)
library(Matrix)
library(dplyr)
library(knitr)
library(lme4)
library(ggthemes)
library(patchwork)
library(cowplot)
library(ggforce)
library(plyr)
library(Rmisc)
library(lattice)
library(multcomp)

#Setting up figure exportation 
png("urchin_survival.png", height=8, width=9.00, units="in", res=300)

#For uniform figure graphics
xticks <- c("Control", "0.1mg/L", "0.5mg/L", "1.0mg/L", "5.0mg/L")

#-------------------------------------------------------------------
#-------------------------------------------------------------------
##Larval urchin survival after 96 hours of DEHP exposure

#Read in the data
data <- read.table("DEHP_Survival.csv", header= TRUE, sep=",")
chemical <- data
chemical

#Setting replicate as a factor to allow us to make it a random effect
chemical$Replicate <- as.factor(chemical$Replicate)
#Setting Treatment as factor
chemical$Treatment <- as.factor(chemical$Treatment)

#Forcing R to compare treatments to the control
chemicals <- within(chemical, Treatment <- relevel(Treatment, ref = "Control"))

#Data analysis with Generalized Linear Mixed Effects Model with replicate as a random effect
modelrandomm <- glmer(Survival ~ Treatment + (1 | Replicate), data = chemicals, binomial)

summary(modelrandomm) 

#----------------------------------------------------------------------------------------
## Figure for larval survival after 96 hours exposure to DEHP

#Read in the data
dataa <- read.table("DEHP_Survival_Graph.csv", header= TRUE, sep=",")
dataa

#Making sure treatment factors are in the correct order
dataa$Treatment <- factor(dataa$Treatment, levels=c("Control", "0.1", "0.5", "1", "5"))

#Plotting the data
p1 <- ggplot(data=dataa, mapping=aes(x=Treatment, y=Survival)) + 
  geom_boxplot() + 
  theme_bw(base_size = 22) + 
  xlab("Treatment") + 
  theme(panel.grid.major = element_blank(), axis.title.y=element_blank()) +
  theme(axis.title.x=element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  scale_x_discrete(labels= element_blank(), limits = c("Control", "0.1", "0.5", "1", "5")) +
  geom_text(y=1.1,x=2,size=9, label = "***") +
  geom_text(y=1,x=3, size=9, label = "***") +
  geom_text(y=1.20,x=4, size=9, label = "**") +
  geom_text(y=1.10,x=5, size=9, label = "**") +
  ggtitle("DEHP") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,1.50,.25), limits = c(0, 1.50)) +
  labs(tag = "B") +
  theme(plot.tag = element_text(vjust = .3))

p1

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

##Larval urchin survival after 96 hours of Irganox1010 exposure

#Read in the data
data <- read.table("IRG_Survival.csv", header= TRUE, sep=",")
urchins <- data
head(urchins)

#Setting replicate as a factor to allow us to make it a random effect
urchins$Replicate <- as.factor(urchins$Replicate)
#Setting Treatment as factor
urchins$Treatment <- as.factor(urchins$Treatment)

#Forcing R to compare treatments to the control
urchin <- within(urchins, Treatment <- relevel(Treatment, ref = "Dcontrol"))

#Data analysis with Generalized Linear Mixed Effects Model with replicate as a random effect
modell <- glmer(Survival ~ Treatment + (1 | Replicate), data = urchin, binomial)

summary(modell) 

#----------------------------------------------------------------------------------------
## Figure for larval survival after a 4 day exposure to Irganox1010

#Read in the data
data <- read.table("IRG_Survival_Graph.csv", header= TRUE, sep=",")
chem2 <- (data)
chem2

#Making sure treatment factors are in the correct order
chem2$Treatment <- factor(chem2$Treatment, levels=c("Control", "0.1", "0.5", "1", "5"))

#Plotting the data
p2 <- ggplot(data=chem2, mapping=aes(x=Treatment, y=Survival)) + 
  geom_boxplot() + 
  theme_bw(base_size = 22) + ylab("Survival") + xlab("Treatment") + 
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank()) +
  theme(panel.grid.minor = element_blank(), axis.title.y=element_blank()) +
  geom_text(y=1.38,x=2,size=9, label = "*") +
  geom_text(y=1.0,x=3, size=9, label = "***") +
  geom_text(y=1.03,x=4, size=9, label = "***") +
  geom_text(y=1.28,x=5, size=9, label = "**") +
  scale_x_discrete(labels = xticks, guide = guide_axis(angle = 45), limits = c("Control", "0.1", "0.5", "1", "5")) +
  ggtitle("Irganox1010") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,1.50,.25), limits = c(0, 1.50)) +
  labs(tag = "D") +
  theme(plot.tag = element_text(vjust = .3))

p2

#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------

##Larval urchin survival after 96 hours of Methylparaben exposure

#Read in the data
data <- read.table("Meth_Survival.csv", header= TRUE, sep=",")
leach <- data
leach

#Setting replicate as a factor to allow us to make it a random effect
leach$Replicate <- as.factor(leach$Replicate)
#Setting Treatment as factor
leach$Treatment <- as.factor(leach$Treatment)

#Forcing R to compare treatments to the control
leaches <- within(leach, Treatment <- relevel(Treatment, ref = "Acontrol"))

#Data analysis with Generalized Linear Mixed Effects Model with replicate as a random effect
analysis <- glmer(Survival ~ Treatment + (1 | Replicate), data = leaches, binomial)

summary(analysis) 

#----------------------------------------------------------------------------------------
## Figure for larval survival after a 4 day exposure to Methylparaben

#Read in the data
data <- read.table("Meth_Survival_Graph.csv", header= TRUE, sep=",")
chem3 <- data
chem3

#Making sure treatment factors are in the correct order
chem3$Treatment <- factor(chem3$Treatment, levels=c("Control", "0.1", "0.5", "1", "5"))

#Plotting the data
ylab <- expression(atop(paste("Proportion Survival"),
                       "Relative to Control"))

p3 <- ggplot(data=chem3, mapping=aes(x=Treatment, y=Survival)) + 
  geom_boxplot() + 
  theme_bw(base_size = 22) + ylab(ylab) + xlab("Treatment") + 
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(labels= element_blank(), limits = c("Control", "0.1", "0.5", "1", "5")) +
  geom_text(y=1.38,x=2, size=9, label = "*") +
  geom_text(y=.88,x=3, size=9, label = "***") +
  ggtitle("Methylparaben") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,1.50,.25), limits = c(0, 1.50)) +
  labs(tag = "A") +
  theme(plot.tag = element_text(vjust = .3))

p3

#----------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------

##Larval urchin survival after 96 hours of UV327 exposure

#Read in the data
data <- read.table("UV327_Survival.csv", header= TRUE, sep=",")
peach <- data
peach

#Setting replicate as a factor to allow us to make it a random effect
peach$Replicate <- as.factor(peach$Replicate)
#Setting Treatment as factor
peach$Treatment <- as.factor(peach$Treatment)

#Forcing R to compare treatments to the control
peaches <- within(peach, Treatment <- relevel(Treatment, ref = "Dcontrol"))

#Data analysis with Generalized Linear Mixed Effects Model with replicate as a random effect
testing <- glmer(Survival ~ Treatment + (1 | Replicate), data = peaches, binomial)

summary(testing) 

#----------------------------------------------------------------------------------------
## Figure for larval survival after a 4 day exposure to UV327

#Read in the data
data <- read.table("UV327_Survival_Graph.csv", header= TRUE, sep=",")
chem4 <- data
chem4

#Making sure treatment factors are in the correct order
chem4$Treatment <- factor(chem4$Treatment, levels=c("Control", "0.1", "0.5", "1", "5"))

#Plotting the data
ylab <- expression(atop(paste("Proportion Survival"),
                        "Relative to Control"))

p4 <- ggplot(data=chem4, mapping=aes(x=Treatment, y=Survival)) + 
  geom_boxplot() + 
  theme_bw(base_size = 22) + ylab(ylab) + xlab("Treatment") + 
  theme(panel.grid.major = element_blank()) +
  theme(axis.title.x=element_blank(), panel.grid.minor = element_blank()) +
  geom_text(y=1.05,x=2, size=9, label = "***") +
  geom_text(y=1.05,x=3, size=9, label = "***") +
  scale_x_discrete(labels = xticks, guide = guide_axis(angle = 45), limits = c("Control", "0.1", "0.5", "1", "5")) +
  ggtitle("UV327") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,1.50,.25), limits = c(0, 1.50)) +
  labs(tag = "C") +
  theme(plot.tag = element_text(vjust = .3))
 
 p4

#------------------------------------------------------------------------------
##Creating the final figure
p3+p1+p4+p2
#Saving the figure image
dev.off()

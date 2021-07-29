########################################################################
####################Body Length Data Analysis###########################

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

#-------------------------------------------------------------------
#Setting up figure exportation 
png("urchin_body_length.png", height=8, width=9.00, units="in", res=300)

#For uniform figure graphics
xticks <- c("Control", "0.1mg/L", "0.5mg/L", "1.0mg/L", "5.0mg/L")

#-------------------------------------------------------------------

##Larval urchin body length after exposure to UV327 for 96 hours

#Read in the data
data <- read.table("UVurchins.csv", header= TRUE, sep=",")
dat <- data
head(dat)

#Make sure all elements are factors
base1 <- within(dat, Treatment <- as.factor(Treatment))

#Force R to compare the treatments to the control
leveldat <- within(base1, Treatment <- relevel(Treatment, ref = "control"))

#Creating the figure
p1 <-  ggplot(data=leveldat) +
       geom_violin() +
       geom_quasirandom(alpha=0.3) +
       stat_summary(fun = mean, geom = "point", size=2) + 
       stat_summary(fun.data = mean_se, geom = "errorbar", width=0.45) +
       aes(x=Treatment, y=Length) +
       theme_bw(base_size = 20) + 
       ylab(expression(paste("Body Length (",mu, m,")", sep=""))) + 
       xlab("Treatment") + 
       theme(panel.grid.major = element_blank(), axis.title.x=element_blank()) +
       theme(panel.grid.minor = element_blank()) +
       geom_text(y=390,x=3,size=9, label = "***") + ylim(150, 400) +
       scale_x_discrete(labels= xticks, guide = guide_axis(angle = 45)) +
       ggtitle("UV327") +
       theme(plot.title = element_text(hjust = 0.5)) +
       labs(tag = "C") +
       theme(plot.tag = element_text(vjust = .3))

p1

#Make sure replicate is treated as a factor
data$Replicate <- as.factor(dat$Replicate)

#Running the analysis
res.aov <- aov(Length ~ Treatment, data = leveldat)
summary(res.aov)
TukeyHSD(res.aov)

#------------------------------------------------------------------
#-------------------------------------------------------------------

##Larval urchin body length after exposure to DEHP for 96 hours

#Read in the data
numbers <- read.table("DEHPurchins.csv", header= TRUE, sep=",")
data <- numbers
head(data)

#Make sure all elemnents are factors
base2 <- within(data, treatment <- as.factor(treatment))

#Force R to compare the treatments to the control
leveldata <- within(base2, treatment <- relevel(treatment, ref = "Acontrol"))

#Creating the figure
p2 <- ggplot(data=leveldata, mapping=aes(x=treatment, y=length)) + 
   geom_violin() +
   geom_quasirandom(alpha=0.3) +
   stat_summary(fun = mean, geom = "point", size=2) + 
   stat_summary(fun.data = mean_se, geom = "errorbar", width=0.45) +
   theme_bw(base_size = 20) + 
   ylab (expression(paste("Body Length (",mu, m,")", sep=""))) + 
   xlab("Treatment") + 
   theme(panel.grid.major = element_blank(), axis.title.x=element_blank()) +
   theme(axis.ticks.x=element_blank(), axis.title.y=element_blank()) +
   theme(panel.grid.minor = element_blank()) +
   scale_x_discrete(labels= element_blank(), guide = guide_axis(angle = 45)) +
   ggtitle("DEHP") +
   theme(plot.title = element_text(hjust = 0.5)) +
   ylim(150, 400) +
   labs(tag = "B") +
   theme(plot.tag = element_text(vjust = .3))

p2

#Make sure replicate is treated as a factor
data$replicate <- as.factor(data$replicate)

#Running the analysis
res.aov <- aov(length ~ treatment, data = leveldata)
summary(res.aov)
TukeyHSD(res.aov)

#-------------------------------------------------------------
#-------------------------------------------------------------

##Larval urchin body length after exposure to Methylparaben for 96 hours

#Reading in the data
data <- read.table("Methurchins.csv", header= TRUE, sep=",")
dataa <- data
head(dataa)

#Make sure all elements are factors
base3 <- within(dataa, treatment <- as.factor(treatment))

#Force R to compare the treatments to the control
level <- within(base3, treatment <- relevel(treatment, ref = "Control"))

#Creating the figure
p3 <- ggplot(data=level, mapping=aes(x=treatment, y=length)) + 
   geom_violin() +
   geom_quasirandom(alpha=0.3) +
   stat_summary(fun = mean, geom = "point", size=2) + 
   stat_summary(fun.data = mean_se, geom = "errorbar", width=.45) + 
   theme_bw(base_size = 20) + 
   ylab (expression(paste("Body Length (",mu, m,")", sep=""))) + 
   xlab("Treatment") + 
   theme(panel.grid.major = element_blank(), axis.title.x=element_blank()) +
   theme(axis.ticks.x=element_blank(), panel.grid.minor = element_blank()) +
   scale_x_discrete(labels= element_blank(), guide = guide_axis(angle = 45)) +
   ggtitle("Methylparaben") +
   theme(plot.title = element_text(hjust = 0.5)) +
   geom_text(y=385,x=5,size=9, label = "**") + 
   ylim(150, 400) +
   labs(tag = "A") +
   theme(plot.tag = element_text(vjust = .3))

p3

#Making sure replicate is treated as a factor
data$replicate <- as.factor(level$replicate)

#Running the analysis
res.aov <- aov(length ~ treatment, data = level)
summary(res.aov)
TukeyHSD(res.aov)

#---------------------------------------------------------------
#----------------------------------------------------------------

##Larval urchin body length after exposure to Irganox1010 for 96 hours

#Reading in the data
dataaa <- read.table("IGurchins.csv", header= TRUE, sep=",")
head(dataaa)

#Making sure all elements are treated as factors
base4 <- within(dataaa, Treatment <- as.factor(Treatment))

#Forcing R to compare the treatments to the control
level4 <- within(base4, Treatment <- relevel(Treatment, ref = "Dcontrol"))

#Creating the figure
p4 <- ggplot(data=level4, mapping=aes(x=Treatment, y=Length)) +
   geom_violin() +
   geom_quasirandom(alpha=0.3) +
   stat_summary(fun = mean, geom = "point", size=2) + 
   stat_summary(fun.data = mean_se, geom = "errorbar", width=.45) + 
   theme_bw(base_size = 20) + 
   ylab (expression(paste("Body Length (",mu, m,")", sep=""))) + 
   xlab("Treatment") + 
   theme(panel.grid.major = element_blank(), axis.title.x=element_blank()) +
   theme(axis.title.y=element_blank(), panel.grid.minor = element_blank()) +
   scale_x_discrete(labels = xticks, guide = guide_axis(angle = 45)) +
   ggtitle("Irganox1010") +
   theme(plot.title = element_text(hjust = 0.5)) +
   geom_text(y=380,x=4,size=9, label = "*") + 
   ylim(150, 400) +
   labs(tag = "D") +
   theme(plot.tag = element_text(vjust = .3))

p4

#Running the analysis
res.aov <- aov(Length ~ Treatment, data = levels)
summary(res.aov)
TukeyHSD(res.aov)

#--------------------------------------------------------------------------------
##Creating the final figure
p3+p2+p1+p4
#Saving figure image
dev.off()


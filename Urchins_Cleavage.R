########################################################################
#############Cleavage Success Data Analysis#############################

library(patchwork)
library(lmerTest)
library(ggplot2)
library(car)
library(rlang)
library(ggpubr)
library(nlme)
library(Matrix)
library(dplyr)
library(knitr)

#----------------------------------------------------------------------

##Larval urchin cleavage analysis after 3 hours exposure to DEHP

#Read in the data
data <- read.table("DEHP_Divide.csv", header= TRUE, sep=",")
cleave1 <- data
head(cleave1)

#Make sure all elements are factors
base1 <- within(cleave1, Treatment <- as.factor(Treatment))

#Force R to compare the treatments to the control
levels1 <- within(base1, Treatment <- relevel(Treatment, ref = "Acontrol"))

#Data analysis with Generalized Linear Model 
cleavage1 <- glm(Divide ~ Treatment, data = levels1, binomial)

summary(cleavage1) 

#---------------------------------------------------------------

##Larval urchin cleavage analysis after 3 hours exposure to Irganox1010

#Read in the data
data <- read.table("Irganox_Divide.csv", header= TRUE, sep=",")
cleave2 <- data
head(cleave2)

#Make sure all elements are factors
base2 <- within(cleave2, Irganox <- as.factor(Irganox))

#Force R to compare the treatments to the control
levels2 <- within(base2, Irganox <- relevel(Irganox, ref = "Dcontrol"))

#Data analysis with Generalized Linear Model 
cleavage2 <- glm(Divide ~ Irganox, data = levels2, binomial)

summary(cleavage2) 

#---------------------------------------------------------------

##Larval urchin cleavage analysis after 3 hours exposure to Methylparaben

#Read in the data
data <- read.table("Meth_Divide.csv", header= TRUE, sep=",")
cleave3 <- data
head(cleave3)

#Make sure all elements are factors
base3 <- within(cleave3, Meth <- as.factor(Meth))

#Force R to compare the treatments to the control
levels3 <- within(base3, Meth <- relevel(Meth, ref = "Acontrol"))

#Data analysis with Generalized Linear Model 
cleavage3 <- glm(Divide ~ Meth, data = levels3, binomial)

summary(cleavage3) 

#---------------------------------------------------------------

##Larval urchin cleavage analysis after 3 hours exposure to UV327

#Read in the data
data <- read.table("UV327_Divide.csv", header= TRUE, sep=",")
cleave4 <- data
head(cleave4)

#Make sure that all elements are factors
base4 <- within(cleave4, UV327 <- as.factor(UV327))

#Force R to compare the treatments to the control
levels4 <- within(base4, UV327 <- relevel(UV327, ref = "Dcontrol"))

#Data analysis with Generalized Linear Model 
cleavage4 <- glm(Divide ~ UV327, data = levels4, binomial)

summary(cleavage4) 

#----------------------------------------------------------------















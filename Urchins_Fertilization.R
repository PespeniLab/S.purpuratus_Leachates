########################################################################
#############Fertilization Success Data Analysis########################

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

##Larval urchin fertilization analysis after 40 minutes post exposure to DEHP

#Read in the data
datta <- read.table("DEHPfertilization.csv", header= TRUE, sep=",")
head(datta)

#Make sure all elements are factors
base1 <- within(datta, DEHP <- as.factor(DEHP))

#Force R to compare the treatments to the control
levels1 <- within(base1, DEHP <- relevel(DEHP, ref = "Acontrol"))

#Data analysis with Generalized Linear Model 
fert1 <- glm(Success ~ DEHP, data = levels1, binomial)

summary(fert1) 

#------------------------------------------------------------------------

##Larval urchin fertilization analysis after 40 minutes post exposure to Irganox1010

#Read in the data
dataa <- read.table("IrgFertilization.csv", header= TRUE, sep=",")
head(dataa)

#Make sure all elements are factors
base2 <- within(dataa, Irganox <- as.factor(Irganox))

#Force R to compare the treatments to the control
levels2 <- within(base2, Irganox <- relevel(Irganox, ref = "Dcontrol"))

#Data analysis with Generalized Linear Model 
fert2 <- glm(Success ~ Irganox, data = levels2, binomial)

summary(fert2) 

#-------------------------------------------------------------

##Larval urchin fertilization analysis after 40 minutes post exposure to Methylparaben

#Read in the data
dattaa <- read.table("MethFertilization.csv", header= TRUE, sep=",")
head(dattaa)

#Make sure all elements are factors
base3 <- within(dattaa, Meth <- as.factor(Meth))

#Force R to compare the treatments to the control
levels3 <- within(base3, Meth <- relevel(Meth, ref = "Acontrol"))

#Data analysis with Generalized Linear Model 
fert3 <- glm(Success ~ Meth, data = levels3, binomial)

summary(fert3) 

#-------------------------------------------------------------

##Larval urchin fertilization analysis after 40 minutes post exposure to UV327

#Read in the data
numbers <- read.table("UV327Fertilization.csv", header= TRUE, sep=",")
head(numbers)

#Make sure elements are factors
base4 <- within(numbers, UV327 <- as.factor(UV327))

#Force R to compare treatments to the control
levels4 <- within(base4, UV327 <- relevel(UV327, ref = "Dcontrol"))

#Data analysis with Generalized Linear Model 
fert4 <- glm(Success ~ UV327, data = levels4, binomial)

summary(fert4) 

#--------------------------------------------------------------
#--------------------------------------------------------------
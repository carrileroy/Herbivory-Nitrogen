# Carri J. LeRoy and Madeline Thompson, 6-13-24
#
# SLR using permutation-Resampling
#
# For 2019 MSH Decomp paper: k rates (SLRs)
# Data in: AFDMall.csv

install.packages("agricolae")
install.packages("lmPerm")
install.packages("psych")
install.packages("FSA")
install.packages("ggplot2")
install.packages("car")
install.packages("multcompView")
install.packages("lsmeans")
install.packages("tidyr")
install.packages("dplyr")
install.packages("magrittr")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("readxl")
install.packages("devtools")
install.packages("ggpubr")

library(lmPerm)           
library(psych)           
library(FSA)              
library(multcompView)     
library(lsmeans)          
library(tidyr)          
library(ggplot2)        
library(dplyr)          
library(magrittr)       	
library(gridExtra)     	  
library(car)     		      
library(tidyverse)       
library(readxl)          
library(devtools)
library(ggpubr)
library(agricolae)
#
#
# Make sure your grouping variables are classified as factors, not characters and not numbers

set.seed(1431)
#
#
# Regressions for k values:
AFDMall$type = factor(AFDMall$type,
                      levels=unique(AFDMall$type))
AFDMall$sex = factor(AFDMall$sex,
                     levels=unique(AFDMall$sex))
AFDMall$harvest = factor(AFDMall$harvest,
                         levels=unique(AFDMall$harvest))
AFDMall$trt = factor(AFDMall$trt,
                     levels=unique(AFDMall$trt))


slr201 = lmp(lnAFDM~days, data=subset(AFDMall, trt==1))
summary(slr201)
slr202 = lmp(lnAFDM~days, data=subset(AFDMall, trt==2))
summary(slr202)
slr203 = lmp(lnAFDM~days, data=subset(AFDMall, trt==3))
summary(slr203)
slr204 = lmp(lnAFDM~days, data=subset(AFDMall, trt==4))
summary(slr204)
slr205 = lmp(lnAFDM~days, data=subset(AFDMall, trt==5))
summary(slr205)
slr206 = lmp(lnAFDM~days, data=subset(AFDMall, trt==6))
summary(slr206)
slr207 = lmp(lnAFDM~days, data=subset(AFDMall, trt==7))
summary(slr207)
slr208 = lmp(lnAFDM~days, data=subset(AFDMall, trt==8))
summary(slr208)
slr209 = lmp(lnAFDM~days, data=subset(AFDMall, trt==9))
summary(slr209)
slr210 = lmp(lnAFDM~days, data=subset(AFDMall, trt==10))
summary(slr210)
slr211 = lmp(lnAFDM~days, data=subset(AFDMall, trt==11))
summary(slr211)
slr212 = lmp(lnAFDM~days, data=subset(AFDMall, trt==12))
summary(slr212)
slr213 = lmp(lnAFDM~days, data=subset(AFDMall, trt==13))
summary(slr213)
slr214 = lmp(lnAFDM~days, data=subset(AFDMall, trt==14))
summary(slr214)
slr215 = lmp(lnAFDM~days, data=subset(AFDMall, trt==15))
summary(slr215)
slr216 = lmp(lnAFDM~days, data=subset(AFDMall, trt==16))
summary(slr216)
slr217 = lmp(lnAFDM~days, data=subset(AFDMall, trt==17))
summary(slr217)
slr218 = lmp(lnAFDM~days, data=subset(AFDMall, trt==18))
summary(slr218)
slr219 = lmp(lnAFDM~days, data=subset(AFDMall, trt==19))
summary(slr219)
slr220 = lmp(lnAFDM~days, data=subset(AFDMall, trt==20))
summary(slr220)
slr221 = lmp(lnAFDM~days, data=subset(AFDMall, trt==21))
summary(slr221)
slr222 = lmp(lnAFDM~days, data=subset(AFDMall, trt==22))
summary(slr222)
slr223 = lmp(lnAFDM~days, data=subset(AFDMall, trt==23))
summary(slr223)
slr224 = lmp(lnAFDM~days, data=subset(AFDMall, trt==24))
summary(slr224)


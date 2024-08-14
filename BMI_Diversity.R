# Carri J. LeRoy and Maddie Thompson, 6-30-24
#
# Two-way ANOVAs using permutation-Resampling
#
# For 2019 MSH Weevil Decomp paper: simple community metrics 2-way pANOVAs;
# Datasets = BMI.csv, BMIH3.csv

if(!require(agricolae)){install.packages("agricolae")}
if(!require(lmPerm)){install.packages("lmPerm")}
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(magrittr)){install.packages("magrittr")}
if(!require(gridExtra)){install.packages("gridExtra")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(readxl)){install.packages("readxl")}
if(!require(devtools)){install.packages("devtools")}
if(!require(ggpubr)){install.packages("ggpubr")}

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

BMI$nitrogen = factor(BMI$nitrogen,
                      levels=unique(BMI$nitrogen))
BMI$sex = factor(BMI$sex,
                 levels=unique(BMI$sex))
BMI$type = factor(BMI$type,
                  levels=unique(BMI$type))
BMI$dummy1 = factor(BMI$dummy1,
                    levels=unique(BMI$dummy1))
BMI$dummy2 = factor(BMI$dummy2,
                    levels=unique(BMI$dummy2))
BMI$Harvest = factor(BMI$Harvest,
                     levels=unique(BMI$Harvest))
BMI$AddN = factor(BMI$AddN, 
                  levels = unique(BMI$AddN))
set.seed(1431)
res.aov1 <- aovp(S~nitrogen*type*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov1)
res.aov1111 <- aovp(S~AddN*type*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov1111)
res.aov2 <- aovp(A~nitrogen*type*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov2)
res.aov2111 <- aovp(A~AddN*type*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov2111)
res.aov3 <- aovp(H~nitrogen*type*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov3)
res.aov3111 <- aovp(H~AddN*type*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov3111)
res.aov101 <- aovp(E~nitrogen*type*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov101)
res.aov1011 <- aovp(E~AddN*type*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov1011)
res.aov102 <- aovp(D.~nitrogen*type*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov102)
res.aov102 <- aovp(D.~AddN*type*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov102)
res.aov4 <- aovp(A~nitrogen*type*Harvest, data = BMI, perm="Prob")
anova(res.aov4)
res.aov4 <- aovp(A~AddN*type*Harvest, data = BMI, perm="Prob")
anova(res.aov4)
res.aov5 <- aovp(E~nitrogen*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov5)
res.aov5 <- aovp(E~AddN*sex*Harvest, data = BMI, perm="Prob")
anova(res.aov5)
res.aov4 <- aovp(H~nitrogen*type*Harvest, data = BMI, perm="Prob")
anova(res.aov4)
res.aov4 <- aovp(H~AddN*type*Harvest, data = BMI, perm="Prob")
anova(res.aov4)
res.aov6 <- aovp(A~dummy1, data = BMI, perm="Prob")
anova(res.aov6)
res.aov7 <- aovp(A~dummy2, data = BMI, perm="Prob")
anova(res.aov7)
res.aov8 <- aovp(E~dummy1, data = BMI, perm="Prob")
anova(res.aov8)
res.aov9 <- aovp(E~dummy2, data = BMI, perm="Prob")
anova(res.aov9)
res.aov10 <- aovp(CG~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov10)
res.aov110 <- aovp(CG~sex*type*Harvest, data = BMI, perm="Prob")
anova(res.aov110)
res.aov11 <- aovp(PH~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov11)
res.aov111 <- aovp(PH~sex*type*Harvest, data = BMI, perm="Prob")
anova(res.aov111)
res.aov12 <- aovp(SC~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov12)
res.aov121 <- aovp(SC~sex*type*nitrogen, data = BMI, perm="Prob")
anova(res.aov121)
res.aov13 <- aovp(SH~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov13)
res.aov131 <- aovp(SH~sex*type*Harvest, data = BMI, perm="Prob")
anova(res.aov131)
res.aov14 <- aovp(EPT.~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov14)
res.aov15 <- aovp(ASEPT~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov15)
res.aov150 <- aovp(Insect.~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov150)
res.aov151 <- aovp(Insect.~sex*type*Harvest, data = BMI, perm="Prob")
anova(res.aov151)
res.aov152 <- aovp(Nonins.~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov152)
res.aov153 <- aovp(Nonins.~sex*type*Harvest, data = BMI, perm="Prob")
anova(res.aov153)
res.aov160 <- aovp(CG.~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov160)
res.aov17 <- aovp(PH.~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov17)
res.aov171 <- aovp(PH.~nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov171)
res.aov18 <- aovp(SC.~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov18)
res.aov181 <- aovp(SC.~sex*type*Harvest, data = BMI, perm="Prob")
anova(res.aov181)
res.aov19 <- aovp(SH.~sex*type*nitrogen*Harvest, data = BMI, perm="Prob")
anova(res.aov19)
#************************************************
# Create plots - A in H3 by Nitrogen*Type, E in H3 by Nitrogen*Sex, and CG, PH, and SH all in H3 by Sex*Type - need to subset the BMI data into BMIH3. 
BMIH3$sex = factor(BMIH3$sex,
                   levels=unique(BMIH3$sex))
BMIH3$type = factor(BMIH3$type,
                    levels=unique(BMIH3$type))
BMIH3$nitrogen = factor(BMIH3$nitrogen,
                        levels=unique(BMIH3$nitrogen))
BMIH3$dummy1 = factor(BMIH3$dummy1,
                      levels=unique(BMIH3$dummy1))
BMIH3$dummy2 = factor(BMIH3$dummy2,
                      levels=unique(BMIH3$dummy2))
BMIH3$dummy3 = factor(BMIH3$dummy3,
                      levels=unique(BMIH3$dummy3))
res.aov20 <- aovp(A~nitrogen*type, data = BMIH3, perm="Prob")
anova(res.aov20)
fit10 <- aovp(A ~ dummy1, data = BMIH3, perm="Prob")
summary(fit10)
TukeyHSD(fit10)
HSD.test(fit10, "dummy1", group=TRUE)
out1 <- HSD.test(fit10, "dummy1", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
res.aov21 <- aovp(E~nitrogen*sex, data = BMIH3, perm="Prob")
anova(res.aov21)
fit11 <- aovp(E ~ dummy2, data = BMIH3, perm="Prob")
summary(fit11)
TukeyHSD(fit11)
HSD.test(fit11, "dummy2", group=TRUE)
out1 <- HSD.test(fit11, "dummy2", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
res.aov22 <- aovp(CG~sex*type, data = BMIH3, perm="Prob")
anova(res.aov22)
fit12 <- aovp(CG ~ dummy3, data = BMIH3, perm="Prob")
summary(fit12)
TukeyHSD(fit12)
HSD.test(fit12, "dummy3", group=TRUE)
out1 <- HSD.test(fit12, "dummy3", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
res.aov23 <- aovp(PH~sex*type, data = BMIH3, perm="Prob")
anova(res.aov23)
fit13 <- aovp(PH ~ dummy3, data = BMIH3, perm="Prob")
summary(fit13)
TukeyHSD(fit13)
HSD.test(fit13, "dummy3", group=TRUE)
out1 <- HSD.test(fit13, "dummy3", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
res.aov24 <- aovp(SH~sex*type, data = BMIH3, perm="Prob")
anova(res.aov24)
fit14 <- aovp(SH ~ dummy3, data = BMIH3, perm="Prob")
summary(fit14)
TukeyHSD(fit14)
HSD.test(fit14, "dummy3", group=TRUE)
out1 <- HSD.test(fit14, "dummy3", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)

#Figures in grayscale: 
plotCG <- ggplot(BMIH3, aes(x = type, y = CG, fill = sex)) + 
  geom_violin(trim = FALSE) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5, position=position_dodge(0.9),
               colour = "black") +
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(0.9), dotsize = 0.5) +
  scale_fill_manual(values = c("white", "gray50")) +
  labs(tag = "(a)", x = element_blank(), y = "CG Abundance") +
  ylim(-38, 200) +
  theme(legend.position = c(0.65, 0.98)) +
  theme(axis.text.x = element_blank()) +
  theme(text = element_text(size = 12))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 10)) +
  geom_text(size=3, x=0.77, y=165, label="a") +
  geom_text(size=3, x=1.22, y=75, label="b") +
  geom_text(size=3, x=1.78, y=125, label="b") +
  geom_text(size=3, x=2.23, y=135, label="b")
plotCG
plotPH <- ggplot(BMIH3, aes(x = type, y = PH, fill = sex)) + 
  geom_violin(trim = FALSE) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5, position=position_dodge(0.9),
               colour = "black") +
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(0.9), dotsize = 0.5) +
  scale_fill_manual(values = c("white", "gray50")) +
  labs(tag = "(b)",x = element_blank(),  
       y = "PH Abundance") +
  ylim(-70, 420) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank()) +
  theme(text = element_text(size = 12))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 10)) +
  geom_text(size=3, x=0.77, y=415, label="a") +
  geom_text(size=3, x=1.22, y=90, label="b") +
  geom_text(size=3, x=1.78, y=240, label="b") +
  geom_text(size=3, x=2.23, y=215, label="b")
plotPH
plotSH <- ggplot(BMIH3, aes(x = type, y = SH, fill = sex)) + 
  geom_violin(trim = FALSE) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5, position=position_dodge(0.9),
               colour = "black") +
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(0.9), dotsize = 0.5) +
  scale_fill_manual(values = c("white", "gray50")) +
  labs(tag = "(c)",x = "Litter type", 
       y = "SH Abundance") +
  ylim(-7, 45) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=0)) +
  theme(text = element_text(size = 12))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 10)) +
  geom_text(size=3, x=0.77, y=40, label="a") +
  geom_text(size=3, x=1.22, y=15, label="b") +
  geom_text(size=3, x=1.78, y=30, label="b") +
  geom_text(size=3, x=2.23, y=25, label="b")
plotSH
gCG <- ggplotGrob(plotCG)
gPH <- ggplotGrob(plotPH)
gSH <- ggplotGrob(plotSH)
g <- arrangeGrob(gCG, gPH, gSH, nrow=3)


# Abundance figure: nitrogen*type
plotA <- ggplot(BMIH3, aes(x = nitrogen, y = A, fill = type)) + 
  geom_violin(trim = FALSE) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5, position=position_dodge(0.9),
               colour = "black") +
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(0.9), dotsize = 0.5) +
  scale_fill_manual(values = c("white", "gray50")) +
  labs(x = "Nitrogen Treatment", 
       y = "Total BMI Abundance") +
  ylim(-160, 620) +
  theme(legend.position = c(0.28, 0.9)) +
  theme(axis.text.x = element_text(angle=0)) +
  theme(text = element_text(size = 12, colour = "black"))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 10)) +
  geom_text(size=3, x=0.77, y=400, label="b") +
  geom_text(size=3, x=1.23, y=180, label="b") +
  geom_text(size=3, x=2, y=380, label="b") +
  geom_text(size=3, x=2.77, y=300, label="b") +
  geom_text(size=3, x=3.23, y=600, label="a")
plotA

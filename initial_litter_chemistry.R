#Carri J. LeRoy and Madeline Thompson, 6-30-24
#
# Two-way ANOVA using permutation-Resampling
#
# For 2019 MSH Weevil Decomp paper: Initial Litter Chemistry, 2-way pANOVAs;
# Dataset = chem.csv (updated C:N to molar element ratios)
# Check to see if you need to install the lmPerm package and other packages needed for analysis and plotting

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
#
#
# Make sure your grouping variables are classified as factors, not characters and not numbers
# This also orders your factors as they are in the datatable, otherwise R with alphabetize them. 
str(chem)
chem$type = factor(chem$type,
                   levels=unique(chem$type))
chem$sex = factor(chem$sex,
                  levels=unique(chem$sex))
chem$nitrogen = factor(chem$nitrogen,
                       levels=unique(chem$nitrogen))
chem$dummy1 = factor(chem$dummy1,
                     levels=unique(chem$dummy1))
chem$dummy2 = factor(chem$dummy2,
                     levels=unique(chem$dummy2))
chem$dummy3 = factor(chem$dummy3,
                     levels=unique(chem$dummy3))
chem$dummy4 = factor(chem$dummy4,
                     levels=unique(chem$dummy4))
chem$addN = factor(chem$addN,
                   levels=unique(chem$addN))
#
# Set a random seed
set.seed(1431)
#
# Run a permutative 2-way ANOVA for %C (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit <- aovp(C~type*sex*nitrogen, data=chem, perm="Prob")
anova(fit)

# Then run it using a dummy variable to force the Tukey test: Not necessary - not significant ANOVA
fit1 <- aovp(C ~ dummy1, data = chem)
summary(fit1)
TukeyHSD
TukeyHSD(fit1)

# to get Tukey letters: 
HSD.test(fit1, "dummy1", group=TRUE)
out1 <- HSD.test(fit1, "dummy1", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)

# Repeat for %N
fit2 <- aovp(N~type*sex*nitrogen, data=chem, perm="Prob")
anova(fit2)

# Repeat for C:N
fit4 <- aovp(CNmolar~type*sex*nitrogen, data=chem, perm="Prob")
anova(fit4)
# Repeat for %CT
fit6 <- aovp(CT~type*sex*nitrogen, data=chem, perm="Prob")
anova(fit6)

# Try with "addN" a categorical variable for add N or not (yes, no)

fit100 <- aovp(C~type*sex*addN, data=chem, perm="Prob")
anova(fit100)
# Repeat for %N
fit101 <- aovp(N~type*sex*addN, data=chem, perm="Prob")
anova(fit101)
# Repeat for C:N
fit102 <- aovp(CNmolar~type*sex*addN, data=chem, perm="Prob")
anova(fit102)
# Repeat for %CT
fit103 <- aovp(CT~type*sex*addN, data=chem, perm="Prob")
anova(fit103)
# FOllow-up 2-way ANOVAs:
fit7 <- aovp(C~type*nitrogen, data=chem, perm="Prob")
anova(fit7) # n.s.
# Repeat for %N
fit8 <- aovp(N~type*nitrogen, data=chem, perm="Prob")
anova(fit8) #n.s.
# Repeat for C:N
fit9 <- aovp(CNmolar~type*nitrogen, data=chem, perm="Prob")
anova(fit9) #n.s.
# Repeat for %CT
fit10 <- aovp(CT~type*nitrogen, data=chem, perm="Prob")
anova(fit10) #n.s.
fit37 <- aovp(C~type*addN, data=chem, perm="Prob")
anova(fit37) # n.s.
# Repeat for %N
fit38 <- aovp(N~type*addN, data=chem, perm="Prob")
anova(fit38) #n.s
fit3 <- aovp(N ~ dummy2, data = chem)
summary(fit3)
TukeyHSD(fit3)
HSD.test(fit3, "dummy", group=TRUE)
out1 <- HSD.test(fit3, "dummy", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)

# Repeat for C:N
fit39 <- aovp(CNmolar~type*addN, data=chem, perm="Prob")
anova(fit39) #n.s.
fit300 <- aovp(CNmolar ~ dummy2, data = chem)
summary(fit300)
TukeyHSD(fit300)
HSD.test(fit300, "dummy", group=TRUE)
out1 <- HSD.test(fit300, "dummy", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
# Repeat for %CT
fit40 <- aovp(CT~type*addN, data=chem, perm="Prob")
anova(fit40) #n.s.
fit57 <- aovp(C~sex*addN, data=chem, perm="Prob")
anova(fit57) # n.s.
# Repeat for %N
fit58 <- aovp(N~sex*addN, data=chem, perm="Prob")
anova(fit58) #n.s.
# Repeat for C:N
fit60 <- aovp(CNmolar~sex*addN, data=chem, perm="Prob")
anova(fit60) #n.s.
# Repeat for %CT
fit61 <- aovp(CT~sex*addN, data=chem, perm="Prob")
anova(fit61) #n.s.
fit11 <- aovp(C~type, data=chem, perm="Prob")
anova(fit11) # n.s.
# Repeat for %N
fit12 <- aovp(N~type, data=chem, perm="Prob")
anova(fit12)
# Repeat for C:N
fit13 <- aovp(CNmolar~type, data=chem, perm="Prob")
anova(fit13) #n.s.
# Repeat for %CT
fit14 <- aovp(CT~type, data=chem, perm="Prob")
anova(fit14) #n.s.
fit21 <- aovp(C~sex, data=chem, perm="Prob")
anova(fit21) # n.s.
# Repeat for %N
fit22 <- aovp(N~sex, data=chem, perm="Prob")
anova(fit22) #n.s.
# Repeat for C:N
fit23 <- aovp(CNmolar~sex, data=chem, perm="Prob")
# Repeat for %CT
fit24 <- aovp(CT~sex, data=chem, perm="Prob")
anova(fit24)
#******************************
#*Nitrogen as a numeric variable: 
chem$nitrogen = as.numeric(chem$nitrogen,
                           levels=unique(chem$nitrogen))

fit15 <- lmp(C~ nitrogen, chem)
summary(fit15) #n.s.
fit16 <- lmp(N ~ nitrogen, chem)
summary(fit16) #n.s.
fit17 <- lmp(CNmolar ~ nitrogen, chem)
summary(fit17) #n.s.
fit18 <- lmp(CT ~ nitrogen, chem)
summary(fit18) #n.s.
#Final Plots - need to show actual data: 
plotN <- ggplot(chem, aes(x = addN, y = N, fill = type)) + 
  geom_violin(trim = FALSE) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.4, position=position_dodge(0.9),
               colour = "black")+
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(0.9)) +
  scale_fill_manual(values = c("gray", "white")) +
  labs(tag = "(a)", x = element_blank(), 
       y = "% Nitrogen") +
  ylim(0, 2.8) +
  theme(legend.position = c(0.25, 0.9)) +
  theme(axis.text.x = element_blank()) +
  theme(text = element_text(size = 12))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 10))
plotN

plotC <- ggplot(chem, aes(x = addN, y = C, fill = type)) + 
  geom_violin(trim = FALSE) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.4, position=position_dodge(0.9),
               colour = "black") +
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(0.9)) +
  scale_fill_manual(values = c("gray", "white")) +
  labs(tag = "(b)", x = element_blank(), y = "% Carbon") +
  ylim(40, 52) +
  theme(legend.position ="none") +
  theme(axis.text.x = element_blank()) +
  theme(text = element_text(size = 12))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())
plotC
plotCN <- ggplot(chem, aes(x = addN, y = CNmolar, fill = type)) + 
  geom_violin(trim = FALSE) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.4, position=position_dodge(0.9),
               colour = "black") +
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(0.9)) +
  scale_fill_manual(values = c("gray", "white")) +
  labs(tag = "(c)", x = element_blank(), 
       y = "C : N") +
  ylim(0, 130) +
  theme(legend.position ="none") +
  theme(axis.text.x = element_blank()) +
  theme(text = element_text(size = 12))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 15)) 
plotCN
plotCT <- ggplot(chem, aes(x = addN, y = CT, fill = type)) + 
  geom_violin(trim = FALSE) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.4, position=position_dodge(0.9),
               colour = "black") +
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(0.9)) +
  scale_fill_manual(values = c("gray", "white")) +
  labs(tag = "(d)", x = "Nitrogen Added", 
       y = "% CT") +
  ylim(0, 32) +
  theme(legend.position ="none") +
  theme(axis.text.x = element_text(angle=0)) +
  theme(text = element_text(size = 12))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 15)) 
plotCT
gN <- ggplotGrob(plotN)
gC <- ggplotGrob(plotC)
gCN <- ggplotGrob(plotCN)
gCT <- ggplotGrob(plotCT)
grid::grid.newpage()
g <- grid::grid.draw(rbind(gN, gC, gCN, gCT))
#Export as PDF, tif, eps - about 12 x 5
ggsave(file="chem.pdf", g, width=8.4, height=20, units="cm", dpi=800)
ggsave(file="chem.tif", g, width=8.4, height=20, units="cm", dpi=800, device='tif')
ggsave(g, file="chem.eps", width=8.4, height=20, units="cm", dpi=800, device="eps")


# Carri J. LeRoy and Maddie Thompson, 6-30-24
# ANCOVA models for Herbivore paper. Data in AFDMall.csv 
# (Data split into Herbivore data: AFDMW.csv and Non-herbivore data: AFDMN.csv)

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

AFDMall$nitrogen = factor(AFDMall$nitrogen,
                          levels=unique(AFDMall$nitrogen))
AFDMall$type = factor(AFDMall$type,
                      levels=unique(AFDMall$type))
AFDMall$sex = factor(AFDMall$sex,
                     levels=unique(AFDMall$sex))

str(AFDMall)
set.seed(1431)

fit2 <- aovp(lnAFDM ~ sex*type*nitrogen*days, data = AFDMall)
anova(fit2)
AFDMW$nitrogen = factor(AFDMW$nitrogen,
                        levels=unique(AFDMW$nitrogen))
AFDMW$sex = factor(AFDMW$sex,
                   levels=unique(AFDMW$sex))
str(AFDMW)
set.seed(1431)

fit7 <- aovp(lnAFDM ~ nitrogen*sex*days, data = AFDMW)
anova(fit7)

AFDMN$nitrogen = factor(AFDMN$nitrogen,
                        levels=unique(AFDMN$nitrogen))
AFDMN$sex = factor(AFDMN$sex,
                   levels=unique(AFDMN$sex))
str(AFDMN)
set.seed(1431)
fit8 <- aovp(lnAFDM ~ nitrogen*sex*days, data = AFDMN)
anova(fit8)

plotX <- ggplot(data=AFDMall, aes(x=days, y=lnAFDM, group=type))+
  geom_point(aes(color=type), pch=19)+
  geom_smooth(aes(linetype=type, color=type), method="lm", se=FALSE)+
  scale_linetype_manual(values=c("dotted", "solid"))+
  scale_color_manual(values=c('gray','#000000'))+
  theme_classic()+
  theme(axis.text.x = element_blank())+
  theme(axis.text = element_text(colour="black"))+
  theme(axis.text = element_text(size = 10))+
  theme(axis.title = element_text(size = 15))+
  theme(legend.position = "none")+
  ylim(3.7, 4.7) +
  labs(tag = "(a)", x = element_blank(), y = "ln % AFDM remaining")+
  annotate(geom="text", size=5, x=20, y=3.9, label="Non-Herbivore", color = "#808080") +
  annotate(geom="text", size=5, x=20, y=4.5, label="Herbivore")
plotX
#

plotW <- ggplot(data=AFDMW, aes(x=days, y=lnAFDM, group=nitrogen))+
  geom_point(aes(color=nitrogen), pch=19)+
  geom_smooth(aes(linetype=nitrogen, color=nitrogen), method="lm", se=FALSE)+
  scale_linetype_manual(values=c("solid", "dotted", "dashed"))+
  scale_color_manual(values=c('#000000','#808080', "gray"))+
  theme_classic()+
  theme(axis.text.x = element_blank())+
  theme(axis.text = element_text(colour="black"))+
  theme(axis.text = element_text(size = 10))+
  theme(axis.title = element_text(size = 15))+
  theme(legend.position = "none")+
  ylim(3.7, 4.7) +
  labs(tag = "(b)", x = element_blank(), y = "ln % AFDM remaining")+
  annotate(geom="text", size=5, x=8, y=4.69, label="Herbivore", color = "#808080") +
  annotate(geom="text", size=5, x=28, y=4.4, label="0 N", color = "#808080") +
  annotate(geom="text", size=5, x=28, y=4.1, label="16 N")
plotW
plotN <- ggplot(data=AFDMN, aes(x=days, y=lnAFDM, group=nitrogen))+
  geom_point(aes(color=nitrogen), pch=19)+
  geom_smooth(aes(linetype=nitrogen, color=nitrogen), method="lm", se=FALSE)+
  scale_linetype_manual(values=c("dashed", "solid", "dotted"))+
  scale_color_manual(values=c('#000000','#808080', "gray"))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.text = element_text(colour="black"))+
  theme(axis.text = element_text(size = 10))+
  theme(axis.title = element_text(size = 15))+
  ylim(3.7, 4.7) +
  labs(tag = "(c)", x = "Days in stream", y = "ln % AFDM remaining")+
  annotate(geom="text", size=5, x=10, y=4.69, label="Non-Herbivore") +
  annotate(geom="text", size=5, x=28, y=3.9, label="8 & 16 N", color = "#808080") +
  annotate(geom="text", size=5, x=28, y=4.4, label="0 N")
plotN
gA <- ggplotGrob(plotX)
gB <- ggplotGrob(plotW)
gC <- ggplotGrob(plotN)
g <- arrangeGrob(gA, gB, gC, nrow=3)

ggsave(file="ANCOVA.pdf", g, width=8.4, height=21, units="cm", dpi=800)
ggsave(g, file="ANCOVA.eps", width=8.4, height=21, units="cm", dpi=800, device="eps")


#follow the instructions at 
#https://www.datanovia.com/en/lessons/mixed-anova-in-r/

library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(emmeans)
library(sjstats)
library(lme4)
library(lmertest)
library(MuMIn)

OD1t1ROI <- read_excel("Desktop/ToPeggy/Behavioral_Analysis/STATS/OD1/Score_v2/OD1t1ROI.xlsx")
OD1t1ROI$ID <- as.factor(OD1t1ROI$ID)  
OD1t1ROI$Group <- factor(OD1t1ROI$Group,
                      levels = c(1,2),
                      labels = c("control","parkinson")) 
OD1t1ROI$Prior <- factor(OD1t1ROI$Prior,
                      levels = c(1,2),
                      labels = c("expected","unexpected")) 
OD1t1ROI$Trial <- as.factor(OD1t1ROI$Trial) 
OD1t1ROI$t1ROI <- as.numeric(OD1t1ROI$t1ROI) 
summary(OD1t1ROI)

ggplot(OD1t1ROI, aes(x = Prior, y = t1ROI, group = interaction(Group, ID), 
                   color = Group)) + stat_summary(fun.y = mean, geom = "line")


nullmodel1 <- lmer( t1ROI ~ 1 + (1|ID) , data = OD1t1ROI, REML=TRUE)
nullmodel2 <- lmer( t1ROI ~ 1 + (1|Trial) , data = OD1t1ROI, REML=TRUE)
nullmodel3 <- lmer( t1ROI ~ 1 + (1|ID) + (1|Trial), data = OD1t1ROI, REML=TRUE)
nullmodel4 <- lmer( t1ROI ~ 1 + (1|ID) + (1|Prior), data = OD1t1ROI, REML=TRUE)
nullmodel5 <- lmer( t1ROI ~ 1 + (1|Prior/Trial), data = OD1t1ROI, REML=TRUE)
nullmodel6 <- lmer( t1ROI ~ 1 + (1|ID) + (1|Prior/Trial) , data = OD1t1ROI, REML=TRUE) #winner

anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5, nullmodel6)

model.1 <- lmer(t1ROI ~ Group * Prior + (1|ID) + (1|Trial), data = OD1t1ROI)
anova(model.1)
confint(model.1, oldNames = FALSE)


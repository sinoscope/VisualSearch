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

OD1deltaRTt1ROI <- read_excel("Desktop/ToPeggy/Behavioral_Analysis/STATS/OD1/Score_v2/OD1deltaRTt1ROI.xlsx")
OD1deltaRTt1ROI$ID <- as.factor(OD1deltaRTt1ROI$ID)  
OD1deltaRTt1ROI$Group <- factor(OD1deltaRTt1ROI$Group,
                      levels = c(1,2),
                      labels = c("control","parkinson")) 
OD1deltaRTt1ROI$Prior <- factor(OD1deltaRTt1ROI$Prior,
                      levels = c(1,2),
                      labels = c("expected","unexpected")) 
OD1deltaRTt1ROI$Trial <- as.factor(OD1deltaRTt1ROI$Trial) 
OD1deltaRTt1ROI$deltaRTt1ROI <- as.numeric(OD1deltaRTt1ROI$deltaRTt1ROI) 
summary(OD1deltaRTt1ROI)

ggplot(OD1deltaRTt1ROI, aes(x = Prior, y = deltaRTt1ROI, group = interaction(Group, ID), 
                   color = Group)) + stat_summary(fun.y = mean, geom = "line")


nullmodel1 <- lmer( deltaRTt1ROI ~ 1 + (1|ID) , data = OD1deltaRTt1ROI, REML=TRUE)
nullmodel2 <- lmer( deltaRTt1ROI ~ 1 + (1|Trial) , data = OD1deltaRTt1ROI, REML=TRUE)
nullmodel3 <- lmer( deltaRTt1ROI ~ 1 + (1|ID) + (1|Trial), data = OD1deltaRTt1ROI, REML=TRUE)
nullmodel4 <- lmer( deltaRTt1ROI ~ 1 + (1|ID) + (1|Prior), data = OD1deltaRTt1ROI, REML=TRUE)
nullmodel5 <- lmer( deltaRTt1ROI ~ 1 + (1|Prior/Trial), data = OD1deltaRTt1ROI, REML=TRUE)
nullmodel6 <- lmer( deltaRTt1ROI ~ 1 + (1|ID) + (1|Prior/Trial) , data = OD1deltaRTt1ROI, REML=TRUE) #winner

anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5, nullmodel6)

model.1 <- lmer(deltaRTt1ROI ~ Group * Prior + (1|ID) + (1|Trial), data = OD1deltaRTt1ROI)
anova(model.1)
confint(model.1, oldNames = FALSE)

model.2 <- lmer(deltaRTt1ROI ~ Group * Prior * (1|ID) * (1|Trial), data = OD1deltaRTt1ROI)
anova(model.2)


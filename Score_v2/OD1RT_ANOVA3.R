#follow the instructions at 
#https://www.datanovia.com/en/lessons/mixed-anova-in-r/

library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(emmeans)
library(sjstats)
library(lme4)
library(lmerTest)
library(MuMIn)

OD1RT <- read_excel("Desktop/ToPeggy/Behavioral_Analysis/STATS/OD1/Score_v2/OD1RT.xlsx")
OD1RT$ID <- as.factor(OD1RT$ID)  
OD1RT$Group <- factor(OD1RT$Group,
                      levels = c(1,2),
                      labels = c("control","parkinson")) 
OD1RT$Prior <- factor(OD1RT$Prior,
                      levels = c(1,2),
                      labels = c("expected","unexpected")) 
OD1RT$Trial <- as.factor(OD1RT$Trial) 
OD1RT$RT <- as.numeric(OD1RT$RT) 
summary(OD1RT)

ggplot(OD1RT, aes(x = Prior, y = RT, group = interaction(Group, ID), 
                   color = Group)) + stat_summary(fun.y = mean, geom = "line")


nullmodel1 <- lmer( RT ~ 1 + (1|ID) , data = OD1RT, REML=TRUE)
nullmodel2 <- lmer( RT ~ 1 + (1|Trial) , data = OD1RT, REML=TRUE)
nullmodel3 <- lmer( RT ~ 1 + (1|ID) + (1|Trial), data = OD1RT, REML=TRUE)
nullmodel4 <- lmer( RT ~ 1 + (1|ID) + (1|Prior), data = OD1RT, REML=TRUE)
nullmodel5 <- lmer( RT ~ 1 + (1|Prior/Trial), data = OD1RT, REML=TRUE)
nullmodel6 <- lmer( RT ~ 1 + (1|ID) + (1|Prior/Trial) , data = OD1RT, REML=TRUE) #winner

anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5, nullmodel6)

model.1 <- lmer(RT ~ Group * Prior + (1|ID) + (1|Trial), data = OD1RT)
anova(model.1)
confint(model.1, oldNames = FALSE)


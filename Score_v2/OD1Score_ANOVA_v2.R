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

OD1Scores <- read_excel("Desktop/ToPeggy/Behavioral_Analysis/STATS/OD1/Score_v2/OD1Scores.xlsx")
OD1Scores$ID <- as.factor(OD1Scores$ID)  
OD1Scores$Group <- as.factor(OD1Scores$Group)  
OD1Scores$Prior <- as.factor(OD1Scores$Prior) 

#tested whether the linear mixed-effects model would give me the same results as
#the classic ANOVA. It did.
scoreTest<- lmer(OD1Scores$Score ~ OD1Scores$Group * OD1Scores$Prior )
anova(scoreTest)
eta_sq(scoreTest,partial=TRUE)
emmeans(scoreTest,list(pairwise ~ Prior), adjust = "tukey") #posthoc
emmeans(scoreTest,list(pairwise ~ Group), adjust = "tukey")
emmeans(scoreTest,pairwise~Group|Prior) #if you have a significant interaction
###
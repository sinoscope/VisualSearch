#follow the instructions at 
#https://www.datanovia.com/en/lessons/mixed-anova-in-r/

library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)

OD1Click1Cor <- read_excel("Desktop/ToPeggy/Behavioral_Analysis/STATS/OD1/Click1Cor_v2/OD1Click1Cors.xlsx")
OD1Click1Cor$ID <- as.factor(OD1Click1Cor$ID)  
OD1Click1Cor$Group <- as.factor(OD1Click1Cor$Group)  
OD1Click1Cor$Prior <- as.factor(OD1Click1Cor$Prior) 
summary(OD1Click1Cor)

OD1Click1Cor %>%
  group_by(Prior, Group) %>%
  get_summary_stats(Click1Cor, type = "mean_sd")

bxp <- ggboxplot(
  OD1Click1Cor, x = "Prior", y = "Click1Cor",
  color = "Group", palette = "jco"
)
bxp

#to check the outliers
OD1Click1Cor %>%
  group_by(Prior, Group) %>%
  identify_outliers(Click1Cor)

#to check if normality assumptions are met (p>0.05 for normal distribution)
OD1Click1Cor %>%
  group_by(Prior, Group) %>%
  shapiro_test(Click1Cor)

#QQ plot. All the points should fall approximately along the reference line, for each cell. So we can assume normality of the data.
ggqqplot(OD1Click1Cor, "Click1Cor", ggtheme = theme_bw()) +
  facet_grid(Prior ~ Group)

#Homogneity of variance assumption (For homogeneity of variances, p > 0.05)
OD1Click1Cor %>%
  group_by(Prior) %>%
  levene_test(Click1Cor ~ Group)
#Homogeneity of covariances assumption (For homogeneity of covariances, p > 0.001)
box_m(OD1Click1Cor[, "Click1Cor", drop = FALSE], OD1Click1Cor$Group)

# Two-way mixed ANOVA test
Click1Cor.aov <- anova_test(
  data = OD1Click1Cor, dv = Click1Cor, wid = ID,
  between = Group, within = Prior
)
get_anova_table(Click1Cor.aov)

#Posthoc procedure for a significant two-way interaction
# Effect of group at each prior level
OD1Click1Cor.Expected <- OD1Click1Cor[ which(OD1Click1Cor$Prior=='1'),]
OD1Click1Cor.Unexpected <- OD1Click1Cor[ which(OD1Click1Cor$Prior=='2'),]
test.expected <- t.test(Click1Cor ~ Group, data = OD1Click1Cor.Expected, var.equal = TRUE)
test.expected

test.unexpected <- t.test(Click1Cor ~ Group, data = OD1Click1Cor.Unexpected, var.equal = TRUE)
test.unexpected
# Pairwise comparisons between group levels
OD1Click1Cor.HC <- OD1Click1Cor[ which(OD1Click1Cor$Group=='1'),]
OD1Click1Cor.PD <- OD1Click1Cor[ which(OD1Click1Cor$Group=='2'),]
test.HC <- t.test(Click1Cor ~ Prior, paired = TRUE, data = OD1Click1Cor.HC)
test.HC

test.PD <- t.test(Click1Cor ~ Prior, paired = TRUE, data = OD1Click1Cor.PD)
test.PD

##Procedure for non-significant two-way interaction
OD1Click1Cor %>%
  pairwise_t_test(
    Click1Cor ~ Prior, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

OD1Click1Cor %>%
  pairwise_t_test(
    Click1Cor ~ Group, 
    p.adjust.method = "bonferroni"
  )

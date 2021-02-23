#follow the instructions at 
#https://www.datanovia.com/en/lessons/mixed-anova-in-r/

library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)

OD1meanRT <- read_excel("Desktop/ToPeggy/Behavioral_Analysis/STATS/OD1/Score_v2/OD1meanRT.xlsx")
OD1meanRT$ID <- as.factor(OD1meanRT$ID)  
OD1meanRT$Group <- as.factor(OD1meanRT$Group)  
OD1meanRT$Prior <- as.factor(OD1meanRT$Prior) 
OD1meanRT$RT <- as.numeric(OD1meanRT$RT) 

OD1meanRT %>%
  group_by(Prior, Group) %>%
  get_summary_stats(RT, type = "mean_sd")

bxp <- ggboxplot(
  OD1meanRT, x = "Prior", y = "RT",
  color = "Group", palette = "jco"
)
bxp

#to check the outliers
OD1meanRT %>%
  group_by(Prior, Group) %>%
  identify_outliers(RT)

#to check if normality assumptions are met (p>0.05 for normal distribution)
OD1meanRT %>%
  group_by(Prior, Group) %>%
  shapiro_test(RT)

#QQ plot. All the points should fall approximately along the reference line, for each cell. So we can assume normality of the data.
ggqqplot(OD1meanRT, "RT", ggtheme = theme_bw()) +
  facet_grid(Prior ~ Group)

#Homogneity of variance assumption (For homogeneity of variances, p > 0.05)
OD1meanRT %>%
  group_by(Prior) %>%
  levene_test(RT ~ Group)
#Homogeneity of covariances assumption (For homogeneity of covariances, p > 0.001)
box_m(OD1meanRT[, "RT", drop = FALSE], OD1meanRT$Group)

# Two-way mixed ANOVA test
RT.aov <- anova_test(
  data = OD1meanRT, dv = RT, wid = ID,
  between = Group, within = Prior
)
get_anova_table(RT.aov)

#Posthoc procedure for a significant two-way interaction
# Effect of group at each prior level
OD1meanRT.Expected <- OD1meanRT[ which(OD1meanRT$Prior=='1'),]
OD1meanRT.Unexpected <- OD1meanRT[ which(OD1meanRT$Prior=='2'),]
test.expected <- t.test(RT ~ Group, data = OD1meanRT.Expected, var.equal = TRUE)
test.expected

test.unexpected <- t.test(RT ~ Group, data = OD1meanRT.Unexpected, var.equal = TRUE)
test.unexpected
# Pairwise comparisons between group levels
OD1meanRT.HC <- OD1meanRT[ which(OD1meanRT$Group=='1'),]
OD1meanRT.PD <- OD1meanRT[ which(OD1meanRT$Group=='2'),]
test.HC <- t.test(RT ~ Prior, paired = TRUE, data = OD1meanRT.HC)
test.HC

test.PD <- t.test(RT ~ Prior, paired = TRUE, data = OD1meanRT.PD)
test.PD

##Procedure for non-significant two-way interaction
OD1meanRT %>%
  pairwise_t_test(
    RT ~ Prior, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

OD1meanRT %>%
  pairwise_t_test(
    RT ~ Group, 
    p.adjust.method = "bonferroni"
  )

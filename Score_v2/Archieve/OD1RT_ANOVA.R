#follow the instructions at 
#https://www.datanovia.com/en/lessons/mixed-anova-in-r/

library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)

OD1Scores <- read_excel("Desktop/ToPeggy/Behavioral_Analysis/STATS/OD1/Score_v2/OD1Scores.xlsx")
OD1Scores$ID <- as.factor(OD1Scores$ID)  
OD1Scores$Group <- as.factor(OD1Scores$Group)  
OD1Scores$Prior <- as.factor(OD1Scores$Prior) 

OD1Scores %>%
  group_by(Prior, Group) %>%
  get_summary_stats(Score, type = "mean_sd")

bxp <- ggboxplot(
  OD1Scores, x = "Prior", y = "Score",
  color = "Group", palette = "jco"
)
bxp

#to check the outliers
OD1Scores %>%
  group_by(Prior, Group) %>%
  identify_outliers(Score)

#to check if normality assumptions are met (p>0.05 for normal distribution)
OD1Scores %>%
  group_by(Prior, Group) %>%
  shapiro_test(Score)

#QQ plot. All the points should fall approximately along the reference line, for each cell. So we can assume normality of the data.
ggqqplot(OD1Scores, "Score", ggtheme = theme_bw()) +
  facet_grid(Prior ~ Group)

#Homogneity of variance assumption (For homogeneity of variances, p > 0.05)
OD1Scores %>%
  group_by(Prior) %>%
  levene_test(Score ~ Group)
#Homogeneity of covariances assumption (For homogeneity of covariances, p > 0.001)
box_m(OD1Scores[, "Score", drop = FALSE], OD1Scores$Group)

# Two-way mixed ANOVA test
score.aov <- anova_test(
  data = OD1Scores, dv = Score, wid = ID,
  between = Group, within = Prior
)
get_anova_table(score.aov)

#Posthoc procedure for a significant two-way interaction
# Effect of group at each prior level
OD1Scores.Expected <- OD1Scores[ which(OD1Scores$Prior=='1'),]
OD1Scores.Unexpected <- OD1Scores[ which(OD1Scores$Prior=='2'),]
test.expected <- t.test(Score ~ Group, data = OD1Scores.Expected, var.equal = TRUE)
test.expected

test.unexpected <- t.test(Score ~ Group, data = OD1Scores.Unexpected, var.equal = TRUE)
test.unexpected
# Pairwise comparisons between group levels
OD1Scores.HC <- OD1Scores[ which(OD1Scores$Group=='1'),]
OD1Scores.PD <- OD1Scores[ which(OD1Scores$Group=='2'),]
test.HC <- t.test(Score ~ Prior, paired = TRUE, data = OD1Scores.HC)
test.HC

test.PD <- t.test(Score ~ Prior, paired = TRUE, data = OD1Scores.PD)
test.PD

##Procedure for non-significant two-way interaction
OD1Scores %>%
  pairwise_t_test(
    Score ~ Prior, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

OD1Scores %>%
  pairwise_t_test(
    Score ~ Group, 
    p.adjust.method = "bonferroni"
  )
## Visualization: boxplots with p-values
pwc <- pwc %>% add_xy_position(x = "Prior")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(score.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
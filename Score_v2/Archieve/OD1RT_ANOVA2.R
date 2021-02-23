#follow the instructions at 
#https://www.datanovia.com/en/lessons/mixed-anova-in-r/

library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)

OD1RT <- read_excel("Desktop/ToPeggy/Behavioral_Analysis/STATS/OD1/Score_v2/OD1RT.xlsx")
OD1RT$ID <- as.factor(OD1RT$ID)  
OD1RT$Group <- as.factor(OD1RT$Group)  
OD1RT$Prior <- as.factor(OD1RT$Prior) 
OD1RT$Trial <- as.factor(OD1RT$Trial) 
OD1RT$RT <- as.numeric(OD1RT$RT) 

library(lme4)
model.1 <- lmer(RT ~ 0 + Group + Prior + (1|Trial), data = OD1RT)
summary(model.1)

library(nlmeU)
form1 <- formula(RT ~ Group)
fm <- lme(form1, random = list(Prior = ~1, Trial = ~1), data=OD1RT, na.action = na.omit, method="ML")
fm
update(fm, random=~1|Prior/Trial)
fm
fxd <- fixef(fm)
fxd

OD1RT %>%
  group_by(Group, Prior, Trial) %>%
  get_summary_stats(RT, type = "mean_sd")

bxp <- ggboxplot(
  OD1RT, x = "Prior", y = "RT",
  color = "Group", palette = "jco"
)
bxp

#to check the outliers
OD1RT %>%
  group_by(Prior, Group) %>%
  identify_outliers(RT)

#to check if normality assumptions are met (p>0.05 for normal distribution)
OD1RT %>%
  group_by(Prior, Group) %>%
  shapiro_test(RT)

#QQ plot. All the points should fall approximately along the reference line, for each cell. So we can assume normality of the data.
ggqqplot(OD1RT, "RT", ggtheme = theme_bw()) +
  facet_grid(Prior ~ Group)

#Homogneity of variance assumption (For homogeneity of variances, p > 0.05)
OD1RT %>%
  group_by(Prior) %>%
  levene_test(RT ~ Group)
#Homogeneity of covariances assumption (For homogeneity of covariances, p > 0.001)
box_m(OD1RT[, "RT", drop = FALSE], OD1RT$Group)

## Three ANOVA
RT.aov <- anova_test(
  data = OD1RT, dv = RT, wid = ID,
  between = Group, within = c(Trial, Prior)
)
get_anova_table(RT.aov)

#Posthoc procedure for a significant two-way interaction
# Effect of group at each prior level
OD1RT.Expected <- OD1RT[ which(OD1RT$Prior=='1'),]
OD1RT.Unexpected <- OD1RT[ which(OD1RT$Prior=='2'),]
test.expected <- t.test(RT ~ Group, data = OD1RT.Expected, var.equal = TRUE)
test.expected

test.unexpected <- t.test(RT ~ Group, data = OD1RT.Unexpected, var.equal = TRUE)
test.unexpected
# Pairwise comparisons between group levels
OD1RT.HC <- OD1RT[ which(OD1RT$Group=='1'),]
OD1RT.PD <- OD1RT[ which(OD1RT$Group=='2'),]
test.HC <- t.test(RT ~ Prior, paired = TRUE, data = OD1RT.HC)
test.HC

test.PD <- t.test(RT ~ Prior, paired = TRUE, data = OD1RT.PD)
test.PD

##Procedure for non-significant two-way interaction
OD1RT %>%
  pairwise_t_test(
    RT ~ Prior, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

OD1RT %>%
  pairwise_t_test(
    RT ~ Group, 
    p.adjust.method = "bonferroni"
  )
## Visualization: boxplots with p-values
pwc <- pwc %>% add_xy_position(x = "Prior")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(RT.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
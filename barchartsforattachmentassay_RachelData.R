library(tidyverse)
library(lme4)
library(multcomp)
library(ggpubr)
library(pscl)


merged_df = readxl::read_excel("C:/Users/some5124/OneDrive - Nexus365/PhD/Results/ScanR adhesion data/allWTcomparisons.xlsx")

genotypes <- c("WT","d353","d360","dboth","dporV","dremA","d2419")
merged_df$Strain <- factor(merged_df$Strain, levels = genotypes)

#merged_df$Strain <- factor(merged_df$Strain)

# contrasts(merged_df$Strain) <- cbind(c(1,-(1/6),-(1/6),-(1/6),-(1/6),-(1/6),-(1/6)),
#                                       c(0,0,0,0,0,0,0),
#                                      c(0,0,0,0,0,0,0),
#                                      c(0,0,0,0,0,0,0),
#                                      c(0,0,0,0,0,0,0),
#                                      c(0,0,0,0,0,0,0))
# 
# alpha <- 0.05
# merged_df %>% 
#   group_by(Strain) %>% 
#   summarize(mean = mean(RatioCorrected),
#             lower = mean(RatioCorrected) - qt(1- alpha/2, (n() - 1))*sd(RatioCorrected)/sqrt(n()),
#             upper = mean(RatioCorrected) + qt(1- alpha/2, (n() - 1))*sd(RatioCorrected)/sqrt(n()))

# Create a boxplot
my_plot <- ggplot(merged_df, aes(x = Strain, y = RatioCorrected)) +
  geom_boxplot(fill = "#00000000", color = "black") +
  labs(title = "Boxplot of RatioCorrected by Strain",
       x = "Strain",
       y = "RatioCorrected") +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Explicitly print the plot
print(my_plot)

#for anova
res_aov <- aov(RatioCorrected ~ Strain, data = merged_df)
summary(res_aov)
#for post-hoc Tukey's test
post_test <- glht(res_aov, linfct = mcp(Strain = "Tukey"))
summary(post_test)


